use std::{
    collections::{hash_map::Entry, HashMap},
    iter::zip,
};

use crate::veri::{Call, Conditions, Const, Expr, ExprId, Type};

#[derive(Debug)]
pub enum Constraint {
    /// Expression x has the given concrete type.
    Concrete { x: ExprId, ty: Type },
    /// Expressions have the same type.
    Same { x: ExprId, y: ExprId },
    /// Expression x is a bitvector with width given by the integer expression w.
    WidthOf { x: ExprId, w: ExprId },
    /// Expression x is an integer with known constant value v.
    IntValue { x: ExprId, v: i128 },
}

impl std::fmt::Display for Constraint {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Concrete { x, ty } => write!(f, "{} = {ty}", x.index()),
            Self::Same { x, y } => write!(f, "{} == {}", x.index(), y.index()),
            Self::WidthOf { x, w } => write!(f, "{} = width_of({})", w.index(), x.index()),
            Self::IntValue { x, v } => write!(f, "{} = int({v})", x.index()),
        }
    }
}

pub fn type_constraints(conditions: &Conditions) -> Vec<Constraint> {
    let builder = ConstraintsBuilder::new(conditions);
    builder.build()
}

struct ConstraintsBuilder<'a> {
    conditions: &'a Conditions,

    constraints: Vec<Constraint>,
}

impl<'a> ConstraintsBuilder<'a> {
    fn new(conditions: &'a Conditions) -> Self {
        Self {
            conditions,
            constraints: Vec::new(),
        }
    }

    fn build(mut self) -> Vec<Constraint> {
        // Expression constraints.
        for (i, expr) in self.conditions.exprs.iter().enumerate() {
            self.veri_expr(ExprId(i), expr);
        }

        // Assumptions.
        for a in &self.conditions.assumptions {
            self.boolean(*a);
        }

        // Assertions.
        for a in &self.conditions.assertions {
            self.boolean(*a);
        }

        // Calls.
        for call in &self.conditions.calls {
            self.call(call);
        }

        self.constraints
    }

    fn veri_expr(&mut self, x: ExprId, expr: &Expr) {
        match expr {
            Expr::Const(Const::Int(v)) => {
                self.concrete(x, Type::Int);
                self.int_value(x, *v);
            }
            Expr::Const(c) => {
                self.concrete(x, c.ty());
            }
            Expr::Variable(v) => {
                let ty = self.conditions.variables[v.index()].ty.clone();
                self.concrete(x, ty);
            }
            Expr::And(y, z) | Expr::Or(y, z) | Expr::Imp(y, z) => {
                self.boolean(x);
                self.boolean(*y);
                self.boolean(*z);
            }
            Expr::Eq(y, z) => {
                self.boolean(x);
                self.same(*y, *z);
            }
            Expr::Lte(y, z) => {
                self.boolean(x);
                self.integer(*y);
                self.integer(*z);
            }
            Expr::BVUlt(y, z) => {
                self.boolean(x);
                self.bit_vector(*y);
                self.bit_vector(*z);

                self.same(*y, *z);
            }
            Expr::BVNeg(y) => {
                self.bit_vector(x);
                self.bit_vector(*y);

                self.same(x, *y);
            }
            Expr::BVAdd(y, z) | Expr::BVAnd(y, z) => {
                self.bit_vector(x);
                self.bit_vector(*y);
                self.bit_vector(*z);

                self.same(x, *y);
                self.same(x, *z);
            }
            Expr::Conditional(c, t, e) => {
                self.boolean(*c);
                self.same(x, *t);
                self.same(x, *e);
            }
            Expr::BVZeroExt(w, y) | Expr::BVSignExt(w, y) | Expr::BVConvTo(w, y) => {
                self.bit_vector(x);
                self.integer(*w);
                self.bit_vector(*y);
                self.width_of(x, *w);
            }
            Expr::BVExtract(h, l, y) => {
                let width = 1 + h
                    .checked_sub(*l)
                    .expect("high bit should not be less than low bit");
                self.bit_vector_of_width(x, width);
                self.bit_vector(*y);
            }
            Expr::WidthOf(y) => {
                self.integer(x);
                self.bit_vector(*y);
                self.width_of(*y, x);
            }
        }
    }

    fn call(&mut self, call: &Call) {
        if call.signatures.is_empty() {
            return;
        }

        // TODO(mbm): support multiple term signatures
        if call.signatures.len() > 1 {
            todo!("multiple term signatures");
        }
        let sig = &call.signatures[0];

        // Arguments.
        assert_eq!(call.args.len(), sig.args.len());
        for (a, ty) in zip(&call.args, &sig.args) {
            self.concrete(*a, ty.clone());
        }

        // Return.
        self.concrete(call.ret, sig.ret.clone());
    }

    fn bit_vector_of_width(&mut self, x: ExprId, width: usize) {
        self.concrete(x, Type::BitVector(Some(width)));
    }

    fn bit_vector(&mut self, x: ExprId) {
        self.concrete(x, Type::BitVector(None));
    }

    fn integer(&mut self, x: ExprId) {
        self.concrete(x, Type::Int);
    }

    fn boolean(&mut self, x: ExprId) {
        self.concrete(x, Type::Bool);
    }

    fn concrete(&mut self, x: ExprId, ty: Type) {
        self.constraints.push(Constraint::Concrete { x, ty });
    }

    fn same(&mut self, x: ExprId, y: ExprId) {
        self.constraints.push(Constraint::Same { x, y });
    }

    fn width_of(&mut self, x: ExprId, w: ExprId) {
        self.constraints.push(Constraint::WidthOf { x, w });
    }

    fn int_value(&mut self, x: ExprId, v: i128) {
        self.constraints.push(Constraint::IntValue { x, v });
    }
}

#[derive(Default)]
pub struct Assignment {
    pub expr_type: HashMap<ExprId, Type>,
    pub int_value: HashMap<ExprId, i128>,
}

impl Assignment {
    pub fn new() -> Self {
        Self {
            expr_type: HashMap::new(),
            int_value: HashMap::new(),
        }
    }

    pub fn validate(&self) -> anyhow::Result<()> {
        // Assigned types should all be concrete.
        for (x, ty) in &self.expr_type {
            if !ty.is_concrete() {
                anyhow::bail!(
                    "non-concrete type {ty} assigned to expression {x}",
                    x = x.index()
                );
            }
        }

        // TODO(mbm): everything with an integer value should have integer type

        Ok(())
    }

    pub fn satisfies_constraints(&self, constraints: &[Constraint]) -> anyhow::Result<()> {
        constraints
            .iter()
            .try_for_each(|c| self.satisfies_constraint(c))
    }

    pub fn satisfies_constraint(&self, constraint: &Constraint) -> anyhow::Result<()> {
        match *constraint {
            Constraint::Concrete { x, ref ty } => self.expect_expr_type_refinement(x, ty),
            Constraint::Same { x, y } => self.expect_same(x, y),
            Constraint::WidthOf { x, w } => self.expect_width_of(x, w),
            Constraint::IntValue { x, v } => self.expect_int_value(x, v),
        }
    }

    // TODO(mbm): better name for expect_expr_type?
    pub fn expect_expr_type(&self, x: ExprId) -> anyhow::Result<&Type> {
        self.expr_type.get(&x).ok_or(anyhow::format_err!(
            "expression {x} missing type assignment",
            x = x.index()
        ))
    }

    fn expect_expr_type_refinement(&self, x: ExprId, base: &Type) -> anyhow::Result<()> {
        let ty = self.expect_expr_type(x)?;
        if !ty.is_refinement_of(base) {
            anyhow::bail!("expected type {ty} to be refinement of {base}")
        }
        Ok(())
    }

    fn expect_same(&self, x: ExprId, y: ExprId) -> anyhow::Result<()> {
        let tx = self.expect_expr_type(x)?;
        let ty = self.expect_expr_type(y)?;
        if tx != ty {
            anyhow::bail!(
                "expressions {x} and {y} should have same type: got {tx} and {ty}",
                x = x.index(),
                y = y.index()
            )
        }

        // TODO(mbm): if integers, the values should match

        Ok(())
    }

    fn expect_width_of(&self, x: ExprId, w: ExprId) -> anyhow::Result<()> {
        // Expression x should be a concrete bitvector.
        let tx = self.expect_expr_type(x)?;
        let &Type::BitVector(Some(width)) = tx else {
            anyhow::bail!(
                "expression {x} should be a bit-vector of known width; got {tx}",
                x = x.index()
            );
        };

        // Expression w should be an integer equal to the width.
        self.expect_int_value(w, width.try_into().unwrap())?;

        Ok(())
    }

    fn expect_int_value(&self, x: ExprId, expect: i128) -> anyhow::Result<()> {
        // Expression x should be an integer.
        let tx = self.expect_expr_type(x)?;
        if *tx != Type::Int {
            anyhow::bail!(
                "expression {x} should be an integer; got {tx}",
                x = x.index()
            );
        };

        // Should have an integer assignment.
        let got = self.int_value.get(&x).copied().ok_or(anyhow::format_err!(
            "expression {x} missing integer value",
            x = x.index()
        ))?;

        if got != expect {
            anyhow::bail!("expected integer value {expect}; got {got}");
        }

        Ok(())
    }

    pub fn pretty_print(&self, conditions: &Conditions) {
        for (i, expr) in conditions.exprs.iter().enumerate() {
            print!("{i}:\t");
            match self.expr_type.get(&ExprId(i)) {
                None => print!("false\t-"),
                Some(typ) => print!("{}\t{typ}", typ.is_concrete()),
            }
            println!("\t{expr}");
        }
    }
}

#[derive(Default)]
pub struct Solver {
    assignment: Assignment,
}

impl Solver {
    pub fn new() -> Self {
        Self {
            assignment: Assignment::new(),
        }
    }

    pub fn solve(mut self, constraints: &Vec<Constraint>) -> anyhow::Result<Assignment> {
        // Iterate until no changes.
        while self.iterate(constraints)? {}

        // Check assignment is reasonable.
        self.assignment.validate()?;
        self.assignment.satisfies_constraints(constraints)?;

        Ok(self.assignment)
    }

    fn iterate(&mut self, constraints: &Vec<Constraint>) -> anyhow::Result<bool> {
        let mut change = false;
        for constraint in constraints {
            // TODO(mbm): remove satisfied constraints from list
            change |= self.constraint(constraint)?;
        }
        Ok(change)
    }

    fn constraint(&mut self, constraint: &Constraint) -> anyhow::Result<bool> {
        log::trace!("process type constraint: {constraint}");
        match constraint {
            Constraint::Concrete { x, ty } => self.set_type(*x, ty),
            Constraint::Same { x, y } => self.same(*x, *y),
            Constraint::WidthOf { x, w } => self.width_of(*x, *w),
            Constraint::IntValue { x, v } => self.set_int_value(*x, *v),
        }
    }

    fn set_type(&mut self, x: ExprId, ty: &Type) -> anyhow::Result<bool> {
        // If we don't know a type for the expression, record it.
        if let Entry::Vacant(e) = self.assignment.expr_type.entry(x) {
            e.insert(ty.clone());
            return Ok(true);
        }

        // If we do, merge this type with the existing one.
        let existing = &self.assignment.expr_type[&x];
        let merged = Type::merge(existing, ty)?;
        if merged != *existing {
            self.assignment.expr_type.insert(x, merged);
            return Ok(true);
        }

        // No change.
        Ok(false)
    }

    fn same(&mut self, x: ExprId, y: ExprId) -> anyhow::Result<bool> {
        // TODO(mbm): union find
        // TODO(mbm): simplify by initializing all expression types to unknown
        match (
            self.assignment.expr_type.get(&x),
            self.assignment.expr_type.get(&y),
        ) {
            (None, None) => Ok(false),
            (Some(tx), None) => {
                self.assignment.expr_type.insert(y, tx.clone());
                Ok(true)
            }
            (None, Some(ty)) => {
                self.assignment.expr_type.insert(x, ty.clone());
                Ok(true)
            }
            (Some(tx), Some(ty)) => {
                if tx == ty {
                    return Ok(false);
                }
                let merged = Type::merge(tx, ty)?;
                self.assignment.expr_type.insert(x, merged.clone());
                self.assignment.expr_type.insert(y, merged.clone());
                Ok(true)
            }
        }
    }

    fn width_of(&mut self, x: ExprId, w: ExprId) -> anyhow::Result<bool> {
        match (
            self.assignment.expr_type.get(&x),
            self.assignment.int_value.get(&w),
        ) {
            (Some(&Type::BitVector(Some(width))), _) => {
                self.set_int_value(w, width.try_into().unwrap())
            }
            (_, Some(&v)) => self.set_type(x, &Type::BitVector(Some(v.try_into().unwrap()))),
            _ => Ok(false),
        }
    }

    fn set_int_value(&mut self, x: ExprId, v: i128) -> anyhow::Result<bool> {
        match self.assignment.int_value.get(&x) {
            None => {
                self.assignment.int_value.insert(x, v);
                Ok(true)
            }
            Some(u) if *u == v => Ok(false),
            Some(_) => anyhow::bail!("incompatible integer values"),
        }
    }
}
