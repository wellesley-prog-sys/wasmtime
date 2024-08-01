use std::{
    cmp::Ordering,
    collections::{hash_map::Entry, HashMap},
    iter::zip,
};

use crate::veri::{Call, Conditions, Const, Expr, ExprId, Type, Width};

#[derive(Clone, Debug, Eq, PartialEq)]
pub enum TypeValue {
    Type(Type),
    Value(Const),
}

impl TypeValue {
    pub fn ty(&self) -> Type {
        match self {
            TypeValue::Type(ty) => ty.clone(),
            TypeValue::Value(c) => c.ty(),
        }
    }

    pub fn refines_type(&self, ty: &Type) -> bool {
        self >= &Self::Type(ty.clone())
    }

    pub fn merge(left: &Self, right: &Self) -> anyhow::Result<Self> {
        Ok(match left.partial_cmp(right) {
            Some(Ordering::Greater) => left.clone(),
            Some(Ordering::Less | Ordering::Equal) => right.clone(),
            None => anyhow::bail!("incompatible type values: {left} and {right}"),
        })
    }
}

impl PartialOrd for TypeValue {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (TypeValue::Type(l), TypeValue::Type(r)) => l.partial_cmp(r),
            (TypeValue::Type(ty), TypeValue::Value(v)) if ty <= &v.ty() => Some(Ordering::Less),
            (TypeValue::Value(v), TypeValue::Type(ty)) if &v.ty() >= ty => Some(Ordering::Greater),
            (TypeValue::Value(l), TypeValue::Value(r)) if l == r => Some(Ordering::Equal),
            _ => None,
        }
    }
}

impl std::fmt::Display for TypeValue {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            TypeValue::Type(ty) => ty.fmt(f),
            TypeValue::Value(c) => c.fmt(f),
        }
    }
}

#[derive(Debug)]
pub enum Constraint {
    /// Expression x has the given type.
    Type { x: ExprId, ty: Type },
    /// Expressions have the same type.
    Same { x: ExprId, y: ExprId },
    /// Expression x is a bitvector with width given by the integer expression w.
    WidthOf { x: ExprId, w: ExprId },
    /// Expression x has known constant value v.
    Value { x: ExprId, c: Const },
}

impl std::fmt::Display for Constraint {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Type { x, ty } => write!(f, "type({}) = {ty}", x.index()),
            Self::Same { x, y } => write!(f, "type({}) = type({})", x.index(), y.index()),
            Self::WidthOf { x, w } => write!(f, "{} = width_of({})", w.index(), x.index()),
            Self::Value { x, c } => write!(f, "{} = value({c})", x.index()),
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
                self.integer(x);
                self.int_value(x, *v);
            }
            Expr::Const(c) => {
                self.ty(x, c.ty());
            }
            Expr::Variable(v) => {
                let ty = self.conditions.variables[v.index()].ty.clone();
                self.ty(x, ty);
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
            self.ty(*a, ty.clone());
        }

        // Return.
        self.ty(call.ret, sig.ret.clone());
    }

    fn bit_vector_of_width(&mut self, x: ExprId, width: usize) {
        self.ty(x, Type::BitVector(Width::Bits(width)));
    }

    fn bit_vector(&mut self, x: ExprId) {
        self.ty(x, Type::BitVector(Width::Unknown));
    }

    fn integer(&mut self, x: ExprId) {
        self.ty(x, Type::Int);
    }

    fn boolean(&mut self, x: ExprId) {
        self.ty(x, Type::Bool);
    }

    fn ty(&mut self, x: ExprId, ty: Type) {
        self.constraints.push(Constraint::Type { x, ty });
    }

    fn same(&mut self, x: ExprId, y: ExprId) {
        self.constraints.push(Constraint::Same { x, y });
    }

    fn width_of(&mut self, x: ExprId, w: ExprId) {
        self.constraints.push(Constraint::WidthOf { x, w });
    }

    fn int_value(&mut self, x: ExprId, v: i128) {
        self.value(x, Const::Int(v));
    }

    fn value(&mut self, x: ExprId, c: Const) {
        self.constraints.push(Constraint::Value { x, c });
    }
}

#[derive(Default)]
pub struct Assignment {
    pub expr_type_value: HashMap<ExprId, TypeValue>,
}

impl Assignment {
    pub fn new() -> Self {
        Self {
            expr_type_value: HashMap::new(),
        }
    }

    pub fn validate(&self) -> anyhow::Result<()> {
        // Assigned types should all be concrete.
        for (x, tv) in &self.expr_type_value {
            let ty = tv.ty();
            if !ty.is_concrete() {
                anyhow::bail!(
                    "non-concrete type {ty} assigned to expression {x}",
                    x = x.index()
                );
            }
        }

        Ok(())
    }

    pub fn satisfies_constraints(&self, constraints: &[Constraint]) -> anyhow::Result<()> {
        constraints
            .iter()
            .try_for_each(|c| self.satisfies_constraint(c))
    }

    pub fn satisfies_constraint(&self, constraint: &Constraint) -> anyhow::Result<()> {
        match *constraint {
            Constraint::Type { x, ref ty } => self.expect_expr_type_refinement(x, ty),
            Constraint::Same { x, y } => self.expect_same(x, y),
            Constraint::WidthOf { x, w } => self.expect_width_of(x, w),
            Constraint::Value { x, ref c } => self.expect_value(x, c),
        }
    }

    pub fn assignment(&self, x: ExprId) -> anyhow::Result<&TypeValue> {
        self.expr_type_value.get(&x).ok_or(anyhow::format_err!(
            "expression {x} missing assignment",
            x = x.index()
        ))
    }

    fn expect_expr_type_refinement(&self, x: ExprId, base: &Type) -> anyhow::Result<()> {
        let tv = self.assignment(x)?;
        if !tv.refines_type(base) {
            anyhow::bail!("expected type {tv} to be refinement of {base}")
        }
        Ok(())
    }

    fn expect_same(&self, x: ExprId, y: ExprId) -> anyhow::Result<()> {
        let tx = self.assignment(x)?.ty();
        let ty = self.assignment(y)?.ty();
        if tx != ty {
            anyhow::bail!(
                "expressions {x} and {y} should have same type: got {tx} and {ty}",
                x = x.index(),
                y = y.index()
            )
        }
        Ok(())
    }

    pub fn bit_vector_width(&self, x: ExprId) -> anyhow::Result<usize> {
        let tyx = self.assignment(x)?.ty();
        let Type::BitVector(Width::Bits(width)) = tyx else {
            anyhow::bail!(
                "expression {x} should be a bit-vector of known width; got {tyx}",
                x = x.index()
            );
        };
        Ok(width)
    }

    fn expect_width_of(&self, x: ExprId, w: ExprId) -> anyhow::Result<()> {
        // Expression x should be a concrete bitvector.
        let width = self.bit_vector_width(x)?;

        // Expression w should be an integer equal to the width.
        self.expect_value(w, &Const::Int(width.try_into().unwrap()))?;

        Ok(())
    }

    pub fn value(&self, x: ExprId) -> anyhow::Result<&Const> {
        let tvx = self.assignment(x)?;
        let TypeValue::Value(c) = tvx else {
            anyhow::bail!(
                "expression {x} should be a known value; got {tvx}",
                x = x.index()
            );
        };
        Ok(c)
    }

    pub fn int_value(&self, x: ExprId) -> anyhow::Result<i128> {
        let c = self.value(x)?;
        let &Const::Int(v) = c else {
            anyhow::bail!(
                "expression {x} should be a known integer value; got {c}",
                x = x.index()
            );
        };
        Ok(v)
    }

    fn expect_value(&self, x: ExprId, expect: &Const) -> anyhow::Result<()> {
        let got = self.value(x)?;
        if got != expect {
            anyhow::bail!("expected value {expect}; got {got}");
        }
        Ok(())
    }

    pub fn pretty_print(&self, conditions: &Conditions) {
        for (i, expr) in conditions.exprs.iter().enumerate() {
            print!("{i}:\t");
            match self.expr_type_value.get(&ExprId(i)) {
                None => print!("false\t-"),
                Some(tv) => print!("{}\t{tv}", tv.ty().is_concrete()),
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
        //self.assignment.validate()?;
        //self.assignment.satisfies_constraints(constraints)?;

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
            Constraint::Type { x, ty } => self.set_type(*x, ty.clone()),
            Constraint::Same { x, y } => self.same(*x, *y),
            Constraint::WidthOf { x, w } => self.width_of(*x, *w),
            Constraint::Value { x, c } => self.set_value(*x, c.clone()),
        }
    }

    fn set_type_value(&mut self, x: ExprId, tv: TypeValue) -> anyhow::Result<bool> {
        log::trace!("set type value: {x:?} = {tv:?}");

        // If we don't have an assignment for the expression, record it.
        if let Entry::Vacant(e) = self.assignment.expr_type_value.entry(x) {
            e.insert(tv);
            return Ok(true);
        }

        // If we do, merge this type value with the existing one.
        let existing = &self.assignment.expr_type_value[&x];
        let merged = TypeValue::merge(existing, &tv)?;
        if merged != *existing {
            self.assignment.expr_type_value.insert(x, merged);
            return Ok(true);
        }

        // No change.
        Ok(false)
    }

    fn set_type(&mut self, x: ExprId, ty: Type) -> anyhow::Result<bool> {
        self.set_type_value(x, TypeValue::Type(ty))
    }

    fn same(&mut self, x: ExprId, y: ExprId) -> anyhow::Result<bool> {
        // TODO(mbm): union find
        // TODO(mbm): simplify by initializing all expression types to unknown
        match (
            self.assignment.expr_type_value.get(&x).cloned(),
            self.assignment.expr_type_value.get(&y).cloned(),
        ) {
            (None, None) => Ok(false),
            (Some(tvx), None) => self.set_type(y, tvx.ty()),
            (None, Some(tvy)) => self.set_type(x, tvy.ty()),
            (Some(tvx), Some(tvy)) => Ok(self.set_type(x, tvy.ty())? | self.set_type(y, tvx.ty())?),
        }
    }

    fn width_of(&mut self, x: ExprId, w: ExprId) -> anyhow::Result<bool> {
        match (
            self.assignment.expr_type_value.get(&x),
            self.assignment.expr_type_value.get(&w),
        ) {
            (
                Some(
                    &TypeValue::Type(Type::BitVector(Width::Bits(width)))
                    | &TypeValue::Value(Const::BitVector(width, _)),
                ),
                _,
            ) => self.set_int_value(w, width.try_into().unwrap()),
            (_, Some(&TypeValue::Value(Const::Int(v)))) => {
                self.set_type(x, Type::BitVector(Width::Bits(v.try_into().unwrap())))
            }
            _ => Ok(false),
        }
    }

    fn set_int_value(&mut self, x: ExprId, v: i128) -> anyhow::Result<bool> {
        self.set_value(x, Const::Int(v))
    }

    fn set_value(&mut self, x: ExprId, c: Const) -> anyhow::Result<bool> {
        self.set_type_value(x, TypeValue::Value(c))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::testing::{assert_partial_order_properties, assert_strictly_increasing};

    #[test]
    fn test_type_value_partial_order_bit_vector() {
        assert_strictly_increasing(&[
            TypeValue::Type(Type::Unknown),
            TypeValue::Type(Type::BitVector(Width::Unknown)),
            TypeValue::Type(Type::BitVector(Width::Bits(64))),
            TypeValue::Value(Const::BitVector(64, 42)),
        ])
    }

    #[test]
    fn test_type_value_partial_order_int() {
        assert_strictly_increasing(&[
            TypeValue::Type(Type::Unknown),
            TypeValue::Type(Type::Int),
            TypeValue::Value(Const::Int(42)),
        ])
    }

    #[test]
    fn test_type_value_partial_order_bool() {
        assert_strictly_increasing(&[
            TypeValue::Type(Type::Unknown),
            TypeValue::Type(Type::Bool),
            TypeValue::Value(Const::Bool(true)),
        ])
    }

    #[test]
    fn test_type_value_partial_order_properties() {
        assert_partial_order_properties(&[
            // Unknown
            TypeValue::Type(Type::Unknown),
            // BitVectors
            TypeValue::Type(Type::BitVector(Width::Unknown)),
            TypeValue::Type(Type::BitVector(Width::Bits(32))),
            TypeValue::Value(Const::BitVector(32, 42)),
            TypeValue::Value(Const::BitVector(32, 43)),
            TypeValue::Type(Type::BitVector(Width::Bits(64))),
            TypeValue::Value(Const::BitVector(64, 42)),
            TypeValue::Value(Const::BitVector(64, 43)),
            // Int
            TypeValue::Type(Type::Int),
            TypeValue::Value(Const::Int(42)),
            TypeValue::Value(Const::Int(43)),
            // Bool
            TypeValue::Type(Type::Bool),
            TypeValue::Value(Const::Bool(false)),
            TypeValue::Value(Const::Bool(true)),
        ]);
    }
}
