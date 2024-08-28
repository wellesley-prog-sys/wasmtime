use std::{
    cmp::Ordering,
    collections::{hash_map::Entry, HashMap},
    iter::zip,
    vec,
};

use cranelift_isle::sema::TermId;

use crate::{
    spec::Signature,
    types::{Compound, Const, Type, Width},
    veri::{Call, Conditions, Expr, ExprId, Symbolic},
};

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

    fn as_value(&self) -> Option<&Const> {
        match self {
            TypeValue::Value(c) => Some(c),
            _ => None,
        }
    }

    pub fn refines_type(&self, ty: &Type) -> bool {
        self >= &Self::Type(ty.clone())
    }

    pub fn merge(left: &Self, right: &Self) -> Option<Self> {
        match left.partial_cmp(right) {
            Some(Ordering::Greater) => Some(left.clone()),
            Some(Ordering::Less | Ordering::Equal) => Some(right.clone()),
            None => None,
        }
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

/// Boolean expression or its negation.
#[derive(Debug, Clone)]
pub enum Literal {
    Var(ExprId),
    Not(ExprId),
}

impl std::fmt::Display for Literal {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Literal::Var(x) => write!(f, "{}", x.index()),
            Literal::Not(x) => write!(f, "\u{00AC}{}", x.index()),
        }
    }
}

#[derive(Debug, Clone)]
pub enum Constraint {
    /// Expression x has the given type.
    Type { x: ExprId, ty: Type },
    /// Expressions have the same type.
    SameType { x: ExprId, y: ExprId },
    /// Expressions have the same type and value.
    Identical { x: ExprId, y: ExprId },
    /// Expression x is a bitvector with width given by the integer expression w.
    WidthOf { x: ExprId, w: ExprId },
    /// Bitvector x is the concatenation bitvectors l and r.
    Concat { x: ExprId, l: ExprId, r: ExprId },
    /// Expression x has known constant value v.
    Value { x: ExprId, c: Const },
    /// Constraint conditioned on a boolean.
    Implies { c: ExprId, then: Box<Constraint> },
    /// Clause is a disjunction that must hold.
    Clause { literals: Vec<Literal> },
}

impl std::fmt::Display for Constraint {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Type { x, ty } => write!(f, "type({}) = {ty}", x.index()),
            Self::SameType { x, y } => write!(f, "type({}) == type({})", x.index(), y.index()),
            Self::Identical { x, y } => write!(f, "{} == {}", x.index(), y.index()),
            Self::WidthOf { x, w } => write!(f, "{} = width_of({})", w.index(), x.index()),
            Self::Concat { x, l, r } => write!(f, "{} = {}:{}", x.index(), l.index(), r.index()),
            Self::Value { x, c } => write!(f, "{} = value({c})", x.index()),
            Self::Implies { c, then } => write!(f, "{} => {then}", c.index()),
            Self::Clause { literals } => write!(
                f,
                "clause({})",
                literals
                    .iter()
                    .map(ToString::to_string)
                    .collect::<Vec<_>>()
                    .join(" \u{2228} ")
            ),
        }
    }
}

#[derive(Clone)]
pub enum Choice {
    TermInstantiation(TermId, Signature),
}

impl std::fmt::Display for Choice {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Choice::TermInstantiation(term_id, sig) => {
                write!(f, "term_instantiation({}, {sig})", term_id.index())
            }
        }
    }
}

#[derive(Clone)]
pub struct Arm {
    choice: Choice,
    constraints: Vec<Constraint>,
}

#[derive(Default, Clone)]
pub struct Branch {
    arms: Vec<Arm>,
}

#[derive(Default)]
pub struct System {
    choices: Vec<Choice>,
    constraints: Vec<Constraint>,
    branches: Vec<Branch>,
}

impl System {
    fn fork(&self) -> Vec<System> {
        let mut branches = self.branches.clone();
        let branch = branches.pop().expect("should have at least one branch");

        let mut children = Vec::new();
        for arm in &branch.arms {
            let mut constraints = self.constraints.clone();
            constraints.extend(arm.constraints.iter().cloned());

            let mut choices = self.choices.clone();
            choices.push(arm.choice.clone());

            children.push(System {
                constraints,
                choices,
                branches: branches.clone(),
            })
        }

        children
    }

    pub fn pretty_print(&self) {
        println!("system {{");

        // Choices
        println!("\tchoices = [");
        for choice in &self.choices {
            println!("\t\t{choice}");
        }
        println!("\t]");

        // Constraints
        println!("\tconstraints = [");
        for constraint in &self.constraints {
            println!("\t\t{constraint}");
        }
        println!("\t]");

        // Branches
        for branch in &self.branches {
            println!("\tbranch {{");
            for arm in &branch.arms {
                println!("\t\t{choice} => [", choice = arm.choice);
                for constraint in &arm.constraints {
                    println!("\t\t\t{constraint}");
                }
                println!("\t\t]");
            }
            println!("\t}}");
        }

        println!("}}");
    }
}

pub fn type_constraint_system(conditions: &Conditions) -> System {
    let builder = SystemBuilder::new(conditions);
    builder.build()
}

struct SystemBuilder<'a> {
    conditions: &'a Conditions,

    system: System,
    arm: Option<Arm>,
}

impl<'a> SystemBuilder<'a> {
    fn new(conditions: &'a Conditions) -> Self {
        Self {
            conditions,
            system: System::default(),
            arm: None,
        }
    }

    fn build(mut self) -> System {
        // Expression constraints.
        for (i, expr) in self.conditions.exprs.iter().enumerate() {
            self.veri_expr(ExprId(i), expr);
        }

        // Assumptions.
        for a in &self.conditions.assumptions {
            self.boolean_value(*a, true);
        }

        // Assertions.
        for a in &self.conditions.assertions {
            self.boolean(*a);
        }

        // Calls.
        for call in &self.conditions.calls {
            self.call(call);
        }

        self.system
    }

    fn veri_expr(&mut self, x: ExprId, expr: &Expr) {
        match expr {
            Expr::Const(c) => {
                self.value(x, c.clone());
            }
            Expr::Variable(v) => {
                let ty = self.conditions.variables[v.index()].ty.clone();
                self.ty(x, ty);
            }
            Expr::And(y, z) => {
                // TODO(mbm): clause implies boolean
                self.boolean(x);
                self.boolean(*y);
                self.boolean(*z);

                // ((NOT X) OR Y)
                self.clause(vec![Literal::Not(x), Literal::Var(*y)]);
                // ((NOT X) OR Z)
                self.clause(vec![Literal::Not(x), Literal::Var(*z)]);
                // (X OR (NOT Y) OR (NOT Z))
                self.clause(vec![Literal::Var(x), Literal::Not(*y), Literal::Not(*z)]);
            }
            Expr::Or(y, z) => {
                self.boolean(x);
                self.boolean(*y);
                self.boolean(*z);

                // ((NOT X) OR Y OR Z)
                self.clause(vec![Literal::Not(x), Literal::Var(*y), Literal::Var(*z)]);
                // (X OR (NOT Y))
                self.clause(vec![Literal::Var(x), Literal::Not(*y)]);
                // (X OR (NOT Z))
                self.clause(vec![Literal::Var(x), Literal::Not(*z)]);
            }
            Expr::Imp(y, z) => {
                self.boolean(x);
                self.boolean(*y);
                self.boolean(*z);

                // ((NOT X) OR (NOT Y) OR Z)
                self.clause(vec![Literal::Not(x), Literal::Not(*y), Literal::Var(*z)]);
                // (X OR Y)
                self.clause(vec![Literal::Var(x), Literal::Var(*y)]);
                // (X OR (NOT Z))
                self.clause(vec![Literal::Var(x), Literal::Not(*y)]);
            }
            Expr::Eq(y, z) => {
                self.boolean(x);
                self.same_type(*y, *z);
                self.constraint(Constraint::Implies {
                    c: x,
                    then: Box::new(Constraint::Identical { x: *y, y: *z }),
                });
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

                self.same_type(*y, *z);
            }
            Expr::BVNot(y) | Expr::BVNeg(y) => {
                self.bit_vector(x);
                self.bit_vector(*y);

                self.same_type(x, *y);
            }
            Expr::BVAdd(y, z)
            | Expr::BVSub(y, z)
            | Expr::BVMul(y, z)
            | Expr::BVAnd(y, z)
            | Expr::BVShl(y, z)
            | Expr::BVLShr(y, z)
            | Expr::BVAShr(y, z) => {
                self.bit_vector(x);
                self.bit_vector(*y);
                self.bit_vector(*z);

                self.same_type(x, *y);
                self.same_type(x, *z);
            }
            Expr::Conditional(c, t, e) => {
                self.boolean(*c);
                self.same_type(x, *t);
                self.same_type(x, *e);
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
            Expr::BVConcat(y, z) => {
                self.bit_vector(x);
                self.bit_vector(*y);
                self.bit_vector(*z);
                self.concat(x, *y, *z);
            }
            Expr::Int2BV(w, y) => {
                self.bit_vector_of_width(x, *w);
                self.integer(*y);
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

        // Branch for the choice of term signature.
        //
        // We do this even for the case of a single signature, since it will
        // preserve metadata about where the type assignment came from.
        self.branch();

        for sig in &call.signatures {
            // Branch arm for
            self.push_arm(Choice::TermInstantiation(call.term, sig.clone()));

            // Arguments.
            assert_eq!(call.args.len(), sig.args.len());
            for (a, ty) in zip(&call.args, &sig.args) {
                self.symbolic(a, ty.clone());
            }

            // Return.
            self.symbolic(&call.ret, sig.ret.clone());

            // Pop branch arm.
            self.pop();
        }
    }

    fn symbolic(&mut self, v: &Symbolic, ty: Compound) {
        match (v, ty) {
            (Symbolic::Scalar(x), Compound::Primitive(ty)) => self.ty(*x, ty),
            (Symbolic::Struct(_), Compound::Struct(_)) => todo!("struct type constraints"),
            // QUESTION(mbm): should Option and Tuple be in a different enum so they don't appear in type inference?
            (Symbolic::Option(_), _) => unimplemented!("option types unsupported"),
            (Symbolic::Tuple(_), _) => unimplemented!("tuple types unsupported"),
            (v, ty) => unreachable!("type mismatch: {v} of type {ty}"),
        }
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
        self.constraint(Constraint::Type { x, ty });
    }

    fn same_type(&mut self, x: ExprId, y: ExprId) {
        self.constraint(Constraint::SameType { x, y });
    }

    fn width_of(&mut self, x: ExprId, w: ExprId) {
        self.constraint(Constraint::WidthOf { x, w });
    }

    fn concat(&mut self, x: ExprId, l: ExprId, r: ExprId) {
        self.constraint(Constraint::Concat { x, l, r });
    }

    fn boolean_value(&mut self, x: ExprId, b: bool) {
        self.value(x, Const::Bool(b));
    }

    fn value(&mut self, x: ExprId, c: Const) {
        self.constraint(Constraint::Value { x, c });
    }

    fn clause(&mut self, literals: Vec<Literal>) {
        self.constraint(Constraint::Clause { literals })
    }

    fn constraint(&mut self, constraint: Constraint) {
        let current = match self.arm.as_mut() {
            Some(arm) => &mut arm.constraints,
            None => &mut self.system.constraints,
        };
        current.push(constraint)
    }

    fn branch(&mut self) {
        self.system.branches.push(Branch::default());
    }

    fn push_arm(&mut self, choice: Choice) {
        assert!(self.arm.is_none());
        self.arm = Some(Arm {
            choice,
            constraints: Vec::new(),
        });
    }

    fn pop(&mut self) {
        let arm = self.arm.take().expect("must have arm");
        self.system
            .branches
            .last_mut()
            .expect("should have branch")
            .arms
            .push(arm);
    }
}

#[derive(Default, Clone)]
pub struct Assignment {
    pub expr_type_value: HashMap<ExprId, TypeValue>,
}

impl Assignment {
    pub fn new() -> Self {
        Self {
            expr_type_value: HashMap::new(),
        }
    }

    pub fn is_concrete(&self) -> bool {
        self.expr_type_value
            .values()
            .all(|tv| tv.ty().is_concrete())
    }

    pub fn satisfies_constraints(&self, constraints: &[Constraint]) -> anyhow::Result<()> {
        constraints
            .iter()
            .try_for_each(|c| self.satisfies_constraint(c))
    }

    pub fn satisfies_constraint(&self, constraint: &Constraint) -> anyhow::Result<()> {
        match *constraint {
            Constraint::Type { x, ref ty } => self.expect_expr_type_refinement(x, ty),
            Constraint::SameType { x, y } => self.expect_same_type(x, y),
            Constraint::Identical { x, y } => self.expect_identical(x, y),
            Constraint::WidthOf { x, w } => self.expect_width_of(x, w),
            Constraint::Concat { x, l, r } => self.expect_concat(x, l, r),
            Constraint::Value { x, ref c } => self.expect_value(x, c),
            Constraint::Implies { c, ref then } => self.expect_implies(c, then),
            Constraint::Clause { ref literals } => self.expect_clause(literals),
        }
    }

    pub fn assignment(&self, x: ExprId) -> Option<&TypeValue> {
        self.expr_type_value.get(&x)
    }

    pub fn try_assignment(&self, x: ExprId) -> anyhow::Result<&TypeValue> {
        self.assignment(x).ok_or(anyhow::format_err!(
            "expression {x} missing assignment",
            x = x.index()
        ))
    }

    pub fn value(&self, x: ExprId) -> Option<&Const> {
        self.assignment(x)?.as_value()
    }

    pub fn try_value(&self, x: ExprId) -> anyhow::Result<&Const> {
        self.value(x).ok_or(anyhow::format_err!(
            "expression {x} should be a known value",
            x = x.index()
        ))
    }

    pub fn bool_value(&self, x: ExprId) -> Option<bool> {
        self.value(x)?.as_bool()
    }

    pub fn int_value(&self, x: ExprId) -> Option<i128> {
        self.value(x)?.as_int()
    }

    pub fn try_int_value(&self, x: ExprId) -> anyhow::Result<i128> {
        self.int_value(x).ok_or(anyhow::format_err!(
            "expression {x} should be a known integer value",
            x = x.index()
        ))
    }

    pub fn literal(&self, lit: &Literal) -> Option<bool> {
        match *lit {
            Literal::Var(x) => self.bool_value(x),
            Literal::Not(x) => Some(!self.bool_value(x)?),
        }
    }

    fn expect_expr_type_refinement(&self, x: ExprId, base: &Type) -> anyhow::Result<()> {
        let tv = self.try_assignment(x)?;
        if !tv.refines_type(base) {
            anyhow::bail!("expected type {tv} to be refinement of {base}")
        }
        Ok(())
    }

    fn expect_same_type(&self, x: ExprId, y: ExprId) -> anyhow::Result<()> {
        let tx = self.try_assignment(x)?.ty();
        let ty = self.try_assignment(y)?.ty();
        if tx != ty {
            anyhow::bail!(
                "expressions {x} and {y} should have same type: got {tx} and {ty}",
                x = x.index(),
                y = y.index()
            )
        }
        Ok(())
    }

    fn expect_identical(&self, x: ExprId, y: ExprId) -> anyhow::Result<()> {
        let tvx = self.try_assignment(x)?;
        let tvy = self.try_assignment(y)?;
        if tvx != tvy {
            anyhow::bail!(
                "expressions {x} and {y} should be identical: got {tvx} and {tvy}",
                x = x.index(),
                y = y.index()
            )
        }
        Ok(())
    }

    pub fn bit_vector_width(&self, x: ExprId) -> Option<usize> {
        self.assignment(x)?.ty().as_bit_vector_width()?.as_bits()
    }

    pub fn try_bit_vector_width(&self, x: ExprId) -> anyhow::Result<usize> {
        self.bit_vector_width(x).ok_or(anyhow::format_err!(
            "expression {x} should be a bit-vector of known width",
            x = x.index()
        ))
    }

    fn expect_width_of(&self, x: ExprId, w: ExprId) -> anyhow::Result<()> {
        // Expression x should be a concrete bitvector.
        let width = self.try_bit_vector_width(x)?;

        // Expression w should be an integer equal to the width.
        self.expect_value(w, &Const::Int(width.try_into().unwrap()))?;

        Ok(())
    }

    fn expect_concat(&self, x: ExprId, l: ExprId, r: ExprId) -> anyhow::Result<()> {
        // All inputs should be bitvectors of known width.
        let x_width = self.try_bit_vector_width(x)?;
        let l_width = self.try_bit_vector_width(l)?;
        let r_width = self.try_bit_vector_width(r)?;

        // Verify x width is the sum of input widths.
        let concat_width = l_width
            .checked_add(r_width)
            .expect("concat width should not overflow");
        if x_width != concat_width {
            anyhow::bail!(
                "expression {x} should be the concatenation of {l} and {r}",
                x = x.index(),
                l = l.index(),
                r = r.index()
            );
        }

        Ok(())
    }

    fn expect_value(&self, x: ExprId, expect: &Const) -> anyhow::Result<()> {
        let got = self.try_value(x)?;
        if got != expect {
            anyhow::bail!("expected value {expect}; got {got}");
        }
        Ok(())
    }

    fn expect_implies(&self, c: ExprId, then: &Constraint) -> anyhow::Result<()> {
        if self.bool_value(c) == Some(true) {
            self.satisfies_constraint(then)
        } else {
            Ok(())
        }
    }

    fn expect_clause(&self, literals: &[Literal]) -> anyhow::Result<()> {
        for literal in literals {
            match self.literal(literal) {
                Some(true) | None => {
                    return Ok(());
                }
                Some(false) => {
                    continue;
                }
            }
        }
        anyhow::bail!("false clause");
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

pub enum Status {
    Solved,
    Inapplicable,
    Underconstrained,
}

impl std::fmt::Display for Status {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Status::Solved => write!(f, "solved"),
            Status::Inapplicable => write!(f, "inapplicable"),
            Status::Underconstrained => write!(f, "underconstrained"),
        }
    }
}

pub struct Solution {
    pub status: Status,
    pub choices: Vec<Choice>,
    pub assignment: Assignment,
}

#[derive(Clone)]
pub struct Solver {
    assignment: Assignment,
}

impl Solver {
    pub fn new() -> Self {
        Self {
            assignment: Assignment::new(),
        }
    }

    pub fn solve(mut self, system: &System) -> Vec<Solution> {
        // Deduce assignments from constraints.
        let result = self.propagate(&system.constraints);
        if let Err(status) = result {
            return vec![Solution {
                status,
                choices: system.choices.clone(),
                assignment: self.assignment,
            }];
        }

        // Done?
        if system.branches.is_empty() {
            let status = if self.assignment.is_concrete() {
                Status::Solved
            } else {
                Status::Underconstrained
            };
            return vec![Solution {
                status,
                choices: system.choices.clone(),
                assignment: self.assignment,
            }];
        };

        // Fork.
        let mut solutions = Vec::new();
        for child in system.fork() {
            let sub = self.clone();
            solutions.extend(sub.solve(&child));
        }

        solutions
    }

    fn propagate(&mut self, constraints: &[Constraint]) -> Result<(), Status> {
        // Iterate until no changes.
        while self.iterate(constraints)? {}
        Ok(())
    }

    fn iterate(&mut self, constraints: &[Constraint]) -> Result<bool, Status> {
        let mut change = false;
        for constraint in constraints {
            // TODO(mbm): remove satisfied constraints from list
            change |= self.constraint(constraint)?;
        }
        Ok(change)
    }

    fn constraint(&mut self, constraint: &Constraint) -> Result<bool, Status> {
        log::trace!("process type constraint: {constraint}");
        match constraint {
            Constraint::Type { x, ty } => self.set_type(*x, ty.clone()),
            Constraint::SameType { x, y } => self.same_type(*x, *y),
            Constraint::Identical { x, y } => self.identical(*x, *y),
            Constraint::WidthOf { x, w } => self.width_of(*x, *w),
            Constraint::Concat { x, l, r } => self.concat(*x, *l, *r),
            Constraint::Value { x, c } => self.set_value(*x, c.clone()),
            Constraint::Implies { c, then } => self.implies(*c, then),
            Constraint::Clause { literals } => self.clause(literals),
        }
    }

    fn set_type_value(&mut self, x: ExprId, tv: TypeValue) -> Result<bool, Status> {
        log::trace!("set type value: {x:?} = {tv:?}");

        // If we don't have an assignment for the expression, record it.
        if let Entry::Vacant(e) = self.assignment.expr_type_value.entry(x) {
            e.insert(tv);
            return Ok(true);
        }

        // If we do, merge this type value with the existing one.
        let existing = &self.assignment.expr_type_value[&x];
        let merged = TypeValue::merge(existing, &tv).ok_or(Status::Inapplicable)?;
        if merged != *existing {
            self.assignment.expr_type_value.insert(x, merged);
            return Ok(true);
        }

        // No change.
        Ok(false)
    }

    fn set_type(&mut self, x: ExprId, ty: Type) -> Result<bool, Status> {
        self.set_type_value(x, TypeValue::Type(ty))
    }

    fn set_bit_vector_width(&mut self, x: ExprId, bits: usize) -> Result<bool, Status> {
        self.set_type(x, Type::BitVector(Width::Bits(bits)))
    }

    fn same_type(&mut self, x: ExprId, y: ExprId) -> Result<bool, Status> {
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

    fn identical(&mut self, x: ExprId, y: ExprId) -> Result<bool, Status> {
        match (
            self.assignment.expr_type_value.get(&x).cloned(),
            self.assignment.expr_type_value.get(&y).cloned(),
        ) {
            (None, None) => Ok(false),
            (Some(tvx), None) => self.set_type_value(y, tvx),
            (None, Some(tvy)) => self.set_type_value(x, tvy),
            (Some(tvx), Some(tvy)) => {
                Ok(self.set_type_value(x, tvy)? | self.set_type_value(y, tvx)?)
            }
        }
    }

    fn width_of(&mut self, x: ExprId, w: ExprId) -> Result<bool, Status> {
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
                self.set_bit_vector_width(x, v.try_into().unwrap())
            }
            _ => Ok(false),
        }
    }

    fn concat(&mut self, x: ExprId, l: ExprId, r: ExprId) -> Result<bool, Status> {
        match (
            self.assignment.bit_vector_width(x),
            self.assignment.bit_vector_width(l),
            self.assignment.bit_vector_width(r),
        ) {
            // Two known: we can infer the third.
            (None, Some(lw), Some(rw)) => {
                // Width equation: |x| = |l| + |r|
                self.set_bit_vector_width(x, lw + rw)
            }
            (Some(xw), None, Some(rw)) => {
                // Width equation: |l| = |x| - |r|
                self.set_bit_vector_width(l, xw.checked_sub(rw).ok_or(Status::Inapplicable)?)
            }
            (Some(xw), Some(lw), None) => {
                // Width equation: |r| = |x| - |l|
                self.set_bit_vector_width(r, xw.checked_sub(lw).ok_or(Status::Inapplicable)?)
            }

            // Zero or one known: cannot deduce anything.
            (None, None, None)
            | (None, None, Some(_))
            | (None, Some(_), None)
            | (Some(_), None, None) => Ok(false),

            // All known: verify correctness.
            (Some(x), Some(y), Some(z)) => {
                if x != y + z {
                    Err(Status::Inapplicable)
                } else {
                    Ok(false)
                }
            }
        }
    }

    fn implies(&mut self, c: ExprId, then: &Constraint) -> Result<bool, Status> {
        if self.assignment.bool_value(c) == Some(true) {
            self.constraint(then)
        } else {
            Ok(false)
        }
    }

    fn clause(&mut self, literals: &[Literal]) -> Result<bool, Status> {
        // Check if we can propogate the value of a single unknown literal.
        let mut unknown = None;
        for literal in literals {
            match (self.assignment.literal(literal), unknown) {
                // One disjunction is known true. Can't deduce anything else.
                (Some(true), _) => {
                    return Ok(false);
                }
                // Known false: also deduce nothing.
                (Some(false), _) => {
                    continue;
                }
                // First unknown literal.
                (None, None) => {
                    unknown = Some(literal);
                }
                // More than one unknown literal: deduce nothing.
                (None, Some(_)) => {
                    return Ok(false);
                }
            }
        }

        // Assign true.
        match unknown {
            Some(lit) => self.set_literal(lit, true),
            None => Ok(false),
        }
    }

    fn set_literal(&mut self, lit: &Literal, b: bool) -> Result<bool, Status> {
        match *lit {
            Literal::Var(x) => self.set_bool_value(x, b),
            Literal::Not(x) => self.set_bool_value(x, !b),
        }
    }

    fn set_bool_value(&mut self, x: ExprId, b: bool) -> Result<bool, Status> {
        self.set_value(x, Const::Bool(b))
    }

    fn set_int_value(&mut self, x: ExprId, v: i128) -> Result<bool, Status> {
        self.set_value(x, Const::Int(v))
    }

    fn set_value(&mut self, x: ExprId, c: Const) -> Result<bool, Status> {
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
        ]);
    }

    #[test]
    fn test_type_value_partial_order_int() {
        assert_strictly_increasing(&[
            TypeValue::Type(Type::Unknown),
            TypeValue::Type(Type::Int),
            TypeValue::Value(Const::Int(42)),
        ]);
    }

    #[test]
    fn test_type_value_partial_order_bool() {
        assert_strictly_increasing(&[
            TypeValue::Type(Type::Unknown),
            TypeValue::Type(Type::Bool),
            TypeValue::Value(Const::Bool(true)),
        ]);
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
