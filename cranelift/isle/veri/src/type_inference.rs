use std::collections::HashMap;

use crate::veri::{Conditions, Expr, ExprId, Type};

#[derive(Debug)]
pub enum Constraint {
    /// Expression x has the given concrete type.
    Concrete { x: ExprId, ty: Type },
    /// Expressions have the same type.
    Same { x: ExprId, y: ExprId },
    /// Expression x is a bitvector with width given by the integer expression w.
    WidthOf { x: ExprId, w: ExprId },
}

impl std::fmt::Display for Constraint {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Concrete { x, ty } => write!(f, "{} = {ty}", x.index()),
            Self::Same { x, y } => write!(f, "{} == {}", x.index(), y.index()),
            Self::WidthOf { x, w } => write!(f, "{} = width_of({})", w.index(), x.index()),
        }
    }
}

pub fn type_constraints(conditions: &Conditions) -> anyhow::Result<Vec<Constraint>> {
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

    fn build(mut self) -> anyhow::Result<Vec<Constraint>> {
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

        // TODO(mbm): term instantiations

        Ok(self.constraints)
    }

    fn veri_expr(&mut self, x: ExprId, expr: &Expr) {
        match expr {
            Expr::Const(c) => self.concrete(x, c.ty()),
            Expr::Variable(v) => {
                let ty = self.conditions.variable_type[v.index()].clone();
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
            Expr::BVAdd(y, z) => {
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
            Expr::BVConvTo(w, y) => {
                self.bit_vector(x);
                self.integer(*w);
                self.bit_vector(*y);
                self.width_of(x, *w);
            }
            Expr::WidthOf(y) => {
                self.integer(x);
                self.bit_vector(*y);
                self.width_of(*y, x);
            }
        }
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
}

pub struct Solver {
    pub expr_type: HashMap<ExprId, Type>,
}

impl Solver {
    pub fn new() -> Self {
        Self {
            expr_type: HashMap::new(),
        }
    }

    pub fn solve(&mut self, constraints: &Vec<Constraint>) -> anyhow::Result<()> {
        while self.iterate(constraints)? {}
        Ok(())
    }

    fn iterate(&mut self, constraints: &Vec<Constraint>) -> anyhow::Result<bool> {
        let mut change = false;
        for constraint in constraints {
            change |= self.constraint(constraint)?;
        }
        Ok(change)
    }

    fn constraint(&mut self, constraint: &Constraint) -> anyhow::Result<bool> {
        match constraint {
            Constraint::Concrete { x, ty } => self.set_type(*x, ty),
            Constraint::Same { x, y } => self.same(*x, *y),
            Constraint::WidthOf { x, w } => self.width_of(*x, *w),
        }
    }

    fn set_type(&mut self, x: ExprId, ty: &Type) -> anyhow::Result<bool> {
        // If we don't know a type for the expression, record it.
        if !self.expr_type.contains_key(&x) {
            self.expr_type.insert(x, ty.clone());
            return Ok(true);
        }

        // If we do, merge this type with the existing one.
        let existing = &self.expr_type[&x];
        let merged = Type::merge(existing, ty)?;
        if merged != *existing {
            self.expr_type.insert(x, merged);
            return Ok(true);
        }

        // No change.
        return Ok(false);
    }

    fn same(&mut self, x: ExprId, y: ExprId) -> anyhow::Result<bool> {
        // TODO(mbm): union find
        // TODO(mbm): simplify by initializing all expression types to unknown
        match (self.expr_type.get(&x), self.expr_type.get(&y)) {
            (None, None) => Ok(false),
            (Some(tx), None) => {
                self.expr_type.insert(y, tx.clone());
                Ok(true)
            }
            (None, Some(ty)) => {
                self.expr_type.insert(x, ty.clone());
                Ok(true)
            }
            (Some(tx), Some(ty)) => {
                if tx == ty {
                    return Ok(false);
                }
                let merged = Type::merge(tx, ty)?;
                self.expr_type.insert(x, merged.clone());
                self.expr_type.insert(y, merged.clone());
                Ok(true)
            }
        }
    }

    fn width_of(&mut self, x: ExprId, w: ExprId) -> anyhow::Result<bool> {
        log::error!("width_of not implemented");
        Ok(false)
    }
}
