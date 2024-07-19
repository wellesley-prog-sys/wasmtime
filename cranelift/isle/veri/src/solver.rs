use easy_smt::{Context, Response, SExpr};

use crate::{
    type_inference::Assignment,
    veri::{Conditions, Const, Expr, ExprId, Type},
};

#[derive(Debug)]
pub enum Verdict {
    Success,
    Failure,
    Unknown,
}

impl Verdict {
    fn from_response_expect_unsat(response: Response) -> Self {
        match response {
            Response::Sat => Self::Failure,
            Response::Unsat => Self::Success,
            Response::Unknown => Self::Unknown,
        }
    }
}

impl std::fmt::Display for Verdict {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(match self {
            Self::Success => "success",
            Self::Failure => "failure",
            Self::Unknown => "unknown",
        })
    }
}

pub struct Solver<'a> {
    smt: Context,
    conditions: &'a Conditions,
    assignment: &'a Assignment,
}

impl<'a> Solver<'a> {
    pub fn new(smt: Context, conditions: &'a Conditions, assignment: &'a Assignment) -> Self {
        Self {
            smt,
            conditions,
            assignment,
        }
    }

    pub fn encode(&mut self) -> anyhow::Result<()> {
        // Expressions
        for (i, expr) in self.conditions.exprs.iter().enumerate() {
            let x = ExprId(i);
            self.declare_expr(x)?;
            if !expr.is_variable() {
                self.assign_expr(x, expr)?;
            }
        }

        Ok(())
    }

    pub fn check_verification_condition(&mut self) -> anyhow::Result<Verdict> {
        // Enter solver context frame.
        self.smt.push()?;

        // Verification Condition
        self.verification_condition()?;

        // Check
        let response = self.smt.check()?;

        // Leave solver context frame.
        self.smt.pop()?;

        Ok(Verdict::from_response_expect_unsat(response))
    }

    fn declare_expr(&mut self, x: ExprId) -> anyhow::Result<()> {
        // Determine expression type.
        let ty = self.assignment.expect_expr_type(x)?;

        // Map to corresponding SMT2 type.
        let sort = self.type_to_sort(ty)?;

        // Declare.
        self.smt.declare_const(self.expr_name(x), sort)?;

        Ok(())
    }

    fn type_to_sort(&self, ty: &Type) -> anyhow::Result<SExpr> {
        match ty {
            &Type::BitVector(Some(width)) => Ok(self.smt.bit_vec_sort(self.smt.numeral(width))),
            &Type::Int => Ok(self.smt.int_sort()),
            &Type::Bool => Ok(self.smt.bool_sort()),
            _ => anyhow::bail!("no smt2 sort for type {ty}"),
        }
    }

    fn assign_expr(&mut self, x: ExprId, expr: &Expr) -> anyhow::Result<()> {
        Ok(self.smt.assert(
            self.smt
                .eq(self.smt.atom(self.expr_name(x)), self.expr_to_smt(expr)?),
        )?)
    }

    fn expr_to_smt(&self, expr: &Expr) -> anyhow::Result<SExpr> {
        match expr {
            &Expr::Const(ref c) => self.constant(c),
            &Expr::And(x, y) => Ok(self.smt.and(self.expr_atom(x), self.expr_atom(y))),
            &Expr::Or(x, y) => Ok(self.smt.or(self.expr_atom(x), self.expr_atom(y))),
            &Expr::Imp(x, y) => Ok(self.smt.imp(self.expr_atom(x), self.expr_atom(y))),
            &Expr::Eq(x, y) => Ok(self.smt.eq(self.expr_atom(x), self.expr_atom(y))),
            &Expr::Lte(x, y) => Ok(self.smt.lte(self.expr_atom(x), self.expr_atom(y))),
            &Expr::BVAdd(x, y) => Ok(self.smt.bvadd(self.expr_atom(x), self.expr_atom(y))),
            &Expr::Conditional(c, t, e) => {
                Ok(self
                    .smt
                    .ite(self.expr_atom(c), self.expr_atom(t), self.expr_atom(e)))
            }
            &Expr::BVConvTo(w, x) => self.conv_to(w, x),
            &Expr::WidthOf(x) => self.width_of(x),
            _ => todo!("expr to smt: {expr:?}"),
        }
    }

    fn constant(&self, constant: &Const) -> anyhow::Result<SExpr> {
        match constant {
            &Const::Bool(true) => Ok(self.smt.true_()),
            &Const::Bool(false) => Ok(self.smt.false_()),
            &Const::Int(v) => Ok(self.smt.numeral(v)),
            &Const::BitVector(w, v) => Ok(self.smt.binary(w, v)),
        }
    }

    fn conv_to(&self, w: ExprId, x: ExprId) -> anyhow::Result<SExpr> {
        // Destination width expression should have known integer value.
        let dst: usize = self
            .assignment
            .int_value
            .get(&w)
            .copied()
            .ok_or(anyhow::anyhow!(
                "destination width of conv_to expression should have known integer value"
            ))?
            .try_into()
            .expect("width should be representable as usize");

        // Expression type should be a bit-vector of known width.
        let tx = self.assignment.expect_expr_type(x)?;
        let &Type::BitVector(Some(src)) = tx else {
            anyhow::bail!("source of conv_to expression should be a bit-vector of known width");
        };

        // Handle depending on source and destination widths.
        if dst > src {
            todo!("conv_to extend")
        } else if dst < src {
            todo!("conv_to extract")
        } else {
            Ok(self.expr_atom(x))
        }
    }

    fn width_of(&self, x: ExprId) -> anyhow::Result<SExpr> {
        // QUESTION(mbm): should width_of expressions be elided or replaced after type inference?

        // Expression type should be a bit-vector of known width.
        let tx = self.assignment.expect_expr_type(x)?;
        let &Type::BitVector(Some(width)) = tx else {
            anyhow::bail!("target of width_of expression should be a bit-vector of known width");
        };

        // Substitute known constant width.
        Ok(self.smt.numeral(width))
    }

    fn verification_condition(&mut self) -> anyhow::Result<()> {
        // Assumptions
        let assumptions = self.smt.and_many(
            self.conditions
                .assumptions
                .iter()
                .map(|a| self.expr_atom(*a)),
        );

        // Assertions
        let assertions = self.smt.and_many(
            self.conditions
                .assertions
                .iter()
                .map(|a| self.expr_atom(*a)),
        );

        // (<assumptions> => <assertions>)
        let vc = self.smt.imp(assumptions, assertions);

        // Assert negation.
        self.smt.assert(self.smt.not(vc))?;

        Ok(())
    }

    fn expr_atom(&self, x: ExprId) -> SExpr {
        self.smt.atom(self.expr_name(x))
    }

    fn expr_name(&self, x: ExprId) -> String {
        // TODO(mbm): ensure expression name uniqueness
        let expr = &self.conditions.exprs[x.index()];
        match expr {
            Expr::Variable(v) => self.conditions.variables[v.index()].name.clone(),
            _ => format!("e{}", x.index()),
        }
    }
}
