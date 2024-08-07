use std::{cmp::Ordering, collections::HashMap, iter::zip};

use anyhow::Context as _;
use easy_smt::{Context, Response, SExpr, SExprData};

use crate::{
    type_inference::Assignment,
    veri::{Conditions, Const, Expr, ExprId, Type, Width},
};

type Model = HashMap<ExprId, Const>;

#[derive(Debug, PartialEq, Eq)]
pub enum Verdict {
    Applicable,
    Inapplicable,
    Success,
    Failure(Model),
    Unknown,
}

impl std::fmt::Display for Verdict {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        f.write_str(match self {
            Self::Applicable => "applicable",
            Self::Inapplicable => "inapplicable",
            Self::Success => "success",
            Self::Failure(_) => "failure",
            Self::Unknown => "unknown",
        })
    }
}

impl Verdict {
    pub fn failed(&self) -> bool {
        matches!(self, Verdict::Inapplicable | Verdict::Failure(_))
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

    pub fn check_assumptions_feasibility(&mut self) -> anyhow::Result<Verdict> {
        // Enter solver context frame.
        self.smt.push()?;

        // Assumptions
        let assumptions = self.all(&self.conditions.assumptions);
        self.smt.assert(assumptions)?;

        // Check
        let verdict = match self.smt.check()? {
            Response::Sat => Verdict::Applicable,
            Response::Unsat => Verdict::Inapplicable,
            Response::Unknown => Verdict::Unknown,
        };

        // Leave solver context frame.
        self.smt.pop()?;

        Ok(verdict)
    }

    pub fn check_verification_condition(&mut self) -> anyhow::Result<Verdict> {
        // Enter solver context frame.
        self.smt.push()?;

        // Verification Condition
        self.verification_condition()?;

        // Check
        let verdict = match self.smt.check()? {
            Response::Sat => Verdict::Failure(self.model()?),
            Response::Unsat => Verdict::Success,
            Response::Unknown => Verdict::Unknown,
        };

        // Leave solver context frame.
        self.smt.pop()?;

        Ok(verdict)
    }

    pub fn model(&mut self) -> anyhow::Result<Model> {
        let xs: Vec<_> = (0..self.conditions.exprs.len()).map(ExprId).collect();
        let expr_atoms = xs.iter().map(|x| self.expr_atom(*x)).collect();
        let values = self.smt.get_value(expr_atoms)?;
        let consts = values
            .iter()
            .map(|(_, ref v)| self.const_from_sexpr(v))
            .collect::<anyhow::Result<Vec<_>>>()?;
        Ok(zip(xs, consts).collect())
    }

    fn declare_expr(&mut self, x: ExprId) -> anyhow::Result<()> {
        // Determine expression type value.
        let tv = self.assignment.try_assignment(x)?;

        // Map to corresponding SMT2 type.
        let sort = self.type_to_sort(&tv.ty())?;

        // Declare.
        self.smt.declare_const(self.expr_name(x), sort)?;

        Ok(())
    }

    fn type_to_sort(&self, ty: &Type) -> anyhow::Result<SExpr> {
        match *ty {
            Type::BitVector(Width::Bits(width)) => {
                Ok(self.smt.bit_vec_sort(self.smt.numeral(width)))
            }
            Type::Int => Ok(self.smt.int_sort()),
            Type::Bool => Ok(self.smt.bool_sort()),
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
        match *expr {
            Expr::Const(ref c) => self.constant(c),
            Expr::And(x, y) => Ok(self.smt.and(self.expr_atom(x), self.expr_atom(y))),
            Expr::Or(x, y) => Ok(self.smt.or(self.expr_atom(x), self.expr_atom(y))),
            Expr::Imp(x, y) => Ok(self.smt.imp(self.expr_atom(x), self.expr_atom(y))),
            Expr::Eq(x, y) => Ok(self.smt.eq(self.expr_atom(x), self.expr_atom(y))),
            Expr::Lte(x, y) => Ok(self.smt.lte(self.expr_atom(x), self.expr_atom(y))),
            Expr::BVUlt(x, y) => Ok(self.smt.bvult(self.expr_atom(x), self.expr_atom(y))),
            Expr::BVNot(x) => Ok(self.smt.bvnot(self.expr_atom(x))),
            Expr::BVNeg(x) => Ok(self.smt.bvneg(self.expr_atom(x))),
            Expr::BVAdd(x, y) => Ok(self.smt.bvadd(self.expr_atom(x), self.expr_atom(y))),
            Expr::BVSub(x, y) => Ok(self.smt.bvsub(self.expr_atom(x), self.expr_atom(y))),
            Expr::BVMul(x, y) => Ok(self.smt.bvmul(self.expr_atom(x), self.expr_atom(y))),
            Expr::BVAnd(x, y) => Ok(self.smt.bvand(self.expr_atom(x), self.expr_atom(y))),
            Expr::BVShl(x, y) => Ok(self.smt.bvshl(self.expr_atom(x), self.expr_atom(y))),
            Expr::BVLShr(x, y) => Ok(self.smt.bvlshr(self.expr_atom(x), self.expr_atom(y))),
            Expr::BVAShr(x, y) => Ok(self.smt.bvashr(self.expr_atom(x), self.expr_atom(y))),
            Expr::Conditional(c, t, e) => {
                Ok(self
                    .smt
                    .ite(self.expr_atom(c), self.expr_atom(t), self.expr_atom(e)))
            }
            Expr::BVZeroExt(w, x) => self.bv_zero_ext(w, x),
            Expr::BVSignExt(w, x) => self.bv_sign_ext(w, x),
            Expr::BVConvTo(w, x) => self.bv_conv_to(w, x),
            Expr::BVExtract(h, l, x) => Ok(self.extend(h, l, self.expr_atom(x))),
            Expr::BVConcat(x, y) => Ok(self.smt.concat(self.expr_atom(x), self.expr_atom(y))),
            Expr::Int2BV(w, x) => Ok(self.int2bv(w, self.expr_atom(x))),
            Expr::WidthOf(x) => self.width_of(x),
            _ => todo!("expr to smt: {expr:?}"),
        }
    }

    fn constant(&self, constant: &Const) -> anyhow::Result<SExpr> {
        match *constant {
            Const::Bool(true) => Ok(self.smt.true_()),
            Const::Bool(false) => Ok(self.smt.false_()),
            Const::Int(v) => Ok(self.smt.numeral(v)),
            Const::BitVector(w, v) => Ok(self.smt.binary(w, v)),
        }
    }

    fn bv_zero_ext(&self, w: ExprId, x: ExprId) -> anyhow::Result<SExpr> {
        // TODO(mbm): dedupe logic with bv_sign_ext and bv_conv_to?

        // Destination width expression should have known integer value.
        let dst: usize = self
            .assignment
            .try_int_value(w)
            .context("destination width of zero_ext expression should have known integer value")?
            .try_into()
            .expect("width should be representable as usize");

        // Expression type should be a bit-vector of known width.
        let src = self
            .assignment
            .bit_vector_width(x)
            .context("source of zero_ext expression should be a bit-vector of known width")?;

        // Build zero_extend expression.
        let padding = dst
            .checked_sub(src)
            .expect("cannot zero extend to smaller width");
        Ok(self.zero_extend(padding, self.expr_atom(x)))
    }

    fn bv_sign_ext(&self, w: ExprId, x: ExprId) -> anyhow::Result<SExpr> {
        // TODO(mbm): dedupe logic with bv_conv_to?

        // Destination width expression should have known integer value.
        let dst: usize = self
            .assignment
            .try_int_value(w)
            .context("destination width of sign_ext expression should have known integer value")?
            .try_into()
            .expect("width should be representable as usize");

        // Expression type should be a bit-vector of known width.
        let src = self
            .assignment
            .bit_vector_width(x)
            .context("source of sign_ext expression should be a bit-vector of known width")?;

        // Build zero_extend expression.
        let padding = dst
            .checked_sub(src)
            .expect("cannot zero extend to smaller width");
        Ok(self.sign_extend(padding, self.expr_atom(x)))
    }

    fn bv_conv_to(&self, w: ExprId, x: ExprId) -> anyhow::Result<SExpr> {
        // Destination width expression should have known integer value.
        let dst: usize = self
            .assignment
            .try_int_value(w)
            .context("destination width of conv_to expression should have known integer value")?
            .try_into()
            .expect("width should be representable as usize");

        // Expression type should be a bit-vector of known width.
        let src = self
            .assignment
            .bit_vector_width(x)
            .context("source of conv_to expression should be a bit-vector of known width")?;

        // Handle depending on source and destination widths.
        match dst.cmp(&src) {
            Ordering::Greater => todo!("conv_to extend"),
            Ordering::Less => todo!("conv_to extract"),
            Ordering::Equal => Ok(self.expr_atom(x)),
        }
    }

    fn width_of(&self, x: ExprId) -> anyhow::Result<SExpr> {
        // QUESTION(mbm): should width_of expressions be elided or replaced after type inference?

        // Expression type should be a bit-vector of known width.
        let width = self
            .assignment
            .bit_vector_width(x)
            .context("target of width_of expression should be a bit-vector of known width")?;

        // Substitute known constant width.
        Ok(self.smt.numeral(width))
    }

    fn verification_condition(&mut self) -> anyhow::Result<()> {
        // (not (<assumptions> => <assertions>))
        let assumptions = self.all(&self.conditions.assumptions);
        let assertions = self.all(&self.conditions.assertions);
        let vc = self.smt.imp(assumptions, assertions);
        self.smt.assert(self.smt.not(vc))?;
        Ok(())
    }

    /// Zero-extend an SMT bit vector to a wider bit vector by adding `padding`
    /// zeroes to the front.
    fn zero_extend(&self, padding: usize, v: SExpr) -> SExpr {
        if padding == 0 {
            return v;
        }
        self.smt.list(vec![
            self.smt.list(vec![
                self.smt.atoms().und,
                self.smt.atom("zero_extend"),
                self.smt.numeral(padding),
            ]),
            v,
        ])
    }

    /// Sign-extend an SMT bit vector to a wider bit vector.
    fn sign_extend(&self, padding: usize, v: SExpr) -> SExpr {
        if padding == 0 {
            return v;
        }
        self.smt.list(vec![
            self.smt.list(vec![
                self.smt.atoms().und,
                self.smt.atom("sign_extend"),
                self.smt.numeral(padding),
            ]),
            v,
        ])
    }

    fn extend(&self, high_bit: usize, low_bit: usize, v: SExpr) -> SExpr {
        assert!(low_bit <= high_bit);
        self.smt
            .extract(high_bit.try_into().unwrap(), low_bit.try_into().unwrap(), v)
    }

    /// Convert an SMT integer to a bit vector of a given width.
    fn int2bv(&self, width: usize, value: SExpr) -> SExpr {
        self.smt.list(vec![
            self.smt.list(vec![
                self.smt.atoms().und,
                self.smt.atom("int2bv"),
                self.smt.numeral(width),
            ]),
            value,
        ])
    }

    /// Parse a constant SMT literal.
    fn const_from_sexpr(&self, sexpr: &SExpr) -> anyhow::Result<Const> {
        let atom = match self.smt.get(*sexpr) {
            SExprData::Atom(a) => a,
            SExprData::List(_) => anyhow::bail!("non-atomic smt expression is not a constant"),
        };

        if atom == "true" {
            Ok(Const::Bool(true))
        } else if atom == "false" {
            Ok(Const::Bool(false))
        } else if let Some(x) = atom.strip_prefix("#x") {
            Ok(Const::BitVector(x.len() * 4, i128::from_str_radix(x, 16)?))
        } else if let Some(x) = atom.strip_prefix("#b") {
            Ok(Const::BitVector(x.len(), i128::from_str_radix(x, 2)?))
        } else {
            todo!("unrecognized constant: {atom}")
        }
    }

    fn all(&self, xs: &[ExprId]) -> SExpr {
        self.smt.and_many(xs.iter().map(|x| self.expr_atom(*x)))
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
