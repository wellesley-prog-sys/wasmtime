use cranelift_isle::{
    ast::{self, Defs, Ident, SpecOp},
    sema::{TermEnv, TermId, TypeEnv},
};
use std::collections::HashMap;

/// Higher-level type, not including bitwidths.
pub enum Type {
    /// The expression is a bitvector, currently modeled in the
    /// logic QF_BV https://SMT-LIB.cs.uiowa.edu/version1/logics/QF_BV.smt
    BitVector,

    /// Use if the width is known
    BitVectorWithWidth(usize),

    /// The expression is an integer (currently used for ISLE type,
    /// representing bitwidth)
    Int,

    /// The expression is a boolean.
    Bool,
}

/// Type-specified constants
pub struct Const {
    pub ty: Type,
    // TODO(mbm): support constants larger than 127 bits
    pub value: i128,
}

/// Spec expression.
pub enum Expr {
    // Terminal nodes
    Var(Ident),
    Const(Const),
    True,
    False,

    // Get the width of a bitvector
    WidthOf(Box<Expr>),

    // Boolean operations
    Not(Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Imp(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    Lte(Box<Expr>, Box<Expr>),
    Lt(Box<Expr>, Box<Expr>),

    BVSgt(Box<Expr>, Box<Expr>),
    BVSgte(Box<Expr>, Box<Expr>),
    BVSlt(Box<Expr>, Box<Expr>),
    BVSlte(Box<Expr>, Box<Expr>),
    BVUgt(Box<Expr>, Box<Expr>),
    BVUgte(Box<Expr>, Box<Expr>),
    BVUlt(Box<Expr>, Box<Expr>),
    BVUlte(Box<Expr>, Box<Expr>),

    BVSaddo(Box<Expr>, Box<Expr>),

    // Bitvector operations
    //      Note: these follow the naming conventions of the SMT theory of bitvectors:
    //      https://SMT-LIB.cs.uiowa.edu/version1/logics/QF_BV.smt
    // Unary operators
    BVNeg(Box<Expr>),
    BVNot(Box<Expr>),
    CLZ(Box<Expr>),
    A64CLZ(Box<Expr>, Box<Expr>),
    CLS(Box<Expr>),
    A64CLS(Box<Expr>, Box<Expr>),
    Rev(Box<Expr>),
    A64Rev(Box<Expr>, Box<Expr>),
    BVPopcnt(Box<Expr>),

    // Binary operators
    BVMul(Box<Expr>, Box<Expr>),
    BVUDiv(Box<Expr>, Box<Expr>),
    BVSDiv(Box<Expr>, Box<Expr>),
    BVAdd(Box<Expr>, Box<Expr>),
    BVSub(Box<Expr>, Box<Expr>),
    BVUrem(Box<Expr>, Box<Expr>),
    BVSrem(Box<Expr>, Box<Expr>),
    BVAnd(Box<Expr>, Box<Expr>),
    BVOr(Box<Expr>, Box<Expr>),
    BVXor(Box<Expr>, Box<Expr>),
    BVRotl(Box<Expr>, Box<Expr>),
    BVRotr(Box<Expr>, Box<Expr>),
    BVShl(Box<Expr>, Box<Expr>),
    BVShr(Box<Expr>, Box<Expr>),
    BVAShr(Box<Expr>, Box<Expr>),

    // Includes type
    BVSubs(Box<Expr>, Box<Expr>, Box<Expr>),

    // Conversions
    // Zero extend, static and dynamic width
    BVZeroExtTo(usize, Box<Expr>),
    BVZeroExtToVarWidth(Box<Expr>, Box<Expr>),

    // Sign extend, static and dynamic width
    BVSignExtTo(usize, Box<Expr>),
    BVSignExtToVarWidth(Box<Expr>, Box<Expr>),

    // Extract specified bits
    BVExtract(usize, usize, Box<Expr>),

    // Concat two bitvectors
    BVConcat(Vec<Expr>),

    // Convert integer to bitvector
    BVIntToBv(usize, Box<Expr>),

    // Convert bitvector to integer
    BVToInt(Box<Expr>),

    // Conversion to wider/narrower bits, without an explicit extend
    BVConvTo(usize, Box<Expr>),
    // Allow the destination width to be symbolic.
    BVConvToVarWidth(Box<Expr>, Box<Expr>),

    // Conditional if-then-else
    Conditional(Box<Expr>, Box<Expr>, Box<Expr>),

    // Switch
    Switch(Box<Expr>, Vec<(Expr, Expr)>),
}

macro_rules! unary_expr {
    ($expr:path, $args:ident, $pos:ident) => {{
        assert_eq!(
            $args.len(),
            1,
            "Unexpected number of args for unary operator at {:?}",
            $pos
        );
        $expr(Box::new(Expr::from_ast(&$args[0])))
    }};
}

macro_rules! binary_expr {
    ($expr:path, $args:ident, $pos:ident) => {{
        assert_eq!(
            $args.len(),
            2,
            "Unexpected number of args for binary operator at {:?}",
            $pos
        );
        $expr(
            Box::new(Expr::from_ast(&$args[0])),
            Box::new(Expr::from_ast(&$args[1])),
        )
    }};
}

macro_rules! ternary_expr {
    ($expr:path, $args:ident, $pos:ident) => {{
        assert_eq!(
            $args.len(),
            3,
            "Unexpected number of args for ternary operator at {:?}",
            $pos
        );
        $expr(
            Box::new(Expr::from_ast(&$args[0])),
            Box::new(Expr::from_ast(&$args[1])),
            Box::new(Expr::from_ast(&$args[2])),
        )
    }};
}

macro_rules! variadic_binary_expr {
    ($expr:path, $args:ident, $pos:ident) => {{
        assert!(
            $args.len() >= 1,
            "Unexpected number of args for variadic binary operator {:?}",
            $pos
        );
        $args
            .iter()
            .map(Expr::from_ast)
            .rev()
            .reduce(|acc, e| $expr(Box::new(e), Box::new(acc)))
            .unwrap()
    }};
}

impl Expr {
    fn from_ast(expr: &ast::SpecExpr) -> Self {
        match expr {
            ast::SpecExpr::ConstInt { val, pos: _ } => Expr::Const(Const {
                ty: Type::Int,
                value: *val,
            }),
            ast::SpecExpr::ConstBitVec { val, width, pos: _ } => Expr::Const(Const {
                ty: Type::BitVectorWithWidth(*width as usize),
                value: *val,
            }),
            ast::SpecExpr::Var { var, pos: _ } => Expr::Var(var.clone()),
            ast::SpecExpr::Op { op, args, pos } => match op {
                // Unary
                //SpecOp::Not => unop(|x| Expr::Not(x), args, pos, env),
                //SpecOp::BVNot => unop(|x| Expr::BVNot(x), args, pos, env),
                //SpecOp::BVNeg => unop(|x| Expr::BVNeg(x), args, pos, env),
                //SpecOp::Rev => unop(|x| Expr::Rev(x), args, pos, env),
                //SpecOp::Clz => unop(|x| Expr::CLZ(x), args, pos, env),
                //SpecOp::Cls => unop(|x| Expr::CLS(x), args, pos, env),
                //SpecOp::Popcnt => unop(|x| Expr::BVPopcnt(x), args, pos, env),
                //SpecOp::BV2Int => unop(|x| Expr::BVToInt(x), args, pos, env),

                // Variadic binops
                SpecOp::And => variadic_binary_expr!(Expr::And, args, pos),
                SpecOp::Or => variadic_binary_expr!(Expr::Or, args, pos),

                // Binary
                SpecOp::Eq => binary_expr!(Expr::Eq, args, pos),
                //SpecOp::Lt => binop(|x, y| Expr::Lt(x, y), args, pos, env),
                SpecOp::Lte => binary_expr!(Expr::Lte, args, pos),
                //SpecOp::Gt => binop(|x, y| Expr::Lt(y, x), args, pos, env),
                //SpecOp::Gte => binop(|x, y| Expr::Lte(y, x), args, pos, env),
                //SpecOp::Imp => binop(|x, y| Expr::Imp(x, y), args, pos, env),
                //SpecOp::BVAnd => binop(|x, y| Expr::BVAnd(x, y), args, pos, env),
                //SpecOp::BVOr => binop(|x, y| Expr::BVOr(x, y), args, pos, env),
                //SpecOp::BVXor => binop(|x, y| Expr::BVXor(x, y), args, pos, env),
                SpecOp::BVAdd => binary_expr!(Expr::BVAdd, args, pos),
                //SpecOp::BVSub => binop(|x, y| Expr::BVSub(x, y), args, pos, env),
                //SpecOp::BVMul => binop(|x, y| Expr::BVMul(x, y), args, pos, env),
                //SpecOp::BVUdiv => binop(|x, y| Expr::BVUDiv(x, y), args, pos, env),
                //SpecOp::BVUrem => binop(|x, y| Expr::BVUrem(x, y), args, pos, env),
                //SpecOp::BVSdiv => binop(|x, y| Expr::BVSDiv(x, y), args, pos, env),
                //SpecOp::BVSrem => binop(|x, y| Expr::BVSrem(x, y), args, pos, env),
                //SpecOp::BVShl => binop(|x, y| Expr::BVShl(x, y), args, pos, env),
                //SpecOp::BVLshr => binop(|x, y| Expr::BVShr(x, y), args, pos, env),
                //SpecOp::BVAshr => binop(|x, y| Expr::BVAShr(x, y), args, pos, env),
                //SpecOp::BVSaddo => binop(|x, y| Expr::BVSaddo(x, y), args, pos, env),
                //SpecOp::BVUle => binop(|x, y| Expr::BVUlte(x, y), args, pos, env),
                //SpecOp::BVUlt => binop(|x, y| Expr::BVUlt(x, y), args, pos, env),
                //SpecOp::BVUgt => binop(|x, y| Expr::BVUgt(x, y), args, pos, env),
                //SpecOp::BVUge => binop(|x, y| Expr::BVUgte(x, y), args, pos, env),
                //SpecOp::BVSlt => binop(|x, y| Expr::BVSlt(x, y), args, pos, env),
                //SpecOp::BVSle => binop(|x, y| Expr::BVSlte(x, y), args, pos, env),
                //SpecOp::BVSgt => binop(|x, y| Expr::BVSgt(x, y), args, pos, env),
                //SpecOp::BVSge => binop(|x, y| Expr::BVSgte(x, y), args, pos, env),
                //SpecOp::Rotr => binop(|x, y| Expr::BVRotr(x, y), args, pos, env),
                //SpecOp::Rotl => binop(|x, y| Expr::BVRotl(x, y), args, pos, env),
                //SpecOp::ZeroExt => match spec_to_usize(&args[0]) {
                //    Some(i) => Expr::BVZeroExtTo(
                //        Box::new(Width::Const(i)),
                //        Box::new(spec_to_expr(&args[1], env)),
                //    ),
                //    None => binop(|x, y| Expr::BVZeroExtToVarWidth(x, y), args, pos, env),
                //},
                //SpecOp::SignExt => match spec_to_usize(&args[0]) {
                //    Some(i) => Expr::BVSignExtTo(
                //        Box::new(Width::Const(i)),
                //        Box::new(spec_to_expr(&args[1], env)),
                //    ),
                //    None => binop(|x, y| Expr::BVSignExtToVarWidth(x, y), args, pos, env),
                //},
                SpecOp::ConvTo => binary_expr!(Expr::BVConvToVarWidth, args, pos),

                // AVH TODO
                //SpecOp::Concat => {
                //    let cases: Vec<Expr> = args.iter().map(|a| spec_to_expr(a, env)).collect();
                //    Expr::BVConcat(cases)
                //}
                //SpecOp::Extract => {
                //    assert_eq!(
                //        args.len(),
                //        3,
                //        "Unexpected number of args for extract operator {:?}",
                //        pos
                //    );
                //    Expr::BVExtract(
                //        spec_to_usize(&args[0]).unwrap(),
                //        spec_to_usize(&args[1]).unwrap(),
                //        Box::new(spec_to_expr(&args[2], env)),
                //    )
                //}
                //SpecOp::Int2BV => {
                //    assert_eq!(
                //        args.len(),
                //        2,
                //        "Unexpected number of args for Int2BV operator {:?}",
                //        pos
                //    );
                //    Expr::BVIntToBv(
                //        spec_to_usize(&args[0]).unwrap(),
                //        Box::new(spec_to_expr(&args[1], env)),
                //    )
                //}
                //SpecOp::Subs => {
                //    assert_eq!(
                //        args.len(),
                //        3,
                //        "Unexpected number of args for subs operator {:?}",
                //        pos
                //    );
                //    Expr::BVSubs(
                //        Box::new(spec_to_expr(&args[0], env)),
                //        Box::new(spec_to_expr(&args[1], env)),
                //        Box::new(spec_to_expr(&args[2], env)),
                //    )
                //}
                SpecOp::WidthOf => unary_expr!(Expr::WidthOf, args, pos),
                SpecOp::If => ternary_expr!(Expr::Conditional, args, pos),
                //SpecOp::Switch => {
                //    assert!(
                //        args.len() > 1,
                //        "Unexpected number of args for switch operator {:?}",
                //        pos
                //    );
                //    let swith_on = spec_to_expr(&args[0], env);
                //    let arms: Vec<(Expr, Expr)> = args[1..]
                //        .iter()
                //        .map(|a| match a {
                //            SpecExpr::Pair { l, r } => {
                //                let l_expr = spec_to_expr(l, env);
                //                let r_expr = spec_to_expr(r, env);
                //                (l_expr, r_expr)
                //            }
                //            _ => unreachable!(),
                //        })
                //        .collect();
                //    Expr::Switch(Box::new(swith_on), arms)
                _ => todo!("spec op: {op:?}"),
            },

            /*
            SpecExpr::ConstBool { val, pos: _ } => Expr::Const(Const {
                ty: Type::Bool,
                value: *val as i128,
                width: 0,
            }),
            SpecExpr::Pair { l, r } => {
                unreachable!(
                    "pairs currently only parsed as part of Switch statements, {:?} {:?}",
                    l, r
                )
            }
            SpecExpr::Enum { name } => {
                if let Some(e) = env.enums.get(&name.0) {
                    e.clone()
                } else {
                    panic!("Can't find model for enum {}", name.0);
                }
            }*/
            _ => todo!("spec expr: {expr:?}"),
        }
    }
}

pub struct Spec {
    // TODO(mbm): spec signature
    provides: Vec<Expr>,
    requires: Vec<Expr>,
}

impl Spec {
    fn from_ast(spec: &ast::Spec) -> Self {
        Self {
            provides: spec.provides.iter().map(Expr::from_ast).collect(),
            requires: spec.requires.iter().map(Expr::from_ast).collect(),
        }
    }
}

pub struct SpecEnv {
    pub term_spec: HashMap<TermId, Spec>,
}

impl SpecEnv {
    pub fn from_ast(defs: &Defs, termenv: &TermEnv, tyenv: &TypeEnv) -> Self {
        let mut env = Self {
            term_spec: HashMap::new(),
        };

        env.collect_specs(defs, termenv, tyenv);

        env
    }

    fn collect_specs(&mut self, defs: &Defs, termenv: &TermEnv, tyenv: &TypeEnv) {
        for def in &defs.defs {
            match def {
                &ast::Def::Spec(ref spec) => {
                    let term_id = termenv
                        .get_term_by_name(tyenv, &spec.term)
                        .expect("spec term should exist");
                    self.term_spec.insert(term_id, Spec::from_ast(spec));
                }
                _ => {}
            }
        }
    }
}
