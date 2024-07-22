use cranelift_isle::{
    ast::{self, Defs, Ident, Model, ModelType, SpecOp},
    sema::{Sym, TermEnv, TermId, TypeEnv, TypeId},
};
use std::collections::HashMap;

// QUESTION(mbm): do we need this layer independent of AST spec types and Veri-IR?

/// Higher-level type, not including bitwidths.
#[derive(Debug, Clone)]
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

impl Type {
    fn from_model(model: &ModelType) -> Self {
        match model {
            ModelType::Int => Self::Int,
            ModelType::Bool => Self::Bool,
            ModelType::BitVec(None) => Self::BitVector,
            ModelType::BitVec(Some(size)) => Self::BitVectorWithWidth(*size),
        }
    }
}

#[derive(Clone)]
pub struct Signature {
    pub args: Vec<Type>,
    pub ret: Type,
}

impl Signature {
    fn from_ast(sig: &ast::Signature) -> Self {
        Self {
            args: sig.args.iter().map(Type::from_model).collect(),
            ret: Type::from_model(&sig.ret),
        }
    }
}

/// Type-specified constants
#[derive(Debug, Clone)]
pub struct Const {
    pub ty: Type,
    // TODO(mbm): support constants larger than 127 bits
    pub value: i128,
}

/// Spec expression.
#[derive(Debug, Clone)]
pub enum Expr {
    // Terminal nodes
    Var(Ident),
    Const(Const),
    // TODO(mbm): Enum identifier should be mapped to TermId
    Enum(Ident),
    //True,
    //False,

    // Get the width of a bitvector
    WidthOf(Box<Expr>),

    // Boolean operations
    // QUESTION(mbm): would it be preferable to use the Binary(Opcode, Box<Expr>, Box<Expr>) form instead?
    //Not(Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Imp(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    Lte(Box<Expr>, Box<Expr>),
    //Lt(Box<Expr>, Box<Expr>),

    //BVSgt(Box<Expr>, Box<Expr>),
    //BVSgte(Box<Expr>, Box<Expr>),
    //BVSlt(Box<Expr>, Box<Expr>),
    //BVSlte(Box<Expr>, Box<Expr>),
    //BVUgt(Box<Expr>, Box<Expr>),
    //BVUgte(Box<Expr>, Box<Expr>),
    BVUlt(Box<Expr>, Box<Expr>),
    //BVUlte(Box<Expr>, Box<Expr>),

    //BVSaddo(Box<Expr>, Box<Expr>),

    //// Bitvector operations
    ////      Note: these follow the naming conventions of the SMT theory of bitvectors:
    ////      https://SMT-LIB.cs.uiowa.edu/version1/logics/QF_BV.smt
    // Unary operators
    BVNeg(Box<Expr>),
    //BVNot(Box<Expr>),
    //CLZ(Box<Expr>),
    //A64CLZ(Box<Expr>, Box<Expr>),
    //CLS(Box<Expr>),
    //A64CLS(Box<Expr>, Box<Expr>),
    //Rev(Box<Expr>),
    //A64Rev(Box<Expr>, Box<Expr>),
    //BVPopcnt(Box<Expr>),

    //// Binary operators
    //BVMul(Box<Expr>, Box<Expr>),
    //BVUDiv(Box<Expr>, Box<Expr>),
    //BVSDiv(Box<Expr>, Box<Expr>),
    BVAdd(Box<Expr>, Box<Expr>),
    //BVSub(Box<Expr>, Box<Expr>),
    //BVUrem(Box<Expr>, Box<Expr>),
    //BVSrem(Box<Expr>, Box<Expr>),
    //BVAnd(Box<Expr>, Box<Expr>),
    BVAnd(Box<Expr>, Box<Expr>),
    //BVOr(Box<Expr>, Box<Expr>),
    //BVXor(Box<Expr>, Box<Expr>),
    //BVRotl(Box<Expr>, Box<Expr>),
    //BVRotr(Box<Expr>, Box<Expr>),
    //BVShl(Box<Expr>, Box<Expr>),
    //BVShr(Box<Expr>, Box<Expr>),
    //BVAShr(Box<Expr>, Box<Expr>),

    //// Includes type
    //BVSubs(Box<Expr>, Box<Expr>, Box<Expr>),

    //// Conversions
    BVZeroExt(Box<Expr>, Box<Expr>),
    BVSignExt(Box<Expr>, Box<Expr>),

    // Extract specified bits
    BVExtract(usize, usize, Box<Expr>),

    //// Concat two bitvectors
    //BVConcat(Vec<Expr>),

    //// Convert integer to bitvector
    //BVIntToBv(usize, Box<Expr>),

    //// Convert bitvector to integer
    //BVToInt(Box<Expr>),

    // Conversion to wider/narrower bits, without an explicit extend.
    BVConvTo(Box<Expr>, Box<Expr>),

    // Conditional if-then-else
    Conditional(Box<Expr>, Box<Expr>, Box<Expr>),
    //// Switch
    //Switch(Box<Expr>, Vec<(Expr, Expr)>),
}

macro_rules! unary_expr {
    ($expr:path, $args:ident, $pos:ident) => {{
        // TODO(mbm): return error instead of assert
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
        // TODO(mbm): return error instead of assert
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
        // TODO(mbm): return error instead of assert
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
        // TODO(mbm): return error instead of assert
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
            ast::SpecExpr::ConstBool { val, pos: _ } => Expr::Const(Const {
                ty: Type::Bool,
                value: if *val { 1 } else { 0 },
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
                SpecOp::BVNeg => unary_expr!(Expr::BVNeg, args, pos),
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
                SpecOp::Imp => binary_expr!(Expr::Imp, args, pos),
                //SpecOp::Gt => binop(|x, y| Expr::Lt(y, x), args, pos, env),
                SpecOp::BVAnd => binary_expr!(Expr::BVAnd, args, pos),
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
                SpecOp::BVUlt => binary_expr!(Expr::BVUlt, args, pos),
                //SpecOp::BVUgt => binop(|x, y| Expr::BVUgt(x, y), args, pos, env),
                //SpecOp::BVUge => binop(|x, y| Expr::BVUgte(x, y), args, pos, env),
                //SpecOp::BVSlt => binop(|x, y| Expr::BVSlt(x, y), args, pos, env),
                //SpecOp::BVSle => binop(|x, y| Expr::BVSlte(x, y), args, pos, env),
                //SpecOp::BVSgt => binop(|x, y| Expr::BVSgt(x, y), args, pos, env),
                //SpecOp::BVSge => binop(|x, y| Expr::BVSgte(x, y), args, pos, env),
                //SpecOp::Rotr => binop(|x, y| Expr::BVRotr(x, y), args, pos, env),
                //SpecOp::Rotl => binop(|x, y| Expr::BVRotl(x, y), args, pos, env),
                SpecOp::ZeroExt => binary_expr!(Expr::BVZeroExt, args, pos),
                SpecOp::SignExt => binary_expr!(Expr::BVSignExt, args, pos),
                SpecOp::ConvTo => binary_expr!(Expr::BVConvTo, args, pos),

                // AVH TODO
                //SpecOp::Concat => {
                //    let cases: Vec<Expr> = args.iter().map(|a| spec_to_expr(a, env)).collect();
                //    Expr::BVConcat(cases)
                //}
                SpecOp::Extract => {
                    // TODO(mbm): return error instead of assert
                    assert_eq!(
                        args.len(),
                        3,
                        "Unexpected number of args for extract operator at {:?}",
                        pos
                    );
                    Expr::BVExtract(
                        spec_expr_to_usize(&args[0]).unwrap(),
                        spec_expr_to_usize(&args[1]).unwrap(),
                        Box::new(Expr::from_ast(&args[2])),
                    )
                }
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
                _ => todo!("ast spec op: {op:?}"),
            },
            /*
            SpecExpr::Pair { l, r } => {
                unreachable!(
                    "pairs currently only parsed as part of Switch statements, {:?} {:?}",
                    l, r
                )
            }*/
            ast::SpecExpr::Enum { name } => Expr::Enum(name.clone()),
            _ => todo!("ast spec expr: {expr:?}"),
        }
    }
}

fn spec_expr_to_usize(expr: &ast::SpecExpr) -> Option<usize> {
    match expr {
        &ast::SpecExpr::ConstInt { val, pos: _ } => {
            // TODO(mbm): return error rather than unwrap
            Some(val.try_into().expect("constant should be unsigned size"))
        }
        _ => None,
    }
}

// QUESTION(mbm): should we make the result explicit in the spec syntax?
static RESULT: &str = "result";

pub struct Spec {
    pub args: Vec<Ident>,
    pub ret: Ident,
    pub provides: Vec<Expr>,
    pub requires: Vec<Expr>,
}

impl Spec {
    fn new() -> Self {
        Self {
            args: Vec::new(),
            ret: Self::result_ident(),
            provides: Vec::new(),
            requires: Vec::new(),
        }
    }

    fn from_ast(spec: &ast::Spec) -> Self {
        Self {
            args: spec.args.clone(),
            ret: Self::result_ident(),
            provides: spec.provides.iter().map(Expr::from_ast).collect(),
            requires: spec.requires.iter().map(Expr::from_ast).collect(),
        }
    }

    fn result_ident() -> Ident {
        Ident(RESULT.to_string(), Default::default())
    }
}

pub struct SpecEnv {
    /// Specification for the given term.
    pub term_spec: HashMap<TermId, Spec>,

    // Type instantiations for the given term.
    pub term_instantiations: HashMap<TermId, Vec<Signature>>,

    /// Model for the given type.
    pub type_model: HashMap<TypeId, Type>,

    /// Value for the given constant.
    pub const_value: HashMap<Sym, Expr>,

    /// Value for enum variant.
    pub enum_value: HashMap<TermId, Expr>,
}

impl SpecEnv {
    pub fn from_ast(defs: &Defs, termenv: &TermEnv, tyenv: &TypeEnv) -> Self {
        let mut env = Self {
            term_spec: HashMap::new(),
            term_instantiations: HashMap::new(),
            type_model: HashMap::new(),
            const_value: HashMap::new(),
            enum_value: HashMap::new(),
        };

        env.collect_models(defs, termenv, tyenv);
        env.collect_instantiations(defs, termenv, tyenv);
        env.collect_specs(defs, termenv, tyenv);
        env.generate_enum_value_specs();

        env
    }

    fn collect_models(&mut self, defs: &Defs, termenv: &TermEnv, tyenv: &TypeEnv) {
        for def in &defs.defs {
            match def {
                ast::Def::Model(Model { name, val }) => match val {
                    ast::ModelValue::TypeValue(model_type) => {
                        // TODO(mbm): error on missing type rather than panic
                        let type_id = tyenv
                            .get_type_by_name(&name)
                            .expect("type name should be defined");
                        self.type_model
                            .insert(type_id, Type::from_model(model_type));
                    }
                    ast::ModelValue::ConstValue(val) => {
                        // TODO(mbm): error on missing constant name rather than panic
                        let sym = tyenv.intern(name).expect("constant name should be defined");
                        // TODO(mbm): enforce that the expression is constant.
                        // TODO(mbm): ensure the type of the expression matches the type of the
                        self.const_value.insert(sym, Expr::from_ast(val));
                    }
                    ast::ModelValue::EnumValues(vals) => {
                        // TODO(mbm): validate enum variants against the type definition
                        // TODO(mbm): ensure the enum doesn't have non-unit variants
                        // TODO(mbm): enforce that the enum values are distinct
                        for (variant, expr) in vals {
                            let ident = ast::Variant::full_name(name, variant);
                            // TODO(mbm): error on missing variant rather than panic
                            let term_id = termenv
                                .get_term_by_name(tyenv, &ident)
                                .expect("enum variant should exist");
                            // TODO(mbm): enforce that the enum value expression is constant
                            let val = Expr::from_ast(expr);
                            self.enum_value.insert(term_id, val);
                        }
                    }
                },
                _ => (),
            }
        }
    }

    fn collect_instantiations(&mut self, defs: &Defs, termenv: &TermEnv, tyenv: &TypeEnv) {
        // Collect form signatures first, as they may be referenced by instantiations.
        let mut form_signature = HashMap::new();
        for def in &defs.defs {
            match def {
                ast::Def::Form(form) => {
                    let signatures: Vec<_> =
                        form.signatures.iter().map(Signature::from_ast).collect();
                    form_signature.insert(form.name.0.clone(), signatures);
                }
                _ => {}
            }
        }

        // Collect instantiations.
        for def in &defs.defs {
            match def {
                ast::Def::Instantiation(inst) => {
                    let term_id = termenv.get_term_by_name(tyenv, &inst.term).unwrap();
                    let sigs = match &inst.form {
                        Some(form) => form_signature[&form.0].clone(),
                        None => inst.signatures.iter().map(Signature::from_ast).collect(),
                    };
                    self.term_instantiations.insert(term_id, sigs);
                }
                _ => {}
            }
        }
    }

    fn collect_specs(&mut self, defs: &Defs, termenv: &TermEnv, tyenv: &TypeEnv) {
        for def in &defs.defs {
            match def {
                ast::Def::Spec(spec) => {
                    let term_id = termenv
                        .get_term_by_name(tyenv, &spec.term)
                        .expect("spec term should exist");
                    self.term_spec.insert(term_id, Spec::from_ast(spec));
                }
                _ => {}
            }
        }
    }

    fn generate_enum_value_specs(&mut self) {
        // Enum values are sugar for term specs of the form `(provide (= result <value>))`.
        for (term_id, val) in &self.enum_value {
            let mut spec = Spec::new();
            spec.provides.push(Expr::Eq(
                Box::new(Expr::Var(spec.ret.clone())),
                Box::new(val.clone()),
            ));
            self.term_spec.insert(*term_id, spec);
        }
    }
}
