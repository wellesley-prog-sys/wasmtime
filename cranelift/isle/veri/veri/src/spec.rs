use cranelift_isle::{
    ast::{self, AttrKind, Def, Ident, Model, ModelType, SpecOp},
    lexer::Pos,
    sema::{Sym, TermEnv, TermId, TypeEnv, TypeId},
};
use std::collections::{HashMap, HashSet};

use crate::types::{Compound, Const};

// QUESTION(mbm): do we need this layer independent of AST spec types and Veri-IR?

/// Spec expression.
#[derive(Debug, Clone)]
pub enum Expr {
    // TODO(mbm): plumb positional information through spec expressions

    // Terminal nodes
    Var(Ident),
    Const(Const),
    // TODO(mbm): Enum identifier should be mapped to TermId
    Enum(Ident),
    //True,
    //False,
    Field(Ident, Box<Expr>),

    // Get the width of a bitvector
    WidthOf(Box<Expr>),

    // Boolean operations
    // QUESTION(mbm): would it be preferable to use the Binary(Opcode, Box<Expr>, Box<Expr>) form instead?
    Not(Box<Expr>),
    And(Box<Expr>, Box<Expr>),
    Or(Box<Expr>, Box<Expr>),
    Imp(Box<Expr>, Box<Expr>),
    Eq(Box<Expr>, Box<Expr>),
    Lte(Box<Expr>, Box<Expr>),
    //Lt(Box<Expr>, Box<Expr>),

    //BVSgt(Box<Expr>, Box<Expr>),
    BVSge(Box<Expr>, Box<Expr>),
    BVSlt(Box<Expr>, Box<Expr>),
    BVSle(Box<Expr>, Box<Expr>),
    //BVUgt(Box<Expr>, Box<Expr>),
    //BVUgte(Box<Expr>, Box<Expr>),
    BVUlt(Box<Expr>, Box<Expr>),
    BVUle(Box<Expr>, Box<Expr>),

    BVSaddo(Box<Expr>, Box<Expr>),

    //// Bitvector operations
    ////      Note: these follow the naming conventions of the SMT theory of bitvectors:
    ////      https://SMT-LIB.cs.uiowa.edu/version1/logics/QF_BV.smt
    // Unary operators
    BVNeg(Box<Expr>),
    BVNot(Box<Expr>),
    //CLZ(Box<Expr>),
    //A64CLZ(Box<Expr>, Box<Expr>),
    //CLS(Box<Expr>),
    //A64CLS(Box<Expr>, Box<Expr>),
    //Rev(Box<Expr>),
    //A64Rev(Box<Expr>, Box<Expr>),
    //BVPopcnt(Box<Expr>),

    //// Binary operators
    //BVUDiv(Box<Expr>, Box<Expr>),
    //BVSDiv(Box<Expr>, Box<Expr>),
    BVAdd(Box<Expr>, Box<Expr>),
    BVSub(Box<Expr>, Box<Expr>),
    BVMul(Box<Expr>, Box<Expr>),
    //BVUrem(Box<Expr>, Box<Expr>),
    //BVSrem(Box<Expr>, Box<Expr>),
    BVAnd(Box<Expr>, Box<Expr>),
    BVOr(Box<Expr>, Box<Expr>),
    //BVXor(Box<Expr>, Box<Expr>),
    //BVRotl(Box<Expr>, Box<Expr>),
    //BVRotr(Box<Expr>, Box<Expr>),
    BVShl(Box<Expr>, Box<Expr>),
    BVLShr(Box<Expr>, Box<Expr>),
    BVAShr(Box<Expr>, Box<Expr>),

    //// Includes type
    //BVSubs(Box<Expr>, Box<Expr>, Box<Expr>),

    // Conversions
    BVZeroExt(Box<Expr>, Box<Expr>),
    BVSignExt(Box<Expr>, Box<Expr>),
    // Conversion to wider/narrower bits, without an explicit extend.
    BVConvTo(Box<Expr>, Box<Expr>),

    // Extract specified bits
    BVExtract(usize, usize, Box<Expr>),

    // Concatenate bitvectors.
    BVConcat(Box<Expr>, Box<Expr>),

    // Convert integer to bitvector.
    Int2BV(usize, Box<Expr>),

    //// Convert bitvector to integer
    //BVToInt(Box<Expr>),

    // Conditional if-then-else
    Conditional(Box<Expr>, Box<Expr>, Box<Expr>),
    // Switch
    Switch(Box<Expr>, Vec<(Expr, Expr)>),
    // Let bindings
    Let(Vec<(Ident, Expr)>, Box<Expr>),
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
            ast::SpecExpr::ConstInt { val, pos: _ } => Expr::Const(Const::Int(*val)),
            ast::SpecExpr::ConstBool { val, pos: _ } => Expr::Const(Const::Bool(*val)),
            ast::SpecExpr::ConstBitVec { val, width, pos: _ } => {
                Expr::Const(Const::BitVector(*width, *val))
            }
            ast::SpecExpr::Var { var, pos: _ } => Expr::Var(var.clone()),
            ast::SpecExpr::Field { field, x, pos: _ } => {
                Expr::Field(field.clone(), Box::new(Expr::from_ast(x)))
            }
            ast::SpecExpr::Op { op, args, pos } => match op {
                // Unary
                SpecOp::Not => unary_expr!(Expr::Not, args, pos),
                SpecOp::BVNot => unary_expr!(Expr::BVNot, args, pos),
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
                SpecOp::BVOr => binary_expr!(Expr::BVOr, args, pos),
                //SpecOp::BVXor => binop(|x, y| Expr::BVXor(x, y), args, pos, env),
                SpecOp::BVAdd => binary_expr!(Expr::BVAdd, args, pos),
                SpecOp::BVSub => binary_expr!(Expr::BVSub, args, pos),
                //SpecOp::BVSub => binop(|x, y| Expr::BVSub(x, y), args, pos, env),
                SpecOp::BVMul => binary_expr!(Expr::BVMul, args, pos),
                //SpecOp::BVMul => binop(|x, y| Expr::BVMul(x, y), args, pos, env),
                //SpecOp::BVUdiv => binop(|x, y| Expr::BVUDiv(x, y), args, pos, env),
                //SpecOp::BVUrem => binop(|x, y| Expr::BVUrem(x, y), args, pos, env),
                //SpecOp::BVSdiv => binop(|x, y| Expr::BVSDiv(x, y), args, pos, env),
                //SpecOp::BVSrem => binop(|x, y| Expr::BVSrem(x, y), args, pos, env),
                SpecOp::BVShl => binary_expr!(Expr::BVShl, args, pos),
                SpecOp::BVLshr => binary_expr!(Expr::BVLShr, args, pos),
                SpecOp::BVAshr => binary_expr!(Expr::BVAShr, args, pos),
                SpecOp::BVUle => binary_expr!(Expr::BVUle, args, pos),
                //SpecOp::BVUlt => binop(|x, y| Expr::BVUlt(x, y), args, pos, env),
                SpecOp::BVUlt => binary_expr!(Expr::BVUlt, args, pos),
                //SpecOp::BVUgt => binop(|x, y| Expr::BVUgt(x, y), args, pos, env),
                //SpecOp::BVUge => binop(|x, y| Expr::BVUgte(x, y), args, pos, env),
                SpecOp::BVSlt => binary_expr!(Expr::BVSlt, args, pos),
                SpecOp::BVSle => binary_expr!(Expr::BVSle, args, pos),
                //SpecOp::BVSgt => binop(|x, y| Expr::BVSgt(x, y), args, pos, env),
                SpecOp::BVSge => binary_expr!(Expr::BVSge, args, pos),
                SpecOp::BVSaddo => binary_expr!(Expr::BVSaddo, args, pos),
                //SpecOp::Rotr => binop(|x, y| Expr::BVRotr(x, y), args, pos, env),
                //SpecOp::Rotl => binop(|x, y| Expr::BVRotl(x, y), args, pos, env),
                SpecOp::ZeroExt => binary_expr!(Expr::BVZeroExt, args, pos),
                SpecOp::SignExt => binary_expr!(Expr::BVSignExt, args, pos),
                SpecOp::ConvTo => binary_expr!(Expr::BVConvTo, args, pos),
                SpecOp::Concat => variadic_binary_expr!(Expr::BVConcat, args, pos),
                SpecOp::Extract => {
                    // TODO(mbm): return error instead of assert
                    assert_eq!(
                        args.len(),
                        3,
                        "Unexpected number of args for extract operator at {pos:?}",
                    );
                    Expr::BVExtract(
                        spec_expr_to_usize(&args[0]).unwrap(),
                        spec_expr_to_usize(&args[1]).unwrap(),
                        Box::new(Expr::from_ast(&args[2])),
                    )
                }
                SpecOp::Int2BV => {
                    // TODO(mbm): return error instead of assert
                    assert_eq!(
                        args.len(),
                        2,
                        "Unexpected number of args for int2bv operator at {pos:?}",
                    );
                    Expr::Int2BV(
                        spec_expr_to_usize(&args[0]).unwrap(),
                        Box::new(Expr::from_ast(&args[1])),
                    )
                }
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
                SpecOp::Switch => {
                    assert!(
                        args.len() > 1,
                        "Unexpected number of args for switch operator {pos:?}",
                    );
                    let on = Expr::from_ast(&args[0]);
                    let arms: Vec<(Expr, Expr)> = args[1..]
                        .iter()
                        .map(|p| match p {
                            ast::SpecExpr::Pair { l, r, pos: _ } => {
                                (Expr::from_ast(l), Expr::from_ast(r))
                            }
                            // TODO(mbm): error rather than panic for non-pair in switch, since it's not actually unreachable
                            _ => unreachable!("switch expression arguments must be pairs"),
                        })
                        .collect();
                    Expr::Switch(Box::new(on), arms)
                }
                _ => todo!("ast spec op: {op:?}"),
            },
            ast::SpecExpr::Let { defs, body, pos: _ } => {
                let defs = defs
                    .iter()
                    .map(|(ident, x)| (ident.clone(), Expr::from_ast(x)))
                    .collect();
                let body = Box::new(Expr::from_ast(body));
                Expr::Let(defs, body)
            }
            ast::SpecExpr::Pair { l, r, pos: _ } => {
                // QUESTION(mbm): is there a cleaner way to handle switch statements without the pair type?
                unreachable!(
                    "pairs must only occur in switch expressions, {:?} {:?}",
                    l, r
                )
            }
            ast::SpecExpr::Enum { name, pos: _ } => Expr::Enum(name.clone()),
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
    pub pos: Pos,
}

impl Spec {
    fn new() -> Self {
        Self {
            args: Vec::new(),
            ret: Self::result_ident(),
            provides: Vec::new(),
            requires: Vec::new(),
            pos: Pos::default(),
        }
    }

    fn from_ast(spec: &ast::Spec) -> Self {
        Self {
            args: spec.args.clone(),
            ret: Self::result_ident(),
            provides: spec.provides.iter().map(Expr::from_ast).collect(),
            requires: spec.requires.iter().map(Expr::from_ast).collect(),
            pos: spec.pos,
        }
    }

    fn result_ident() -> Ident {
        Ident(RESULT.to_string(), Pos::default())
    }
}

#[derive(Debug, Clone)]
pub struct Signature {
    pub args: Vec<Compound>,
    pub ret: Compound,
}

impl Signature {
    fn from_ast(sig: &ast::Signature) -> Self {
        Self {
            args: sig.args.iter().map(Compound::from_ast).collect(),
            ret: Compound::from_ast(&sig.ret),
        }
    }
}

impl std::fmt::Display for Signature {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "({args}) -> {ret}",
            args = self
                .args
                .iter()
                .map(ToString::to_string)
                .collect::<Vec<_>>()
                .join(", "),
            ret = self.ret
        )
    }
}

pub struct SpecEnv {
    /// Specification for the given term.
    pub term_spec: HashMap<TermId, Spec>,

    /// Terms that should be chained.
    pub chain: HashSet<TermId>,

    // Type instantiations for the given term.
    pub term_instantiations: HashMap<TermId, Vec<Signature>>,

    /// Model for the given type.
    pub type_model: HashMap<TypeId, Compound>,

    /// Value for the given constant.
    pub const_value: HashMap<Sym, Expr>,

    /// Value for enum variant.
    pub enum_value: HashMap<TermId, Expr>,
}

impl SpecEnv {
    pub fn from_ast(defs: &[Def], termenv: &TermEnv, tyenv: &TypeEnv) -> Self {
        let mut env = Self {
            term_spec: HashMap::new(),
            chain: HashSet::new(),
            term_instantiations: HashMap::new(),
            type_model: HashMap::new(),
            const_value: HashMap::new(),
            enum_value: HashMap::new(),
        };

        env.collect_models(defs, termenv, tyenv);
        env.collect_instantiations(defs, termenv, tyenv);
        env.collect_specs(defs, termenv, tyenv);
        env.collect_attrs(defs, termenv, tyenv);
        env.check_for_chained_terms_with_spec();

        env
    }

    fn collect_models(&mut self, defs: &[Def], termenv: &TermEnv, tyenv: &TypeEnv) {
        for def in defs {
            if let ast::Def::Model(Model { name, val }) = def {
                match val {
                    ast::ModelValue::TypeValue(model_type) => {
                        self.set_model_type(name, model_type, tyenv);
                    }
                    ast::ModelValue::ConstValue(val) => {
                        // TODO(mbm): error on missing constant name rather than panic
                        let sym = tyenv.intern(name).expect("constant name should be defined");
                        // TODO(mbm): enforce that the expression is constant.
                        // TODO(mbm): ensure the type of the expression matches the type of the
                        self.const_value.insert(sym, Expr::from_ast(val));
                    }
                    ast::ModelValue::EnumValues(model_type, vals) => {
                        self.set_model_type(name, model_type, tyenv);

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
                            self.enum_value.insert(term_id, val.clone());

                            // Synthesize spec.
                            //
                            // Enum values are sugar for term specs of the form
                            // `(provide (= result <value>))`.
                            let mut spec = Spec::new();
                            spec.pos = expr.pos();
                            spec.provides.push(Expr::Eq(
                                Box::new(Expr::Var(spec.ret.clone())),
                                Box::new(val.clone()),
                            ));
                            self.term_spec.insert(term_id, spec);
                        }
                    }
                }
            }
        }
    }

    fn set_model_type(&mut self, name: &Ident, model_type: &ModelType, tyenv: &TypeEnv) {
        // TODO(mbm): error on missing type rather than panic
        let type_id = tyenv
            .get_type_by_name(name)
            .expect("type name should be defined");
        // TODO(mbm): error on duplicate model
        assert!(
            !self.type_model.contains_key(&type_id),
            "duplicate type model: {name}",
            name = name.0
        );
        self.type_model
            .insert(type_id, Compound::from_ast(model_type));
    }

    fn collect_instantiations(&mut self, defs: &[Def], termenv: &TermEnv, tyenv: &TypeEnv) {
        // Collect form signatures first, as they may be referenced by instantiations.
        let mut form_signature = HashMap::new();
        for def in defs {
            if let ast::Def::Form(form) = def {
                let signatures: Vec<_> = form.signatures.iter().map(Signature::from_ast).collect();
                form_signature.insert(form.name.0.clone(), signatures);
            }
        }

        // Collect instantiations.
        for def in defs {
            if let ast::Def::Instantiation(inst) = def {
                let term_id = termenv.get_term_by_name(tyenv, &inst.term).unwrap();
                let sigs = match &inst.form {
                    Some(form) => form_signature[&form.0].clone(),
                    None => inst.signatures.iter().map(Signature::from_ast).collect(),
                };
                self.term_instantiations.insert(term_id, sigs);
            }
        }
    }

    fn collect_specs(&mut self, defs: &[Def], termenv: &TermEnv, tyenv: &TypeEnv) {
        for def in defs {
            if let ast::Def::Spec(spec) = def {
                let term_id = termenv
                    .get_term_by_name(tyenv, &spec.term)
                    .expect("spec term should exist");
                self.term_spec.insert(term_id, Spec::from_ast(spec));
            }
        }
    }

    fn collect_attrs(&mut self, defs: &[Def], termenv: &TermEnv, tyenv: &TypeEnv) {
        for def in defs {
            if let ast::Def::Attr(attr) = def {
                let term_id = termenv
                    .get_term_by_name(tyenv, &attr.term)
                    .expect("attr term should exist");
                for attr_kind in &attr.kinds {
                    match attr_kind {
                        AttrKind::Chain => {
                            self.chain.insert(term_id);
                        }
                    }
                }
            }
        }
    }

    fn check_for_chained_terms_with_spec(&self) {
        for term_id in &self.chain {
            // TODO(mbm): error rather than panic
            assert!(
                !self.term_spec.contains_key(term_id),
                "chained term should not have spec"
            );
        }
    }

    /// Report whether the given term has a specification.
    pub fn has_spec(&self, term_id: TermId) -> bool {
        self.term_spec.contains_key(&term_id)
    }
}
