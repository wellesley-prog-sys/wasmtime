use crate::{
    expand::{Constrain, Expansion},
    program::Program,
    spec::{self, Arm, Constructor, Signature, State},
    trie::{binding_type, BindingType},
    types::{Compound, Const, Type, Variant, Width},
};
use anyhow::{bail, format_err, Result};
use cranelift_isle::{
    ast::Ident,
    sema::{Sym, TermId, TypeId, VariantId},
    trie_again::{Binding, BindingId, Constraint, TupleIndex},
};
use std::{
    collections::{hash_map::Entry, HashMap, HashSet},
    iter::zip,
};

declare_id!(
    /// The id of an expression within verification Conditions.
    #[must_use]
    ExprId
);

declare_id!(
    /// The id of a variable within verification Conditions.
    VariableId
);

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Expr {
    // Terminals.
    Const(Const),
    Variable(VariableId),

    // Boolean.
    Not(ExprId),
    And(ExprId, ExprId),
    Or(ExprId, ExprId),
    Imp(ExprId, ExprId),
    Eq(ExprId, ExprId),
    Lt(ExprId, ExprId),
    Lte(ExprId, ExprId),

    BVUgt(ExprId, ExprId),
    BVUge(ExprId, ExprId),
    BVUlt(ExprId, ExprId),
    BVUle(ExprId, ExprId),

    BVSgt(ExprId, ExprId),
    BVSge(ExprId, ExprId),
    BVSlt(ExprId, ExprId),
    BVSle(ExprId, ExprId),

    BVSaddo(ExprId, ExprId),

    // Unary.
    BVNot(ExprId),
    BVNeg(ExprId),
    Cls(ExprId),

    // Binary.
    BVAdd(ExprId, ExprId),
    BVSub(ExprId, ExprId),
    BVMul(ExprId, ExprId),
    BVSDiv(ExprId, ExprId),
    BVUDiv(ExprId, ExprId),
    BVSRem(ExprId, ExprId),
    BVURem(ExprId, ExprId),
    BVAnd(ExprId, ExprId),
    BVOr(ExprId, ExprId),
    BVXor(ExprId, ExprId),
    BVShl(ExprId, ExprId),
    BVLShr(ExprId, ExprId),
    BVAShr(ExprId, ExprId),
    BVRotl(ExprId, ExprId),
    BVRotr(ExprId, ExprId),

    // ITE
    Conditional(ExprId, ExprId, ExprId),

    // Bitwidth conversion.
    BVZeroExt(ExprId, ExprId),
    BVSignExt(ExprId, ExprId),
    BVConvTo(ExprId, ExprId),

    // Extract specified bit range.
    BVExtract(usize, usize, ExprId),

    // Concatenate bitvectors.
    BVConcat(ExprId, ExprId),

    // Integer conversion.
    Int2BV(ExprId, ExprId),
    BV2Nat(ExprId),

    // Bitwidth.
    WidthOf(ExprId),
}

impl Expr {
    pub fn is_variable(&self) -> bool {
        matches!(self, Self::Variable(_))
    }

    pub fn sources(&self) -> Vec<ExprId> {
        match self {
            Expr::Const(_) | Expr::Variable(_) => Vec::new(),
            // Unary
            Expr::Not(x)
            | Expr::BVNot(x)
            | Expr::BVNeg(x)
            | Expr::BVExtract(_, _, x)
            | Expr::BV2Nat(x)
            | Expr::Cls(x)
            | Expr::WidthOf(x) => vec![*x],

            // Binary
            Expr::And(x, y)
            | Expr::Or(x, y)
            | Expr::Imp(x, y)
            | Expr::Eq(x, y)
            | Expr::Lt(x, y)
            | Expr::Lte(x, y)
            | Expr::BVUgt(x, y)
            | Expr::BVUge(x, y)
            | Expr::BVUlt(x, y)
            | Expr::BVUle(x, y)
            | Expr::BVSgt(x, y)
            | Expr::BVSge(x, y)
            | Expr::BVSlt(x, y)
            | Expr::BVSle(x, y)
            | Expr::BVSaddo(x, y)
            | Expr::BVAdd(x, y)
            | Expr::BVSub(x, y)
            | Expr::BVMul(x, y)
            | Expr::BVSDiv(x, y)
            | Expr::BVUDiv(x, y)
            | Expr::BVSRem(x, y)
            | Expr::BVURem(x, y)
            | Expr::BVAnd(x, y)
            | Expr::BVOr(x, y)
            | Expr::BVXor(x, y)
            | Expr::BVShl(x, y)
            | Expr::BVLShr(x, y)
            | Expr::BVAShr(x, y)
            | Expr::BVRotl(x, y)
            | Expr::BVRotr(x, y)
            | Expr::BVZeroExt(x, y)
            | Expr::BVSignExt(x, y)
            | Expr::BVConvTo(x, y)
            | Expr::Int2BV(x, y)
            | Expr::BVConcat(x, y) => vec![*x, *y],

            // Ternary
            Expr::Conditional(c, t, e) => vec![*c, *t, *e],
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expr::Const(c) => write!(f, "const({c})"),
            Expr::Variable(v) => write!(f, "var({})", v.index()),
            Expr::Not(x) => write!(f, "!{}", x.index()),
            Expr::And(x, y) => write!(f, "{} && {}", x.index(), y.index()),
            Expr::Or(x, y) => write!(f, "{} || {}", x.index(), y.index()),
            Expr::Imp(x, y) => write!(f, "{} => {}", x.index(), y.index()),
            Expr::Eq(x, y) => write!(f, "{} == {}", x.index(), y.index()),
            Expr::Lt(x, y) => write!(f, "{} < {}", x.index(), y.index()),
            Expr::Lte(x, y) => write!(f, "{} <= {}", x.index(), y.index()),
            Expr::BVUgt(x, y) => write!(f, "bvugt({}, {})", x.index(), y.index()),
            Expr::BVUge(x, y) => write!(f, "bvuge({}, {})", x.index(), y.index()),
            Expr::BVUlt(x, y) => write!(f, "bvult({}, {})", x.index(), y.index()),
            Expr::BVUle(x, y) => write!(f, "bvule({}, {})", x.index(), y.index()),
            Expr::BVSgt(x, y) => write!(f, "bvsgt({}, {})", x.index(), y.index()),
            Expr::BVSge(x, y) => write!(f, "bvsge({}, {})", x.index(), y.index()),
            Expr::BVSlt(x, y) => write!(f, "bvslt({}, {})", x.index(), y.index()),
            Expr::BVSle(x, y) => write!(f, "bvsle({}, {})", x.index(), y.index()),
            Expr::BVSaddo(x, y) => write!(f, "bvsaddo({}, {})", x.index(), y.index()),
            Expr::BVNot(x) => write!(f, "bvnot({})", x.index()),
            Expr::BVNeg(x) => write!(f, "bvneg({})", x.index()),
            Expr::Cls(x) => write!(f, "cls({})", x.index()),
            Expr::BVAdd(x, y) => write!(f, "bvadd({}, {})", x.index(), y.index()),
            Expr::BVSub(x, y) => write!(f, "bvsub({}, {})", x.index(), y.index()),
            Expr::BVMul(x, y) => write!(f, "bvmul({}, {})", x.index(), y.index()),
            Expr::BVSDiv(x, y) => write!(f, "bvsdiv({}, {})", x.index(), y.index()),
            Expr::BVUDiv(x, y) => write!(f, "bvudiv({}, {})", x.index(), y.index()),
            Expr::BVSRem(x, y) => write!(f, "bvsrem({}, {})", x.index(), y.index()),
            Expr::BVURem(x, y) => write!(f, "bvurem({}, {})", x.index(), y.index()),
            Expr::BVAnd(x, y) => write!(f, "bvand({}, {})", x.index(), y.index()),
            Expr::BVOr(x, y) => write!(f, "bvor({}, {})", x.index(), y.index()),
            Expr::BVXor(x, y) => write!(f, "bvxor({}, {})", x.index(), y.index()),
            Expr::BVShl(x, y) => write!(f, "bvshl({}, {})", x.index(), y.index()),
            Expr::BVLShr(x, y) => write!(f, "bvlshr({}, {})", x.index(), y.index()),
            Expr::BVAShr(x, y) => write!(f, "bvashr({}, {})", x.index(), y.index()),
            Expr::BVRotl(x, y) => write!(f, "bvrotl({}, {})", x.index(), y.index()),
            Expr::BVRotr(x, y) => write!(f, "bvrotr({}, {})", x.index(), y.index()),
            Expr::Conditional(c, t, e) => {
                write!(f, "{} ? {} : {}", c.index(), t.index(), e.index())
            }
            Expr::BVZeroExt(w, x) => write!(f, "bv_zero_ext({}, {})", w.index(), x.index()),
            Expr::BVSignExt(w, x) => write!(f, "bv_zero_ext({}, {})", w.index(), x.index()),
            Expr::BVConvTo(w, x) => write!(f, "bv_conv_to({}, {})", w.index(), x.index()),
            Expr::BVExtract(h, l, x) => write!(f, "bv_extract({h}, {l}, {})", x.index()),
            Expr::BVConcat(x, y) => write!(f, "bv_concat({}, {})", x.index(), y.index()),
            Expr::Int2BV(w, x) => write!(f, "int2bv({}, {})", w.index(), x.index()),
            Expr::BV2Nat(x) => write!(f, "bv2nat({})", x.index()),
            Expr::WidthOf(x) => write!(f, "width_of({})", x.index()),
        }
    }
}

// QUESTION(mbm): can we merge `Model` and `Assignment` from type inference?
pub type Model = HashMap<ExprId, Const>;

// QUESTION(mbm): does the distinction between expressions and variables make sense?
#[derive(Debug)]
pub struct Variable {
    pub ty: Type,
    pub name: String,
}

impl Variable {
    fn component_name(prefix: &str, field: &str) -> String {
        format!("{prefix}_{field}")
    }
}

#[derive(Debug, Clone)]
pub struct SymbolicOption {
    some: ExprId,
    inner: Box<Symbolic>,
}

#[derive(Debug, Clone)]
pub struct SymbolicField {
    pub name: String,
    pub value: Symbolic,
}

impl SymbolicField {
    fn eval(&self, model: &Model) -> Result<FieldValue> {
        Ok(FieldValue {
            name: self.name.clone(),
            value: self.value.eval(model)?,
        })
    }
}

#[derive(Debug, Clone)]
pub struct SymbolicEnum {
    pub ty: TypeId,
    pub discriminant: ExprId,
    pub variants: Vec<SymbolicVariant>,
}

impl SymbolicEnum {
    fn try_variant_by_name(&self, name: &str) -> Result<&SymbolicVariant> {
        self.variants
            .iter()
            .find(|v| v.name == name)
            .ok_or(format_err!("no variant with name {name}"))
    }

    fn validate(&self) -> Result<()> {
        // Expect the variants to have distinct discriminants in the range [0, num_variants).
        for (expect, variant) in self.variants.iter().enumerate() {
            if variant.discriminant != expect {
                bail!(
                    "variant '{name}' has unexpected discriminant",
                    name = variant.name
                );
            }
        }
        Ok(())
    }
}

#[derive(Debug, Clone)]
pub struct SymbolicVariant {
    pub name: String,
    pub id: VariantId,
    pub discriminant: usize,
    pub value: Symbolic,
}

impl SymbolicVariant {
    fn try_field_by_name(&self, name: &str) -> Result<&SymbolicField> {
        self.fields()?
            .iter()
            .find(|f| f.name == name)
            .ok_or(format_err!("no field with name {name}"))
    }

    fn field_values(&self) -> Result<Vec<Symbolic>> {
        Ok(self.fields()?.iter().map(|f| f.value.clone()).collect())
    }

    fn fields(&self) -> Result<&Vec<SymbolicField>> {
        self.value
            .as_struct()
            .ok_or(format_err!("variant value is not a struct"))
    }
}

impl std::fmt::Display for SymbolicVariant {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{name} {value}", name = self.name, value = self.value)
    }
}

#[derive(Debug, Clone)]
pub enum Symbolic {
    Scalar(ExprId),
    Struct(Vec<SymbolicField>),
    Enum(SymbolicEnum),
    Option(SymbolicOption),
    Tuple(Vec<Symbolic>),
}

impl Symbolic {
    fn as_scalar(&self) -> Option<ExprId> {
        match self {
            Self::Scalar(x) => Some(*x),
            _ => None,
        }
    }

    fn as_struct(&self) -> Option<&Vec<SymbolicField>> {
        match self {
            Self::Struct(fields) => Some(fields),
            _ => None,
        }
    }

    fn as_enum(&self) -> Option<&SymbolicEnum> {
        match self {
            Self::Enum(e) => Some(e),
            _ => None,
        }
    }

    fn as_option(&self) -> Option<&SymbolicOption> {
        match self {
            Self::Option(opt) => Some(opt),
            _ => None,
        }
    }

    fn as_tuple(&self) -> Option<&Vec<Symbolic>> {
        match self {
            Self::Tuple(fields) => Some(fields),
            _ => None,
        }
    }

    fn elements(&self) -> &[Symbolic] {
        match self {
            Self::Tuple(fields) => &fields[..],
            v => std::slice::from_ref(v),
        }
    }

    fn eval(&self, model: &Model) -> Result<Value> {
        match self {
            Symbolic::Scalar(x) => Ok(Value::Const(
                model
                    .get(x)
                    .ok_or(format_err!("undefined expression in model"))?
                    .clone(),
            )),
            Symbolic::Struct(fields) => Ok(Value::Struct(
                fields
                    .iter()
                    .map(|f| f.eval(model))
                    .collect::<Result<_>>()?,
            )),
            Symbolic::Enum(e) => {
                // Determine the enum variant by looking up the discriminant.
                let discriminant = model
                    .get(&e.discriminant)
                    .ok_or(format_err!("undefined discriminant in model"))?
                    .as_int()
                    .ok_or(format_err!(
                        "model value for discriminant is not an integer"
                    ))?
                    .try_into()
                    .unwrap();
                let variant = e
                    .variants
                    .iter()
                    .find(|v| v.discriminant == discriminant)
                    .ok_or(format_err!("no variant with discriminant {discriminant}"))?;
                Ok(Value::Enum(Box::new(VariantValue {
                    name: variant.name.clone(),
                    value: variant.value.eval(model)?,
                })))
            }
            Symbolic::Option(opt) => match model.get(&opt.some) {
                Some(Const::Bool(true)) => {
                    Ok(Value::Option(Some(Box::new(opt.inner.eval(model)?))))
                }
                Some(Const::Bool(false)) => Ok(Value::Option(None)),
                Some(_) => bail!("model value for option some is not boolean"),
                None => bail!("undefined expression in model"),
            },
            Symbolic::Tuple(elements) => Ok(Value::Tuple(
                elements
                    .iter()
                    .map(|s| s.eval(model))
                    .collect::<Result<_>>()?,
            )),
        }
    }

    // Build a new value by applying the given map function to all constituent
    // scalars in this symbolic value.
    fn scalar_map<F>(&self, f: &mut F) -> Symbolic
    where
        F: FnMut(ExprId) -> ExprId,
    {
        match self {
            Symbolic::Scalar(x) => Symbolic::Scalar(f(*x)),
            Symbolic::Struct(fields) => Symbolic::Struct(
                fields
                    .iter()
                    .map(|field| SymbolicField {
                        name: field.name.clone(),
                        value: field.value.scalar_map(f),
                    })
                    .collect(),
            ),
            v => todo!("scalar map: {v:?}"),
        }
    }

    fn merge<F>(a: &Symbolic, b: &Symbolic, merge: &mut F) -> Result<Symbolic>
    where
        F: FnMut(ExprId, ExprId) -> ExprId,
    {
        if std::mem::discriminant(a) != std::mem::discriminant(b) {
            bail!("conditional arms have incompatible types");
        }
        match (a, b) {
            (Symbolic::Scalar(a), Symbolic::Scalar(b)) => Ok(merge(*a, *b).into()),
            (Symbolic::Struct(a_fields), Symbolic::Struct(b_fields)) => {
                assert_eq!(a_fields.len(), b_fields.len());
                Ok(Symbolic::Struct(
                    zip(a_fields, b_fields)
                        .map(|(a, b)| {
                            assert_eq!(a.name, b.name);
                            Ok(SymbolicField {
                                name: a.name.clone(),
                                value: Symbolic::merge(&a.value, &b.value, merge)?,
                            })
                        })
                        .collect::<Result<_>>()?,
                ))
            }
            (Symbolic::Enum(a), Symbolic::Enum(b)) => {
                assert_eq!(a.ty, b.ty);
                let ty = a.ty;
                let discriminant = merge(a.discriminant, b.discriminant);
                assert_eq!(a.variants.len(), b.variants.len());
                let variants = zip(&a.variants, &b.variants)
                    .map(|(a, b)| {
                        assert_eq!(a.name, b.name);
                        assert_eq!(a.id, b.id);
                        assert_eq!(a.discriminant, b.discriminant);
                        Ok(SymbolicVariant {
                            name: a.name.clone(),
                            id: a.id,
                            discriminant: a.discriminant,
                            value: Symbolic::merge(&a.value, &b.value, merge)?,
                        })
                    })
                    .collect::<Result<_>>()?;
                Ok(Symbolic::Enum(SymbolicEnum {
                    ty,
                    discriminant,
                    variants,
                }))
            }
            case => todo!("symbolic merge types: {case:?}"),
        }
    }
}

impl From<ExprId> for Symbolic {
    fn from(x: ExprId) -> Self {
        Symbolic::Scalar(x)
    }
}

impl TryFrom<Symbolic> for ExprId {
    type Error = anyhow::Error;

    fn try_from(v: Symbolic) -> Result<Self, Self::Error> {
        v.as_scalar().ok_or(format_err!("should be scalar value"))
    }
}

impl std::fmt::Display for Symbolic {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Symbolic::Scalar(x) => write!(f, "{}", x.index()),
            Symbolic::Struct(fields) => write!(
                f,
                "{{{fields}}}",
                fields = fields
                    .iter()
                    .map(|f| format!("{}: {}", f.name, f.value))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Symbolic::Enum(e) => write!(
                f,
                "{{{discriminant}, {variants}}}",
                discriminant = e.discriminant.index(),
                variants = e
                    .variants
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Symbolic::Option(SymbolicOption { some, inner }) => {
                write!(f, "Option{{some: {}, inner: {inner}}}", some.index())
            }
            Symbolic::Tuple(vs) => write!(
                f,
                "({vs})",
                vs = vs
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

#[derive(Clone, Debug)]
pub enum Value {
    Const(Const),
    Struct(Vec<FieldValue>),
    Enum(Box<VariantValue>),
    Option(Option<Box<Value>>),
    Tuple(Vec<Value>),
}

#[derive(Debug, Clone)]
pub struct FieldValue {
    name: String,
    value: Value,
}

#[derive(Debug, Clone)]
pub struct VariantValue {
    name: String,
    value: Value,
}

impl std::fmt::Display for Value {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Value::Const(c) => c.fmt(f),
            Value::Struct(fields) => write!(
                f,
                "{{{fields}}}",
                fields = fields
                    .iter()
                    .map(|f| format!("{}: {}", f.name, f.value))
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
            Value::Enum(v) => write!(f, "{name} {value}", name = v.name, value = v.value),
            Value::Option(Some(v)) => write!(f, "Some({v})"),
            Value::Option(None) => write!(f, "None"),
            Value::Tuple(elements) => write!(
                f,
                "({})",
                elements
                    .iter()
                    .map(|v| v.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ),
        }
    }
}

// QUESTION(mbm): is `Call` the right name? consider `Term`, `TermInstance`, ...?
#[derive(Debug)]
pub struct Call {
    pub term: TermId,
    pub args: Vec<Symbolic>,
    pub ret: Symbolic,
    pub signatures: Vec<Signature>,
}

// Type qualifier, for example derived from an `(as ...)` expression.
#[derive(Debug)]
pub struct Qualifier {
    pub value: Symbolic,
    pub ty: Compound,
}

/// Verification conditions for an expansion.
#[derive(Debug, Default)]
pub struct Conditions {
    pub exprs: Vec<Expr>,
    pub assumptions: Vec<ExprId>,
    pub assertions: Vec<ExprId>,
    pub variables: Vec<Variable>,
    pub calls: Vec<Call>,
    pub qualifiers: Vec<Qualifier>,
}

impl Conditions {
    pub fn from_expansion(expansion: &Expansion, prog: &Program) -> Result<Self> {
        let builder = ConditionsBuilder::new(expansion, prog);
        builder.build()
    }

    pub fn pretty_print(&self, prog: &Program) {
        println!("conditions {{");

        // Expressions
        println!("\texprs = [");
        for (i, expr) in self.exprs.iter().enumerate() {
            println!("\t\t{i}:\t{expr}");
        }
        println!("\t]");

        // Assumptions
        println!("\tassumptions = [");
        for expr_id in &self.assumptions {
            println!("\t\t{}", expr_id.index());
        }
        println!("\t]");

        // Assertions
        println!("\tassertions = [");
        for expr_id in &self.assertions {
            println!("\t\t{}", expr_id.index());
        }
        println!("\t]");

        // Variables
        println!("\tvariables = [");
        for (i, v) in self.variables.iter().enumerate() {
            println!("\t\t{i}:\t{ty}\t{name}", ty = v.ty, name = v.name);
        }
        println!("\t]");

        // Calls
        // TODO(mbm): prettier pretty printing code
        println!("\tcalls = [");
        for call in &self.calls {
            println!("\t\tcall {{");
            println!("\t\t\tterm = {}", prog.term_name(call.term));
            if !call.args.is_empty() {
                println!("\t\t\targs = [");
                for arg in &call.args {
                    println!("\t\t\t\t{}", arg);
                }
                println!("\t\t\t]");
            }
            println!("\t\t\tret = {}", call.ret);
            if !call.signatures.is_empty() {
                println!("\t\t\tsignatures = [");
                for sig in &call.signatures {
                    println!("\t\t\t\tsignature {{");
                    if !sig.args.is_empty() {
                        println!("\t\t\t\t\targs = [");
                        for arg in &sig.args {
                            println!("\t\t\t\t\t\t{arg}");
                        }
                        println!("\t\t\t\t\t]");
                    }
                    println!("\t\t\t\t\tret = {}", sig.ret);
                    println!("\t\t\t\t}}");
                }
                println!("\t\t\t]");
            }
            println!("\t\t}}");
        }
        println!("\t]");

        println!("}}");
    }

    pub fn validate(&self) -> Result<()> {
        // Ensure there are no dangling expressions.
        let reachable = self.reachable();
        for x in (0..self.exprs.len()).map(ExprId) {
            if self.exprs[x.index()].is_variable() {
                continue;
            }
            if !reachable.contains(&x) {
                bail!("expression {x} is unreachable", x = x.index());
            }
        }

        Ok(())
    }

    fn reachable(&self) -> HashSet<ExprId> {
        let mut reach = HashSet::new();

        let mut stack: Vec<ExprId> = Vec::new();
        stack.extend(&self.assumptions);
        stack.extend(&self.assertions);

        while let Some(x) = stack.pop() {
            if reach.contains(&x) {
                continue;
            }

            reach.insert(x);
            let expr = &self.exprs[x.index()];
            stack.extend(expr.sources());
        }

        reach
    }

    pub fn print_model(&self, model: &Model, prog: &Program) -> Result<()> {
        // Calls
        for call in &self.calls {
            println!(
                "{term_name}({args}) -> {ret}",
                term_name = prog.term_name(call.term),
                args = call
                    .args
                    .iter()
                    .map(|a| Ok(a.eval(model)?.to_string()))
                    .collect::<Result<Vec<_>>>()?
                    .join(", "),
                ret = call.ret.eval(model)?
            );
        }

        Ok(())
    }
}

#[derive(Copy, Clone)]
enum Invocation {
    Caller,
    Callee,
}

#[derive(Copy, Clone)]
enum Domain {
    Total,
    Partial(ExprId),
}

impl Domain {
    fn from_return_value(value: &Symbolic) -> (Self, Symbolic) {
        match value {
            Symbolic::Option(opt) => (Self::Partial(opt.some), (*opt.inner).clone()),
            v => (Self::Total, v.clone()),
        }
    }
}

#[derive(Clone, Debug)]
struct Variables(HashMap<String, Symbolic>);

impl Variables {
    fn new() -> Self {
        Self(HashMap::new())
    }

    fn get(&self, name: &String) -> Result<&Symbolic> {
        self.0
            .get(name)
            .ok_or(format_err!("undefined variable {name}"))
    }

    fn set(&mut self, name: String, value: Symbolic) -> Result<()> {
        match self.0.entry(name) {
            Entry::Occupied(e) => {
                bail!("redefinition of variable {name}", name = e.key());
            }
            Entry::Vacant(e) => {
                e.insert(value);
                Ok(())
            }
        }
    }
}

struct ConditionsBuilder<'a> {
    expansion: &'a Expansion,
    prog: &'a Program,

    state: Variables,
    modified_state: HashSet<String>,
    binding_value: HashMap<BindingId, Symbolic>,
    expr_map: HashMap<Expr, ExprId>,
    conditions: Conditions,
}

impl<'a> ConditionsBuilder<'a> {
    fn new(expansion: &'a Expansion, prog: &'a Program) -> Self {
        Self {
            expansion,
            prog,
            state: Variables::new(),
            modified_state: HashSet::new(),
            binding_value: HashMap::new(),
            expr_map: HashMap::new(),
            conditions: Conditions::default(),
        }
    }

    fn build(mut self) -> Result<Conditions> {
        // State initialization.
        for state in &self.prog.specenv.state {
            self.init_state(state)?;
        }

        // Bindings.
        for (i, binding) in self.expansion.bindings.iter().enumerate() {
            if let Some(binding) = binding {
                self.add_binding(i.try_into().unwrap(), binding)?;
            }
        }

        // Callee contract for the term under expansion.
        self.constructor(
            self.expansion.result,
            self.expansion.term,
            &self.expansion.parameters,
            Invocation::Callee,
        )?;

        // Constraints.
        for constrain in &self.expansion.constraints {
            let holds = self.constrain(constrain)?;
            self.conditions.assumptions.push(holds);
        }

        // Equals.
        for (a, b) in self.expansion.equalities() {
            let eq = self.bindings_equal(a, b);
            self.conditions.assumptions.push(eq);
        }

        // State defaults.
        for state in &self.prog.specenv.state {
            // The default only applies if the state was not modified.
            if !self.modified_state.contains(&state.name.0) {
                self.assume_state_default(state)?;
            }
        }

        // Validate
        self.conditions.validate()?;

        Ok(self.conditions)
    }

    fn init_state(&mut self, state: &State) -> Result<()> {
        let name = &state.name.0;
        let value = self.alloc_value(&state.ty, name.clone())?;
        self.state.set(name.clone(), value)?;
        Ok(())
    }

    fn assume_state_default(&mut self, state: &State) -> anyhow::Result<()> {
        // Evaluate the default spec expression in a scope that only defines
        // the state variable itself.
        let mut vars = Variables::new();
        let name = &state.name.0;
        vars.set(name.clone(), self.state.get(name)?.clone())?;
        let expr = self.spec_expr(&state.default, &vars)?;

        // The expression should define an assumption about the state variable,
        // so should be a scalar boolean.
        self.conditions.assumptions.push(expr.try_into()?);

        Ok(())
    }

    fn add_binding(&mut self, id: BindingId, binding: &Binding) -> Result<()> {
        // Exit if already added.
        if self.binding_value.contains_key(&id) {
            return Ok(());
        }

        // Allocate a value.
        let binding_type = self.binding_type(binding);
        let name = format!("b{}", id.index());
        let value = self.alloc_binding(&binding_type, name)?;
        self.binding_value.insert(id, value);

        // Ensure dependencies have been added.
        for source in binding.sources() {
            let source_binding = self
                .expansion
                .binding(*source)
                .expect("source binding should be defined");
            self.add_binding(*source, source_binding)?;
        }

        // Generate conditions depending on binding type.
        match binding {
            Binding::ConstInt { val, ty } => self.const_int(id, *val, *ty),

            Binding::ConstPrim { val } => self.const_prim(id, *val),

            // Argument binding has no associated constraints.
            Binding::Argument { .. } => Ok(()),

            Binding::Extractor { term, parameter } => self.extractor(id, *term, *parameter),

            Binding::Constructor {
                term, parameters, ..
            } => self.constructor(id, *term, parameters, Invocation::Caller),

            Binding::Iterator { .. } => unimplemented!("iterator bindings"),

            Binding::MakeVariant {
                ty,
                variant,
                fields,
            } => self.make_variant(id, *ty, *variant, fields),

            Binding::MatchVariant {
                source,
                variant,
                field,
            } => self.match_variant(id, *source, *variant, *field),

            Binding::MakeSome { inner } => self.make_some(id, *inner),

            Binding::MatchSome { source } => self.match_some(id, *source),

            Binding::MatchTuple { source, field } => self.match_tuple(id, *source, *field),
        }
    }

    fn const_int(&mut self, id: BindingId, val: i128, ty: TypeId) -> Result<()> {
        let eq = self.equals_const_int(id, val, ty)?;
        self.conditions.assumptions.push(eq);
        Ok(())
    }

    fn equals_const_int(&mut self, id: BindingId, val: i128, ty: TypeId) -> Result<ExprId> {
        // Determine modeled type.
        let ty_name = self.prog.type_name(ty);
        let ty = self
            .prog
            .specenv
            .type_model
            .get(&ty)
            .ok_or(format_err!("no model for type {ty_name}"))?
            .as_primitive()
            .ok_or(format_err!("constant must have basic type"))?;

        // Construct value of the determined type.
        let value = self.spec_typed_value(val, ty)?.into();

        // Destination binding equals constant value.
        let eq = self.values_equal(self.binding_value[&id].clone(), value);
        Ok(eq)
    }

    fn const_prim(&mut self, id: BindingId, val: Sym) -> Result<()> {
        let eq = self.equals_const_prim(id, val)?;
        self.conditions.assumptions.push(eq);
        Ok(())
    }

    fn equals_const_prim(&mut self, id: BindingId, val: Sym) -> Result<ExprId> {
        // Lookup value.
        let spec_value = self.prog.specenv.const_value.get(&val).ok_or(format_err!(
            "value of constant {const_name} is unspecified",
            const_name = self.prog.tyenv.syms[val.index()]
        ))?;
        let value = self.spec_expr_no_vars(spec_value)?;

        // Destination binding equals constant value.
        let eq = self.values_equal(self.binding_value[&id].clone(), value);
        Ok(eq)
    }

    fn extractor(&mut self, id: BindingId, term: TermId, parameter: BindingId) -> Result<()> {
        // Arguments are the actually the return values of an
        // extractor, possibly wrapped in an Option<..> type.
        let (domain, ret) = Domain::from_return_value(&self.binding_value[&id]);
        let args = ret.elements();

        // Result maps to the parameter of an extractor.
        let result = self.binding_value[&parameter].clone();

        // Call extractor.
        self.call(term, args, result, Invocation::Caller, domain)
    }

    fn constructor(
        &mut self,
        id: BindingId,
        term: TermId,
        parameters: &[BindingId],
        invocation: Invocation,
    ) -> Result<()> {
        // Arguments.
        let mut args = Vec::new();
        for parameter_binding_id in parameters {
            let x = self
                .binding_value
                .get(parameter_binding_id)
                .expect("parameter binding should be defined")
                .clone();
            args.push(x);
        }

        // Return value.
        let (domain, result) = Domain::from_return_value(&self.binding_value[&id]);

        // Call constructor.
        self.call(term, &args, result, invocation, domain)
    }

    fn call(
        &mut self,
        term: TermId,
        args: &[Symbolic],
        ret: Symbolic,
        invocation: Invocation,
        domain: Domain,
    ) -> Result<()> {
        // Lookup spec.
        let term_name = self.prog.term_name(term);
        let term_spec = self
            .prog
            .specenv
            .term_spec
            .get(&term)
            .ok_or(format_err!("no spec for term {term_name}",))?;

        // Assignment of signature variables to expressions.
        let mut vars = self.state.clone();

        // Arguments.
        if term_spec.args.len() != args.len() {
            bail!("incorrect number of arguments for term {term_name}");
        }
        for (name, arg) in zip(&term_spec.args, args) {
            vars.set(name.0.clone(), arg.clone())?;
        }

        // Return value.
        vars.set(term_spec.ret.0.clone(), ret.clone())?;

        // Requires.
        let mut requires: Vec<ExprId> = Vec::new();
        for require in &term_spec.requires {
            let require = self.spec_expr(require, &vars)?.try_into()?;
            requires.push(require);
        }

        // Provides.
        let mut provides: Vec<ExprId> = Vec::new();
        for provide in &term_spec.provides {
            let provide = self.spec_expr(provide, &vars)?.try_into()?;
            provides.push(provide);
        }

        // Matches.
        let mut matches: Vec<ExprId> = Vec::new();
        for m in &term_spec.matches {
            let m = self.spec_expr(m, &vars)?.try_into()?;
            matches.push(m);
        }

        // Partial function.
        // REVIEW(mbm): pin down semantics for partial function specifications.
        if let Domain::Partial(p) = domain {
            let all_matches = self.all(matches);
            let eq = self.exprs_equal(p, all_matches);
            self.conditions.assumptions.push(eq);
        } else if !matches.is_empty() {
            bail!("spec matches on non-partial function");
        }

        // Assert/assume depending on caller or callee.
        match invocation {
            Invocation::Caller => {
                self.conditions.assertions.extend(requires);
                self.conditions.assumptions.extend(provides);
            }
            Invocation::Callee => {
                self.conditions.assumptions.extend(requires);
                self.conditions.assertions.extend(provides);
            }
        }

        // Record modified state.
        self.modified_state
            .extend(term_spec.modifies.iter().map(|v| v.0.clone()));

        // Record callsite.
        self.record_term_instantiation(term, args.to_vec(), ret)?;

        Ok(())
    }

    fn record_term_instantiation(
        &mut self,
        term: TermId,
        args: Vec<Symbolic>,
        ret: Symbolic,
    ) -> Result<()> {
        let signatures = self
            .prog
            .specenv
            .resolve_term_instantiations(&term, &self.prog.tyenv)?;
        self.conditions.calls.push(Call {
            term,
            args,
            ret,
            signatures,
        });
        Ok(())
    }

    fn make_variant(
        &mut self,
        id: BindingId,
        ty: TypeId,
        variant: VariantId,
        fields: &[BindingId],
    ) -> Result<()> {
        // Lookup term corresponding to variant.
        let variant_term_id = self.prog.get_variant_term(ty, variant);

        // Invoke as a constructor.
        self.constructor(id, variant_term_id, fields, Invocation::Caller)?;

        Ok(())
    }

    fn match_variant(
        &mut self,
        id: BindingId,
        source: BindingId,
        variant: VariantId,
        field: TupleIndex,
    ) -> Result<()> {
        // Source binding should be an enum.
        let e = self.binding_value[&source]
            .as_enum()
            .ok_or(format_err!(
                "target of variant constraint should be an enum"
            ))?
            .clone();

        // Lookup enum type via corresponding constriant,
        let tys: Vec<_> = self
            .expansion
            .constraints
            .iter()
            .flat_map(|c| match c {
                Constrain::Match(id, Constraint::Variant { ty, variant: v, .. })
                    if *id == source && *v == variant =>
                {
                    Some(ty)
                }
                _ => None,
            })
            .collect();
        if tys.len() != 1 {
            bail!("expected exactly one variant constraint for match variant binding");
        }
        let ty = tys[0];

        // Lookup variant and field.
        let variant_type = self.prog.tyenv.get_variant(*ty, variant);
        let variant_name = self.prog.tyenv.syms[variant_type.name.index()].as_str();

        let field_sym = variant_type.fields[field.index()].name;
        let field_name = &self.prog.tyenv.syms[field_sym.index()];

        // Destination binding.
        let v = self.binding_value[&id].clone();

        // Assumption: if the variant matches then the destination binding
        // equals the projected field.
        let variant = e.try_variant_by_name(variant_name)?;
        let field = variant.try_field_by_name(field_name)?;

        let discriminator = self.discriminator(&e, variant);
        let eq = self.values_equal(v, field.value.clone());
        let constraint = self.dedup_expr(Expr::Imp(discriminator, eq));
        self.conditions.assumptions.push(constraint);

        Ok(())
    }

    fn make_some(&mut self, id: BindingId, inner: BindingId) -> Result<()> {
        // Destination binding should be an option.
        let opt = self.binding_value[&id]
            .as_option()
            .expect("destination of make_some binding should be an option")
            .clone();

        // Inner binding.
        let inner = self.binding_value[&inner].clone();

        // Assumption: option is Some.
        self.conditions.assumptions.push(opt.some);

        // Assumption: option value is equal to this binding.
        let eq = self.values_equal(inner, (*opt.inner).clone());
        self.conditions.assumptions.push(eq);

        Ok(())
    }

    fn match_some(&mut self, id: BindingId, source: BindingId) -> Result<()> {
        // Source should be an option.
        let opt = self.binding_value[&source]
            .as_option()
            .expect("source of match_some binding should be an option")
            .clone();

        // Destination binding.
        let v = self.binding_value[&id].clone();

        // Assumption: if the option is some, then the inner value
        // equals this binding.
        let eq = self.values_equal(v, (*opt.inner).clone());
        let constraint = self.dedup_expr(Expr::Imp(opt.some, eq));
        self.conditions.assumptions.push(constraint);

        Ok(())
    }

    fn match_tuple(&mut self, id: BindingId, source: BindingId, field: TupleIndex) -> Result<()> {
        // Source should be a tuple. Access its fields.
        let fields = self.binding_value[&source]
            .as_tuple()
            .expect("source of match_tuple binding should be a tuple")
            .clone();

        // Destination binding.
        let v = self.binding_value[&id].clone();

        // Assumption: indexed field should equal this binding.
        let eq = self.values_equal(v, fields[field.index()].clone());
        self.conditions.assumptions.push(eq);

        Ok(())
    }

    fn constrain(&mut self, constrain: &Constrain) -> Result<ExprId> {
        match constrain {
            Constrain::Match(binding_id, constraint) => self.constraint(*binding_id, constraint),
            Constrain::NotAll(constrains) => {
                let cs = constrains
                    .iter()
                    .map(|c| self.constrain(c))
                    .collect::<Result<_>>()?;
                let all = self.all(cs);
                let not_all = self.dedup_expr(Expr::Not(all));
                Ok(not_all)
            }
        }
    }

    fn constraint(&mut self, binding_id: BindingId, constraint: &Constraint) -> Result<ExprId> {
        match constraint {
            Constraint::Some => self.constraint_some(binding_id),
            Constraint::ConstPrim { val } => self.equals_const_prim(binding_id, *val),
            Constraint::ConstInt { val, ty } => self.equals_const_int(binding_id, *val, *ty),
            Constraint::Variant {
                ty,
                variant,
                fields: _,
            } => self.constraint_variant(binding_id, *ty, *variant),
        }
    }

    fn constraint_some(&mut self, binding_id: BindingId) -> Result<ExprId> {
        // Constrained binding should be an option.
        let opt = self.binding_value[&binding_id]
            .as_option()
            .expect("target of some constraint should be an option")
            .clone();

        // Constraint: option is Some.
        Ok(opt.some)
    }

    fn constraint_variant(
        &mut self,
        binding_id: BindingId,
        ty: TypeId,
        variant: VariantId,
    ) -> Result<ExprId> {
        // Constrained binding should be an enum.
        let e = self.binding_value[&binding_id]
            .as_enum()
            .ok_or(format_err!(
                "target of variant constraint should be an enum"
            ))?
            .clone();

        // TODO(mbm): check the enum type is correct?

        // Lookup variant.
        let variant_type = self.prog.tyenv.get_variant(ty, variant);
        let variant_name = self.prog.tyenv.syms[variant_type.name.index()].as_str();

        // Assumption: discriminant equals variant.
        let variant = e.try_variant_by_name(variant_name)?;
        let discriminator = self.discriminator(&e, variant);
        Ok(discriminator)
    }

    fn spec_expr(&mut self, expr: &spec::Expr, vars: &Variables) -> Result<Symbolic> {
        match &expr.x {
            spec::ExprKind::Var(v) => {
                let v = vars.get(&v.0)?;
                Ok(v.clone())
            }

            spec::ExprKind::Const(c) => Ok(self.constant(c.clone()).into()),

            spec::ExprKind::Constructor(constructor) => self.construct(constructor, vars),

            spec::ExprKind::Field(name, x) => {
                let x = self.spec_expr(x, vars)?;
                self.spec_field(name, x)
            }

            spec::ExprKind::Discriminator(variant, x) => {
                let x = self.spec_expr(x, vars)?;
                self.spec_discriminator(variant, x)
            }

            // TODO(mbm): fix boilerplate for common expressions
            spec::ExprKind::Not(x) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                Ok(self.scalar(Expr::Not(x)))
            }

            spec::ExprKind::And(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::And(x, y)))
            }

            spec::ExprKind::Or(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::Or(x, y)))
            }

            spec::ExprKind::Imp(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::Imp(x, y)))
            }

            spec::ExprKind::Eq(x, y) => {
                let x = self.spec_expr(x, vars)?;
                let y = self.spec_expr(y, vars)?;
                Ok(self.values_equal(x, y).into())
            }

            spec::ExprKind::Lt(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::Lt(x, y)))
            }

            spec::ExprKind::Lte(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::Lte(x, y)))
            }

            spec::ExprKind::Gt(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::Lt(y, x)))
            }

            spec::ExprKind::Gte(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::Lte(y, x)))
            }

            spec::ExprKind::BVUlt(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVUlt(x, y)))
            }

            spec::ExprKind::BVUle(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVUle(x, y)))
            }

            spec::ExprKind::BVSge(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVSge(x, y)))
            }

            spec::ExprKind::BVSlt(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVSlt(x, y)))
            }

            spec::ExprKind::BVSle(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVSle(x, y)))
            }

            spec::ExprKind::BVSgt(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVSgt(x, y)))
            }

            spec::ExprKind::BVUgt(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVUgt(x, y)))
            }

            spec::ExprKind::BVUge(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVUge(x, y)))
            }

            spec::ExprKind::BVSaddo(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVSaddo(x, y)))
            }

            spec::ExprKind::BVNot(x) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVNot(x)))
            }

            spec::ExprKind::BVNeg(x) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVNeg(x)))
            }

            spec::ExprKind::Cls(x) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                Ok(self.scalar(Expr::Cls(x)))
            }

            spec::ExprKind::BVAdd(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVAdd(x, y)))
            }

            spec::ExprKind::BVSub(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVSub(x, y)))
            }

            spec::ExprKind::BVMul(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVMul(x, y)))
            }

            spec::ExprKind::BVSDiv(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVSDiv(x, y)))
            }

            spec::ExprKind::BVAnd(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVAnd(x, y)))
            }

            spec::ExprKind::BVOr(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVOr(x, y)))
            }

            spec::ExprKind::BVXor(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVXor(x, y)))
            }

            spec::ExprKind::BVShl(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVShl(x, y)))
            }

            spec::ExprKind::BVLShr(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVLShr(x, y)))
            }

            spec::ExprKind::BVAShr(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVAShr(x, y)))
            }

            spec::ExprKind::BVUDiv(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVUDiv(x, y)))
            }

            spec::ExprKind::BVURem(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVURem(x, y)))
            }

            spec::ExprKind::BVSRem(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVSRem(x, y)))
            }

            spec::ExprKind::BVRotl(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVRotl(x, y)))
            }

            spec::ExprKind::BVRotr(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVRotr(x, y)))
            }

            spec::ExprKind::Conditional(c, t, e) => {
                let c = self.spec_expr(c, vars)?.try_into()?;
                let t = self.spec_expr(t, vars)?;
                let e = self.spec_expr(e, vars)?;
                self.conditional(c, t, e)
            }

            spec::ExprKind::Switch(on, arms) => self.spec_switch(on, arms, vars),

            spec::ExprKind::Match(on, arms) => self.spec_match(on, arms, vars),

            spec::ExprKind::Let(defs, body) => self.spec_let(defs, body, vars),

            spec::ExprKind::With(decls, body) => self.spec_with(decls, body, vars),

            spec::ExprKind::Macro(ident, args) => self.spec_macro(ident, args, vars),

            spec::ExprKind::BVZeroExt(w, x) => {
                let w = self.spec_expr(w, vars)?.try_into()?;
                let x = self.spec_expr(x, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVZeroExt(w, x)))
            }

            spec::ExprKind::BVSignExt(w, x) => {
                let w = self.spec_expr(w, vars)?.try_into()?;
                let x = self.spec_expr(x, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVSignExt(w, x)))
            }

            spec::ExprKind::BVConvTo(w, x) => {
                let w = self.spec_expr(w, vars)?.try_into()?;
                let x = self.spec_expr(x, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVConvTo(w, x)))
            }

            spec::ExprKind::BVExtract(h, l, x) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVExtract(*h, *l, x)))
            }

            spec::ExprKind::BVConcat(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVConcat(x, y)))
            }

            spec::ExprKind::Int2BV(w, x) => {
                let w = self.spec_expr(w, vars)?.try_into()?;
                let x = self.spec_expr(x, vars)?.try_into()?;
                Ok(self.scalar(Expr::Int2BV(w, x)))
            }

            spec::ExprKind::BV2Nat(x) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                Ok(self.scalar(Expr::BV2Nat(x)))
            }

            spec::ExprKind::WidthOf(x) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                Ok(self.scalar(Expr::WidthOf(x)))
            }

            spec::ExprKind::As(x, ty) => {
                let x = self.spec_expr(x, vars)?;
                self.conditions.qualifiers.push(Qualifier {
                    value: x.clone(),
                    ty: ty.clone(),
                });
                Ok(x)
            }
        }
    }

    fn spec_expr_no_vars(&mut self, expr: &spec::Expr) -> Result<Symbolic> {
        let no_vars = Variables::new();
        self.spec_expr(expr, &no_vars)
    }

    fn spec_typed_value(&mut self, val: i128, ty: &Type) -> Result<ExprId> {
        match ty {
            Type::Bool => Ok(self.boolean(match val {
                0 => false,
                1 => true,
                _ => bail!("boolean value must be zero or one"),
            })),
            Type::Int => Ok(self.constant(Const::Int(val))),
            Type::BitVector(Width::Bits(w)) => {
                Ok(self.constant(Const::BitVector(*w, val.try_into()?)))
            }
            _ => bail!("cannot construct constant of type {ty}"),
        }
    }

    fn construct(&mut self, constructor: &Constructor, vars: &Variables) -> Result<Symbolic> {
        match constructor {
            Constructor::Enum {
                name,
                variant,
                args,
            } => {
                // Lookup ISLE type by name.
                let type_id = self
                    .prog
                    .tyenv
                    .get_type_by_name(name)
                    .ok_or(format_err!("unknown enum type {name}", name = name.0))?;

                // Determine type model.
                let model = self
                    .prog
                    .specenv
                    .type_model
                    .get(&type_id)
                    .ok_or(format_err!(
                        "unspecified model for type {name}",
                        name = name.0
                    ))?;

                // Should be an enum.
                let e = model.as_enum().ok_or(format_err!(
                    "{name} expected to have enum type",
                    name = name.0
                ))?;

                // Lookup variant.
                let variant =
                    e.variants
                        .iter()
                        .find(|v| v.name.0 == variant.0)
                        .ok_or(format_err!(
                            "unknown variant {variant}",
                            variant = variant.0
                        ))?;

                // Discriminant: constant value since we are constructing a known variant.
                let discriminant = self.constant(Const::Int(variant.id.index().try_into()?));

                // Variants: undefined except for the variant under construction.
                let variants = e
                    .variants
                    .iter()
                    .map(|v| {
                        // For all except the variant under construction, allocate an undefined variant.
                        if v.id != variant.id {
                            // QUESTION(mbm): use undef variant or IfThen and fresh bits in solver?
                            return self.alloc_variant(v, "undef".to_string());
                        }

                        // Construct a variant provided arguments.
                        assert_eq!(args.len(), v.fields.len());
                        let fields = zip(&v.fields, args)
                            .map(|(f, a)| {
                                Ok(SymbolicField {
                                    name: f.name.0.clone(),
                                    value: self.spec_expr(a, vars)?,
                                })
                            })
                            .collect::<Result<_>>()?;
                        Ok(SymbolicVariant {
                            name: v.name.0.clone(),
                            id: v.id,
                            discriminant: v.id.index(),
                            value: Symbolic::Struct(fields),
                        })
                    })
                    .collect::<Result<_>>()?;

                Ok(self.new_enum(type_id, discriminant, variants)?)
            }
        }
    }

    fn spec_field(&mut self, name: &Ident, v: Symbolic) -> Result<Symbolic> {
        log::trace!("access field {name} from {v}", name = name.0);

        let fields = v
            .as_struct()
            .ok_or(format_err!("field access from non-struct value"))?;

        let field = fields
            .iter()
            .find(|f| f.name == name.0)
            .ok_or(format_err!("missing struct field: {}", name.0))?;

        Ok(field.value.clone())
    }

    fn spec_discriminator(&mut self, name: &Ident, v: Symbolic) -> Result<Symbolic> {
        let e = v
            .as_enum()
            .ok_or(format_err!("discriminator for non-enum value"))?;
        let variant = e.try_variant_by_name(&name.0)?;
        let discriminator = self.discriminator(e, variant);
        Ok(discriminator.into())
    }

    fn discriminator(&mut self, e: &SymbolicEnum, variant: &SymbolicVariant) -> ExprId {
        let discriminant = self.constant(Const::Int(variant.discriminant.try_into().unwrap()));
        self.exprs_equal(e.discriminant, discriminant)
    }

    fn spec_switch(
        &mut self,
        on: &spec::Expr,
        arms: &[(spec::Expr, spec::Expr)],
        vars: &Variables,
    ) -> Result<Symbolic> {
        // Generate branch arms.
        let on = self.spec_expr(on, vars)?;
        let cases = arms
            .iter()
            .map(|(value, then)| {
                let value = self.spec_expr(value, vars)?;
                let cond = self.values_equal(on.clone(), value);
                Ok((cond, self.spec_expr(then, vars)?))
            })
            .collect::<Result<Vec<_>>>()?;

        // Build an expression splitting over cases.
        self.cases(&cases)
    }

    fn spec_match(&mut self, on: &spec::Expr, arms: &[Arm], vars: &Variables) -> Result<Symbolic> {
        // Generate the enum value to match on.
        let on = self.spec_expr(on, vars)?;
        let e = on.as_enum().ok_or(format_err!("match on non-enum value"))?;

        // Generate cases.
        let mut cases = Vec::new();
        for arm in arms {
            // Lookup the variant.
            let variant = e.try_variant_by_name(&arm.variant.0)?;

            // Arm condition is that the discriminant matches the variant.
            let cond = self.discriminator(e, variant);

            // Arm value is the result of the body expression, evaluated with
            // the variants fields brought into scope.
            let Some(fields) = variant.value.as_struct() else {
                bail!("variant {name} must have struct value", name = variant.name);
            };
            if arm.args.len() != fields.len() {
                bail!(
                    "incorrect number of arguments for variant {name}",
                    name = variant.name
                );
            }
            let mut arm_vars = vars.clone();
            for (arg, field) in zip(&arm.args, fields) {
                arm_vars.set(arg.0.clone(), field.value.clone())?;
            }
            let body = self.spec_expr(&arm.body, &arm_vars)?;

            // Add case for this match arm.
            cases.push((cond, body));
        }

        // Build an expression splitting over cases.
        self.cases(&cases)
    }

    fn cases(&mut self, cases: &[(ExprId, Symbolic)]) -> Result<Symbolic> {
        // Build an undefined fallback value.
        let Some((_, value)) = cases.last() else {
            bail!("must have at least one case");
        };
        let fallback = value.scalar_map(&mut |_| self.undef_variable());

        // Represent as nested conditionals.
        cases
            .iter()
            .rev()
            .cloned()
            .try_fold(fallback, |acc, (cond, then)| {
                self.conditional(cond, then, acc)
            })
    }

    fn spec_let(
        &mut self,
        defs: &[(Ident, spec::Expr)],
        body: &spec::Expr,
        vars: &Variables,
    ) -> Result<Symbolic> {
        // Evaluate let defs.
        let mut let_vars = vars.clone();
        for (name, expr) in defs {
            let expr = self.spec_expr(expr, &let_vars)?;
            let_vars.set(name.0.clone(), expr)?;
        }

        // Evaluate body in let-binding scope.
        self.spec_expr(body, &let_vars)
    }

    fn spec_with(
        &mut self,
        decls: &[Ident],
        body: &spec::Expr,
        vars: &Variables,
    ) -> Result<Symbolic> {
        // Declare new variables.
        let mut with_vars = vars.clone();
        for name in decls {
            // QUESTION(mbm): allow with scopes to optionally specify types?
            let expr = Symbolic::Scalar(self.alloc_variable(Type::Unknown, name.0.clone()));
            with_vars.set(name.0.clone(), expr)?;
        }

        // Evaluate body in new scope.
        self.spec_expr(body, &with_vars)
    }

    fn spec_macro(
        &mut self,
        name: &Ident,
        args: &[spec::Expr],
        vars: &Variables,
    ) -> Result<Symbolic> {
        // Lookup macro.
        let macro_defn = self
            .prog
            .specenv
            .macros
            .get(&name.0)
            .ok_or(format_err!("unknown macro {name}", name = name.0))?;

        // Build macro expansion scope.
        // QUESTION(mbm): should macros be able to access global state?
        let mut macro_vars = Variables::new();
        if macro_defn.params.len() != args.len() {
            bail!(
                "incorrect number of arguments for macro {name}",
                name = name.0
            );
        }
        for (param, arg) in zip(&macro_defn.params, args) {
            let arg = self.spec_expr(arg, vars)?;
            macro_vars.set(param.0.clone(), arg)?;
        }

        // Evaluate macro body.
        self.spec_expr(&macro_defn.body, &macro_vars)
    }

    fn conditional(&mut self, c: ExprId, t: Symbolic, e: Symbolic) -> Result<Symbolic> {
        Symbolic::merge(&t, &e, &mut |t, e| {
            self.dedup_expr(Expr::Conditional(c, t, e))
        })
    }

    fn bindings_equal(&mut self, a: BindingId, b: BindingId) -> ExprId {
        // TODO(mbm): can this be done without clones?
        let a = self.binding_value[&a].clone();
        let b = self.binding_value[&b].clone();
        self.values_equal(a, b)
    }

    fn values_equal(&mut self, a: Symbolic, b: Symbolic) -> ExprId {
        match (a, b) {
            (Symbolic::Scalar(u), Symbolic::Scalar(v)) => self.exprs_equal(u, v),

            (Symbolic::Struct(us), Symbolic::Struct(vs)) => {
                // Field-wise equality.
                // TODO(mbm): can we expect that structs are the same length?
                assert_eq!(us.len(), vs.len(), "field length mismatch");
                let fields_eq = zip(us, vs)
                    .map(|(fu, fv)| {
                        assert_eq!(fu.name, fv.name, "field name mismatch");
                        self.values_equal(fu.value, fv.value)
                    })
                    .collect();

                // All fields must be equal.
                self.all(fields_eq)
            }

            (Symbolic::Enum(u), Symbolic::Enum(v)) => {
                // Discriminant equality.
                let discriminants_eq = self.exprs_equal(u.discriminant, v.discriminant);
                let mut equalities = vec![discriminants_eq];

                // Variant equality conditions.
                assert_eq!(u.variants.len(), v.variants.len(), "variant count mismatch");
                let variants_eq = zip(&u.variants, &v.variants).map(|(uv, vv)| {
                    assert_eq!(uv.name, vv.name, "variant name mismatch");
                    let ud = self.discriminator(&u, uv);
                    let eq = self.values_equal(uv.value.clone(), vv.value.clone());
                    self.dedup_expr(Expr::Imp(ud, eq))
                });
                equalities.extend(variants_eq);

                // Combine discriminant and variant conditions.
                self.all(equalities)
            }

            (Symbolic::Tuple(us), Symbolic::Tuple(vs)) => {
                // Field-wise equality.
                // TODO(mbm): can we expect that tuples are the same length?
                assert_eq!(us.len(), vs.len(), "tuple length mismatch");
                let fields_eq = zip(us, vs).map(|(u, v)| self.values_equal(u, v)).collect();

                // All fields must be equal.
                self.all(fields_eq)
            }

            ref c => todo!("values equal: {c:?}"),
        }
    }

    fn exprs_equal(&mut self, lhs: ExprId, rhs: ExprId) -> ExprId {
        self.dedup_expr(Expr::Eq(lhs, rhs))
    }

    fn all(&mut self, exprs: Vec<ExprId>) -> ExprId {
        exprs
            .into_iter()
            .reduce(|acc, e| self.dedup_expr(Expr::And(acc, e)))
            .unwrap_or_else(|| self.boolean(true))
    }

    fn boolean(&mut self, value: bool) -> ExprId {
        self.constant(Const::Bool(value))
    }

    fn constant(&mut self, c: Const) -> ExprId {
        self.dedup_expr(Expr::Const(c))
    }

    /// Determine the type of the given binding in the context of the
    /// [Expansion] we are constructing verification conditions for.
    fn binding_type(&self, binding: &Binding) -> BindingType {
        binding_type(
            binding,
            self.expansion.term,
            self.prog,
            |binding_id: BindingId| self.expansion.bindings[binding_id.index()].clone().unwrap(),
        )
    }

    fn alloc_binding(&mut self, binding_type: &BindingType, name: String) -> Result<Symbolic> {
        match binding_type {
            BindingType::Base(type_id) => self.alloc_model(*type_id, name),
            BindingType::Option(inner_type) => {
                let some = self.alloc_variable(Type::Bool, Variable::component_name(&name, "some"));
                let inner = Box::new(
                    self.alloc_binding(inner_type, Variable::component_name(&name, "inner"))?,
                );
                Ok(Symbolic::Option(SymbolicOption { some, inner }))
            }
            BindingType::Tuple(inners) => {
                let inners = inners
                    .iter()
                    .enumerate()
                    .map(|(i, inner_type)| {
                        self.alloc_binding(
                            inner_type,
                            Variable::component_name(&name, &i.to_string()),
                        )
                    })
                    .collect::<Result<_>>()?;
                Ok(Symbolic::Tuple(inners))
            }
        }
    }

    fn alloc_value(&mut self, ty: &Compound, name: String) -> Result<Symbolic> {
        match ty {
            Compound::Primitive(ty) => Ok(Symbolic::Scalar(self.alloc_variable(ty.clone(), name))),
            Compound::Struct(fields) => Ok(Symbolic::Struct(
                fields
                    .iter()
                    .map(|f| {
                        Ok(SymbolicField {
                            name: f.name.0.clone(),
                            value: self
                                .alloc_value(&f.ty, Variable::component_name(&name, &f.name.0))?,
                        })
                    })
                    .collect::<Result<_>>()?,
            )),
            Compound::Enum(e) => {
                let discriminant =
                    self.alloc_variable(Type::Int, Variable::component_name(&name, "discriminant"));
                let variants = e
                    .variants
                    .iter()
                    .map(|v| self.alloc_variant(v, name.clone()))
                    .collect::<Result<_>>()?;
                Ok(self.new_enum(e.id, discriminant, variants)?)
            }
            Compound::Named(_) => {
                let ty = self.prog.specenv.resolve_type(ty, &self.prog.tyenv)?;
                self.alloc_value(&ty, name)
            }
        }
    }

    fn new_enum(
        &mut self,
        ty: TypeId,
        discriminant: ExprId,
        variants: Vec<SymbolicVariant>,
    ) -> Result<Symbolic> {
        // Construct symbolic enum and ensure it's valid.
        let e = SymbolicEnum {
            ty,
            discriminant,
            variants,
        };
        e.validate()?;

        // Assume discriminant invariant: positive integer less than number of
        // variants.
        let zero = self.constant(Const::Int(0));
        let num_variants = self.constant(Const::Int(e.variants.len().try_into()?));
        let discriminant_positive = self.dedup_expr(Expr::Lte(zero, discriminant));
        let discriminant_less_than_num_variants =
            self.dedup_expr(Expr::Lt(discriminant, num_variants));
        let discriminant_in_range = self.dedup_expr(Expr::And(
            discriminant_positive,
            discriminant_less_than_num_variants,
        ));
        self.conditions.assumptions.push(discriminant_in_range);

        // Variant term instantiations.
        let ret = Symbolic::Enum(e.clone());
        for variant in &e.variants {
            let term = self.prog.get_variant_term(e.ty, variant.id);
            let args = variant.field_values()?;
            self.record_term_instantiation(term, args, ret.clone())?;
        }

        Ok(ret)
    }

    fn alloc_variant(&mut self, variant: &Variant, name: String) -> Result<SymbolicVariant> {
        let name = Variable::component_name(&name, &variant.name.0);
        Ok(SymbolicVariant {
            name: variant.name.0.clone(),
            id: variant.id,
            discriminant: variant.id.index(),
            value: self.alloc_value(&variant.ty(), name)?,
        })
    }

    fn alloc_model(&mut self, type_id: TypeId, name: String) -> Result<Symbolic> {
        let type_name = self.prog.type_name(type_id);
        let ty = self
            .prog
            .specenv
            .type_model
            .get(&type_id)
            .ok_or(format_err!("unspecified model for type {type_name}"))?;
        self.alloc_value(ty, name)
    }

    fn undef_variable(&mut self) -> ExprId {
        self.alloc_variable(Type::Unknown, "undef".to_string())
    }

    fn alloc_variable(&mut self, ty: Type, name: String) -> ExprId {
        let v = VariableId(self.conditions.variables.len());
        self.conditions.variables.push(Variable { ty, name });
        self.dedup_expr(Expr::Variable(v))
    }

    fn scalar(&mut self, expr: Expr) -> Symbolic {
        Symbolic::Scalar(self.dedup_expr(expr))
    }

    fn dedup_expr(&mut self, expr: Expr) -> ExprId {
        if let Some(id) = self.expr_map.get(&expr) {
            *id
        } else {
            let id = ExprId(self.conditions.exprs.len());
            self.conditions.exprs.push(expr.clone());
            self.expr_map.insert(expr, id);
            id
        }
    }
}
