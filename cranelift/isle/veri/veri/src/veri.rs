use crate::{
    expand::Expansion,
    program::Program,
    spec::{self, Signature},
    trie_again::{binding_type, BindingType},
    types::{Compound, Const, Type, Width},
};
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
    Lte(ExprId, ExprId),

    BVUlt(ExprId, ExprId),
    BVUle(ExprId, ExprId),
    BVSge(ExprId, ExprId),
    BVSlt(ExprId, ExprId),
    BVSle(ExprId, ExprId),

    BVSaddo(ExprId, ExprId),

    // Unary.
    BVNot(ExprId),
    BVNeg(ExprId),

    // Binary.
    BVAdd(ExprId, ExprId),
    BVSub(ExprId, ExprId),
    BVMul(ExprId, ExprId),
    BVAnd(ExprId, ExprId),
    BVOr(ExprId, ExprId),
    BVShl(ExprId, ExprId),
    BVLShr(ExprId, ExprId),
    BVAShr(ExprId, ExprId),

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
    Int2BV(usize, ExprId),

    // Bitwidth.
    WidthOf(ExprId),
}

impl Expr {
    pub fn is_variable(&self) -> bool {
        matches!(self, Self::Variable(_))
    }

    pub fn sources(&self) -> Vec<ExprId> {
        match self {
            Self::Const(_) | Self::Variable(_) => Vec::new(),
            // Unary
            &Self::Not(x)
            | &Self::BVNot(x)
            | &Self::BVNeg(x)
            | &Self::BVExtract(_, _, x)
            | &Self::Int2BV(_, x)
            | &Self::WidthOf(x) => vec![x],

            // Binary
            &Self::And(x, y)
            | &Self::Or(x, y)
            | &Self::Imp(x, y)
            | &Self::Eq(x, y)
            | &Self::Lte(x, y)
            | &Self::BVUlt(x, y)
            | &Self::BVUle(x, y)
            | &Self::BVSge(x, y)
            | &Self::BVSlt(x, y)
            | &Self::BVSle(x, y)
            | &Self::BVSaddo(x, y)
            | &Self::BVAdd(x, y)
            | &Self::BVSub(x, y)
            | &Self::BVMul(x, y)
            | &Self::BVAnd(x, y)
            | &Self::BVOr(x, y)
            | &Self::BVShl(x, y)
            | &Self::BVLShr(x, y)
            | &Self::BVAShr(x, y)
            | &Self::BVZeroExt(x, y)
            | &Self::BVSignExt(x, y)
            | &Self::BVConvTo(x, y)
            | &Self::BVConcat(x, y) => vec![x, y],

            // Ternary
            &Self::Conditional(c, t, e) => vec![c, t, e],
        }
    }
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Const(c) => write!(f, "const({c})"),
            Self::Variable(v) => write!(f, "var({})", v.index()),
            Self::Not(x) => write!(f, "!{}", x.index()),
            Self::And(x, y) => write!(f, "{} && {}", x.index(), y.index()),
            Self::Or(x, y) => write!(f, "{} || {}", x.index(), y.index()),
            Self::Imp(x, y) => write!(f, "{} => {}", x.index(), y.index()),
            Self::Eq(x, y) => write!(f, "{} == {}", x.index(), y.index()),
            Self::Lte(x, y) => write!(f, "{} <= {}", x.index(), y.index()),
            Self::BVUlt(x, y) => write!(f, "bvult({}, {})", x.index(), y.index()),
            Self::BVUle(x, y) => write!(f, "bvule({}, {})", x.index(), y.index()),
            Self::BVSge(x, y) => write!(f, "bvsge({}, {})", x.index(), y.index()),
            Self::BVSlt(x, y) => write!(f, "bvslt({}, {})", x.index(), y.index()),
            Self::BVSle(x, y) => write!(f, "bvsle({}, {})", x.index(), y.index()),
            Self::BVSaddo(x, y) => write!(f, "bvsaddo({}, {})", x.index(), y.index()),
            Self::BVNot(x) => write!(f, "bvnot({})", x.index()),
            Self::BVNeg(x) => write!(f, "bvneg({})", x.index()),
            Self::BVAdd(x, y) => write!(f, "bvadd({}, {})", x.index(), y.index()),
            Self::BVSub(x, y) => write!(f, "bvsub({}, {})", x.index(), y.index()),
            Self::BVMul(x, y) => write!(f, "bvmul({}, {})", x.index(), y.index()),
            Self::BVAnd(x, y) => write!(f, "bvand({}, {})", x.index(), y.index()),
            Self::BVOr(x, y) => write!(f, "bvor({}, {})", x.index(), y.index()),
            Self::BVShl(x, y) => write!(f, "bvshl({}, {})", x.index(), y.index()),
            Self::BVLShr(x, y) => write!(f, "bvlshr({}, {})", x.index(), y.index()),
            Self::BVAShr(x, y) => write!(f, "bvashr({}, {})", x.index(), y.index()),
            Self::Conditional(c, t, e) => {
                write!(f, "{} ? {} : {}", c.index(), t.index(), e.index())
            }
            Self::BVZeroExt(w, x) => write!(f, "bv_zero_ext({}, {})", w.index(), x.index()),
            Self::BVSignExt(w, x) => write!(f, "bv_zero_ext({}, {})", w.index(), x.index()),
            Self::BVConvTo(w, x) => write!(f, "bv_conv_to({}, {})", w.index(), x.index()),
            Self::BVExtract(h, l, x) => write!(f, "bv_extract({h}, {l}, {})", x.index()),
            Self::BVConcat(x, y) => write!(f, "bv_concat({}, {})", x.index(), y.index()),
            Self::Int2BV(w, x) => write!(f, "int2bv({w}, {})", x.index()),
            Self::WidthOf(x) => write!(f, "width_of({})", x.index()),
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
    name: String,
    value: Symbolic,
}

impl SymbolicField {
    fn eval(&self, model: &Model) -> anyhow::Result<FieldValue> {
        Ok(FieldValue {
            name: self.name.clone(),
            value: self.value.eval(model)?,
        })
    }
}

#[derive(Debug, Clone)]
pub enum Symbolic {
    Scalar(ExprId),
    Struct(Vec<SymbolicField>),
    Option(SymbolicOption),
    Tuple(Vec<Symbolic>),
}

#[derive(Debug, Clone)]
pub struct FieldValue {
    name: String,
    value: Value,
}

#[derive(Clone, Debug)]
pub enum Value {
    Const(Const),
    Struct(Vec<FieldValue>),
    Option(Option<Box<Value>>),
    Tuple(Vec<Value>),
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

    fn eval(&self, model: &Model) -> anyhow::Result<Value> {
        match self {
            Symbolic::Scalar(x) => Ok(Value::Const(
                model
                    .get(x)
                    .ok_or(anyhow::format_err!("undefined expression in model"))?
                    .clone(),
            )),
            Symbolic::Struct(fields) => Ok(Value::Struct(
                fields
                    .iter()
                    .map(|f| f.eval(model))
                    .collect::<anyhow::Result<_>>()?,
            )),
            Symbolic::Option(opt) => match model.get(&opt.some) {
                Some(Const::Bool(true)) => {
                    Ok(Value::Option(Some(Box::new(opt.inner.eval(model)?))))
                }
                Some(Const::Bool(false)) => Ok(Value::Option(None)),
                Some(_) => anyhow::bail!("model value for option some is not boolean"),
                None => anyhow::bail!("undefined expression in model"),
            },
            Symbolic::Tuple(elements) => Ok(Value::Tuple(
                elements
                    .iter()
                    .map(|s| s.eval(model))
                    .collect::<anyhow::Result<_>>()?,
            )),
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
        v.as_scalar()
            .ok_or(anyhow::format_err!("should be scalar value"))
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

/// Verification conditions for an expansion.
#[derive(Debug, Default)]
pub struct Conditions {
    pub exprs: Vec<Expr>,
    pub assumptions: Vec<ExprId>,
    pub assertions: Vec<ExprId>,
    pub variables: Vec<Variable>,
    pub calls: Vec<Call>,
}

impl Conditions {
    pub fn from_expansion(expansion: &Expansion, prog: &Program) -> anyhow::Result<Self> {
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

    pub fn validate(&self) -> anyhow::Result<()> {
        // Ensure there are no dangling expressions.
        let reachable = self.reachable();
        for x in (0..self.exprs.len()).map(ExprId) {
            if self.exprs[x.index()].is_variable() {
                continue;
            }
            if !reachable.contains(&x) {
                anyhow::bail!("expression {x} is unreachable", x = x.index());
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

    pub fn print_model(&self, model: &Model, prog: &Program) -> anyhow::Result<()> {
        // Calls
        for call in &self.calls {
            println!(
                "{term_name}({args}) -> {ret}",
                term_name = prog.term_name(call.term),
                args = call
                    .args
                    .iter()
                    .map(|a| Ok(a.eval(model)?.to_string()))
                    .collect::<anyhow::Result<Vec<_>>>()?
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

struct ConditionsBuilder<'a> {
    expansion: &'a Expansion,
    prog: &'a Program,

    binding_value: HashMap<BindingId, Symbolic>,
    expr_map: HashMap<Expr, ExprId>,
    conditions: Conditions,
}

impl<'a> ConditionsBuilder<'a> {
    fn new(expansion: &'a Expansion, prog: &'a Program) -> Self {
        Self {
            expansion,
            prog,
            binding_value: HashMap::new(),
            expr_map: HashMap::new(),
            conditions: Conditions::default(),
        }
    }

    fn build(mut self) -> anyhow::Result<Conditions> {
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
        for (binding_id, constraints) in &self.expansion.constraints {
            for constraint in constraints {
                self.add_constraint(*binding_id, constraint)?;
            }
        }

        // Equals.
        for (a, b) in self.expansion.equalities() {
            let eq = self.bindings_equal(a, b);
            self.conditions.assumptions.push(eq);
        }

        // Validate
        self.conditions.validate()?;

        Ok(self.conditions)
    }

    fn add_binding(&mut self, id: BindingId, binding: &Binding) -> anyhow::Result<()> {
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

            Binding::MatchVariant { .. } => todo!("binding: match_variant"),

            Binding::MakeSome { inner } => self.make_some(id, *inner),

            Binding::MatchSome { source } => self.match_some(id, *source),

            Binding::MatchTuple { source, field } => self.match_tuple(id, *source, *field),
        }
    }

    fn const_int(&mut self, id: BindingId, val: i128, ty: TypeId) -> anyhow::Result<()> {
        // Determine modeled type.
        let ty_name = self.prog.type_name(ty);
        let ty = self
            .prog
            .specenv
            .type_model
            .get(&ty)
            .ok_or(anyhow::format_err!("no model for type {ty_name}"))?
            .as_primitive()
            .ok_or(anyhow::format_err!("constant must have basic type"))?;

        // Construct value of the determined type.
        let value = self.spec_typed_value(val, ty)?.into();

        // Assumption: destination binding equals constant value.
        let eq = self.values_equal(self.binding_value[&id].clone(), value);
        self.conditions.assumptions.push(eq);

        Ok(())
    }

    fn const_prim(&mut self, id: BindingId, val: Sym) -> anyhow::Result<()> {
        // Lookup value.
        let spec_value = self
            .prog
            .specenv
            .const_value
            .get(&val)
            .ok_or(anyhow::format_err!(
                "value of constant {const_name} is unspecified",
                const_name = self.prog.tyenv.syms[val.index()]
            ))?;
        let value = self.spec_expr_no_vars(spec_value)?;

        // Assumption: destination binding equals constant value.
        let eq = self.values_equal(self.binding_value[&id].clone(), value);
        self.conditions.assumptions.push(eq);

        Ok(())
    }

    fn extractor(
        &mut self,
        id: BindingId,
        term: TermId,
        parameter: BindingId,
    ) -> anyhow::Result<()> {
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
    ) -> anyhow::Result<()> {
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
    ) -> anyhow::Result<()> {
        // Lookup spec.
        let term_name = self.prog.term_name(term);
        let term_spec = self
            .prog
            .specenv
            .term_spec
            .get(&term)
            .ok_or(anyhow::format_err!("no spec for term {term_name}",))?;

        // Assignment of signature variables to expressions.
        let mut vars = HashMap::new();

        // Arguments.
        if term_spec.args.len() != args.len() {
            anyhow::bail!("incorrect number of arguments for term {term_name}");
        }
        for (name, arg) in zip(&term_spec.args, args) {
            vars.insert(name.0.clone(), arg.clone());
        }

        // Return value.
        vars.insert(term_spec.ret.0.clone(), ret.clone());

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

        // Partial function.
        // REVIEW(mbm): pin down semantics for partial function specifications.
        if let Domain::Partial(p) = domain {
            let all_requires = self.all(requires.clone());
            let eq = self.exprs_equal(p, all_requires);
            self.conditions.assumptions.push(eq);
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

        // Record callsite.
        let signatures = self
            .prog
            .specenv
            .term_instantiations
            .get(&term)
            .cloned()
            .unwrap_or_default();
        self.conditions.calls.push(Call {
            term,
            args: args.to_vec(),
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
    ) -> anyhow::Result<()> {
        // TODO(mbm): make_variant binding conditions generation must account for enum type models
        log::warn!("make_variant binding partially implemented");

        // Lookup term corresponding to variant.
        let variant_term_id = self.prog.get_variant_term(ty, variant);

        // Invoke as a constructor.
        self.constructor(id, variant_term_id, fields, Invocation::Caller)?;

        Ok(())
    }

    fn make_some(&mut self, id: BindingId, inner: BindingId) -> anyhow::Result<()> {
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

    fn match_some(&mut self, id: BindingId, source: BindingId) -> anyhow::Result<()> {
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

    fn match_tuple(
        &mut self,
        id: BindingId,
        source: BindingId,
        field: TupleIndex,
    ) -> anyhow::Result<()> {
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

    fn add_constraint(
        &mut self,
        binding_id: BindingId,
        constraint: &Constraint,
    ) -> anyhow::Result<()> {
        match constraint {
            Constraint::Some => self.constraint_some(binding_id),
            Constraint::ConstPrim { val } => self.const_prim(binding_id, *val),
            _ => todo!("constraint: {constraint:?}"),
        }
    }

    fn constraint_some(&mut self, binding_id: BindingId) -> anyhow::Result<()> {
        // Constrained binding should be an option.
        let opt = self.binding_value[&binding_id]
            .as_option()
            .expect("target of some constraint should be an option")
            .clone();

        // Assumption: option is Some.
        self.conditions.assumptions.push(opt.some);

        Ok(())
    }

    fn spec_expr(
        &mut self,
        expr: &spec::Expr,
        vars: &HashMap<String, Symbolic>,
    ) -> anyhow::Result<Symbolic> {
        match expr {
            spec::Expr::Var(v) => {
                let var_name = &v.0;
                let v = vars
                    .get(var_name)
                    .ok_or(anyhow::format_err!("undefined variable {var_name}"))?;
                Ok(v.clone())
            }

            spec::Expr::Const(c) => Ok(self.constant(c.clone()).into()),

            spec::Expr::Enum(name) => self.spec_enum(name),

            spec::Expr::Field(name, x) => {
                let x = self.spec_expr(x, vars)?;
                self.spec_field(name, x)
            }

            // TODO(mbm): fix boilerplate for common expressions
            spec::Expr::Not(x) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                Ok(self.scalar(Expr::Not(x)))
            }

            spec::Expr::And(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::And(x, y)))
            }

            spec::Expr::Or(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::Or(x, y)))
            }

            spec::Expr::Imp(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::Imp(x, y)))
            }

            spec::Expr::Eq(x, y) => {
                let x = self.spec_expr(x, vars)?;
                let y = self.spec_expr(y, vars)?;
                Ok(self.values_equal(x, y).into())
            }

            spec::Expr::Lte(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::Lte(x, y)))
            }

            spec::Expr::BVUlt(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVUlt(x, y)))
            }

            spec::Expr::BVUle(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVUle(x, y)))
            }

            spec::Expr::BVSge(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVSge(x, y)))
            }

            spec::Expr::BVSlt(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVSlt(x, y)))
            }

            spec::Expr::BVSle(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVSle(x, y)))
            }

            spec::Expr::BVSaddo(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVSaddo(x, y)))
            }

            spec::Expr::BVNot(x) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVNot(x)))
            }

            spec::Expr::BVNeg(x) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVNeg(x)))
            }

            spec::Expr::BVAdd(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVAdd(x, y)))
            }

            spec::Expr::BVSub(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVSub(x, y)))
            }

            spec::Expr::BVMul(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVMul(x, y)))
            }

            spec::Expr::BVAnd(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVAnd(x, y)))
            }

            spec::Expr::BVOr(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVOr(x, y)))
            }

            spec::Expr::BVShl(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVShl(x, y)))
            }

            spec::Expr::BVLShr(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVLShr(x, y)))
            }

            spec::Expr::BVAShr(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVAShr(x, y)))
            }

            spec::Expr::Conditional(c, t, e) => {
                let c = self.spec_expr(c, vars)?.try_into()?;
                let t = self.spec_expr(t, vars)?;
                let e = self.spec_expr(e, vars)?;
                self.conditional(c, t, e)
            }

            spec::Expr::Switch(on, arms) => self.spec_switch(on, arms, vars),

            spec::Expr::Let(defs, body) => self.spec_let(defs, body, vars),

            spec::Expr::BVZeroExt(w, x) => {
                let w = self.spec_expr(w, vars)?.try_into()?;
                let x = self.spec_expr(x, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVZeroExt(w, x)))
            }

            spec::Expr::BVSignExt(w, x) => {
                let w = self.spec_expr(w, vars)?.try_into()?;
                let x = self.spec_expr(x, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVSignExt(w, x)))
            }

            spec::Expr::BVConvTo(w, x) => {
                let w = self.spec_expr(w, vars)?.try_into()?;
                let x = self.spec_expr(x, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVConvTo(w, x)))
            }

            spec::Expr::BVExtract(h, l, x) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVExtract(*h, *l, x)))
            }

            spec::Expr::BVConcat(x, y) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                let y = self.spec_expr(y, vars)?.try_into()?;
                Ok(self.scalar(Expr::BVConcat(x, y)))
            }

            spec::Expr::Int2BV(w, x) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                Ok(self.scalar(Expr::Int2BV(*w, x)))
            }

            spec::Expr::WidthOf(x) => {
                let x = self.spec_expr(x, vars)?.try_into()?;
                Ok(self.scalar(Expr::WidthOf(x)))
            }

            spec::Expr::BVSubs(x, y, z) => {
                todo!()
                // create a new Expr::BVSubs case
            } 
        }
    }

    fn spec_expr_no_vars(&mut self, expr: &spec::Expr) -> anyhow::Result<Symbolic> {
        let no_vars = HashMap::new();
        self.spec_expr(expr, &no_vars)
    }

    fn spec_typed_value(&mut self, val: i128, ty: &Type) -> anyhow::Result<ExprId> {
        match ty {
            Type::Bool => Ok(self.boolean(match val {
                0 => false,
                1 => true,
                _ => anyhow::bail!("boolean value must be zero or one"),
            })),
            Type::Int => Ok(self.constant(Const::Int(val))),
            Type::BitVector(Width::Bits(w)) => {
                Ok(self.constant(Const::BitVector(*w, val.try_into()?)))
            }
            _ => anyhow::bail!("cannot construct constant of type {ty}"),
        }
    }

    fn spec_enum(&mut self, name: &Ident) -> anyhow::Result<Symbolic> {
        // Lookup variant term.
        // TODO(mbm): move term lookup to the SpecEnv construction stage?
        let variant_name = &name.0;
        let term_id = self
            .prog
            .termenv
            .get_term_by_name(&self.prog.tyenv, name)
            .ok_or(anyhow::format_err!("unknown enum variant {variant_name}"))?;

        // Lookup specified value.
        let spec_value = self
            .prog
            .specenv
            .enum_value
            .get(&term_id)
            .ok_or(anyhow::format_err!(
                "value of enum variant {variant_name} is unspecified"
            ))?;
        self.spec_expr_no_vars(spec_value)
    }

    fn spec_field(&mut self, name: &Ident, v: Symbolic) -> anyhow::Result<Symbolic> {
        log::trace!("access field {name} from {v}", name = name.0);

        let fields = v
            .as_struct()
            .ok_or(anyhow::format_err!("field access from non-struct value"))?;

        let field = fields
            .iter()
            .find(|f| f.name == name.0)
            .ok_or(anyhow::format_err!("missing struct field: {}", name.0))?;

        Ok(field.value.clone())
    }

    fn spec_switch(
        &mut self,
        on: &spec::Expr,
        arms: &[(spec::Expr, spec::Expr)],
        vars: &HashMap<String, Symbolic>,
    ) -> anyhow::Result<Symbolic> {
        // Generate sub-expressions.
        let on = self.spec_expr(on, vars)?;
        let mut arms = arms
            .iter()
            .map(|(value, then)| {
                let value = self.spec_expr(value, vars)?;
                let cond = self.values_equal(on.clone(), value);
                Ok((cond, self.spec_expr(then, vars)?))
            })
            .collect::<anyhow::Result<Vec<_>>>()?;

        // Exhaustiveness: assert one condition must hold.
        let conds = arms.iter().map(|(cond, _)| cond).cloned().collect();
        let exhaustive = self.any(conds);
        self.conditions.assertions.push(exhaustive);

        // Represent as nested conditionals.
        //
        // Note the condition of the last arm is not explicitly checked: we rely
        // on the exhaustiveness assertion.
        //
        // QUESTION(mbm): is it correct to always assert the exchaustiveness
        // condition? Or should it be treated as a requires, which is asserted
        // or assumed depending on which side it appears on?
        let (_, last) = arms.pop().expect("switch must have at least one arm");
        arms.iter()
            .rev()
            .cloned()
            .try_fold(last, |acc, (cond, then)| self.conditional(cond, then, acc))
    }

    fn spec_let(
        &mut self,
        defs: &[(Ident, spec::Expr)],
        body: &spec::Expr,
        vars: &HashMap<String, Symbolic>,
    ) -> anyhow::Result<Symbolic> {
        // Evaluate let defs.
        let mut let_vars = vars.clone();
        for (name, expr) in defs {
            let expr = self.spec_expr(expr, &let_vars)?;
            match let_vars.entry(name.0.clone()) {
                Entry::Occupied(_) => {
                    anyhow::bail!("let expression shadows variable {name}", name = name.0)
                }
                Entry::Vacant(e) => {
                    e.insert(expr);
                }
            }
        }

        // Evaluate body in let-binding scope.
        self.spec_expr(body, &let_vars)
    }

    fn conditional(&mut self, c: ExprId, t: Symbolic, e: Symbolic) -> anyhow::Result<Symbolic> {
        if std::mem::discriminant(&t) != std::mem::discriminant(&e) {
            anyhow::bail!("conditional arms have incompatible types");
        }
        match (t, e) {
            (Symbolic::Scalar(t), Symbolic::Scalar(e)) => {
                Ok(self.scalar(Expr::Conditional(c, t, e)))
            }
            case => todo!("conditional arm types: {case:?}"),
        }
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

    fn any(&mut self, exprs: Vec<ExprId>) -> ExprId {
        exprs
            .into_iter()
            .reduce(|acc, e| self.dedup_expr(Expr::Or(acc, e)))
            .unwrap_or_else(|| self.boolean(false))
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

    fn alloc_binding(
        &mut self,
        binding_type: &BindingType,
        name: String,
    ) -> anyhow::Result<Symbolic> {
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
                    .collect::<anyhow::Result<_>>()?;
                Ok(Symbolic::Tuple(inners))
            }
        }
    }

    fn alloc_value(&mut self, ty: &Compound, name: String) -> anyhow::Result<Symbolic> {
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
                    .collect::<anyhow::Result<_>>()?,
            )),
            Compound::Named(type_name) => {
                // TODO(mbm): named type model cycle detection
                // TODO(mbm): type ID lookup should happen in SpecEnv
                let type_id = self
                    .prog
                    .tyenv
                    .get_type_by_name(type_name)
                    .ok_or(anyhow::format_err!("unknown type {}", type_name.0))?;
                self.alloc_model(type_id, name)
            }
        }
    }

    fn alloc_model(&mut self, type_id: TypeId, name: String) -> anyhow::Result<Symbolic> {
        let type_name = self.prog.type_name(type_id);
        let ty = self
            .prog
            .specenv
            .type_model
            .get(&type_id)
            .ok_or(anyhow::format_err!(
                "unspecified model for type {type_name}"
            ))?;
        self.alloc_value(ty, name)
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
