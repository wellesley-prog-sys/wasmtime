use crate::{
    expand::Expansion,
    program::Program,
    spec::{self, Signature},
    trie_again::{binding_type, BindingType},
    types::{Const, Type, Width},
};
use cranelift_isle::{
    ast,
    sema::{Sym, TermId, TypeId, VariantId},
    trie_again::{Binding, BindingId, Constraint, TupleIndex},
};
use std::{
    collections::{HashMap, HashSet},
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
    And(ExprId, ExprId),
    Or(ExprId, ExprId),
    Imp(ExprId, ExprId),
    Eq(ExprId, ExprId),
    Lte(ExprId, ExprId),

    BVUlt(ExprId, ExprId),

    // Unary.
    BVNot(ExprId),
    BVNeg(ExprId),

    // Binary.
    BVAdd(ExprId, ExprId),
    BVSub(ExprId, ExprId),
    BVMul(ExprId, ExprId),
    BVAnd(ExprId, ExprId),
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
            &Self::BVNot(x)
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
            | &Self::BVAdd(x, y)
            | &Self::BVSub(x, y)
            | &Self::BVMul(x, y)
            | &Self::BVAnd(x, y)
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
            Self::And(x, y) => write!(f, "{} && {}", x.index(), y.index()),
            Self::Or(x, y) => write!(f, "{} || {}", x.index(), y.index()),
            Self::Imp(x, y) => write!(f, "{} => {}", x.index(), y.index()),
            Self::Eq(x, y) => write!(f, "{} == {}", x.index(), y.index()),
            Self::Lte(x, y) => write!(f, "{} <= {}", x.index(), y.index()),
            Self::BVUlt(x, y) => write!(f, "bvult({}, {})", x.index(), y.index()),
            Self::BVNot(x) => write!(f, "bvnot({})", x.index()),
            Self::BVNeg(x) => write!(f, "bvneg({})", x.index()),
            Self::BVAdd(x, y) => write!(f, "bvadd({}, {})", x.index(), y.index()),
            Self::BVSub(x, y) => write!(f, "bvsub({}, {})", x.index(), y.index()),
            Self::BVMul(x, y) => write!(f, "bvmul({}, {})", x.index(), y.index()),
            Self::BVAnd(x, y) => write!(f, "bvand({}, {})", x.index(), y.index()),
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

// QUESTION(mbm): can we merge `Model` and `Assignment` from type inference?
pub type Model = HashMap<ExprId, Const>;

// QUESTION(mbm): is `Call` the right name? consider `Term`, `TermInstance`, ...?
#[derive(Debug)]
pub struct Call {
    pub term: TermId,
    pub args: Vec<ExprId>,
    pub ret: ExprId,
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
                    println!("\t\t\t\t{}", arg.index());
                }
                println!("\t\t\t]");
            }
            println!("\t\t\tret = {}", call.ret.index());
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
                    .map(|a| Ok(model
                        .get(a)
                        .ok_or(anyhow::format_err!("undefined argument in model"))?
                        .to_string()))
                    .collect::<anyhow::Result<Vec<_>>>()?
                    .join(", "),
                ret = model
                    .get(&call.ret)
                    .ok_or(anyhow::format_err!("undefined return value in model"))?
            );
        }

        Ok(())
    }
}

#[derive(Debug, Clone)]
struct OptionValue {
    some: ExprId,
    inner: Box<Value>,
}

#[derive(Debug, Clone)]
enum Value {
    Expr(ExprId),
    Option(OptionValue),
    Tuple(Vec<Value>),
}

impl Value {
    fn as_expr(&self) -> Option<ExprId> {
        match self {
            Self::Expr(x) => Some(*x),
            _ => None,
        }
    }

    fn as_option(&self) -> Option<&OptionValue> {
        match self {
            Self::Option(opt) => Some(opt),
            _ => None,
        }
    }

    fn as_tuple(&self) -> Option<&Vec<Value>> {
        match self {
            Self::Tuple(fields) => Some(fields),
            _ => None,
        }
    }

    fn elements(&self) -> &[Value] {
        match self {
            Self::Tuple(fields) => &fields[..],
            v => std::slice::from_ref(v),
        }
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
    fn from_return_value(value: &Value) -> (Self, &Value) {
        match value {
            Value::Option(opt) => (Self::Partial(opt.some), &opt.inner),
            v => (Self::Total, v),
        }
    }
}

struct ConditionsBuilder<'a> {
    expansion: &'a Expansion,
    prog: &'a Program,

    binding_value: HashMap<BindingId, Value>,
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
        let value = self.alloc_binding_value(&binding_type, name)?;
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
            .ok_or(anyhow::format_err!("no model for type {ty_name}"))?;

        // Construct value of the determined type.
        let value = self.spec_typed_value(val, ty)?;

        // Destination binding should be a base expression.
        let x = self.binding_value[&id]
            .as_expr()
            .expect("destination of const_int binding should be an expression");

        // Assumption: expression equals constant value.
        let eq = self.exprs_equal(x, value);
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

        // Destination binding should be a base expression.
        let x = self.binding_value[&id]
            .as_expr()
            .expect("destination of const_prim binding should be an expression");

        // Assumption: expression equals constant value.
        let eq = self.exprs_equal(x, value);
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
        let rets = ret.elements();

        let mut args = Vec::new();
        for ret in rets {
            let x = ret
                .as_expr()
                .expect("extractor return should be an expression");
            args.push(x);
        }

        // Result maps to the parameter of an extractor.
        let result = self.binding_value[&parameter]
            .as_expr()
            .ok_or(anyhow::format_err!(
                "extractor parameter must be an expression"
            ))?;

        // Call extractor.
        self.call(term, &args, result, Invocation::Caller, domain)
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
                .as_expr()
                .expect("constructor parameter should be an expression");
            args.push(x);
        }

        // Return value.
        let (domain, result) = Domain::from_return_value(&self.binding_value[&id]);
        let result = result
            .as_expr()
            .expect("constructor return should be an expression");

        // Call constructor.
        self.call(term, &args, result, invocation, domain)
    }

    fn call(
        &mut self,
        term: TermId,
        args: &[ExprId],
        ret: ExprId,
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
            vars.insert(name.0.clone(), *arg);
        }

        // Return value.
        vars.insert(term_spec.ret.0.clone(), ret);

        // Requires.
        let mut requires = Vec::new();
        for require in &term_spec.requires {
            let require = self.spec_expr(require, &vars)?;
            requires.push(require);
        }

        // Provides.
        let mut provides = Vec::new();
        for provide in &term_spec.provides {
            let provide = self.spec_expr(provide, &vars)?;
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
        let eq = self.values_equal(&inner, &opt.inner);
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
        let eq = self.values_equal(&v, &opt.inner);
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
        let eq = self.values_equal(&v, &fields[field.index()]);
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
        vars: &HashMap<String, ExprId>,
    ) -> anyhow::Result<ExprId> {
        match expr {
            spec::Expr::Var(v) => {
                let var_name = &v.0;
                let v = vars
                    .get(var_name)
                    .ok_or(anyhow::format_err!("undefined variable {var_name}"))?;
                Ok(*v)
            }

            spec::Expr::Const(c) => Ok(self.constant(c.clone())),

            spec::Expr::Enum(name) => self.spec_enum(name),

            // TODO(mbm): fix boilerplate for common expressions
            spec::Expr::And(x, y) => {
                let x = self.spec_expr(x, vars)?;
                let y = self.spec_expr(y, vars)?;
                Ok(self.dedup_expr(Expr::And(x, y)))
            }

            spec::Expr::Or(x, y) => {
                let x = self.spec_expr(x, vars)?;
                let y = self.spec_expr(y, vars)?;
                Ok(self.dedup_expr(Expr::Or(x, y)))
            }

            spec::Expr::Imp(x, y) => {
                let x = self.spec_expr(x, vars)?;
                let y = self.spec_expr(y, vars)?;
                Ok(self.dedup_expr(Expr::Imp(x, y)))
            }

            spec::Expr::Eq(x, y) => {
                let x = self.spec_expr(x, vars)?;
                let y = self.spec_expr(y, vars)?;
                Ok(self.exprs_equal(x, y))
            }

            spec::Expr::Lte(x, y) => {
                let x = self.spec_expr(x, vars)?;
                let y = self.spec_expr(y, vars)?;
                Ok(self.dedup_expr(Expr::Lte(x, y)))
            }

            spec::Expr::BVUlt(x, y) => {
                let x = self.spec_expr(x, vars)?;
                let y = self.spec_expr(y, vars)?;
                Ok(self.dedup_expr(Expr::BVUlt(x, y)))
            }

            spec::Expr::BVNot(x) => {
                let x = self.spec_expr(x, vars)?;
                Ok(self.dedup_expr(Expr::BVNot(x)))
            }

            spec::Expr::BVNeg(x) => {
                let x = self.spec_expr(x, vars)?;
                Ok(self.dedup_expr(Expr::BVNeg(x)))
            }

            spec::Expr::BVAdd(x, y) => {
                let x = self.spec_expr(x, vars)?;
                let y = self.spec_expr(y, vars)?;
                Ok(self.dedup_expr(Expr::BVAdd(x, y)))
            }

            spec::Expr::BVSub(x, y) => {
                let x = self.spec_expr(x, vars)?;
                let y = self.spec_expr(y, vars)?;
                Ok(self.dedup_expr(Expr::BVSub(x, y)))
            }

            spec::Expr::BVMul(x, y) => {
                let x = self.spec_expr(x, vars)?;
                let y = self.spec_expr(y, vars)?;
                Ok(self.dedup_expr(Expr::BVMul(x, y)))
            }

            spec::Expr::BVAnd(x, y) => {
                let x = self.spec_expr(x, vars)?;
                let y = self.spec_expr(y, vars)?;
                Ok(self.dedup_expr(Expr::BVAnd(x, y)))
            }

            spec::Expr::BVShl(x, y) => {
                let x = self.spec_expr(x, vars)?;
                let y = self.spec_expr(y, vars)?;
                Ok(self.dedup_expr(Expr::BVShl(x, y)))
            }

            spec::Expr::BVLShr(x, y) => {
                let x = self.spec_expr(x, vars)?;
                let y = self.spec_expr(y, vars)?;
                Ok(self.dedup_expr(Expr::BVLShr(x, y)))
            }

            spec::Expr::BVAShr(x, y) => {
                let x = self.spec_expr(x, vars)?;
                let y = self.spec_expr(y, vars)?;
                Ok(self.dedup_expr(Expr::BVAShr(x, y)))
            }

            spec::Expr::Conditional(c, t, e) => {
                let c = self.spec_expr(c, vars)?;
                let t = self.spec_expr(t, vars)?;
                let e = self.spec_expr(e, vars)?;
                Ok(self.dedup_expr(Expr::Conditional(c, t, e)))
            }

            spec::Expr::Switch(on, arms) => self.spec_switch(on, arms, vars),

            spec::Expr::BVZeroExt(w, x) => {
                let w = self.spec_expr(w, vars)?;
                let x = self.spec_expr(x, vars)?;
                Ok(self.dedup_expr(Expr::BVZeroExt(w, x)))
            }

            spec::Expr::BVSignExt(w, x) => {
                let w = self.spec_expr(w, vars)?;
                let x = self.spec_expr(x, vars)?;
                Ok(self.dedup_expr(Expr::BVSignExt(w, x)))
            }

            spec::Expr::BVConvTo(w, x) => {
                let w = self.spec_expr(w, vars)?;
                let x = self.spec_expr(x, vars)?;
                Ok(self.dedup_expr(Expr::BVConvTo(w, x)))
            }

            spec::Expr::BVExtract(h, l, x) => {
                let x = self.spec_expr(x, vars)?;
                Ok(self.dedup_expr(Expr::BVExtract(*h, *l, x)))
            }

            spec::Expr::BVConcat(x, y) => {
                let x = self.spec_expr(x, vars)?;
                let y = self.spec_expr(y, vars)?;
                Ok(self.dedup_expr(Expr::BVConcat(x, y)))
            }

            spec::Expr::Int2BV(w, x) => {
                let x = self.spec_expr(x, vars)?;
                Ok(self.dedup_expr(Expr::Int2BV(*w, x)))
            }

            spec::Expr::WidthOf(x) => {
                let x = self.spec_expr(x, vars)?;
                Ok(self.dedup_expr(Expr::WidthOf(x)))
            }
        }
    }

    fn spec_expr_no_vars(&mut self, expr: &spec::Expr) -> anyhow::Result<ExprId> {
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
            Type::BitVector(Width::Bits(w)) => Ok(self.constant(Const::BitVector(*w, val))),
            _ => anyhow::bail!("cannot construct constant of type {ty}"),
        }
    }

    fn spec_enum(&mut self, name: &ast::Ident) -> anyhow::Result<ExprId> {
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

    fn spec_switch(
        &mut self,
        on: &spec::Expr,
        arms: &[(spec::Expr, spec::Expr)],
        vars: &HashMap<String, ExprId>,
    ) -> anyhow::Result<ExprId> {
        // Generate sub-expressions.
        let on = self.spec_expr(on, vars)?;
        let mut arms = arms
            .iter()
            .map(|(value, then)| {
                let value = self.spec_expr(value, vars)?;
                let cond = self.exprs_equal(on, value);
                Ok((cond, self.spec_expr(then, vars)?))
            })
            .collect::<anyhow::Result<Vec<_>>>()?;

        // Exhaustiveness: assert one condition must hold.
        let conds = arms.iter().map(|(cond, _)| cond).copied().collect();
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
        Ok(arms.iter().rev().fold(last, |acc, (cond, then)| {
            self.dedup_expr(Expr::Conditional(*cond, *then, acc))
        }))
    }

    fn bindings_equal(&mut self, a: BindingId, b: BindingId) -> ExprId {
        // TODO(mbm): can this be done without clones?
        let a = self.binding_value[&a].clone();
        let b = self.binding_value[&b].clone();
        self.values_equal(&a, &b)
    }

    fn values_equal(&mut self, a: &Value, b: &Value) -> ExprId {
        match (a, b) {
            (Value::Expr(u), Value::Expr(v)) => self.exprs_equal(*u, *v),

            (Value::Tuple(us), Value::Tuple(vs)) => {
                // Field-wise equality.
                // TODO(mbm): can we expect that tuples are the same length?
                assert_eq!(us.len(), vs.len(), "tuple length mismatch");
                let fields_eq = zip(us, vs).map(|(u, v)| self.values_equal(u, v)).collect();

                // All fields must be equal.
                self.all(fields_eq)
            }

            _ => todo!("values equal: {a:?} == {b:?}"),
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

    fn alloc_binding_value(
        &mut self,
        binding_type: &BindingType,
        name: String,
    ) -> anyhow::Result<Value> {
        match binding_type {
            BindingType::Base(type_id) => {
                let type_name = self.prog.type_name(*type_id);
                let ty = self
                    .prog
                    .specenv
                    .type_model
                    .get(type_id)
                    .ok_or(anyhow::format_err!(
                        "unspecified model for type {type_name}"
                    ))?;
                Ok(Value::Expr(self.alloc_variable(ty.clone(), name)))
            }
            BindingType::Option(inner_type) => {
                let some = self.alloc_variable(Type::Bool, Variable::component_name(&name, "some"));
                let inner = Box::new(
                    self.alloc_binding_value(inner_type, Variable::component_name(&name, "inner"))?,
                );
                Ok(Value::Option(OptionValue { some, inner }))
            }
            BindingType::Tuple(inners) => {
                let inners = inners
                    .iter()
                    .enumerate()
                    .map(|(i, inner_type)| {
                        self.alloc_binding_value(
                            inner_type,
                            Variable::component_name(&name, &i.to_string()),
                        )
                    })
                    .collect::<anyhow::Result<_>>()?;
                Ok(Value::Tuple(inners))
            }
        }
    }

    fn alloc_variable(&mut self, ty: Type, name: String) -> ExprId {
        let v = VariableId(self.conditions.variables.len());
        self.conditions.variables.push(Variable { ty, name });
        self.dedup_expr(Expr::Variable(v))
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
