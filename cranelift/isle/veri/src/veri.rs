use crate::{
    expand::Expansion,
    program::Program,
    spec,
    trie_again::{binding_type, BindingType},
};
use cranelift_isle::{
    sema::{Sym, TermId, TypeId},
    trie_again::{Binding, BindingId, Constraint, TupleIndex},
};
use std::{collections::HashMap, iter::zip};

declare_id!(
    /// The id of an expression within verification Conditions.
    ExprId
);

declare_id!(
    /// The id of a variable within verification Conditions.
    VariableId
);

// QUESTION(mbm): do we need yet another type enum?
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Type {
    Unknown,
    BitVector(Option<usize>),
    Int,
    Bool,
}

impl Type {
    fn from_spec_type(spec_type: &spec::Type) -> Self {
        match spec_type {
            spec::Type::BitVector => Self::BitVector(None),
            spec::Type::BitVectorWithWidth(width) => Self::BitVector(Some(*width)),
            spec::Type::Int => Self::Int,
            spec::Type::Bool => Self::Bool,
        }
    }
}

impl std::fmt::Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Unknown => write!(f, "unk"),
            Self::BitVector(Some(w)) => write!(f, "bv {w}"),
            Self::BitVector(None) => write!(f, "bv _"),
            Self::Int => write!(f, "int"),
            Self::Bool => write!(f, "bool"),
        }
    }
}

// QUESTION(mbm): can this be deduped with the corresponding spec constant type?
#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Const {
    Bool(bool),
    Int(i128),
    BitVector(usize, i128),
}

impl std::fmt::Display for Const {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::Bool(b) => write!(f, "{b}"),
            Self::Int(v) => write!(f, "{v}"),
            Self::BitVector(w, v) => write!(f, "#b{v:0>w$}"),
        }
    }
}

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

    // Binary.
    BVAdd(ExprId, ExprId),

    // ITE
    Conditional(ExprId, ExprId, ExprId),

    // Bitwidth conversion.
    BVConvTo(ExprId, ExprId),

    // Bitwidth.
    WidthOf(ExprId),
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
            Self::BVAdd(x, y) => write!(f, "bvadd({}, {})", x.index(), y.index()),
            Self::Conditional(c, t, e) => {
                write!(f, "{} ? {} : {}", c.index(), t.index(), e.index())
            }
            Self::BVConvTo(w, x) => write!(f, "bv_conv_to({}, {})", w.index(), x.index()),
            Self::WidthOf(x) => write!(f, "width_of({})", x.index()),
        }
    }
}

/// Verification conditions for an expansion.
#[derive(Default, Debug)]
pub struct Conditions {
    pub exprs: Vec<Expr>,
    pub assumptions: Vec<ExprId>,
    pub assertions: Vec<ExprId>,
    pub variable_type: Vec<Type>,
}

impl Conditions {
    pub fn from_expansion(expansion: &Expansion, prog: &Program) -> anyhow::Result<Self> {
        let builder = ConditionsBuilder::new(expansion, prog);
        builder.build()
    }

    pub fn pretty_print(&self) {
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

        // Variable Types
        println!("\tvariable_type = [");
        for (i, ty) in self.variable_type.iter().enumerate() {
            println!("\t\t{i}:\t{ty}");
        }
        println!("\t]");

        println!("}}");
    }
}

#[derive(Clone, Debug)]
struct OptionValue {
    some: VariableId,
    inner: Box<Value>,
}

#[derive(Clone, Debug)]
enum Value {
    Var(VariableId),
    Option(OptionValue),
    Tuple(Vec<Value>),
}

impl Value {
    fn as_var(&self) -> Option<VariableId> {
        match self {
            Self::Var(v) => Some(*v),
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

enum Invocation {
    Caller,
    Callee,
}

enum Domain {
    Total,
    Partial(VariableId),
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

        //// Callee contract for the term under expansion.
        //self.constructor(
        //    self.expansion.result,
        //    self.expansion.term,
        //    &self.expansion.parameters,
        //    Invocation::Callee,
        //)?;

        // Constraints.
        for (binding_id, constraints) in &self.expansion.constraints {
            for constraint in constraints {
                self.add_constraint(*binding_id, constraint)?;
            }
        }

        // Equals.
        for (a, b) in self.expansion.equalities() {
            self.bindings_equal(a, b);
        }

        Ok(self.conditions)
    }

    fn add_binding(&mut self, id: BindingId, binding: &Binding) -> anyhow::Result<()> {
        // Exit if already added.
        if self.binding_value.contains_key(&id) {
            return Ok(());
        }

        // Allocate a value.
        let binding_type = self.binding_type(binding);
        let value = self.alloc_binding_value(&binding_type)?;
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
            } => self.constructor(id, *term, &*parameters, Invocation::Caller),

            Binding::Iterator { .. } => unimplemented!("iterator bindings"),

            Binding::MakeVariant { .. } => {
                // TODO(mbm): implement make_variant bindings
                log::error!("make_variant binding unimplemented");
                Ok(())
            }

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

        // Destination binding should be a variable.
        let v = self.binding_value[&id]
            .as_var()
            .expect("destination of const_int binding should be a variable");

        // Assumption: variable equals constant value.
        let v = self.var(v);
        self.exprs_equal(v, value);

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
        let no_vars = HashMap::new();
        let value = self.spec_expr(spec_value, &no_vars)?;

        // Destination binding should be a variable.
        let v = self.binding_value[&id]
            .as_var()
            .expect("destination of const_prim binding should be a variable");

        // Assumption: variable equals constant value.
        let v = self.var(v);
        self.exprs_equal(v, value);

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
            let v = ret.as_var().expect("extractor return should be a variable");
            args.push(v);
        }

        // Result maps to the parameter of an extractor.
        let result = self.binding_value[&parameter]
            .as_var()
            .ok_or(anyhow::format_err!(
                "extractor parameter must be a variable"
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
            let v = self
                .binding_value
                .get(parameter_binding_id)
                .expect("parameter binding should be defined")
                .as_var()
                .expect("constructor parameter should be a variable");
            args.push(v);
        }

        // Return value.
        let (domain, result) = Domain::from_return_value(&self.binding_value[&id]);
        let result = result
            .as_var()
            .expect("constructor return should be a variable");

        // Call constructor.
        self.call(term, &args, result, invocation, domain)
    }

    fn call(
        &mut self,
        term: TermId,
        args: &Vec<VariableId>,
        result: VariableId,
        invocation: Invocation,
        domain: Domain,
    ) -> anyhow::Result<()> {
        // Lookup spec.
        let term_spec = self
            .prog
            .specenv
            .term_spec
            .get(&term)
            .ok_or(anyhow::format_err!(
                "no spec for term {term_name}",
                term_name = self.prog.term_name(term)
            ))?;

        // Assignment of signature variables.
        let mut vars = HashMap::new();

        // Arguments.
        // QUESTION(mbm): is mismatch of parameters and spec arguments an assertion failure or error return?
        assert_eq!(term_spec.args.len(), args.len());
        for (name, v) in zip(&term_spec.args, args) {
            vars.insert(name.0.clone(), *v);
        }

        // Return value.
        vars.insert(term_spec.ret.0.clone(), result);

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
            let in_domain = self.var(p);
            let all_requires = self.all(requires.clone());
            self.exprs_equal(in_domain, all_requires);
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
        let some = self.var(opt.some);
        self.conditions.assumptions.push(some);

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
        let some = self.var(opt.some);
        let eq = self.values_equal(&v, &opt.inner);
        let constraint = self.dedup_expr(Expr::Imp(some, eq));
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
        let some = self.var(opt.some);
        self.conditions.assumptions.push(some);

        Ok(())
    }

    fn spec_expr(
        &mut self,
        expr: &spec::Expr,
        vars: &HashMap<String, VariableId>,
    ) -> anyhow::Result<ExprId> {
        match expr {
            spec::Expr::Var(v) => {
                let var_name = &v.0;
                let v = vars
                    .get(var_name)
                    .ok_or(anyhow::format_err!("undefined variable {var_name}"))?;
                Ok(self.var(*v))
            }

            spec::Expr::Const(c) => self.spec_const(c),

            spec::Expr::Or(x, y) => {
                let x = self.spec_expr(x, vars)?;
                let y = self.spec_expr(y, vars)?;
                Ok(self.dedup_expr(Expr::Or(x, y)))
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

            spec::Expr::BVAdd(x, y) => {
                let x = self.spec_expr(x, vars)?;
                let y = self.spec_expr(y, vars)?;
                Ok(self.dedup_expr(Expr::BVAdd(x, y)))
            }

            spec::Expr::Conditional(c, t, e) => {
                let c = self.spec_expr(c, vars)?;
                let t = self.spec_expr(t, vars)?;
                let e = self.spec_expr(e, vars)?;
                Ok(self.dedup_expr(Expr::Conditional(c, t, e)))
            }

            spec::Expr::BVConvToVarWidth(w, x) => {
                let w = self.spec_expr(w, vars)?;
                let x = self.spec_expr(x, vars)?;
                Ok(self.dedup_expr(Expr::BVConvTo(w, x)))
            }

            spec::Expr::WidthOf(x) => {
                let x = self.spec_expr(x, vars)?;
                Ok(self.dedup_expr(Expr::WidthOf(x)))
            }

            e => todo!("spec expr: {e:?}"),
        }
    }

    fn spec_const(&mut self, c: &spec::Const) -> anyhow::Result<ExprId> {
        self.spec_typed_value(c.value, &c.ty)
    }

    fn spec_typed_value(&mut self, val: i128, ty: &spec::Type) -> anyhow::Result<ExprId> {
        match ty {
            spec::Type::Bool => Ok(self.boolean(match val {
                0 => false,
                1 => true,
                _ => anyhow::bail!("boolean value must be zero or one"),
            })),
            spec::Type::Int => Ok(self.constant(Const::Int(val))),
            spec::Type::BitVectorWithWidth(w) => Ok(self.constant(Const::BitVector(*w, val))),
            spec::Type::BitVector => anyhow::bail!("bitvector constant must have known width"),
        }
    }

    fn bindings_equal(&mut self, a: BindingId, b: BindingId) -> ExprId {
        // TODO(mbm): can this be done without clones?
        let a = self.binding_value[&a].clone();
        let b = self.binding_value[&b].clone();
        self.values_equal(&a, &b)
    }

    fn values_equal(&mut self, a: &Value, b: &Value) -> ExprId {
        match (a, b) {
            (Value::Var(u), Value::Var(v)) => self.variables_equal(*u, *v),

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

    fn variables_equal(&mut self, u: VariableId, v: VariableId) -> ExprId {
        let u = self.var(u);
        let v = self.var(v);
        self.exprs_equal(u, v)
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

    fn var(&mut self, v: VariableId) -> ExprId {
        self.dedup_expr(Expr::Variable(v))
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

    fn alloc_binding_value(&mut self, binding_type: &BindingType) -> anyhow::Result<Value> {
        match binding_type {
            BindingType::Base(type_id) => {
                // TODO(mbm): how to handle missing type models? use unknown default or error?
                let ty = self
                    .prog
                    .specenv
                    .type_model
                    .get(type_id)
                    .map(Type::from_spec_type)
                    .unwrap_or(Type::Unknown);
                Ok(Value::Var(self.alloc_variable(ty)))
            }
            BindingType::Option(inner_type) => {
                let some = self.alloc_variable(Type::Bool);
                let inner = Box::new(self.alloc_binding_value(inner_type)?);
                Ok(Value::Option(OptionValue { some, inner }))
            }
            BindingType::Tuple(inners) => {
                let inners = inners
                    .iter()
                    .map(|inner_type| self.alloc_binding_value(inner_type))
                    .collect::<anyhow::Result<_>>()?;
                Ok(Value::Tuple(inners))
            }
        }
    }

    fn alloc_variable(&mut self, ty: Type) -> VariableId {
        let id = self.conditions.variable_type.len();
        self.conditions.variable_type.push(ty);
        VariableId(id)
    }

    fn dedup_expr(&mut self, expr: Expr) -> ExprId {
        if let Some(id) = self.expr_map.get(&expr) {
            *id
        } else {
            let id = ExprId(self.conditions.exprs.len().try_into().unwrap());
            self.conditions.exprs.push(expr.clone());
            self.expr_map.insert(expr, id);
            id
        }
    }
}
