use std::{collections::HashMap, iter::zip};

use cranelift_isle::trie_again::{Binding, BindingId};

use crate::{
    expand::Expansion,
    program::Program,
    spec,
    trie_again::{binding_type, BindingType},
};

declare_id!(
    /// The id of an expression within verification Conditions.
    ExprId
);

declare_id!(
    /// The id of a variable within verification Conditions.
    VariableId
);

// QUESTION(mbm): do we need yet another type enum?
#[derive(Debug)]
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

#[derive(Clone, Debug, Eq, Hash, PartialEq)]
pub enum Expr {
    // Terminals.
    False,
    True,
    Variable(VariableId),

    // Boolean.
    And(ExprId, ExprId),
    Imp(ExprId, ExprId),
    Eq(ExprId, ExprId),
}

impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Self::False => write!(f, "false"),
            Self::True => write!(f, "true"),
            Self::Variable(v) => write!(f, "v{}", v.index()),

            Self::And(x, y) => write!(f, "{} && {}", x.index(), y.index()),
            Self::Imp(x, y) => write!(f, "{} => {}", x.index(), y.index()),
            Self::Eq(x, y) => write!(f, "{} == {}", x.index(), y.index()),
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
    Base(VariableId),
    Option(OptionValue),
    Tuple(Vec<Value>),
}

impl Value {
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
        // TODO: callsite specifications
        // TODO: pub result: BindingId,

        // Bindings.
        for (i, binding) in self.expansion.bindings.iter().enumerate() {
            if let Some(binding) = binding {
                self.add_binding(i.try_into().unwrap(), binding)?;
            }
        }

        // TODO: pub constraints: BTreeMap<BindingId, Vec<Constraint>>,
        // TODO: pub equals: DisjointSets<BindingId>,

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

        //
        match binding {
            // ConstInt
            Binding::ConstPrim { .. } => {
                // TODO(mbm): const_prim
            }

            Binding::Argument { .. } => {
                // Argument binding has no associated constraints.
            }

            Binding::Extractor { .. } => {
                // TODO(mbm): extractor
            }

            Binding::Constructor { .. } => {
                // TODO(mbm): constructor
            }

            Binding::Iterator { .. } => unimplemented!("iterator bindings"),

            Binding::MakeVariant { .. } => {
                // TODO(mbm): make_variant
            }

            Binding::MakeSome { inner } => {
                // Destination binding should be an option.
                let opt = self.binding_value[&id]
                    .as_option()
                    .expect("destination of make_some binding should be an option")
                    .clone();

                // Inner binding.
                let inner = self.binding_value[inner].clone();

                // Assumption: option is Some.
                let some = self.dedup_expr(Expr::Variable(opt.some));
                self.conditions.assumptions.push(some);

                // Assumption: option value is equal to this binding.
                let eq = self.values_equal(&inner, &opt.inner);
                self.conditions.assumptions.push(eq);
            }

            Binding::MatchSome { source } => {
                // Source should be an option.
                let opt = self.binding_value[source]
                    .as_option()
                    .expect("source of match_some binding should be an option")
                    .clone();

                // Destination binding.
                let v = self.binding_value[&id].clone();

                // Assumption: if the option is some, then the inner value
                // equals this binding.
                let some = self.dedup_expr(Expr::Variable(opt.some));
                let eq = self.values_equal(&v, &opt.inner);
                let constraint = self.dedup_expr(Expr::Imp(some, eq));
                self.conditions.assumptions.push(constraint);
            }

            Binding::MatchTuple { source, field } => {
                // Source should be a tuple. Access its fields.
                let fields = self.binding_value[source]
                    .as_tuple()
                    .expect("source of match_tuple binding should be a tuple")
                    .clone();

                // Destination binding.
                let v = self.binding_value[&id].clone();

                // Assumption: indexed field should equal this binding.
                let eq = self.values_equal(&v, &fields[field.index()]);
                self.conditions.assumptions.push(eq);
            }

            _ => todo!("add binding: {binding:?}"),
        }

        Ok(())
    }

    //fn bindings_equal(&mut self, a: BindingId, b: BindingId) -> ExprId {
    //    // NIT(mbm): possible to avoid clone here?
    //    self.values_equal(
    //        &self.binding_value[&a].clone(),
    //        &self.binding_value[&b].clone(),
    //    )
    //}

    fn values_equal(&mut self, a: &Value, b: &Value) -> ExprId {
        match (a, b) {
            (Value::Base(u), Value::Base(v)) => self.variables_equal(*u, *v),

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
        let lhs = self.dedup_expr(Expr::Variable(u));
        let rhs = self.dedup_expr(Expr::Variable(v));
        self.exprs_equal(lhs, rhs)
    }

    fn exprs_equal(&mut self, lhs: ExprId, rhs: ExprId) -> ExprId {
        self.dedup_expr(Expr::Eq(lhs, rhs))
    }

    fn all(&mut self, exprs: Vec<ExprId>) -> ExprId {
        exprs
            .into_iter()
            .reduce(|acc, e| self.dedup_expr(Expr::And(acc, e)))
            .unwrap_or_else(|| self.dedup_expr(Expr::True))
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
                Ok(Value::Base(self.alloc_variable(ty)))
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
