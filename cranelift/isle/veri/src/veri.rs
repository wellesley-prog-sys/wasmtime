use std::collections::HashMap;

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

// TODO(mbm): do we need yet another type enum?
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

pub enum Expr {
    // TODO: veri ir types
}

/// Verification conditions for an expansion.
#[derive(Default)]
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
}

enum Value {
    Base(VariableId),
    Option { some: VariableId, inner: Box<Value> },
    Tuple(Vec<Value>),
}

struct ConditionsBuilder<'a> {
    expansion: &'a Expansion,
    prog: &'a Program,

    binding_value: HashMap<BindingId, Value>,
    conditions: Conditions,
}

impl<'a> ConditionsBuilder<'a> {
    fn new(expansion: &'a Expansion, prog: &'a Program) -> Self {
        Self {
            expansion,
            prog,
            binding_value: HashMap::new(),
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
        // Allocate value for the binding.
        let binding_type = self.binding_type(binding);
        let value = self.alloc_binding_value(&binding_type)?;
        self.binding_value.insert(id, value);
        Ok(())
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
                let model =
                    self.prog
                        .specenv
                        .type_model
                        .get(type_id)
                        .ok_or(anyhow::format_err!(
                            "no model for type {type_name}",
                            type_name = self.prog.type_name(*type_id)
                        ))?;
                let ty = Type::from_spec_type(model);
                Ok(Value::Base(self.alloc_variable(ty)))
            }
            BindingType::Option(inner_type) => {
                let some = self.alloc_variable(Type::Bool);
                let inner = Box::new(self.alloc_binding_value(inner_type)?);
                Ok(Value::Option { some, inner })
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
}
