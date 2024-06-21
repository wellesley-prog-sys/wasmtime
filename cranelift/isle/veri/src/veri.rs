use std::collections::HashMap;

use cranelift_isle::trie_again::{Binding, BindingId};

use crate::expand::Expansion;

declare_id!(
    /// The id of an expression within verification Conditions.
    ExprId
);

declare_id!(
    /// The id of a variable within verification Conditions.
    VariableId
);

pub enum Expr {}

/// Verification conditions for an expansion.
#[derive(Default)]
pub struct Conditions {
    pub exprs: Vec<Expr>,
    pub assumptions: Vec<ExprId>,
    pub assertions: Vec<ExprId>,
}

impl Conditions {
    pub fn from_expansion(expansion: &Expansion) -> Self {
        let mut builder = ConditionsBuilder::default();
        builder.add_expansion(expansion);
        builder.conditions
    }
}

enum Value {}

#[derive(Default)]
struct ConditionsBuilder {
    conditions: Conditions,
    binding_value: HashMap<BindingId, Value>,
}

impl ConditionsBuilder {
    fn add_expansion(&mut self, expansion: &Expansion) {
        // TODO: callsite specifications

        // Bindings.
        for (i, binding) in expansion.bindings.iter().enumerate() {
            if let Some(binding) = binding {
                self.add_binding(i.try_into().unwrap(), binding);
            }
        }

        // TODO: pub constraints: BTreeMap<BindingId, Vec<Constraint>>,
        // TODO: pub equals: DisjointSets<BindingId>,
        // TODO: pub result: BindingId,
    }

    fn add_binding(&mut self, id: BindingId, binding: &Binding) {
        // Bail if already added.
        if self.binding_value.contains_key(&id) {
            return;
        }

        match binding {
            _ => todo!("add binding: {binding:?}"),
        }
    }
}
