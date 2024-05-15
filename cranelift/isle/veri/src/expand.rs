use std::collections::HashMap;

use cranelift_isle::{
    disjointsets::DisjointSets,
    lexer::Pos,
    trie_again::{Binding, BindingId, Constraint, Rule, RuleSet},
};

#[derive(Debug, Default, Clone)]
pub struct Expansion {
    pub pos: Pos,
    pub bindings: Vec<Binding>,
    pub constraints: HashMap<BindingId, Constraint>,
    pub result: BindingId,
}

impl Expansion {
    fn set_constraint(&mut self, binding_id: BindingId, constraint: Constraint) {
        assert!(!self.constraints.contains_key(&binding_id));
        self.constraints.insert(binding_id, constraint);
    }
}

pub struct Expander<'a> {
    rule_set: &'a RuleSet,

    // Binding map from the source rule set to the expansion.
    binding_remap: HashMap<BindingId, BindingId>,

    // Equality sets for the current rule.
    equals: DisjointSets<BindingId>,

    // Expansion under construction.
    expansion: Expansion,
}

impl<'a> Expander<'a> {
    pub fn new(rule_set: &'a RuleSet) -> Self {
        Self {
            rule_set,
            binding_remap: HashMap::new(),
            equals: DisjointSets::default(),
            expansion: Expansion::default(),
        }
    }

    pub fn rules(&mut self) -> Vec<Expansion> {
        self.rule_set
            .rules
            .iter()
            .map(|rule| self.rule(rule))
            .collect()
    }

    fn rule(&mut self, rule: &Rule) -> Expansion {
        // Reset state.
        self.binding_remap.clear();
        self.equals = rule.equals.clone();
        self.expansion = Expansion::default();

        // Constraints.
        for i in 0..self.rule_set.bindings.len() {
            let binding_id = i.try_into().unwrap();
            if let Some(constraint) = rule.get_constraint(binding_id) {
                let expansion_binding_id = self.add_binding(binding_id);
                self.expansion
                    .set_constraint(expansion_binding_id, constraint);
            }
        }

        // TODO: argument bindings

        // Result.
        self.expansion.result = self.add_binding(rule.result);

        // TODO: iterators
        // TODO: prio
        // TODO: impure

        // TODO
        self.expansion.clone()
    }

    fn add_binding(&mut self, binding_id: BindingId) -> BindingId {
        // Check if it has already been added.
        if self.binding_remap.contains_key(&binding_id) {
            return self.binding_remap[&binding_id];
        }

        // Resolve equality mapping.
        if let Some(repr) = self.equals.find(binding_id) {
            if repr != binding_id {
                let expansion_binding_id = self.add_binding(repr);
                self.binding_remap.insert(binding_id, expansion_binding_id);
                return expansion_binding_id;
            }
        }

        // Add dependencies first.
        let binding = &self.rule_set.bindings[binding_id.index()];
        for source in binding.sources() {
            self.add_binding(*source);
        }

        // Reindex this binding.
        let reindexed = self.reindex_binding(binding);

        // Insert into expension bindings list.
        let expansion_binding_id: BindingId = self.expansion.bindings.len().try_into().unwrap();
        self.expansion.bindings.push(reindexed);

        // Record binding mapping.
        self.binding_remap.insert(binding_id, expansion_binding_id);
        return expansion_binding_id;
    }

    fn reindex_binding(&self, binding: &Binding) -> Binding {
        match binding {
            Binding::Argument { .. } | Binding::ConstInt { .. } | Binding::ConstPrim { .. } => {
                binding.clone()
            }

            Binding::Extractor { term, parameter } => Binding::Extractor {
                term: *term,
                parameter: self.binding_remap[parameter],
            },

            Binding::Constructor {
                term,
                parameters,
                instance,
            } => Binding::Constructor {
                term: *term,
                parameters: self.reindex_binding_ids(parameters),
                instance: *instance,
            },

            Binding::MakeVariant {
                ty,
                variant,
                fields,
            } => Binding::MakeVariant {
                ty: *ty,
                variant: *variant,
                fields: self.reindex_binding_ids(fields),
            },

            Binding::MatchVariant {
                source,
                variant,
                field,
            } => Binding::MatchVariant {
                source: self.binding_remap[source],
                variant: *variant,
                field: *field,
            },

            Binding::MatchSome { source } => Binding::MatchSome {
                source: self.binding_remap[source],
            },

            Binding::MatchTuple { source, field } => Binding::MatchTuple {
                source: self.binding_remap[source],
                field: *field,
            },

            _ => todo!("reindex binding: {binding:?}"),
        }
    }

    fn reindex_binding_ids(&self, binding_ids: &Box<[BindingId]>) -> Box<[BindingId]> {
        binding_ids
            .iter()
            .map(|binding_id| self.binding_remap[binding_id])
            .collect()
    }
}
