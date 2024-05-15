use std::collections::{HashMap, HashSet};

use crate::program::Program;
use cranelift_isle::{
    disjointsets::DisjointSets,
    sema::TermId,
    trie_again::{Binding, BindingId, Constraint, Rule, RuleSet},
};

#[derive(Debug, Clone)]
pub struct Expansion {
    pub term: TermId,
    pub bindings: Vec<Binding>,
    pub constraints: HashMap<BindingId, Vec<Constraint>>,
    pub result: BindingId,
}

impl Expansion {
    fn add_constraint(&mut self, binding_id: BindingId, constraint: Constraint) {
        self.constraints
            .entry(binding_id)
            .or_default()
            .push(constraint);
    }
}

pub struct Expander {
    prog: Program,
    term_rule_sets: HashMap<TermId, RuleSet>,

    /// Terms which can be considered for inlining.
    inlineable: HashSet<TermId>,

    /// Expansions under construction.
    stack: Vec<Expansion>,

    /// Completed expansions.
    complete: Vec<Expansion>,
}

impl Expander {
    pub fn new(prog: Program, term_rule_sets: HashMap<TermId, RuleSet>) -> Self {
        Self {
            prog,
            term_rule_sets,
            inlineable: HashSet::new(),
            stack: Vec::new(),
            complete: Vec::new(),
        }
    }

    // Push an initial expansion for a constructor call of the given term.
    pub fn constructor(&mut self, term_id: TermId) {
        // Lookup term.
        let term = self.prog.term(term_id);
        assert!(term.has_constructor());

        // Push argument bindings.
        let sig = term
            .constructor_sig(&self.prog.tyenv)
            .expect("should have constructor signature");
        let mut bindings = Vec::new();
        let mut parameters = Vec::new();
        for i in 0..sig.param_tys.len() {
            let parameter = bindings.len().try_into().unwrap();
            bindings.push(Binding::Argument {
                index: i.try_into().unwrap(),
            });
            parameters.push(parameter);
        }

        // Binding for the constructor call.
        let result = bindings.len().try_into().unwrap();
        bindings.push(Binding::Constructor {
            term: term_id,
            parameters: parameters.into(),
            instance: 0,
        });

        // Store.
        let expansion = Expansion {
            term: term_id,
            bindings,
            constraints: HashMap::new(),
            result,
        };
        self.stack.push(expansion);
    }

    pub fn inline(&mut self, term_id: TermId) {
        self.inlineable.insert(term_id);
    }

    pub fn expansions(&self) -> &Vec<Expansion> {
        &self.complete
    }

    pub fn expand(&mut self) {
        while !self.stack.is_empty() {
            println!("stack size: {}", self.stack.len());
            let expansion = self.stack.pop().unwrap();
            self.extend(expansion);
        }
    }

    fn extend(&mut self, expansion: Expansion) {
        println!("---------------");
        println!("extend: {expansion:?}");

        // Look for a candidate for inlining. If none, we're done.
        let inline_binding_id = match self.inline_candidate(&expansion) {
            Some(binding_id) => binding_id,
            None => {
                self.complete.push(expansion);
                return;
            }
        };

        // Inline constructor.
        println!("inline candidate: {inline_binding_id:?}");
        let binding = &expansion.bindings[inline_binding_id.index()];
        let (term_id, parameters) = match binding {
            Binding::Constructor {
                term, parameters, ..
            } => (term, parameters),
            _ => unreachable!("expect constructor binding"),
        };

        let rule_set = &self.term_rule_sets[term_id];
        for rule in &rule_set.rules {
            let mut apply = Application::new(expansion.clone());
            let inlined = apply.rule(rule_set, rule, parameters, inline_binding_id);
            println!("inlined: {inlined:?}");
            self.stack.push(inlined);
        }
    }

    // Look for a binding that could be inlined, if any.
    fn inline_candidate(&self, expansion: &Expansion) -> Option<BindingId> {
        for (i, binding) in expansion.bindings.iter().enumerate() {
            match binding {
                Binding::Constructor { term, .. } if self.inlineable.contains(term) => {
                    return Some(i.try_into().unwrap());
                }
                _ => continue,
            }
        }
        None
    }
}

struct Application {
    expansion: Expansion,
    equals: DisjointSets<BindingId>,
    binding_remap: HashMap<BindingId, BindingId>,
}

impl Application {
    fn new(expansion: Expansion) -> Self {
        Self {
            binding_remap: HashMap::new(),
            equals: DisjointSets::default(),
            expansion,
        }
    }

    fn rule(
        &mut self,
        rule_set: &RuleSet,
        rule: &Rule,
        parameters: &Box<[BindingId]>,
        result: BindingId,
    ) -> Expansion {
        // Arguments.
        for (i, parameter) in parameters.iter().enumerate() {
            // Lookup binding ID from the source rule.
            let arg = Binding::Argument {
                index: i.try_into().unwrap(),
            };
            let binding_id = rule_set
                .find_binding(&arg)
                .expect("should have argument binding");

            // Map argument binding to the provided parameter.
            self.binding_remap.insert(binding_id, *parameter);
        }

        // Result.
        self.add_binding(rule_set, rule.result, Some(result));

        // Constraints.
        for i in 0..rule_set.bindings.len() {
            let binding_id = i.try_into().unwrap();
            if let Some(constraint) = rule.get_constraint(binding_id) {
                let expansion_binding_id = self.add_binding(rule_set, binding_id, None);
                self.expansion
                    .add_constraint(expansion_binding_id, constraint);
            }
        }

        // TODO: iterators, prio, impure?

        // Return expanded rule.
        self.expansion.clone()
    }

    fn add_binding(
        &mut self,
        rule_set: &RuleSet,
        binding_id: BindingId,
        dst_binding_id: Option<BindingId>,
    ) -> BindingId {
        // Check if it has already been added.
        if self.binding_remap.contains_key(&binding_id) {
            return self.binding_remap[&binding_id];
        }

        // Resolve equality mapping.
        if let Some(repr) = self.equals.find(binding_id) {
            if repr != binding_id {
                let expansion_binding_id = self.add_binding(rule_set, repr, None);
                self.binding_remap.insert(binding_id, expansion_binding_id);
                return expansion_binding_id;
            }
        }

        // Add dependencies first.
        let binding = &rule_set.bindings[binding_id.index()];
        for source in binding.sources() {
            self.add_binding(rule_set, *source, None);
        }

        // Reindex this binding.
        let reindexed = self.reindex_binding(binding);

        // Insert into expansion bindings list.
        let expansion_binding_id = if let Some(dst_binding_id) = dst_binding_id {
            self.expansion.bindings[dst_binding_id.index()] = reindexed;
            dst_binding_id
        } else {
            let expansion_binding_id: BindingId = self.expansion.bindings.len().try_into().unwrap();
            self.expansion.bindings.push(reindexed);
            expansion_binding_id
        };

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
