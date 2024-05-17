use std::collections::{HashMap, HashSet};

use crate::program::Program;
use cranelift_isle::{
    sema::TermId,
    trie_again::{Binding, BindingId, Constraint, Rule, RuleSet},
};

#[derive(Debug, Clone)]
pub struct Expansion {
    pub term: TermId,
    pub bindings: Vec<Option<Binding>>,
    pub constraints: HashMap<BindingId, Vec<Constraint>>,
    // TODO: equals
    pub result: BindingId,
}

impl Expansion {
    fn add_constraint(&mut self, binding_id: BindingId, constraint: Constraint) {
        self.constraints
            .entry(binding_id)
            .or_default()
            .push(constraint);
    }

    fn validate(&self) {
        // Bindings: all references should be valid.
        for binding in self.bindings.iter().flatten() {
            for source in binding.sources() {
                assert!(self.is_defined(*source));
            }
        }

        // Constraints: should refer to defined bindings.
        for binding in self.constraints.keys() {
            assert!(self.is_defined(*binding));
        }

        // Result: should be defined.
        assert!(self.is_defined(self.result));
    }

    fn is_defined(&self, binding_id: BindingId) -> bool {
        self.bindings[binding_id.index()].is_some()
    }

    fn substitute(&mut self, target: BindingId, replace: BindingId) {
        // Reindex bindings.
        let mut reindex = Reindex::new();
        reindex.map(target, replace);
        for binding in self.bindings.iter_mut().flatten() {
            *binding = reindex.binding(binding);
        }

        // Delete the target binding.
        self.bindings[target.index()] = None;

        // Constraints.
        let mut constraints = HashMap::new();
        for (binding_id, constraint) in &self.constraints {
            constraints.insert(reindex.id(binding_id), constraint.clone());
        }
        self.constraints = constraints;

        // Result.
        self.result = reindex.id(&self.result);
    }
}

pub struct Expander<'a> {
    prog: &'a Program,
    term_rule_sets: HashMap<TermId, RuleSet>,

    /// Terms which can be considered for inlining.
    inlineable: HashSet<TermId>,

    /// Expansions under construction.
    stack: Vec<Expansion>,

    /// Completed expansions.
    complete: Vec<Expansion>,
}

impl<'a> Expander<'a> {
    pub fn new(prog: &'a Program, term_rule_sets: HashMap<TermId, RuleSet>) -> Self {
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
            bindings.push(Some(Binding::Argument {
                index: i.try_into().unwrap(),
            }));
            parameters.push(parameter);
        }

        // Binding for the constructor call.
        let result = bindings.len().try_into().unwrap();
        bindings.push(Some(Binding::Constructor {
            term: term_id,
            parameters: parameters.into(),
            instance: 0,
        }));

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
        // Internal constructors are supported for inlining.
        let term = self.prog.term(term_id);
        assert!(term.has_constructor(), "only constructors may be inlined");
        assert!(
            !term.has_external_constructor(),
            "should be internal constructor"
        );

        // Add to inlineable set.
        self.inlineable.insert(term_id);
    }

    fn finish(&mut self, expansion: Expansion) {
        expansion.validate();
        self.complete.push(expansion);
    }

    pub fn expansions(&self) -> &Vec<Expansion> {
        &self.complete
    }

    pub fn expand(&mut self) {
        while !self.stack.is_empty() {
            let expansion = self.stack.pop().unwrap();
            self.extend(expansion);
        }
    }

    fn extend(&mut self, expansion: Expansion) {
        // Look for a candidate for inlining. If none, we're done.
        let inline_binding_id = match self.inline_candidate(&expansion) {
            Some(binding_id) => binding_id,
            None => {
                self.finish(expansion);
                return;
            }
        };

        // Inline constructor.
        let binding = &expansion.bindings[inline_binding_id.index()];
        let (term_id, parameters) = match binding {
            Some(Binding::Constructor {
                term, parameters, ..
            }) => (term, parameters),
            _ => unreachable!("expect constructor binding"),
        };

        let rule_set = &self.term_rule_sets[term_id];
        for rule in &rule_set.rules {
            let mut apply = Application::new(expansion.clone());
            let inlined = apply.rule(rule_set, rule, parameters, inline_binding_id);
            self.stack.push(inlined);
        }
    }

    // Look for a binding that could be inlined, if any.
    fn inline_candidate(&self, expansion: &Expansion) -> Option<BindingId> {
        for (i, binding) in expansion.bindings.iter().enumerate() {
            match binding {
                Some(Binding::Constructor { term, .. }) if self.inlineable.contains(term) => {
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
    import_reindex: Reindex,
}

impl Application {
    fn new(expansion: Expansion) -> Self {
        Self {
            expansion,
            import_reindex: Reindex::new(),
        }
    }

    fn rule(
        &mut self,
        rule_set: &RuleSet,
        rule: &Rule,
        parameters: &Box<[BindingId]>,
        call_site: BindingId,
    ) -> Expansion {
        //
        struct Substitution {
            target: BindingId,
            replace: BindingId,
        }
        let mut substitutions = Vec::new();

        // Arguments.
        for (i, parameter) in parameters.iter().enumerate() {
            // Lookup binding ID from the source rule.
            let arg = Binding::Argument {
                index: i.try_into().unwrap(),
            };
            let binding_id = rule_set
                .find_binding(&arg)
                .expect("should have argument binding");

            // Import into expansion.
            let arg_binding_id = self.add_binding(rule_set, binding_id);

            // Substitute argument with the parameter.
            substitutions.push(Substitution {
                target: arg_binding_id,
                replace: *parameter,
            });
        }

        // Constraints.
        for i in 0..rule_set.bindings.len() {
            let binding_id = i.try_into().unwrap();
            if let Some(constraint) = rule.get_constraint(binding_id) {
                let expansion_binding_id = self.add_binding(rule_set, binding_id);
                self.expansion
                    .add_constraint(expansion_binding_id, constraint);
            }
        }

        // TODO: equals
        // TODO: iterators, prio, impure?

        // Result.
        //
        // Once imported, the callsite should be substituted for the result binding.
        let result_binding_id = self.add_binding(rule_set, rule.result);
        substitutions.push(Substitution {
            target: call_site,
            replace: result_binding_id,
        });

        // Process substitutions.
        for substitution in substitutions.iter().rev() {
            self.expansion
                .substitute(substitution.target, substitution.replace);
        }

        // Return expanded rule.
        self.expansion.clone()
    }

    fn add_binding(&mut self, rule_set: &RuleSet, binding_id: BindingId) -> BindingId {
        // Check if it has already been added.
        if self.import_reindex.is_mapped(&binding_id) {
            return self.import_reindex.id(&binding_id);
        }

        // Add dependencies first.
        let binding = &rule_set.bindings[binding_id.index()];
        for source in binding.sources() {
            self.add_binding(rule_set, *source);
        }

        // Reindex this binding.
        let reindexed = self.import_reindex.binding(binding);

        // Insert into expansion bindings list.
        let expansion_binding_id: BindingId = self.expansion.bindings.len().try_into().unwrap();
        self.expansion.bindings.push(Some(reindexed));

        // Record binding mapping.
        self.import_reindex.map(binding_id, expansion_binding_id);
        return expansion_binding_id;
    }
}

/// Reindexing binding IDs.
struct Reindex {
    to: HashMap<BindingId, BindingId>,
}

impl Reindex {
    fn new() -> Self {
        Self { to: HashMap::new() }
    }

    fn is_mapped(&self, binding_id: &BindingId) -> bool {
        self.to.contains_key(binding_id)
    }

    fn map(&mut self, from: BindingId, to: BindingId) {
        self.to.insert(from, to);
    }

    fn id(&self, binding_id: &BindingId) -> BindingId {
        self.to.get(binding_id).unwrap_or(binding_id).clone()
    }

    fn ids(&self, binding_ids: &Box<[BindingId]>) -> Box<[BindingId]> {
        binding_ids
            .iter()
            .map(|binding_id| self.id(binding_id))
            .collect()
    }

    fn binding(&self, binding: &Binding) -> Binding {
        match binding {
            Binding::Argument { .. } | Binding::ConstInt { .. } | Binding::ConstPrim { .. } => {
                binding.clone()
            }

            Binding::Extractor { term, parameter } => Binding::Extractor {
                term: *term,
                parameter: self.id(parameter),
            },

            Binding::Constructor {
                term,
                parameters,
                instance,
            } => Binding::Constructor {
                term: *term,
                parameters: self.ids(parameters),
                instance: *instance,
            },

            Binding::MakeVariant {
                ty,
                variant,
                fields,
            } => Binding::MakeVariant {
                ty: *ty,
                variant: *variant,
                fields: self.ids(fields),
            },

            Binding::MatchVariant {
                source,
                variant,
                field,
            } => Binding::MatchVariant {
                source: self.id(source),
                variant: *variant,
                field: *field,
            },

            Binding::MatchSome { source } => Binding::MatchSome {
                source: self.id(source),
            },

            Binding::MatchTuple { source, field } => Binding::MatchTuple {
                source: self.id(source),
                field: *field,
            },

            _ => todo!("reindex binding: {binding:?}"),
        }
    }
}
