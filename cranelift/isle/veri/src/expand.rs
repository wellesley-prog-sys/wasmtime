use std::collections::{BTreeMap, HashMap, HashSet};

use crate::{program::Program, reachability::Reachability};
use cranelift_isle::{
    disjointsets::DisjointSets,
    sema::{RuleId, TermId},
    trie_again::{Binding, BindingId, Constraint, Rule, RuleSet},
};

#[derive(Debug, Clone)]
pub struct Expansion {
    pub term: TermId,
    pub rules: Vec<RuleId>,
    pub bindings: Vec<Option<Binding>>,
    // QUESTION(mbm): are multiple constraints per binding necessary?
    pub constraints: BTreeMap<BindingId, Vec<Constraint>>,
    pub equals: DisjointSets<BindingId>,
    pub result: BindingId,
}

impl Expansion {
    /// Check basic feasibility of the expansion.
    ///
    /// A false return value means the expansion is known to never be viable.
    /// However, a successful feasibility check leaves open the possibility that
    /// the expansion is inapplicable for other reasons.
    pub fn is_feasible(&self) -> bool {
        // Assert data structure invariants.
        self.validate();

        // Check if any constraints are incompatible.
        for (binding_id, constraints) in &self.constraints {
            let binding = self.bindings[binding_id.index()]
                .as_ref()
                .expect("constrained binding must be defined");
            for constraint in constraints {
                if !constraint.compatible(binding) {
                    return false;
                }
            }
        }

        return true;
    }

    fn add_constraint(&mut self, binding_id: BindingId, constraint: Constraint) {
        self.constraints
            .entry(binding_id)
            .or_default()
            .push(constraint);
    }

    fn push_binding(&mut self, binding: Binding) -> BindingId {
        let binding_id = self.bindings.len().try_into().unwrap();
        self.bindings.push(Some(binding));
        binding_id
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

    pub fn binding(&self, binding_id: BindingId) -> Option<&Binding> {
        self.bindings.get(binding_id.index())?.as_ref()
    }

    pub fn equalities(&self) -> Vec<(BindingId, BindingId)> {
        let mut eqs = Vec::new();
        for (i, binding) in self.bindings.iter().enumerate() {
            if binding.is_none() {
                continue;
            }
            let binding_id = i.try_into().unwrap();
            if let Some(eq) = self.equals.find(binding_id) {
                if eq != binding_id {
                    eqs.push((binding_id, eq));
                }
            }
        }
        eqs
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
        let mut constraints = BTreeMap::new();
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
    term_rule_sets: &'a HashMap<TermId, RuleSet>,
    reach: Reachability<'a>,

    /// Terms which can be considered for inlining.
    inlineable: HashSet<TermId>,

    /// Whether to drop expansions as soon as they are deemed to be infeasible.
    prune_infeasible: bool,

    /// Expansions under construction.
    stack: Vec<Expansion>,

    /// Completed expansions.
    complete: Vec<Expansion>,
}

impl<'a> Expander<'a> {
    pub fn new(prog: &'a Program, term_rule_sets: &'a HashMap<TermId, RuleSet>) -> Self {
        Self {
            prog,
            term_rule_sets,
            reach: Reachability::build(term_rule_sets),
            inlineable: HashSet::new(),
            prune_infeasible: true,
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
            rules: Vec::new(),
            bindings,
            constraints: BTreeMap::new(),
            equals: DisjointSets::default(),
            result,
        };
        assert!(expansion.is_feasible());
        self.stack.push(expansion);
    }

    /// Set whether to prune infeasible expansions. If enabled, expansions will
    /// be dropped as soon as they are deemed to be not feasible.
    pub fn set_prune_infeasible(&mut self, enabled: bool) {
        self.prune_infeasible = enabled;
    }

    /// Marks term ID as inlinable.
    pub fn inline(&mut self, term_id: TermId) {
        // Must be inlineable.
        assert!(self.may_inline(term_id));

        // Add to inlineable set.
        self.inlineable.insert(term_id);
    }

    /// Reports whether the given term can be inlined. Internal acyclic
    /// constructors can be inlined.
    pub fn may_inline(&mut self, term_id: TermId) -> bool {
        // Internal constructors are supported for inlining.
        let term = self.prog.term(term_id);
        if !term.has_constructor() {
            return false;
        }

        if term.has_external_constructor() {
            return false;
        }

        // Cyclic terms cannot be inlined.
        if self.reach.is_cyclic(term_id) {
            return false;
        }

        true
    }

    /// Mark all possible terms as candidates for inlining, provided they have
    /// at most the given number of rules (0 for no limit) and are not in an
    /// explicit exclusion set.
    pub fn enable_maximal_inlining(&mut self, max_rules: usize, exclude: &HashSet<TermId>) {
        for (term_id, rule_set) in self.term_rule_sets {
            // HACK(mbm): merge these heuristics with may_inline
            if max_rules > 0 && rule_set.rules.len() > max_rules {
                continue;
            }

            if exclude.contains(&term_id) {
                continue;
            }

            // Mark inlinable, provided it meets base requirements.
            if self.may_inline(*term_id) {
                self.inline(*term_id);
            }
        }
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
            if !self.prune_infeasible || inlined.is_feasible() {
                self.stack.push(inlined);
            }
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
        // Record the application of this rule.
        self.expansion.rules.push(rule.id);

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

        // Equals.
        for i in 0..rule_set.bindings.len() {
            let binding_id = i.try_into().unwrap();
            if let Some(equal_binding_id) = rule.equals.find(binding_id) {
                if equal_binding_id != binding_id {
                    let expansion_binding_id = self.add_binding(rule_set, binding_id);
                    let expansion_equal_binding_id = self.add_binding(rule_set, equal_binding_id);
                    self.expansion
                        .equals
                        .merge(expansion_binding_id, expansion_equal_binding_id);
                }
            }
        }

        // TODO: iterators, prio?

        // Impure.
        for impure_binding_id in &rule.impure {
            self.add_binding(rule_set, *impure_binding_id);
        }

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
        let expansion_binding_id = self.expansion.push_binding(reindexed);

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

            Binding::MakeSome { inner } => Binding::MakeSome {
                inner: self.id(inner),
            },

            Binding::MatchSome { source } => Binding::MatchSome {
                source: self.id(source),
            },

            Binding::MatchTuple { source, field } => Binding::MatchTuple {
                source: self.id(source),
                field: *field,
            },

            Binding::Iterator { .. } => unimplemented!("iterator bindings not supported"),
        }
    }
}

pub struct ExpansionsBuilder<'a> {
    prog: &'a Program,
    root: TermId,
    inline: Vec<TermId>,
    prune_infeasible: bool,
    maximal_inlining: bool,
    max_rules: usize,
    exclude_inline: HashSet<TermId>,
}

impl<'a> ExpansionsBuilder<'a> {
    pub fn new(prog: &'a Program, root_name: &str) -> anyhow::Result<Self> {
        let root = prog
            .get_term_by_name(root_name)
            .ok_or(anyhow::format_err!("unknown term {}", root_name))?;

        Ok(Self {
            prog,
            root,
            inline: Vec::new(),
            prune_infeasible: true,
            maximal_inlining: false,
            max_rules: 0,
            exclude_inline: HashSet::new(),
        })
    }

    pub fn inline_term(&mut self, term_name: &str) -> anyhow::Result<()> {
        let term_id = self
            .prog
            .get_term_by_name(&term_name)
            .ok_or(anyhow::format_err!("unknown term {term_name}"))?;
        self.inline.push(term_id);
        Ok(())
    }

    pub fn inline_terms(&mut self, term_names: &Vec<String>) -> anyhow::Result<()> {
        for term_name in term_names {
            self.inline_term(term_name)?;
        }
        Ok(())
    }

    pub fn set_prune_infeasible(&mut self, enabled: bool) {
        self.prune_infeasible = enabled;
    }

    pub fn set_maximal_inlining(&mut self, enabled: bool) {
        self.maximal_inlining = enabled;
    }

    pub fn set_max_rules(&mut self, max_rules: usize) {
        self.max_rules = max_rules;
    }

    pub fn exclude_inline_term(&mut self, term_name: &str) -> anyhow::Result<()> {
        let term_id = self
            .prog
            .get_term_by_name(&term_name)
            .ok_or(anyhow::format_err!("unknown term {term_name}"))?;
        self.exclude_inline.insert(term_id);
        Ok(())
    }

    pub fn exclude_inline_terms(&mut self, term_names: &Vec<String>) -> anyhow::Result<()> {
        for term_name in term_names {
            self.exclude_inline_term(term_name)?;
        }
        Ok(())
    }

    pub fn expansions(self) -> anyhow::Result<Vec<Expansion>> {
        let term_rule_sets: HashMap<_, _> = self.prog.build_trie()?.into_iter().collect();
        let mut expander = Expander::new(self.prog, &term_rule_sets);
        expander.constructor(self.root);
        expander.set_prune_infeasible(self.prune_infeasible);

        for inline_term_id in self.inline {
            expander.inline(inline_term_id);
        }

        if self.maximal_inlining {
            expander.enable_maximal_inlining(self.max_rules, &self.exclude_inline);
        }

        expander.expand();

        Ok(expander.expansions().clone())
    }
}
