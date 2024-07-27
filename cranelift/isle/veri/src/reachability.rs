use std::collections::{HashMap, HashSet};

use cranelift_isle::{
    sema::TermId,
    trie_again::{Binding, RuleSet},
};

pub struct Reachability<'a> {
    term_rule_sets: &'a HashMap<TermId, RuleSet>,
    reachable: HashMap<TermId, HashSet<TermId>>,
}

impl<'a> Reachability<'a> {
    pub fn build(term_rule_sets: &'a HashMap<TermId, RuleSet>) -> Self {
        Self {
            term_rule_sets,
            reachable: HashMap::new(),
        }
    }

    pub fn reachable(&mut self, term_id: TermId) -> &HashSet<TermId> {
        if !self.reachable.contains_key(&term_id) {
            let reachable = self.search(term_id);
            self.reachable.insert(term_id, reachable);
        }
        self.reachable.get(&term_id).unwrap()
    }

    fn search(&mut self, term_id: TermId) -> HashSet<TermId> {
        let mut reachable = HashSet::new();
        let mut stack = vec![term_id];

        while let Some(term_id) = stack.pop() {
            if !self.term_rule_sets.contains_key(&term_id) {
                continue;
            }

            let used = used_terms(&self.term_rule_sets[&term_id]);
            for used_term_id in used {
                if reachable.contains(&used_term_id) {
                    continue;
                }
                reachable.insert(used_term_id);
                stack.push(used_term_id);
            }
        }

        reachable
    }

    pub fn is_cyclic(&mut self, term_id: TermId) -> bool {
        self.reachable(term_id).contains(&term_id)
    }
}

pub fn used_terms(rule_set: &RuleSet) -> HashSet<TermId> {
    rule_set
        .bindings
        .iter()
        .filter_map(binding_used_term)
        .collect()
}

pub fn binding_used_term(binding: &Binding) -> Option<TermId> {
    match binding {
        Binding::Constructor { term, .. } | Binding::Extractor { term, .. } => Some(*term),
        // TODO(mbm): make variant uses the variant constructor term?
        _ => None,
    }
}
