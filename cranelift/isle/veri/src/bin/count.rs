use std::collections::{HashMap, HashSet};

use clap::Parser;
use cranelift_codegen_meta::{generate_isle, isle::get_isle_compilations};
use cranelift_isle::{
    sema::TermId,
    trie_again::{BindingId, Rule, RuleSet},
};
use cranelift_isle_veri::{
    program::Program,
    reachability::{self, Reachability},
};

#[derive(Parser)]
struct Opts {
    /// Name of the ISLE compilation.
    #[arg(long, required = true)]
    name: String,

    /// Path to codegen crate directory.
    #[arg(long, required = true)]
    codegen_crate_dir: std::path::PathBuf,

    /// Working directory.
    #[arg(long, required = true)]
    work_dir: std::path::PathBuf,

    /// Whether to disable expansion of internal extractors.
    #[arg(long)]
    no_expand_internal_extractors: bool,

    /// Term to count.
    #[arg(long, required = true)]
    term_name: String,
}

impl Opts {
    fn isle_input_files(&self) -> anyhow::Result<Vec<std::path::PathBuf>> {
        // Generate ISLE files.
        let gen_dir = &self.work_dir;
        generate_isle(gen_dir)?;

        // Lookup ISLE compilations.
        let compilations = get_isle_compilations(&self.codegen_crate_dir, gen_dir);

        // Return inputs from the matching compilation, if any.
        Ok(compilations
            .lookup(&self.name)
            .ok_or(anyhow::format_err!(
                "unknown ISLE compilation: {}",
                self.name
            ))?
            .inputs())
    }
}

fn main() -> anyhow::Result<()> {
    let opts = Opts::parse();

    // Read ISLE inputs.
    let inputs = opts.isle_input_files()?;
    let prog = Program::from_files(&inputs, !opts.no_expand_internal_extractors)?;

    // Derive rule sets.
    let term_rule_sets: HashMap<_, _> = prog.build_trie()?.into_iter().collect();

    // Lookup term to count.
    let term_id = prog
        .get_term_by_name(opts.term_name.as_str())
        .ok_or(anyhow::format_err!("unknown term {}", opts.term_name))?;
    println!("term = {}", opts.term_name);
    println!("id = {}", term_id.index());

    // Count expansions.
    let mut expansion_counter = ExpansionCounter::new(&prog, &term_rule_sets);
    let n = expansion_counter.term(term_id, "".to_string());
    println!("expansions = {}", n);

    Ok(())
}

struct ExpansionCounter<'a> {
    prog: &'a Program,
    term_rule_sets: &'a HashMap<TermId, RuleSet>,
    reach: Reachability<'a>,
}

impl<'a> ExpansionCounter<'a> {
    fn new(prog: &'a Program, term_rule_sets: &'a HashMap<TermId, RuleSet>) -> Self {
        Self {
            prog,
            term_rule_sets,
            reach: Reachability::build(term_rule_sets),
        }
    }

    fn term(&mut self, term_id: TermId, indent: String) -> usize {
        println!(
            "{indent}{term_name}",
            term_name = self.prog.term_name(term_id)
        );

        if !self.may_expand(term_id) {
            1
        } else {
            let rule_set = &self.term_rule_sets[&term_id];
            self.rule_set(rule_set, indent.clone())
        }
    }

    fn may_expand(&mut self, term_id: TermId) -> bool {
        self.term_rule_sets.contains_key(&term_id) && !self.reach.is_cyclic(term_id)
    }

    fn rule_set(&mut self, rule_set: &RuleSet, indent: String) -> usize {
        let mut n = 0;
        for rule in &rule_set.rules {
            n += self.rule(rule_set, rule, indent.clone());
            println!(
                "{indent}n={n} rule={}",
                rule.pos.pretty_print_line(&self.prog.tyenv.filenames)
            );
        }
        n
    }

    fn rule(&mut self, rule_set: &RuleSet, rule: &Rule, indent: String) -> usize {
        let binding_ids = rule_bindings(rule_set, rule);
        let mut n = 1;
        for binding_id in binding_ids {
            let binding = &rule_set.bindings[binding_id.index()];
            if let Some(term_id) = reachability::binding_used_term(binding) {
                n *= self.term(term_id, format!("{indent}.\t"));
            }
        }
        n
    }
}

fn rule_bindings(rule_set: &RuleSet, rule: &Rule) -> HashSet<BindingId> {
    // TODO(mbm): duplicates logic in expand::Application

    // Initialize stack of bindings used directly by the rule.
    let mut stack = Vec::new();
    stack.push(rule.result);
    // TODO(mbm): equals
    // TODO(mbm): constraints
    // TODO(mbm): iterators, prio, impure?

    // Collect dependencies.
    let mut binding_ids = HashSet::new();
    while !stack.is_empty() {
        let binding_id = stack.pop().unwrap();

        if binding_ids.contains(&binding_id) {
            continue;
        }
        binding_ids.insert(binding_id);

        let binding = &rule_set.bindings[binding_id.index()];
        stack.extend(binding.sources());
    }

    binding_ids
}
