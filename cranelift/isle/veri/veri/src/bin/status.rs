use std::collections::HashMap;

use anyhow::{format_err, Result};
use clap::Parser;
use cranelift_codegen_meta::{generate_isle, isle::get_isle_compilations};
use cranelift_isle::sema::TermId;
use cranelift_isle_veri::expand::{Chaining, Expander, Expansion};
use cranelift_isle_veri::program::Program;

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

    /// Skip expansions containing terms with this tag.
    #[arg(long = "skip-tag", value_name = "TAG")]
    skip_tags: Vec<String>,
}

impl Opts {
    fn isle_input_files(&self) -> Result<Vec<std::path::PathBuf>> {
        // Generate ISLE files.
        let gen_dir = &self.work_dir;
        generate_isle(gen_dir)?;

        // Lookup ISLE compilations.
        let compilations = get_isle_compilations(&self.codegen_crate_dir, gen_dir);

        // Return inputs from the matching compilation, if any.
        Ok(compilations
            .lookup(&self.name)
            .ok_or(format_err!("unknown ISLE compilation: {}", self.name))?
            .paths()?)
    }
}

fn main() -> Result<()> {
    let opts = Opts::parse();

    // Read ISLE inputs.
    let inputs = opts.isle_input_files()?;
    let expand_internal_extractors = false;
    let prog = Program::from_files(&inputs, expand_internal_extractors)?;
    let term_rule_sets: HashMap<_, _> = prog.build_trie()?.into_iter().collect();

    // Generate expansions.
    let chaining = Chaining::new(&prog, &term_rule_sets)?;
    let mut expander = Expander::new(&prog, &term_rule_sets, chaining);
    expander.add_root_term_name("lower")?;
    expander.set_prune_infeasible(true);
    expander.expand();

    // Show status.
    status(expander.expansions(), &opts.skip_tags, &prog);

    Ok(())
}

fn status(expansions: &Vec<Expansion>, skip_tags: &[String], prog: &Program) {
    // Report config
    println!("CONFIG");
    println!("skip_tags\t{skip_tags}", skip_tags = skip_tags.join(","));
    println!();

    // Collect status
    let mut total = 0usize;
    let mut num_out_of_scope = 0usize;
    let mut num_specified = 0usize;
    let mut term_unspecified_counts: HashMap<TermId, isize> = HashMap::new();
    for expansion in expansions {
        total += 1;

        if skip(expansion, skip_tags, prog) {
            num_out_of_scope += 1;
            continue;
        }

        let unspecified = unspecified_terms(expansion, prog);
        if unspecified.is_empty() {
            num_specified += 1;
        }
        for term_id in unspecified {
            *term_unspecified_counts.entry(term_id).or_default() += 1;
        }
    }

    // Summary
    println!("SUMMARY");

    let num_in_scope = total - num_out_of_scope;
    let coverage = (num_specified as f64 / num_in_scope as f64) * 100.0;

    println!("total\t{total}");
    println!("out_of_scope\t{num_out_of_scope}");
    println!("in_scope\t{num_in_scope}");
    println!("specified\t{num_specified}");
    println!("coverage\t{coverage:.2}");
    println!();

    // Unspecified terms
    println!("UNSPECIFIED");

    let mut term_unspecified_counts: Vec<_> = term_unspecified_counts.into_iter().collect();
    term_unspecified_counts.sort_by_key(|(_, count)| -*count);
    for (term_id, count) in term_unspecified_counts {
        println!("{term}\t{count}", term = prog.term_name(term_id));
    }
}

fn skip(expansion: &Expansion, skip_tags: &[String], prog: &Program) -> bool {
    let tags = expansion.term_tags(prog);
    for tag in skip_tags {
        if tags.contains(tag) {
            return true;
        }
    }
    false
}

fn unspecified_terms(expansion: &Expansion, prog: &Program) -> Vec<TermId> {
    expansion
        .terms(prog)
        .iter()
        .copied()
        .filter(|term_id| !prog.specenv.has_spec(*term_id))
        .collect()
}
