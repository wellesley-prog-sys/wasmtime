use std::collections::HashMap;

use clap::Parser;
use cranelift_codegen_meta::{generate_isle, isle::get_isle_compilations};
use cranelift_isle::overlap;
use cranelift_isle_veri::expand::Expander;
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
    let expand_internal_extractors = true;
    let prog = Program::from_files(&inputs, expand_internal_extractors)?;

    // Derive rule sets.
    let term_rule_sets: HashMap<_, _> = overlap::check(&prog.tyenv, &prog.termenv)?
        .into_iter()
        .collect();

    // Lookup term to expand.
    let term_name = "lower";
    let term_id = prog
        .get_term_by_name(&term_name)
        .ok_or(anyhow::format_err!("unknown term {term_name}"))?;

    // Expand.
    let rule_set = &term_rule_sets[&term_id];
    let mut expander = Expander::new(rule_set);
    let expansions = expander.rules();

    // Report.
    for expansion in &expansions {
        println!("{expansion:?}");
    }

    Ok(())
}
