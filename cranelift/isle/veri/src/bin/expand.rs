use std::collections::HashMap;

use clap::Parser;
use cranelift_codegen_meta::{generate_isle, isle::get_isle_compilations};
use cranelift_isle::overlap;
use cranelift_isle::trie_again::BindingId;
use cranelift_isle_veri::debug::{binding_string, binding_type, constraint_string};
use cranelift_isle_veri::expand::{Expander, Expansion};
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

    /// Whether to disable expansion of internal extractors.
    #[arg(long)]
    no_expand_internal_extractors: bool,

    /// Term to expand.
    #[arg(long, required = true)]
    term_name: String,

    /// Term names to inline.
    #[arg(long)]
    inline: Vec<String>,
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

    // Lookup term to expand.
    let term_id = prog
        .get_term_by_name(opts.term_name.as_str())
        .ok_or(anyhow::format_err!("unknown term {}", opts.term_name))?;

    // Configure inline terms.
    let mut inline_term_names = opts.inline.clone();
    inline_term_names.push(opts.term_name.clone());

    let mut inline_term_ids = Vec::new();
    for inline_term_name in &inline_term_names {
        let term_id = prog
            .get_term_by_name(&inline_term_name)
            .ok_or(anyhow::format_err!("unknown term {inline_term_name}"))?;
        inline_term_ids.push(term_id);
        println!("inline term: {inline_term_name}");
    }

    // Expand.
    let mut expander = Expander::new(&prog, term_rule_sets);
    expander.constructor(term_id);
    for inline_term_id in inline_term_ids {
        expander.inline(inline_term_id);
    }

    expander.expand();

    // Report.
    let expansions = expander.expansions();
    println!("expansions = {}", expansions.len());
    for expansion in expansions {
        print_expansion(&prog, expansion);
    }

    Ok(())
}

pub fn print_expansion(prog: &Program, expansion: &Expansion) {
    println!("expansion {{");

    // Term.
    println!("\tterm = {}", prog.term_name(expansion.term));

    // Bindings.
    let lookup_binding =
        |binding_id: BindingId| expansion.bindings[binding_id.index()].clone().unwrap();
    println!("\tbindings = [");
    for (i, binding) in expansion.bindings.iter().enumerate() {
        if let Some(binding) = binding {
            let ty = binding_type(binding, expansion.term, &prog, lookup_binding);
            println!(
                "\t\t{i}: {}\t{}",
                ty.display(&prog.tyenv),
                binding_string(binding, expansion.term, &prog, lookup_binding),
            );
        }
    }
    println!("\t]");

    // Constraints.
    println!("\tconstraints = [");
    for (binding_id, constraints) in &expansion.constraints {
        for constraint in constraints {
            println!(
                "\t\t{}:\t{}",
                binding_id.index(),
                constraint_string(&constraint, &prog.tyenv)
            );
        }
    }
    println!("\t]");

    // Result.
    println!("\tresult = {}", expansion.result.index());

    println!("}}");
}
