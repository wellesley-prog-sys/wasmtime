use std::collections::HashMap;

use clap::Parser;
use cranelift_codegen_meta::{generate_isle, isle::get_isle_compilations};
use cranelift_isle::overlap;
use cranelift_isle::trie_again::BindingId;
use cranelift_isle_veri::debug::{binding_string, binding_type};
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

    // Config.
    let term_name = "sink_load_into_addr";
    let inline_term_names = vec![term_name, "add_imm_to_addr"];

    // Read ISLE inputs.
    let inputs = opts.isle_input_files()?;
    let expand_internal_extractors = true;
    let prog = Program::from_files(&inputs, expand_internal_extractors)?;

    // Derive rule sets.
    let term_rule_sets: HashMap<_, _> = overlap::check(&prog.tyenv, &prog.termenv)?
        .into_iter()
        .collect();

    // Lookup term to expand.
    let term_id = prog
        .get_term_by_name(&term_name)
        .ok_or(anyhow::format_err!("unknown term {term_name}"))?;

    let mut inline_term_ids = Vec::new();
    for inline_term_name in &inline_term_names {
        let term_id = prog
            .get_term_by_name(&inline_term_name)
            .ok_or(anyhow::format_err!("unknown term {term_name}"))?;
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
    for expansion in expander.expansions() {
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

    // pub constraints: HashMap<BindingId, Vec<Constraint>>,
    // pub result: BindingId,
    println!("}}");
}
