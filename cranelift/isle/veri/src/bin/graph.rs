use clap::Parser;
use cranelift_codegen_meta::{generate_isle, isle::get_isle_compilations};
use cranelift_isle::trie_again::BindingId;
use cranelift_isle_veri::{
    debug::binding_string,
    expand::{Expansion, ExpansionsBuilder},
    program::Program,
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

    /// Filter to expansions involving this rule.
    #[arg(long, required = true)]
    rule: String,
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
    let _ = env_logger::try_init();
    let opts = Opts::parse();

    // Read ISLE inputs.
    let inputs = opts.isle_input_files()?;
    let expand_internal_extractors = false;
    let prog = Program::from_files(&inputs, expand_internal_extractors)?;

    // Lookup target rule.
    let rule = prog
        .get_rule_by_identifier(&opts.rule)
        .ok_or(anyhow::format_err!(
            "unknown rule: {rule_name}",
            rule_name = opts.rule
        ))?;

    // Generate expansions.
    // TODO(mbm): don't hardcode the expansion configuration
    let root_term = "lower";
    let mut expansions_builder = ExpansionsBuilder::new(&prog, root_term)?;
    expansions_builder.chain_term(root_term)?;
    expansions_builder.set_maximal_chaining(true);
    expansions_builder.set_max_rules(6);
    expansions_builder.exclude_chain_term("operand_size")?;

    // Process expansions.
    let expansions = expansions_builder.expansions()?;
    for expansion in &expansions {
        if !expansion.rules.contains(&rule.id) {
            continue;
        }
        expansion_graph(expansion, &prog);
    }

    Ok(())
}

fn expansion_graph(expansion: &Expansion, prog: &Program) {
    // Header.
    println!("graph {{");
    println!("\tnode [shape=box, fontname=monospace];");

    // Binding nodes.
    let lookup_binding =
        |binding_id: BindingId| expansion.bindings[binding_id.index()].clone().unwrap();
    for (i, binding) in expansion.bindings.iter().enumerate() {
        if let Some(binding) = binding {
            println!(
                "\tb{i} [label=\"{i}: {}\"];",
                binding_string(binding, expansion.term, prog, lookup_binding)
            );
        }
    }

    // Edges.
    for (i, binding) in expansion.bindings.iter().enumerate() {
        if let Some(binding) = binding {
            for source in binding.sources() {
                println!("\tb{i} -- b{j};", j = source.index());
            }
        }
    }

    println!("}}");
}
