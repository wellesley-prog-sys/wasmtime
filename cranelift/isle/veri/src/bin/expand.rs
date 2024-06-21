use clap::Parser;
use cranelift_codegen_meta::{generate_isle, isle::get_isle_compilations};
use cranelift_isle_veri::debug::print_expansion;
use cranelift_isle_veri::expand::ExpansionsBuilder;
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

    /// Whether to disable pruning of infeasible expansions.
    #[arg(long)]
    no_prune_infeasible: bool,

    /// Term names to inline.
    #[arg(long, value_name = "TERM_NAME")]
    inline: Vec<String>,

    /// Whether to enable maximal inlining.
    #[arg(long)]
    maximal_inlining: bool,

    /// Maximum rules: only inline terms with at most this many rules.
    #[arg(long, default_value = "0")]
    max_rules: usize,

    /// Terms to exclude from inlining.
    #[arg(long, value_name = "TERM_NAME")]
    exclude_inline: Vec<String>,
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

    // Build expansions.
    let mut expansions_builder = ExpansionsBuilder::new(&prog, &opts.term_name)?;
    expansions_builder.set_prune_infeasible(!opts.no_prune_infeasible);

    // Configure inline terms.
    expansions_builder.inline_term(&opts.term_name)?;
    expansions_builder.inline_terms(&opts.inline)?;

    // Configure maximal inlining.
    expansions_builder.set_maximal_inlining(opts.maximal_inlining);
    expansions_builder.set_max_rules(opts.max_rules);
    expansions_builder.exclude_inline_terms(&opts.exclude_inline)?;

    // Report.
    let expansions = expansions_builder.expansions()?;
    println!("expansions = {}", expansions.len());
    for expansion in &expansions {
        print_expansion(&prog, expansion);
    }

    Ok(())
}
