use clap::Parser;
use cranelift_codegen_meta::{generate_isle, isle::get_isle_compilations};
use cranelift_isle_veri::{expand::ExpansionsBuilder, explorer::ExplorerWriter, program::Program};

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

    /// Output directory for explorer files.
    #[arg(long, required = true)]
    output_dir: std::path::PathBuf,

    /// Whether to enable graph generation.
    #[arg(long)]
    graphs: bool,
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

    // Generate expansions.
    // TODO(mbm): don't hardcode the expansion configuration
    let root_term = "lower";
    let mut expansions_builder = ExpansionsBuilder::new(&prog, root_term)?;
    expansions_builder.inline_term(root_term)?;
    expansions_builder.set_maximal_inlining(true);
    expansions_builder.set_max_rules(6);
    expansions_builder.exclude_inline_term("operand_size")?;
    let expansions = expansions_builder.expansions()?;

    // Generate explorer.
    let mut explorer_writer = ExplorerWriter::new(opts.output_dir, &prog, &expansions);
    if opts.graphs {
        explorer_writer.enable_graphs();
    }
    explorer_writer.write()?;

    Ok(())
}
