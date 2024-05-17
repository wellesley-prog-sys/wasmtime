use clap::Parser;
use cranelift_codegen_meta::{generate_isle, isle::get_isle_compilations};
use cranelift_isle::overlap;
use cranelift_isle_veri::{debug::print_rule_set, program::Program};

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

    // Dump rule sets.
    let term_rule_sets = overlap::check(&prog.tyenv, &prog.termenv)?;
    for (term_id, rule_set) in &term_rule_sets {
        print_rule_set(&prog, term_id, rule_set);
    }

    Ok(())
}
