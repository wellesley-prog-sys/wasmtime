use clap::Parser;
use cranelift_codegen_meta::{generate_isle, isle::get_isle_compilations};
use cranelift_isle_veri::{debug::print_expansion, expand::ExpansionsBuilder, program::Program};

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
    let root_term = "lower";
    let mut expansions_builder = ExpansionsBuilder::new(&prog, &root_term)?;
    expansions_builder.inline_term(&root_term)?;
    expansions_builder.set_maximal_inlining(true);
    expansions_builder.set_max_rules(4);
    expansions_builder.exclude_inline_term("operand_size")?;

    // Report.
    let expansions = expansions_builder.expansions()?;
    println!("expansions = {}", expansions.len());
    for expansion in &expansions {
        if !expansion.rules.contains(&rule.id) {
            continue;
        }
        print_expansion(&prog, expansion);
    }

    Ok(())
}
