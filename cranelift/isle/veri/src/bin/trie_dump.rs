use clap::Parser;
use cranelift_codegen_meta::{generate_isle, isle::get_isle_compilations};
use cranelift_isle::overlap;
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

    // Dump rule sets.
    let term_rule_sets = overlap::check(&prog.tyenv, &prog.termenv)?;
    for (term_id, rule_set) in &term_rule_sets {
        println!("term {{");
        println!("\tname = {}", prog.term_name(*term_id));

        // Bindings.
        println!("\tbindings = [");
        for (i, binding) in rule_set.bindings.iter().enumerate() {
            println!("\t\t{i}:\t{binding:?}");
        }
        println!("\t]");

        // Rules.
        println!("\trules = [");
        for rule in &rule_set.rules {
            assert_eq!(rule.iterators.len(), 0);
            // TODO(mbm): how to handle impure?

            println!("\t\t{{");
            println!(
                "\t\t\tpos = {}",
                rule.pos.pretty_print_line(&prog.tyenv.filenames[..])
            );
            println!("\t\t\tconstraints = [");
            for i in 0..rule_set.bindings.len() {
                if let Some(constraint) = rule.get_constraint(i.try_into().unwrap()) {
                    println!("\t\t\t\t{}: {:?}", i, constraint);
                }
            }
            println!("\t\t\t]");
            if !rule.equals.is_empty() {
                println!("\t\t\tequals = {:?}", rule.equals);
            }
            println!("\t\t\tprio = {}", rule.prio);
            println!("\t\t\tresult = {}", rule.result.index());
            println!("\t\t}}");
        }
        println!("\t]");

        println!("}}");
    }

    Ok(())
}
