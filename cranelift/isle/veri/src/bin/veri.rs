use clap::Parser;
use cranelift_codegen_meta::{generate_isle, isle::get_isle_compilations};
use cranelift_isle_veri::{
    debug::print_expansion,
    expand::{Expansion, ExpansionsBuilder},
    program::Program,
    type_inference::{type_constraints, Solver},
    veri::{Conditions, ExprId},
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
    let mut expansions_builder = ExpansionsBuilder::new(&prog, &root_term)?;
    expansions_builder.inline_term(&root_term)?;
    expansions_builder.set_maximal_inlining(true);
    expansions_builder.set_max_rules(4);
    expansions_builder.exclude_inline_term("operand_size")?;

    // Process expansions.
    let expansions = expansions_builder.expansions()?;
    println!("expansions = {}", expansions.len());
    for expansion in &expansions {
        if !expansion.rules.contains(&rule.id) {
            continue;
        }
        print_expansion(&prog, expansion);
        verify_expansion(expansion, &prog)?;
    }

    Ok(())
}

fn verify_expansion(expansion: &Expansion, prog: &Program) -> anyhow::Result<()> {
    // Verification conditions.
    let conditions = Conditions::from_expansion(expansion, prog)?;
    conditions.pretty_print(prog);

    // Type constraints.
    let constraints = type_constraints(&conditions)?;
    // TOOD(mbm): pretty print method for type constraints
    println!("type constraints = [");
    for constraint in &constraints {
        println!("\t{constraint}");
    }
    println!("]");

    // Solve.
    let solver = Solver::new();
    let expr_type = solver.solve(&constraints)?;

    // Dump types.
    // TOOD(mbm): type `Assignment` struct with validation and pretty printer methods
    for (i, expr) in conditions.exprs.iter().enumerate() {
        print!("{i}:\t");
        match expr_type.get(&ExprId(i)) {
            None => print!("false\t-"),
            Some(typ) => print!("{}\t{typ}", typ.is_concrete()),
        }
        println!("\t{expr}");
    }

    Ok(())
}