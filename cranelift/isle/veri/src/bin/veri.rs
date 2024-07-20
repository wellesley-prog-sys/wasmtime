use clap::Parser;
use cranelift_codegen_meta::{generate_isle, isle::get_isle_compilations};
use cranelift_isle_veri::{
    debug::print_expansion,
    expand::{Expansion, ExpansionsBuilder},
    program::Program,
    solver::Solver,
    type_inference::{self, type_constraints},
    veri::Conditions,
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

    /// Path to SMT2 replay file.
    #[arg(long, required = true)]
    smt2_replay_path: std::path::PathBuf,
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
    expansions_builder.set_max_rules(6);
    expansions_builder.exclude_inline_term("operand_size")?;

    // Process expansions.
    let expansions = expansions_builder.expansions()?;
    println!("expansions = {}", expansions.len());
    for expansion in &expansions {
        if !expansion.rules.contains(&rule.id) {
            continue;
        }
        print_expansion(&prog, expansion);
        verify_expansion(expansion, &prog, &opts.smt2_replay_path)?;
    }

    Ok(())
}

fn verify_expansion(
    expansion: &Expansion,
    prog: &Program,
    replay_path: &std::path::Path,
) -> anyhow::Result<()> {
    // Verification conditions.
    let conditions = Conditions::from_expansion(expansion, prog)?;
    conditions.pretty_print(prog);

    // Type constraints.
    let constraints = type_constraints(&conditions);
    // TOOD(mbm): pretty print method for type constraints
    println!("type constraints = [");
    for constraint in &constraints {
        println!("\t{constraint}");
    }
    println!("]");

    // Infer types.
    println!("type assignment:");
    let type_solver = type_inference::Solver::new();
    let assignment = type_solver.solve(&constraints)?;
    assignment.pretty_print(&conditions);

    // Solve.
    println!("solve:");
    let replay_file = std::fs::File::create(replay_path)?;
    let smt = easy_smt::ContextBuilder::new()
        .solver("z3", ["-smt2", "-in"])
        .replay_file(Some(replay_file))
        .build()?;

    let mut solver = Solver::new(smt, &conditions, &assignment);
    solver.encode()?;
    let applicable = solver.check_assumptions_feasibility()?;
    println!("applicable = {applicable}");
    let verified = solver.check_verification_condition()?;
    println!("verified = {verified}");

    Ok(())
}
