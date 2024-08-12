use clap::Parser;
use cranelift_codegen_meta::{generate_isle, isle::get_isle_compilations};
use cranelift_isle::sema::Rule;
use cranelift_isle_veri::{
    debug::print_expansion,
    expand::{Expansion, ExpansionsBuilder},
    program::Program,
    solver::{Applicability, Solver, Verification},
    type_inference::{self, type_constraint_system, Assignment},
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
    #[arg(long)]
    rule: Option<String>,

    /// Path to SMT2 replay file.
    #[arg(long, required = true)]
    smt2_replay_path: std::path::PathBuf,

    /// Per-query timeout, in seconds.
    #[arg(long, default_value = "10")]
    timeout: u32,
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

    // Lookup named target rule, if any.
    let target = if let Some(id) = opts.rule {
        Some(
            prog.get_rule_by_identifier(&id)
                .ok_or(anyhow::format_err!("unknown rule {id}"))?,
        )
    } else {
        None
    };

    // Generate expansions.
    // TODO(mbm): don't hardcode the expansion configuration
    let root_term = "lower";
    let mut expansions_builder = ExpansionsBuilder::new(&prog, root_term)?;
    expansions_builder.inline_term(root_term)?;
    expansions_builder.set_maximal_inlining(true);
    expansions_builder.set_max_rules(6);
    expansions_builder.exclude_inline_term("operand_size")?;

    // Process expansions.
    let expansions = expansions_builder.expansions()?;
    println!("expansions = {}", expansions.len());
    for expansion in &expansions {
        if !should_verify(expansion, target, &prog) {
            continue;
        }
        print_expansion(&prog, expansion);
        verify_expansion(expansion, &prog, &opts.smt2_replay_path, opts.timeout)?;
    }

    Ok(())
}

/// Heuristic to select which expansions we attempt verification for.
/// Specifically, verify all expansions where the first rule is named.
fn should_verify(expansion: &Expansion, target: Option<&Rule>, prog: &Program) -> bool {
    // If an explicit target rule is specified, limit to expansions containing it.
    if let Some(target) = target {
        return expansion.rules.contains(&target.id);
    }

    // Accept if first rule is named.
    let rule_id = expansion
        .rules
        .first()
        .expect("expansion should have at least one rule");
    let rule = prog.rule(*rule_id);
    rule.name.is_some()
}

fn verify_expansion(
    expansion: &Expansion,
    prog: &Program,
    replay_path: &std::path::Path,
    timeout_seconds: u32,
) -> anyhow::Result<()> {
    // Verification conditions.
    let conditions = Conditions::from_expansion(expansion, prog)?;
    conditions.pretty_print(prog);

    // Type constraints.
    let system = type_constraint_system(&conditions);
    system.pretty_print();

    // Infer types.
    let type_solver = type_inference::Solver::new();
    let solutions = type_solver.solve(&system);

    for solution in &solutions {
        println!("type solution status = {}", solution.status);
        match solution.status {
            type_inference::Status::Solved => (),
            type_inference::Status::Inapplicable => continue,
            type_inference::Status::Underconstrained => {
                anyhow::bail!("underconstrained type inference")
            }
        }

        verify_expansion_type_instantiation(
            prog,
            &conditions,
            &solution.assignment,
            replay_path,
            timeout_seconds,
        )?;
    }

    Ok(())
}

fn verify_expansion_type_instantiation(
    prog: &Program,
    conditions: &Conditions,
    assignment: &Assignment,
    replay_path: &std::path::Path,
    timeout_seconds: u32,
) -> anyhow::Result<()> {
    // Show type assignment.
    println!("type assignment:");
    assignment.pretty_print(&conditions);

    // Solve.
    println!("solve:");
    let replay_file = std::fs::File::create(replay_path)?;
    let smt = easy_smt::ContextBuilder::new()
        .solver(
            "z3",
            ["-smt2", "-in", &format!("-t:{}", timeout_seconds * 1000)],
        )
        .replay_file(Some(replay_file))
        .build()?;

    let mut solver = Solver::new(smt, &conditions, assignment);
    solver.encode()?;

    let applicability = solver.check_assumptions_feasibility()?;
    println!("applicability = {applicability}");
    match applicability {
        Applicability::Applicable => (),
        Applicability::Inapplicable => anyhow::bail!("inapplicable rule"),
        Applicability::Unknown => anyhow::bail!("could not prove applicability"),
    };

    let verification = solver.check_verification_condition()?;
    println!("verification = {verification}");
    match verification {
        Verification::Failure(model) => {
            println!("model:");
            conditions.print_model(&model, prog)?;
            anyhow::bail!("verification failed");
        }
        Verification::Success | Verification::Unknown => (),
    };

    Ok(())
}
