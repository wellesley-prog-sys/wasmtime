use clap::Parser;
use cranelift_codegen_meta::{generate_isle, isle::get_isle_compilations};
use cranelift_isle::{
    sema::{self, Rule},
    trie_again::RuleSet,
};
use cranelift_isle_veri::{
    debug::print_expansion,
    expand::{Chaining, Expander, Expansion},
    program::Program,
    solver::{Applicability, Solver, Verification},
    type_inference::{self, type_constraint_system, Assignment},
    veri::Conditions,
};
use std::collections::HashMap;

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

    /// Skip solver.
    #[arg(long, env = "ISLE_VERI_SKIP_SOLVER")]
    skip_solver: bool,
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
    let term_rule_sets: HashMap<_, _> = prog.build_trie()?.into_iter().collect();

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
    let chaining = Chaining::new(&prog, &term_rule_sets)?;
    let mut expander = Expander::new(&prog, &term_rule_sets, chaining);
    expander.add_root_term_name("lower")?;
    expander.set_prune_infeasible(true);
    expander.expand();

    // Process expansions.
    let expansions = expander.expansions();
    println!("expansions = {}", expansions.len());
    for expansion in expansions {
        if !should_verify(expansion, target, &prog) {
            continue;
        }
        print_expansion(&prog, expansion);
        verify_expansion(
            expansion,
            &term_rule_sets,
            &prog,
            &opts.smt2_replay_path,
            opts.timeout,
            opts.skip_solver,
        )?;
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
    term_rule_set: &HashMap<sema::TermId, RuleSet>,
    prog: &Program,
    replay_path: &std::path::Path,
    timeout_seconds: u32,
    skip_solver: bool,
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
        // Show type assignment.
        for choice in &solution.choices {
            println!("choice = {}", choice);
        }
        println!("type solution status = {}", solution.status);
        println!("type assignment:");
        solution.assignment.pretty_print(&conditions);

        match solution.status {
            type_inference::Status::Solved => (),
            type_inference::Status::Inapplicable => continue,
            type_inference::Status::Underconstrained => {
                anyhow::bail!("underconstrained type inference")
            }
        }

        // Verify.
        if skip_solver {
            println!("skip solver");
            continue;
        }

        verify_expansion_type_instantiation(
            prog,
            &conditions,
            &term_rule_set,
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
    term_rule_set: &HashMap<sema::TermId, RuleSet>,
    assignment: &Assignment,
    replay_path: &std::path::Path,
    timeout_seconds: u32,
) -> anyhow::Result<()> {
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

    let mut solver = Solver::new(smt, conditions, assignment);
    solver.encode()?;

    let applicability = solver.check_assumptions_feasibility()?;
    println!("applicability = {applicability}");
    match applicability {
        Applicability::Applicable => (),
        Applicability::Inapplicable => return Ok(()),
        Applicability::Unknown => anyhow::bail!("could not prove applicability"),
    };

    let verification = solver.check_verification_condition()?;
    println!("verification = {verification}");
    match verification {
        Verification::Failure(model) => {
            println!("model:");
            conditions.print_model(&model, prog)?;
            // TODO(ashley): Still modifying this
            println!("rule:");
            for sexpr in conditions.testing_print_with_trie(
                &conditions.expansion,
                &term_rule_set,
                &model,
                prog,
                &solver.smt,
            ) {
                println!("{}", solver.smt.display(sexpr));
            }

            for sexpr in conditions.print_rule(&conditions.expansion, &model, prog, &solver.smt) {
                println!("{}", solver.smt.display(sexpr));
            }

            anyhow::bail!("verification failed");
        }
        Verification::Success | Verification::Unknown => (),
    };

    Ok(())
}
