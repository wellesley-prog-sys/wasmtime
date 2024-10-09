use anyhow::{bail, format_err, Result};
use clap::{Parser, ValueEnum};
use cranelift_codegen_meta::{generate_isle, isle::get_isle_compilations};
use cranelift_isle::sema::Rule;
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

    /// Skip expansions containing terms with this tag.
    #[arg(long = "skip-tag", value_name = "TAG")]
    skip_tags: Vec<String>,

    /// Path to SMT2 replay file.
    #[arg(long, required = true)]
    smt2_replay_path: std::path::PathBuf,

    /// Solver backend to use.
    #[arg(long = "solver", default_value = "cvc5")]
    solver_backend: SolverBackend,

    /// Per-query timeout, in seconds.
    #[arg(long, default_value = "10")]
    timeout: u32,

    /// Skip solver.
    #[arg(long, env = "ISLE_VERI_SKIP_SOLVER")]
    skip_solver: bool,

    /// Dump debug output.
    #[arg(long)]
    debug: bool,
}

impl Opts {
    fn isle_input_files(&self) -> Result<Vec<std::path::PathBuf>> {
        // Generate ISLE files.
        let gen_dir = &self.work_dir;
        generate_isle(gen_dir)?;

        // Lookup ISLE compilations.
        let compilations = get_isle_compilations(&self.codegen_crate_dir, gen_dir);

        // Return inputs from the matching compilation, if any.
        Ok(compilations
            .lookup(&self.name)
            .ok_or(format_err!("unknown ISLE compilation: {}", self.name))?
            .paths()?)
    }
}

#[derive(ValueEnum, Clone)]
enum SolverBackend {
    Z3,
    CVC5,
}

impl SolverBackend {
    fn prog(&self) -> &str {
        match self {
            SolverBackend::Z3 => "z3",
            SolverBackend::CVC5 => "cvc5",
        }
    }

    fn args(&self, timeout_seconds: u32) -> Vec<String> {
        match self {
            SolverBackend::Z3 => vec![
                "-smt2".to_string(),
                "-in".to_string(),
                format!("-t:{}", timeout_seconds * 1000),
            ],
            SolverBackend::CVC5 => vec![
                "--incremental".to_string(),
                "--print-success".to_string(),
                format!("--tlimit-per={ms}", ms = timeout_seconds * 1000),
                "-".to_string(),
            ],
        }
    }
}

fn main() -> Result<()> {
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
                .ok_or(format_err!("unknown rule {id}"))?,
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
    for (i, expansion) in expansions.iter().enumerate() {
        if !should_verify(expansion, target, &opts.skip_tags, &prog) {
            continue;
        }

        // Report.
        println!("#{i}\t{}", expansion_description(expansion, &prog));
        if opts.debug {
            print_expansion(&prog, expansion);
        }

        verify_expansion(
            expansion,
            &prog,
            &opts.solver_backend,
            &opts.smt2_replay_path,
            opts.timeout,
            opts.skip_solver,
            opts.debug,
        )?;
    }

    Ok(())
}

/// Heuristic to select which expansions we attempt verification for.
fn should_verify(
    expansion: &Expansion,
    target: Option<&Rule>,
    skip_tags: &[String],
    prog: &Program,
) -> bool {
    verify_include(expansion, target, prog) && !verify_exclude(expansion, skip_tags, prog)
}

/// Is the expansion in the set of expansions to verify?
fn verify_include(expansion: &Expansion, target: Option<&Rule>, prog: &Program) -> bool {
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

/// Is the expansion excluded from verification?
fn verify_exclude(expansion: &Expansion, skip_tags: &[String], prog: &Program) -> bool {
    // Skip if the expansion involves skipped tags.
    let tags = expansion.term_tags(prog);
    for skip_tag in skip_tags {
        if tags.contains(skip_tag) {
            log::info!("skip expansion with tag: {}", skip_tag);
            return true;
        }
    }
    return false;
}

fn expansion_description(expansion: &Expansion, prog: &Program) -> String {
    let rule_id = expansion
        .rules
        .first()
        .expect("expansion should have at least one rule");
    let rule = prog.rule(*rule_id);
    rule.identifier(&prog.tyenv, &prog.files)
}

fn verify_expansion(
    expansion: &Expansion,
    prog: &Program,
    solver_backend: &SolverBackend,
    replay_path: &std::path::Path,
    timeout_seconds: u32,
    skip_solver: bool,
    debug: bool,
) -> Result<()> {
    // Verification conditions.
    let conditions = Conditions::from_expansion(expansion, prog)?;
    if debug {
        conditions.pretty_print(prog);
    }

    // Type constraints.
    let system = type_constraint_system(&conditions);
    if debug {
        system.pretty_print();
    }

    // Infer types.
    let type_solver = type_inference::Solver::new();
    let solutions = type_solver.solve(&system);

    for solution in &solutions {
        // Show type assignment.
        for choice in &solution.choices {
            println!("\tchoice = {}", choice);
        }
        println!("\t\ttype solution status = {}", solution.status);
        if debug {
            println!("type assignment:");
            solution.assignment.pretty_print(&conditions);
        }

        match solution.status {
            type_inference::Status::Solved => (),
            type_inference::Status::Inapplicable => continue,
            type_inference::Status::Underconstrained => {
                bail!("underconstrained type inference")
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
            &solution.assignment,
            solver_backend,
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
    solver_backend: &SolverBackend,
    replay_path: &std::path::Path,
    timeout_seconds: u32,
) -> Result<()> {
    // Solve.
    let replay_file = std::fs::File::create(replay_path)?;
    let binary = solver_backend.prog();
    let args = solver_backend.args(timeout_seconds);
    let smt = easy_smt::ContextBuilder::new()
        .solver(binary, &args)
        .replay_file(Some(replay_file))
        .build()?;

    let mut solver = Solver::new(smt, conditions, assignment)?;
    solver.encode()?;

    let applicability = solver.check_assumptions_feasibility()?;
    println!("\t\tapplicability = {applicability}");
    match applicability {
        Applicability::Applicable => (),
        Applicability::Inapplicable => return Ok(()),
        Applicability::Unknown => bail!("could not prove applicability"),
    };

    let verification = solver.check_verification_condition()?;
    println!("\t\tverification = {verification}");
    match verification {
        Verification::Failure(model) => {
            println!("model:");
            conditions.print_model(&model, prog)?;
            bail!("verification failed");
        }
        Verification::Success | Verification::Unknown => (),
    };

    Ok(())
}
