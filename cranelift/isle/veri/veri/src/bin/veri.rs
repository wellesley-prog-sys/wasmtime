use std::time::Duration;

use anyhow::{format_err, Result};
use clap::{ArgAction, Parser};
use cranelift_codegen_meta::{generate_isle, isle::get_isle_compilations};
use cranelift_isle_veri::runner::{Filter, Runner, SolverBackend, SolverRule};

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

    /// Filter expansions.
    #[arg(long = "filter", value_name = "FILTER")]
    filters: Vec<Filter>,

    /// Don't skip expansions tagged TODO.
    #[arg(long = "no-skip-todo", action = ArgAction::SetFalse)]
    skip_todo: bool,

    /// Solver backend to use.
    #[arg(long = "solver", default_value = "cvc5", env = "ISLE_VERI_SOLVER")]
    solver_backend: SolverBackend,

    /// Solver selection rule of the form `<solver>=<predicate>`. Earlier rules take precedence.
    #[arg(long = "solver-rule")]
    solver_rules: Vec<SolverRule>,

    /// Ignore explicit solver selection tags `solver_<solver>`.
    #[arg(long)]
    ignore_solver_tags: bool,

    /// Per-query timeout, in seconds.
    #[arg(long, default_value = "30", env = "ISLE_VERI_TIMEOUT")]
    timeout: u64,

    /// Number of threads to use.
    #[arg(long, default_value = "1")]
    num_threads: usize,

    /// Log directory.
    #[arg(long)]
    log_dir: Option<std::path::PathBuf>,

    /// Write results to files under log directory. (Use 0 to select automatically.)
    #[arg(long)]
    results_to_log_dir: bool,

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

fn main() -> Result<()> {
    env_logger::builder().format_target(false).init();
    let opts = Opts::parse();

    // Setup thread pool.
    rayon::ThreadPoolBuilder::new()
        .num_threads(opts.num_threads)
        .build_global()?;
    log::info!("num theads: {}", rayon::current_num_threads());

    // Read ISLE inputs.
    let inputs = opts.isle_input_files()?;
    let root_term = if opts.name != "opt" {
        "lower"
    } else {
        "simplify"
    };
    let mut runner = Runner::from_files(&inputs, root_term)?;

    // Configure runner.
    if !opts.filters.is_empty() {
        runner.filters(&opts.filters);
    } else {
        runner.include_first_rule_named();
    }
    if opts.skip_todo {
        runner.skip_tag("TODO");
    }

    runner.set_default_solver_backend(opts.solver_backend.into());
    if !opts.ignore_solver_tags {
        runner.add_solver_tag_rules();
    }
    for solver_rule in opts.solver_rules {
        runner.add_solver_rule(solver_rule);
    }

    runner.set_timeout(Duration::from_secs(opts.timeout));
    if let Some(log_dir) = opts.log_dir {
        runner.set_log_dir(log_dir);
    }
    runner.set_results_to_log_dir(opts.results_to_log_dir);
    runner.skip_solver(opts.skip_solver);
    runner.debug(opts.debug);

    // Run.
    runner.run()
}
