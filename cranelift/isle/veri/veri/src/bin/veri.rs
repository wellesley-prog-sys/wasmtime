use std::time::Duration;

use anyhow::{format_err, Result};
use clap::{Parser, ValueEnum};
use cranelift_codegen_meta::{generate_isle, isle::get_isle_compilations};
use cranelift_isle_veri::runner::{Runner, SolverBackend};

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

    /// Solver backend to use.
    #[arg(long = "solver", default_value = "cvc5")]
    solver_backend: SolverBackendOption,

    /// Per-query timeout, in seconds.
    #[arg(long, default_value = "10")]
    timeout: u64,

    /// Log directory.
    #[arg(long)]
    log_dir: Option<std::path::PathBuf>,

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
enum SolverBackendOption {
    Z3,
    CVC5,
}

impl From<SolverBackendOption> for SolverBackend {
    fn from(opt: SolverBackendOption) -> Self {
        match opt {
            SolverBackendOption::Z3 => SolverBackend::Z3,
            SolverBackendOption::CVC5 => SolverBackend::CVC5,
        }
    }
}

fn main() -> Result<()> {
    let _ = env_logger::try_init();
    let opts = Opts::parse();

    // Read ISLE inputs.
    let inputs = opts.isle_input_files()?;
    let mut runner = Runner::from_files(&inputs)?;

    // Configure runner.
    runner.include_first_rule_named();

    if let Some(id) = opts.rule {
        runner.target_rule(&id)?;
    }

    for skip_tag in opts.skip_tags {
        runner.skip_tag(skip_tag);
    }

    runner.set_solver_backend(opts.solver_backend.into());
    runner.set_timeout(Duration::from_secs(opts.timeout));
    if let Some(log_dir) = opts.log_dir {
        runner.set_log_dir(log_dir);
    }
    runner.skip_solver(opts.skip_solver);
    runner.debug(opts.debug);

    // Run.
    runner.run()
}
