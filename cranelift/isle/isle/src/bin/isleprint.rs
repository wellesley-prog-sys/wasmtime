use clap::Parser;
use cranelift_isle as isle;
use std::path::PathBuf;

#[derive(Parser)]
struct Opts {
    /// The input ISLE DSL source files.
    #[clap(required = true)]
    inputs: Vec<PathBuf>,

    /// Line length.
    #[clap(long, default_value = "78")]
    width: usize,
}

fn main() -> Result<(), isle::error::Errors> {
    let _ = env_logger::try_init();

    let opts = Opts::parse();

    let lexer = isle::lexer::Lexer::from_files(&opts.inputs)?;
    let defs = isle::parser::parse(lexer)?;

    let mut stdout = std::io::stdout();
    isle::printer::print(&defs, opts.width, &mut stdout)?;

    Ok(())
}
