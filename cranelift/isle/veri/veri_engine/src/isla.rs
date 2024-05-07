use anyhow::Context as _;
use clap::Parser;
use cranelift_codegen::print_errors::pretty_error;
use cranelift_codegen::Context;
use cranelift_reader::{parse_sets_and_triple, parse_test, ParseOptions};
use crossbeam::queue::SegQueue;
use isla_lib::bitvector::{b129::B129, BV};
use isla_lib::config::ISAConfig;
use isla_lib::error::IslaError;
use isla_lib::executor::{self, LocalFrame, TaskState};
use isla_lib::init::initialize_architecture;
use isla_lib::ir::{AssertionMode, Def, IRTypeInfo, Name, Symtab, Val};
use isla_lib::ir_lexer::new_ir_lexer;
use isla_lib::ir_parser;
use isla_lib::memory::Memory;
use isla_lib::simplify::{self, WriteOpts};
use isla_lib::smt::{self, Checkpoint, Event, Solver};
use sha2::{Digest, Sha256};
use std::fs::File;
use std::io::prelude::*;
use std::io::{self, BufWriter, Read};
use std::path::{Path, PathBuf};
use std::sync::Arc;

#[derive(Parser)]
struct Options {
    /// Configure Cranelift settings.
    #[clap(long = "set")]
    settings: Vec<String>,

    /// Specify the Cranelift target.
    #[clap(long = "target")]
    target: String,

    /// Architecture definition.
    #[clap(long)]
    arch: PathBuf,

    /// ISA config file.
    #[clap(long)]
    isa_config: PathBuf,

    /// Specify an input file to be used. Use '-' for stdin.
    file: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let options = Options::parse();

    // Parse input CLIF test file.
    let buffer = read_to_string(&options.file)?;
    let test_file = parse_test(&buffer, ParseOptions::default())
        .with_context(|| format!("failed to parse {}", options.file.to_string_lossy()))?;

    // Determine ISA settings.
    let parsed = parse_sets_and_triple(&options.settings, &options.target)?;
    let fisa = parsed.as_fisa();
    let isa = fisa.isa.or(test_file.isa_spec.unique_isa());

    let isa = match isa {
        None => anyhow::bail!("compilation requires a target isa"),
        Some(isa) => isa,
    };

    // Parse ISLA Architecture.
    let contents = read_to_string(&options.arch)?;
    let mut symtab = Symtab::new();
    let mut arch = parse_ir::<B129>(&contents, &mut symtab)?;

    // ISLA ISA Config.
    let mut hasher = Sha256::new();
    let type_info = IRTypeInfo::new(&arch);
    let isa_config = match ISAConfig::<B129>::from_file(
        &mut hasher,
        &options.isa_config,
        None,
        &symtab,
        &type_info,
    ) {
        Ok(isa_config) => isa_config,
        Err(msg) => return Err(anyhow::Error::msg(msg)),
    };

    // Compile functions.
    for (func, _) in test_file.functions {
        let mut context = Context::for_function(func);
        let mut mem = vec![];
        let _compiled_code = context
            .compile_and_emit(isa, &mut mem, &mut Default::default())
            .map_err(|err| anyhow::anyhow!("{}", pretty_error(&err.func, err.inner)))?;
        trace_function(
            &mem,
            &mut arch,
            symtab.clone(),
            type_info.clone(),
            &isa_config,
        )?;
    }

    Ok(())
}

fn trace_function<B: BV>(
    mc: &[u8],
    arch: &mut Vec<Def<Name, B>>,
    symtab: Symtab,
    type_info: IRTypeInfo,
    isa_config: &ISAConfig<B>,
) -> anyhow::Result<()> {
    let use_model_reg_init = true;
    let iarch = initialize_architecture(
        arch,
        symtab,
        type_info,
        &isa_config,
        AssertionMode::Optimistic,
        use_model_reg_init,
    );
    let shared_state = &&iarch.shared_state;

    let initial_checkpoint = Checkpoint::new();
    let solver_cfg = smt::Config::new();
    let solver_ctx = smt::Context::new(solver_cfg);
    let mut solver = Solver::from_checkpoint(&solver_ctx, initial_checkpoint);
    let checkpoint = smt::checkpoint(&mut solver);

    let opcode_vals = opcode_vals(mc)?;
    print!("opcode vals = {:?}", opcode_vals);

    let footprint_function = "zisla_footprint";
    let function_id = shared_state.symtab.lookup(&footprint_function);
    let (args, ret_ty, instrs) = shared_state.functions.get(&function_id).unwrap();
    let memory = Memory::new();
    let task_state = TaskState::<B>::new();
    let task = LocalFrame::new(function_id, args, ret_ty, Some(&opcode_vals), instrs)
        .add_lets(&iarch.lets)
        .add_regs(&iarch.regs)
        .set_memory(memory)
        .task_with_checkpoint(0, &task_state, checkpoint);

    let num_threads = 1;
    let queue = Arc::new(SegQueue::new());
    executor::start_multi(
        num_threads,
        None,
        vec![task],
        shared_state,
        queue.clone(),
        &executor::trace_collector,
    );

    loop {
        match queue.pop() {
            Some(Ok((_, mut events))) => {
                simplify::hide_initialization(&mut events);
                simplify::remove_unused(&mut events);
                simplify::propagate_forwards_used_once(&mut events);
                simplify::commute_extract(&mut events);
                simplify::eval(&mut events);

                let events: Vec<Event<B>> = events.drain(..).rev().collect();
                let stdout = std::io::stdout();
                let mut handle = BufWriter::with_capacity(5 * usize::pow(2, 20), stdout.lock());
                let write_opts = WriteOpts::default();
                simplify::write_events_with_opts(&mut handle, &events, &shared_state, &write_opts)
                    .unwrap();
                handle.flush().unwrap()
            }

            // Error during execution
            Some(Err(err)) => {
                let msg = format!("{}", err);
                eprintln!(
                    "{}",
                    err.source_loc().message::<PathBuf>(
                        None,
                        shared_state.symtab.files(),
                        &msg,
                        true,
                        true
                    )
                );
                anyhow::bail!("{}", err);
            }
            // Empty queue
            None => break,
        }
    }

    Ok(())
}

fn opcode_vals<B: BV>(mc: &[u8]) -> anyhow::Result<Vec<Val<B>>> {
    let mut vals = Vec::new();
    for opcode_bytes in mc.chunks(4) {
        let val = Val::Bits(B::from_u32(u32::from_le_bytes(
            opcode_bytes.try_into().unwrap(),
        )));
        vals.push(val);
    }
    Ok(vals)
}

fn parse_ir<'a, 'input, B: BV>(
    contents: &'input str,
    symtab: &'a mut Symtab<'input>,
) -> anyhow::Result<Vec<Def<Name, B>>> {
    match ir_parser::IrParser::new().parse(symtab, new_ir_lexer(&contents)) {
        Ok(ir) => Ok(ir),
        Err(_) => Err(anyhow::Error::msg("bad")),
    }
}

/// Read an entire file into a string.
fn read_to_string<P: AsRef<Path>>(path: P) -> anyhow::Result<String> {
    let mut buffer = String::new();
    let path = path.as_ref();
    if path == Path::new("-") {
        let stdin = io::stdin();
        let mut stdin = stdin.lock();
        stdin
            .read_to_string(&mut buffer)
            .context("failed to read stdin to string")?;
    } else {
        let mut file = File::open(path)?;
        file.read_to_string(&mut buffer)
            .with_context(|| format!("failed to read {} to string", path.display()))?;
    }
    Ok(buffer)
}
