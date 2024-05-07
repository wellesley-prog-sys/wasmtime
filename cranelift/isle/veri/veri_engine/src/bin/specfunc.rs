use crossbeam::queue::SegQueue;
use isla::opts;
use isla_lib::bitvector::{b64::B64, BV};
use isla_lib::error::IslaError;
use isla_lib::executor::{self, LocalFrame, TaskState};
use isla_lib::init::{initialize_architecture, Initialized};
use isla_lib::ir::{AssertionMode, Val};
use isla_lib::memory::Memory;
use isla_lib::simplify::{self, WriteOpts};
use isla_lib::smt::{self, Checkpoint, Event, Solver};
use isla_lib::zencode;
use sha2::{Digest, Sha256};
use std::io::prelude::*;
use std::io::BufWriter;
use std::path::PathBuf;
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    // Command-line options.
    let opts = opts::common_opts();

    // Build ISLA architecture.
    let mut hasher = Sha256::new();
    let (matches, arch) = opts::parse::<B64>(&mut hasher, &opts);
    let opts::CommonOpts {
        num_threads: _,
        mut arch,
        symtab,
        type_info,
        isa_config,
        source_path: _,
    } = opts::parse_with_arch(&mut hasher, &opts, &matches, &arch);
    let use_model_reg_init = true;
    let iarch = initialize_architecture(
        &mut arch,
        symtab,
        type_info,
        &isa_config,
        AssertionMode::Optimistic,
        use_model_reg_init,
    );

    // ISLA trace.
    let paths = trace_execute_function(&iarch)?;

    // Dump.
    for events in paths {
        write_events_to_stdout(&events, &iarch)?;
    }

    Ok(())
}

fn trace_execute_function<'ir, B: BV>(
    iarch: &'ir Initialized<'ir, B>,
) -> anyhow::Result<Vec<Vec<Event<B>>>> {
    let shared_state = &&iarch.shared_state;

    let initial_checkpoint = Checkpoint::new();
    let solver_cfg = smt::Config::new();
    let solver_ctx = smt::Context::new(solver_cfg);
    let mut solver = Solver::from_checkpoint(&solver_ctx, initial_checkpoint);
    let checkpoint = smt::checkpoint(&mut solver);

    // // val execute_aarch64_instrs_integer_conditional_compare_register : forall 'datasize 'm 'n ('sub_op : Bool),
    // //   (0 <= 'n & 'n <= 31 & 0 <= 'm & 'm <= 31 & 'datasize in {32, 64}).
    // //   (bits(4), int('datasize), bits(4), int('m), int('n), bool('sub_op)) -> unit
    // // function execute_aarch64_instrs_integer_conditional_compare_register (condition, datasize, flags__arg, m, n, sub_op) = {

    // let execute_function =
    //     zencode::encode("execute_aarch64_instrs_integer_conditional_compare_register");
    // // condition: bits(4)
    // let condition = Val::Bits(B::new(7, 4));
    // // datasize: int('datasize)
    // let datasize = Val::I64(64);
    // // flags__arg
    // let flags_arg = Val::Bits(B::new(3, 4));
    // // m
    // let rm = Val::I64(3);
    // // n
    // let rn = Val::I64(4);
    // // sub_op
    // let sub_op = Val::Bool(false);

    // val execute_aarch64_instrs_integer_arithmetic_add_sub_carry : forall 'd 'datasize 'm 'n ('setflags : Bool) ('sub_op : Bool),
    //   (0 <= 'n & 'n <= 31 & 0 <= 'm & 'm <= 31 & 'datasize in {32, 64} & 0 <= 'd & 'd <= 31).
    //   (int('d), int('datasize), int('m), int('n), bool('setflags), bool('sub_op)) -> unit
    //
    // function execute_aarch64_instrs_integer_arithmetic_add_sub_carry (d, datasize, m, n, setflags, sub_op) = {

    let execute_function =
        zencode::encode("execute_aarch64_instrs_integer_arithmetic_add_sub_carry");
    // d
    let rd = Val::I64(2);
    // datasize
    let datasize = Val::I64(64);
    // m
    let rm = Val::I64(3);
    // n
    let rn = Val::I64(4);
    // setflags
    let setflags = Val::Bool(true);
    // sub_op
    let sub_op = Val::Bool(false);

    let function_id = shared_state.symtab.lookup(&execute_function);
    let (args, ret_ty, instrs) = shared_state.functions.get(&function_id).unwrap();
    let memory = Memory::new();
    let task_state = TaskState::<B>::new();
    let task = LocalFrame::new(
        function_id,
        args,
        ret_ty,
        Some(&[rd, datasize, rm, rn, setflags, sub_op]),
        instrs,
    )
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

    let mut paths = Vec::new();
    loop {
        match queue.pop() {
            Some(Ok((_, mut events))) => {
                //simplify::hide_initialization(&mut events);
                simplify::remove_extra_register_fields(&mut events);
                simplify::remove_repeated_register_reads(&mut events);
                simplify::remove_unused_register_assumptions(&mut events);
                simplify::remove_unused(&mut events);
                simplify::propagate_forwards_used_once(&mut events);
                simplify::commute_extract(&mut events);
                simplify::eval(&mut events);

                let events: Vec<Event<B>> = events.drain(..).rev().collect();
                paths.push(events);
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

    Ok(paths)
}

/// Write ISLA trace events to stdout.
fn write_events_to_stdout<'ir, B: BV>(
    events: &Vec<Event<B>>,
    iarch: &'ir Initialized<'ir, B>,
) -> anyhow::Result<()> {
    let stdout = std::io::stdout().lock();
    write_events(events, iarch, stdout)
}

fn write_events<'ir, B: BV>(
    events: &Vec<Event<B>>,
    iarch: &'ir Initialized<'ir, B>,
    w: impl Sized + Write,
) -> anyhow::Result<()> {
    let mut handle = BufWriter::with_capacity(5 * usize::pow(2, 20), w);
    let write_opts = WriteOpts::default();
    simplify::write_events_with_opts(&mut handle, &events, &iarch.shared_state, &write_opts)
        .unwrap();
    handle.flush().unwrap();

    Ok(())
}
