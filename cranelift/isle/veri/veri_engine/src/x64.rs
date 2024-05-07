use clap::Parser;
use cranelift_codegen::isa::x64::{
    self,
    inst::{args::*, *},
};
use cranelift_codegen::{settings, MachBuffer, MachInstEmit, Reg, Writable};
use crossbeam::queue::SegQueue;
use isla_lib::init::{initialize_architecture, Initialized};
use isla_lib::ir::{AssertionMode, Def, IRTypeInfo, Name, Symtab, Ty, Val};
use isla_lib::ir_lexer::new_ir_lexer;
use isla_lib::ir_parser;
use isla_lib::log;
use isla_lib::memory::{Memory, SmtKind};
use isla_lib::simplify::{self, WriteOpts};
use isla_lib::smt::smtlib::{self, bits64};
use isla_lib::smt::{self, Checkpoint, Event, ReadOpts, Solver, Sym};
use isla_lib::source_loc::SourceLoc;
use isla_lib::{
    bitvector::{b64::B64, BV},
    zencode,
};
use isla_lib::{config::ISAConfig, executor::unfreeze_frame};
use isla_lib::{
    error::{ExecError, IslaError},
    memory::Address,
};
use isla_lib::{
    executor::{self, freeze_frame, LocalFrame, TaskState},
    ir::linearize,
};
use sha2::{Digest, Sha256};
use std::io::prelude::*;
use std::io::BufWriter;
use std::path::PathBuf;
use std::sync::Arc;

#[derive(Parser)]
struct Options {
    /// Architecture definition.
    #[clap(long)]
    arch: PathBuf,

    /// ISA config file.
    #[clap(long)]
    isa_config: PathBuf,

    /// Trace output directory.
    #[clap(long, default_value = ".")]
    output_dir: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let options = Options::parse();
    log::set_flags(log::VERBOSE);

    // Parse ISLA Architecture.
    let contents = std::fs::read_to_string(&options.arch)?;
    let mut symtab = Symtab::new();
    let mut arch = parse_ir::<B64>(&contents, &mut symtab)?;

    // ISLA ISA Config.
    let mut hasher = Sha256::new();
    let type_info = IRTypeInfo::new(&arch);
    let isa_config = match ISAConfig::<B64>::from_file(
        &mut hasher,
        &options.isa_config,
        None,
        &symtab,
        &type_info,
    ) {
        Ok(isa_config) => isa_config,
        Err(msg) => anyhow::bail!(msg),
    };

    let linearize_functions = vec![
        // See: https://github.com/rems-project/isla-testgen/blob/a9759247bfdff9c9c39d95a4cfd85318e5bf50fe/c86-command
        //"fdiv_int",
        "bool_to_bits",
        "bits_to_bool",
        //"bit_to_bool",
        //"isEven",
        "signed_byte_p_int",
        "zf_spec",
        //"b_xor",
        //"logand_int",
        //"not_bit",
        //"floor2",
    ];
    for name in linearize_functions {
        linearize_function(&mut arch, name, &mut symtab)?;
    }

    let use_model_reg_init = true;
    let iarch = initialize_architecture(
        &mut arch,
        symtab,
        type_info,
        &isa_config,
        AssertionMode::Optimistic,
        use_model_reg_init,
    );

    // Assemble x64 instruction.
    let rdx = regs::rdx();
    let r13 = regs::r13();
    let w_rdx = Writable::<Reg>::from_reg(rdx);
    let inst = Inst::AluRmiR {
        size: OperandSize::Size64,
        op: AluRmiROpcode::Add,
        src1: Gpr::new(rdx).unwrap(),
        src2: GprMemImm::new(RegMemImm::reg(r13)).unwrap(),
        dst: WritableGpr::from_writable_reg(w_rdx).unwrap(),
    };

    let machine_code = assemble(&inst);
    println!("machine code = {:02x?}", machine_code);

    // ISLA trace.
    let paths = trace_opcode(&machine_code, &iarch)?;

    // Dump.
    for (i, events) in paths.iter().enumerate() {
        let filename = format!("trace{i:04}.out");
        let path = options.output_dir.join(filename);
        log!(log::VERBOSE, format!("write trace to {}", path.display()));

        let file = std::fs::File::create(path)?;
        write_events(&events, &iarch, file)?;
    }

    log!(
        log::VERBOSE,
        format!("generated {} trace paths", paths.len())
    );

    Ok(())
}

/// Assemble x64 instruction.
fn assemble(inst: &Inst) -> Vec<u8> {
    let flags = settings::Flags::new(settings::builder());
    let isa_flag_builder = x64::settings::builder();
    let isa_flags = x64::settings::Flags::new(&flags, &isa_flag_builder);
    let emit_info = EmitInfo::new(flags, isa_flags);
    let mut buffer = MachBuffer::new();
    inst.emit(&[], &mut buffer, &emit_info, &mut Default::default());
    let buffer = buffer.finish(&Default::default(), &mut Default::default());
    return buffer.data().to_vec();
}

fn trace_opcode<'ir, B: BV>(
    opcode: &Vec<u8>,
    iarch: &'ir Initialized<'ir, B>,
) -> anyhow::Result<Vec<Vec<Event<B>>>> {
    let shared_state = &&iarch.shared_state;
    let symtab = &shared_state.symtab;

    let initial_checkpoint = Checkpoint::new();
    let solver_cfg = smt::Config::new();
    let solver_ctx = smt::Context::new(solver_cfg);
    let mut solver = Solver::from_checkpoint(&solver_ctx, initial_checkpoint);
    let memory = Memory::new();

    // Initialization -----------------------------------------------------------
    // - run initialization function to set processor in 64-bit mode

    let init_function = zencode::encode("initialise_64_bit_mode");
    let function_id = shared_state.symtab.lookup(&init_function);
    let (args, ret_ty, instrs) = shared_state.functions.get(&function_id).unwrap();

    let task_state = TaskState::<B>::new();
    let checkpoint = smt::checkpoint(&mut solver);
    let task = LocalFrame::new(function_id, args, ret_ty, None, instrs)
        .add_lets(&iarch.lets)
        .add_regs(&iarch.regs)
        .set_memory(memory)
        .task_with_checkpoint(0, &task_state, checkpoint);

    let queue = Arc::new(SegQueue::new());
    executor::start_single(
        task,
        &shared_state,
        &queue,
        &move |_tid, _task_id, result, _shared_state, mut solver, queue| match result {
            Ok((_, frame)) => {
                queue.push((freeze_frame(&frame), smt::checkpoint(&mut solver)));
            }
            Err(err) => panic!("Initialisation failed: {:?}", err),
        },
    );
    assert_eq!(queue.len(), 1);
    let (frame, checkpoint) = queue.pop().expect("pop failed");

    let mut solver = Solver::from_checkpoint(&solver_ctx, checkpoint);

    // Initialize registers -----------------------------------------------------
    // - set symbolic values for all general-purpose registers

    let mut local_frame = executor::unfreeze_frame(&frame);
    for (n, ty) in &shared_state.registers {
        // Only handle general-purpose registers.
        if let Ty::Bits(bits) = ty {
            let var = solver.fresh();
            solver.add(smtlib::Def::DeclareConst(var, smtlib::Ty::BitVec(*bits)));
            let val = Val::Symbolic(var);
            local_frame.regs_mut().assign(*n, val, shared_state);
        }
    }

    let frame = freeze_frame(&local_frame);

    // Setup memory -------------------------------------------------------------

    let mut local_frame = unfreeze_frame(&frame);

    const INIT_PC: Address = 0x401000;
    local_frame
        .memory_mut()
        .add_symbolic_code_region(INIT_PC..INIT_PC + 0x10000);

    let memory_var = solver.fresh();
    solver.add(smtlib::Def::DeclareConst(
        memory_var,
        smtlib::Ty::Array(
            Box::new(smtlib::Ty::BitVec(64)),
            Box::new(smtlib::Ty::BitVec(8)),
        ),
    ));

    let memory_client_info: Box<dyn isla_lib::memory::MemoryCallbacks<B>> =
        Box::new(SeqMemory { memory_var });
    local_frame.memory_mut().set_client_info(memory_client_info);

    let frame = freeze_frame(&local_frame);

    // Set initial program counter ----------------------------------------------

    let local_frame = unfreeze_frame(&frame);

    let pc_name = zencode::encode("rip");
    let pc_id = symtab.lookup(&pc_name);
    let pc = local_frame.regs().get_last_if_initialized(pc_id).unwrap();

    solver.add(smtlib::Def::Assert(smtlib::Exp::Eq(
        Box::new(smt_value(pc).unwrap()),
        Box::new(smtlib::Exp::Bits64(B64::from_u64(INIT_PC))),
    )));

    let frame = freeze_frame(&local_frame);

    // Setup opcode -------------------------------------------------------------

    let local_frame = unfreeze_frame(&frame);

    let read_val = local_frame
        .memory()
        .read(
            Val::Unit, /* read_kind */
            pc.clone(),
            Val::I128(opcode.len() as i128),
            &mut solver,
            false,
            ReadOpts::ifetch(),
        )
        .unwrap();

    let opcode_var = solver.fresh();
    solver.add(smtlib::Def::DeclareConst(
        opcode_var,
        smtlib::Ty::BitVec(8 * opcode.len() as u32),
    ));
    solver.add(smtlib::Def::Assert(smtlib::Exp::Eq(
        Box::new(smtlib::Exp::Var(opcode_var)),
        Box::new(smt_bytes(opcode)),
    )));

    let read_exp = smt_value(&read_val).unwrap();
    solver.add(smtlib::Def::Assert(smtlib::Exp::Eq(
        Box::new(smtlib::Exp::Var(opcode_var)),
        Box::new(read_exp),
    )));

    let frame = freeze_frame(&local_frame);

    // Debug dump ---------------------------------------------------------------

    assert_eq!(solver.check_sat(), smt::SmtResult::Sat);
    let checkpoint = smt::checkpoint(&mut solver);
    dump_checkpoint(&checkpoint, iarch)?;

    // Execute fetch and decode -------------------------------------------------

    let local_frame = unfreeze_frame(&frame);

    local_frame.memory().log();

    let run_instruction_function = zencode::encode("x86_fetch_decode_execute");
    let function_id = shared_state.symtab.lookup(&run_instruction_function);
    let (args, ret_ty, instrs) = shared_state.functions.get(&function_id).unwrap();

    let task_state = TaskState::<B>::new();
    let task = local_frame
        .new_call(function_id, args, ret_ty, None, instrs)
        .task_with_checkpoint(1, &task_state, checkpoint);

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
                // simplify::hide_initialization(&mut events);
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

#[derive(Debug, Clone)]
struct SeqMemory {
    memory_var: Sym,
}

impl<B: BV> isla_lib::memory::MemoryCallbacks<B> for SeqMemory {
    fn symbolic_read(
        &self,
        regions: &[isla_lib::memory::Region<B>],
        solver: &mut Solver<B>,
        value: &Val<B>,
        _read_kind: &Val<B>,
        address: &Val<B>,
        bytes: u32,
        _tag: &Option<Val<B>>,
        opts: &ReadOpts,
    ) {
        use isla_lib::smt::smtlib::{Def, Exp};

        let read_exp = smt_value(value)
            .unwrap_or_else(|err| panic!("Bad memory read value {:?}: {}", value, err));
        let addr_exp = smt_value(address)
            .unwrap_or_else(|err| panic!("Bad read address value {:?}: {}", address, err));
        let read_prop = Exp::Eq(
            Box::new(read_exp.clone()),
            Box::new(smt_read_exp(self.memory_var, &addr_exp, bytes as u64)),
        );
        let kind = if opts.is_ifetch {
            SmtKind::ReadInstr
        } else if opts.is_exclusive {
            // We produce a dummy read so that failed store exclusives still get address
            // constraints, but the memory must be writable.
            SmtKind::WriteData
        } else {
            SmtKind::ReadData
        };
        let address_constraint =
            isla_lib::memory::smt_address_constraint(regions, &addr_exp, bytes, kind, solver, None);

        let full_constraint = Exp::And(Box::new(address_constraint), Box::new(read_prop));

        solver.add(Def::Assert(full_constraint));
    }

    fn symbolic_write(
        &mut self,
        regions: &[isla_lib::memory::Region<B>],
        solver: &mut Solver<B>,
        _value: Sym,
        _read_kind: &Val<B>,
        address: &Val<B>,
        data: &Val<B>,
        bytes: u32,
        _tag: &Option<Val<B>>,
        _opts: &smt::WriteOpts,
    ) {
        use isla_lib::smt::smtlib::{Def, Exp};

        let data_exp = smt_value(data)
            .unwrap_or_else(|err| panic!("Bad memory write value {:?}: {}", data, err));
        let addr_exp = smt_value(address)
            .unwrap_or_else(|err| panic!("Bad write address value {:?}: {}", address, err));
        // TODO: endianness?
        let mut mem_exp = Exp::Store(
            Box::new(Exp::Var(self.memory_var)),
            Box::new(addr_exp.clone()),
            Box::new(Exp::Extract(7, 0, Box::new(data_exp.clone()))),
        );
        for i in 1..bytes {
            mem_exp = Exp::Store(
                Box::new(mem_exp),
                Box::new(Exp::Bvadd(
                    Box::new(addr_exp.clone()),
                    Box::new(bits64(i as u64, 64)),
                )),
                Box::new(Exp::Extract(i * 8 + 7, i * 8, Box::new(data_exp.clone()))),
            )
        }
        self.memory_var = solver.fresh();
        solver.add(Def::DefineConst(self.memory_var, mem_exp));
        let kind = SmtKind::WriteData;
        let address_constraint =
            isla_lib::memory::smt_address_constraint(regions, &addr_exp, bytes, kind, solver, None);
        solver.add(Def::Assert(address_constraint));
    }

    fn symbolic_write_tag(
        &mut self,
        _regions: &[isla_lib::memory::Region<B>],
        _solver: &mut Solver<B>,
        _value: Sym,
        _write_kind: &Val<B>,
        _address: &Val<B>,
        _tag: &Val<B>,
    ) {
    }
}

fn smt_value<B: BV>(v: &Val<B>) -> Result<smtlib::Exp<Sym>, ExecError> {
    isla_lib::primop_util::smt_value(v, SourceLoc::unknown())
}

fn smt_read_exp(memory: Sym, addr_exp: &smtlib::Exp<Sym>, bytes: u64) -> smtlib::Exp<Sym> {
    use smtlib::Exp;
    // TODO: endianness?
    let mut mem_exp = Exp::Select(Box::new(Exp::Var(memory)), Box::new(addr_exp.clone()));
    for i in 1..bytes {
        mem_exp = Exp::Concat(
            Box::new(Exp::Select(
                Box::new(Exp::Var(memory)),
                Box::new(Exp::Bvadd(
                    Box::new(addr_exp.clone()),
                    Box::new(bits64(i as u64, 64)),
                )),
            )),
            Box::new(mem_exp),
        )
    }
    mem_exp
}

fn smt_bytes<V>(bytes: &Vec<u8>) -> smtlib::Exp<V> {
    let mut bits = Vec::with_capacity(bytes.len() * 8);
    for byte in bytes {
        for i in 0..8 {
            bits.push((byte >> i) & 1 == 1);
        }
    }
    smtlib::Exp::Bits(bits)
}

fn dump_checkpoint<'ir, B: BV>(
    checkpoint: &Checkpoint<B>,
    iarch: &'ir Initialized<'ir, B>,
) -> anyhow::Result<()> {
    if let Some(trace) = checkpoint.trace() {
        let events: Vec<Event<B>> = trace.to_vec().into_iter().cloned().collect();
        write_events_to_stdout(&events, iarch)?;
    }
    Ok(())
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

/// Parse Jib IR.
fn parse_ir<'a, 'input, B: BV>(
    contents: &'input str,
    symtab: &'a mut Symtab<'input>,
) -> anyhow::Result<Vec<Def<Name, B>>> {
    match ir_parser::IrParser::new().parse(symtab, new_ir_lexer(&contents)) {
        Ok(ir) => Ok(ir),
        Err(_) => Err(anyhow::Error::msg("bad")),
    }
}

fn linearize_function<'ir, B: BV>(
    arch: &mut Vec<Def<Name, B>>,
    name: &str,
    symtab: &mut Symtab<'ir>,
) -> anyhow::Result<()> {
    log!(log::VERBOSE, format!("linearize function {}", name));

    // Lookup function name.
    let target = match symtab.get(&zencode::encode(name)) {
        Some(t) => t,
        None => anyhow::bail!("function {} could not be found", name),
    };

    // Find signature.
    let (_args, ret) = lookup_signature(arch, target)?;

    // Rewrite.
    for def in arch.iter_mut() {
        if let Def::Fn(f, _, body) = def {
            if *f == target {
                let rewritten = linearize::linearize(body.to_vec(), &ret, symtab);
                *body = rewritten;
            }
        }
    }

    Ok(())
}

fn lookup_signature<B: BV>(
    arch: &Vec<Def<Name, B>>,
    target: Name,
) -> anyhow::Result<(Vec<Ty<Name>>, Ty<Name>)> {
    for def in arch {
        match def {
            Def::Val(f, args, ret) if *f == target => return Ok((args.clone(), ret.clone())),
            _ => (),
        }
    }
    anyhow::bail!("could not find type signature")
}
