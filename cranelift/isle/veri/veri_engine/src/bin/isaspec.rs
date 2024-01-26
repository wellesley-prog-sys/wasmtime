use cranelift_codegen::isa::aarch64::inst::*;
use cranelift_codegen::settings;
use cranelift_codegen::AllocationConsumer;
use cranelift_codegen::MachBuffer;
use cranelift_codegen::MachInstEmit;
use cranelift_isle::ast::{Ident, Spec, SpecExpr, SpecOp};
use cranelift_isle::lexer::Pos;
use cranelift_isle::printer;
use crossbeam::queue::SegQueue;
use isla::opts;
use isla_lib::bitvector::{b64::B64, BV};
use isla_lib::error::IslaError;
use isla_lib::executor::{self, LocalFrame, TaskState};
use isla_lib::init::{initialize_architecture, Initialized};
use isla_lib::ir::Name;
use isla_lib::ir::{AssertionMode, Val};
use isla_lib::memory::Memory;
use isla_lib::simplify::{self, WriteOpts};
use isla_lib::smt::smtlib;
use isla_lib::smt::{self, Checkpoint, Event, Solver, Sym};
use isla_lib::zencode;
use itertools::Itertools;
use sha2::{Digest, Sha256};
use std::collections::{HashMap, HashSet};
use std::io::prelude::*;
use std::io::BufWriter;
use std::path::PathBuf;
use std::sync::Arc;

fn main() -> anyhow::Result<()> {
    // Command-line options.
    let mut opts = opts::common_opts();
    opts.optflag("", "filter", "filter relevant events from the trace");

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

    // Spec configuration.
    let cfg = define();

    // Generate spec.
    let mut spec_converter = SpecConverter::new(cfg, &iarch);
    let spec = spec_converter.generate()?;
    printer::dump(&spec)?;

    // // Assemble.
    // let opcodes = opcodes(&inst);
    // assert_eq!(opcodes.len(), 1);
    // let opcode = opcodes[0];

    // // Show assembly.
    // let asm = inst.print_with_state(&mut EmitState::default(), &mut AllocationConsumer::new(&[]));

    // println!("--------------------------------------------------");
    // println!("inst = {inst:?}");
    // println!("opcode = {opcode:08x}");
    // println!("asm = {asm}");
    // println!("config = {cfg:?}");
    // println!("");

    Ok(())
}

/// Define specificiations to generate.
fn define() -> SpecConfig {
    SpecConfig {
        // Spec signature.
        term: "MInst.AluRRR".to_string(),
        args: ["alu_op", "size", "rd", "rn", "rm"]
            .map(String::from)
            .to_vec(),

        cases: vec![InstConfig {
            inst: Inst::AluRRR {
                alu_op: ALUOp::Add,
                size: OperandSize::Size64,
                rd: writable_xreg(4),
                rn: xreg(5),
                rm: xreg(6),
            },

            // Requires.
            require: vec![
                spec_eq(
                    spec_var("alu_op".to_string()),
                    spec_enum("ALUOp".to_string(), "Add".to_string()),
                ),
                spec_eq(
                    spec_var("size".to_string()),
                    spec_enum("OperandSize".to_string(), "Size64".to_string()),
                ),
            ],

            // Register read/write bindings.
            reg_read: HashMap::from([
                ("R5".to_string(), "rn".to_string()),
                ("R6".to_string(), "rm".to_string()),
            ]),
            reg_write: HashMap::from([("R4".to_string(), "rd".to_string())]),
        }],
    }
}

// struct SpecBuilder<'ir, B: BV> {
//     term: String,
//     params: Vec<String>,
//     inst: Inst,
//     iarch: &'ir Initialized<'ir, B>,
// }
//
// impl<'ir, B: BV> SpecBuilder<'ir, B> {
//     fn build(&self) -> anyhow::Result<Spec> {
//         // Assemble instruction.
//         let opcodes = opcodes(&self.inst);
//         assert_eq!(opcodes.len(), 1);
//         let opcode = opcodes[0];
//
//         // ISLA trace.
//         let paths = trace_opcode(opcode, &self.iarch)?;
//
//         // TODO(mbm): handle multiple paths
//         assert_eq!(paths.len(), 1);
//         let events = &paths[0];
//
//         // Filter.
//         let events = tree_shake(events);
//
//         // Generate spec.
//         trace_to_spec(&events)
//     }
// }

fn assemble(inst: &Inst) -> Vec<u8> {
    let flags = settings::Flags::new(settings::builder());
    let emit_info = EmitInfo::new(flags);
    let mut buffer = MachBuffer::new();
    inst.emit(&[], &mut buffer, &emit_info, &mut Default::default());
    let buffer = buffer.finish(&Default::default(), &mut Default::default());
    return buffer.data().to_vec();
}

fn opcodes(inst: &Inst) -> Vec<u32> {
    let machine_code = assemble(&inst);
    let mut opcodes = Vec::new();
    for opcode_bytes in machine_code.chunks(4) {
        assert_eq!(opcode_bytes.len(), 4);
        opcodes.push(u32::from_le_bytes(opcode_bytes.try_into().unwrap()));
    }
    opcodes
}

fn trace_opcode<'ir, B: BV>(
    opcode: u32,
    iarch: &'ir Initialized<'ir, B>,
) -> anyhow::Result<Vec<Vec<Event<B>>>> {
    let shared_state = &&iarch.shared_state;

    let initial_checkpoint = Checkpoint::new();
    let solver_cfg = smt::Config::new();
    let solver_ctx = smt::Context::new(solver_cfg);
    let mut solver = Solver::from_checkpoint(&solver_ctx, initial_checkpoint);
    let checkpoint = smt::checkpoint(&mut solver);

    let opcode_val = Val::Bits(B::from_u32(opcode));

    let footprint_function = zencode::encode("isla_footprint_no_init");
    let function_id = shared_state.symtab.lookup(&footprint_function);
    let (args, ret_ty, instrs) = shared_state.functions.get(&function_id).unwrap();
    let memory = Memory::new();
    let task_state = TaskState::<B>::new();
    let task = LocalFrame::new(function_id, args, ret_ty, Some(&[opcode_val]), instrs)
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
                simplify::hide_initialization(&mut events);
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

fn event_writes<B: BV>(event: &Event<B>) -> HashSet<Sym> {
    match event {
        Event::Smt(def, _, _) => match def {
            smtlib::Def::DefineConst(v, _) => HashSet::from([*v]),
            _ => HashSet::new(),
        },
        Event::ReadReg(_, _, val) => val_uses(val),
        _ => HashSet::new(),
    }
}

fn defns<B: BV>(events: &Vec<Event<B>>) -> HashMap<Sym, usize> {
    let mut defn_idx = HashMap::new();
    for (i, event) in events.iter().enumerate() {
        for sym in event_writes(event) {
            defn_idx.insert(sym, i);
        }
    }
    defn_idx
}

fn exp_uses(exp: &smtlib::Exp<Sym>) -> HashSet<Sym> {
    use smtlib::Exp::*;
    match exp {
        Var(sym) => HashSet::from([*sym]),
        Bits(_) | Bits64(_) | Enum(_) | Bool(_) | FPConstant(..) | FPRoundingMode(_) => {
            HashSet::new()
        }
        Not(exp)
        | Bvnot(exp)
        | Bvneg(exp)
        | Extract(_, _, exp)
        | ZeroExtend(_, exp)
        | SignExtend(_, exp)
        | FPUnary(_, exp) => exp_uses(exp),
        Eq(lhs, rhs)
        | Neq(lhs, rhs)
        | And(lhs, rhs)
        | Or(lhs, rhs)
        | Bvand(lhs, rhs)
        | Bvor(lhs, rhs)
        | Bvxor(lhs, rhs)
        | Bvnand(lhs, rhs)
        | Bvnor(lhs, rhs)
        | Bvxnor(lhs, rhs)
        | Bvadd(lhs, rhs)
        | Bvsub(lhs, rhs)
        | Bvmul(lhs, rhs)
        | Bvudiv(lhs, rhs)
        | Bvsdiv(lhs, rhs)
        | Bvurem(lhs, rhs)
        | Bvsrem(lhs, rhs)
        | Bvsmod(lhs, rhs)
        | Bvult(lhs, rhs)
        | Bvslt(lhs, rhs)
        | Bvule(lhs, rhs)
        | Bvsle(lhs, rhs)
        | Bvuge(lhs, rhs)
        | Bvsge(lhs, rhs)
        | Bvugt(lhs, rhs)
        | Bvsgt(lhs, rhs)
        | Bvshl(lhs, rhs)
        | Bvlshr(lhs, rhs)
        | Bvashr(lhs, rhs)
        | Concat(lhs, rhs)
        | FPBinary(_, lhs, rhs) => {
            let lhs_uses = exp_uses(lhs);
            let rhs_uses = exp_uses(rhs);
            &lhs_uses | &rhs_uses
        }
        Ite(cond, then_exp, else_exp) => {
            let cond_uses = exp_uses(cond);
            let then_uses = exp_uses(then_exp);
            let else_uses = exp_uses(else_exp);
            let uses = &cond_uses | &then_uses;
            &uses | &else_uses
        }
        //App(f, args) => {
        //    uses.insert(*f, uses.get(f).unwrap_or(&0) + 1);
        //    for arg in args {
        //        uses_in_exp(uses, arg);
        //    }
        //}
        //Select(array, index) => {
        //    uses_in_exp(uses, array);
        //    uses_in_exp(uses, index)
        //}
        //Store(array, index, val) => {
        //    uses_in_exp(uses, array);
        //    uses_in_exp(uses, index);
        //    uses_in_exp(uses, val)
        //}
        //Distinct(exps) => {
        //    for exp in exps {
        //        uses_in_exp(uses, exp);
        //    }
        //}
        //FPRoundingUnary(_, rm, exp) => {
        //    uses_in_exp(uses, rm);
        //    uses_in_exp(uses, exp);
        //}
        //FPRoundingBinary(_, rm, lhs, rhs) => {
        //    uses_in_exp(uses, rm);
        //    uses_in_exp(uses, lhs);
        //    uses_in_exp(uses, rhs)
        //}
        //FPfma(rm, x, y, z) => {
        //    uses_in_exp(uses, rm);
        //    uses_in_exp(uses, x);
        //    uses_in_exp(uses, y);
        //    uses_in_exp(uses, z)
        //}
        _ => todo!("not yet implemented expression: {:?}", exp),
    }
}

fn smt_def_uses(def: &smtlib::Def) -> HashSet<Sym> {
    match def {
        // DeclareConst(Sym, Ty),
        // DeclareFun(Sym, Vec<Ty>, Ty),
        smtlib::Def::DefineConst(_, exp) => exp_uses(&exp),
        // DefineEnum(Name, usize),
        // Assert(Exp<Sym>),
        _ => HashSet::new(),
    }
}

fn val_uses<B: BV>(val: &Val<B>) -> HashSet<Sym> {
    // See: simplify::uses_in_value
    use Val::*;
    match val {
        Symbolic(sym) => HashSet::from([*sym]),
        // MixedBits(segments) => segments.iter().for_each(|segment| match segment {
        //     BitsSegment::Symbolic(v) => {
        //         uses.insert(*v, uses.get(v).unwrap_or(&0) + 1);
        //     }
        //     BitsSegment::Concrete(_) => (),
        // }),
        I64(_) | I128(_) | Bool(_) | Bits(_) | Enum(_) | String(_) | Unit | Ref(_) | Poison => {
            HashSet::new()
        }
        // List(vals) | Vector(vals) => vals.iter().for_each(|val| uses_in_value(uses, val)),
        Struct(fields) => fields
            .iter()
            .map(|(_, val)| val_uses(val))
            .fold(HashSet::new(), |acc, uses| &acc | &uses),
        // Ctor(_, val) => uses_in_value(uses, val),
        // SymbolicCtor(v, possibilities) => {
        //     uses.insert(*v, uses.get(v).unwrap_or(&0) + 1);
        //     possibilities
        //         .iter()
        //         .for_each(|(_, val)| uses_in_value(uses, val))
        // }
        _ => todo!("not yet implemented value: {:?}", val),
    }
}

fn uses<B: BV>(event: &Event<B>) -> HashSet<Sym> {
    match event {
        Event::Smt(def, _, _) => smt_def_uses(&def),
        Event::WriteReg(_, _, val) => val_uses(val),
        _ => HashSet::new(),
    }
}

fn tree_shake<B: BV>(events: &Vec<Event<B>>) -> Vec<Event<B>> {
    // Definitions.
    let defn_idx = defns(events);

    // Work list: populate with register writes.
    let mut work_list = Vec::new();
    let mut live = HashSet::new();
    for (i, event) in events.iter().enumerate() {
        match event {
            Event::WriteReg(_, _, val) => val_uses(val).iter().for_each(|sym| {
                // Mark live.
                live.insert(i);

                // Push the variable to be visited.
                let d = defn_idx[&sym];
                live.insert(d);
                work_list.push(d);
            }),
            _ => continue,
        };
    }

    // Process.
    while !work_list.is_empty() {
        let i = work_list.pop().unwrap();
        assert!(live.contains(&i), "visited events should be live");
        let event = &events[i];

        // Mark uses live.
        for u in uses(&event) {
            // Lookup definition of this dependency.
            assert!(defn_idx.contains_key(&u), "no definition for {:?}", u);
            let ui = defn_idx[&u];
            if live.contains(&ui) {
                continue;
            }

            live.insert(ui);
            work_list.push(ui);
        }
    }

    // Filter down to live events.
    let mut events: Vec<_> = events
        .iter()
        .enumerate()
        .filter_map(|(i, event)| if live.contains(&i) { Some(event) } else { None })
        .cloned()
        .collect();

    // Simplify pass.
    events.reverse();
    simplify::propagate_forwards_used_once(&mut events);
    events.reverse();

    events
}

fn write_events<'ir, B: BV>(
    events: &Vec<Event<B>>,
    iarch: &'ir Initialized<'ir, B>,
) -> anyhow::Result<()> {
    // Print.
    let stdout = std::io::stdout();
    let mut handle = BufWriter::with_capacity(5 * usize::pow(2, 20), stdout.lock());
    let write_opts = WriteOpts::default();
    simplify::write_events_with_opts(&mut handle, &events, &iarch.shared_state, &write_opts)
        .unwrap();
    handle.flush().unwrap();

    Ok(())
}

#[derive(Clone)]
struct Conditions {
    requires: Vec<SpecExpr>,
    provides: Vec<SpecExpr>,
}

impl Conditions {
    fn new() -> Self {
        Self {
            requires: Vec::new(),
            provides: Vec::new(),
        }
    }

    fn merge(cs: Vec<Self>) -> Self {
        match cs.len() {
            0 => Self::new(),
            1 => cs[0].clone(),
            _ => Self {
                requires: vec![spec_or(
                    cs.iter().map(|c| spec_and(c.requires.clone())).collect(),
                )],
                provides: cs
                    .iter()
                    .map(|c| {
                        spec_binary(
                            SpecOp::Imp,
                            spec_and(c.requires.clone()),
                            spec_and(c.provides.clone()),
                        )
                    })
                    .collect(),
            },
        }
    }
}

#[derive(Debug, Clone)]
struct SpecConfig {
    term: String,
    args: Vec<String>,
    cases: Vec<InstConfig>,
}

#[derive(Debug, Clone)]
struct InstConfig {
    inst: Inst,
    require: Vec<SpecExpr>,
    reg_read: HashMap<String, String>,
    reg_write: HashMap<String, String>,
}

struct SpecConverter<'ir, B: BV> {
    cfg: SpecConfig,
    iarch: &'ir Initialized<'ir, B>,
}

impl<'ir, B: BV> SpecConverter<'ir, B> {
    fn new(cfg: SpecConfig, iarch: &'ir Initialized<'ir, B>) -> Self {
        Self {
            cfg: cfg.clone(),
            iarch,
        }
    }

    fn generate(&self) -> anyhow::Result<Spec> {
        // Derive conditions for each case.
        let conds: Vec<Conditions> = self
            .cfg
            .cases
            .iter()
            .map(|c| self.case(c))
            .collect::<Result<_, _>>()?;
        let cond = Conditions::merge(conds);

        let spec = Spec {
            term: spec_ident(self.cfg.term.clone()),
            args: self.cfg.args.iter().cloned().map(spec_ident).collect(),
            requires: cond.requires,
            provides: cond.provides,
        };

        Ok(spec)
    }

    fn case(&self, case: &InstConfig) -> anyhow::Result<Conditions> {
        // Assemble instruction.
        let opcodes = opcodes(&case.inst);
        assert_eq!(opcodes.len(), 1);
        let opcode = opcodes[0];

        // ISLA trace.
        let paths = trace_opcode(opcode, &self.iarch)?;

        // TODO(mbm): handle multiple paths
        assert_eq!(paths.len(), 1);
        let events = &paths[0];

        // Filter.
        let events = tree_shake(events);

        // Convert.
        let mut converter = TraceConverter::new(case.clone(), &self.iarch);
        let conds = converter.convert()?;

        Ok(conds)
    }
}

struct TraceConverter<'ir, B: BV> {
    cfg: InstConfig,
    iarch: &'ir Initialized<'ir, B>,

    // Keep track of registers read and written in the trace.
    reg_reads: HashSet<String>,
    reg_writes: HashSet<String>,

    // Types of SMT variables and functions. Required for ISLA type inference.
    ty: HashMap<Sym, smtlib::Ty>,
    funty: HashMap<Sym, (Vec<smtlib::Ty>, smtlib::Ty)>,
}

impl<'ir, B: BV> TraceConverter<'ir, B> {
    fn new(cfg: InstConfig, iarch: &'ir Initialized<'ir, B>) -> Self {
        Self {
            cfg: cfg.clone(),
            iarch,

            reg_reads: HashSet::new(),
            reg_writes: HashSet::new(),
            ty: HashMap::new(),
            funty: HashMap::new(),
        }
    }

    fn convert(&mut self) -> anyhow::Result<Conditions> {
        // Assemble instruction.
        let opcodes = opcodes(&self.cfg.inst);
        assert_eq!(opcodes.len(), 1);
        let opcode = opcodes[0];

        // ISLA trace.
        let paths = trace_opcode(opcode, &self.iarch)?;

        // TODO(mbm): handle multiple paths
        assert_eq!(paths.len(), 1);
        let events = &paths[0];

        // Filter.
        let events = tree_shake(events);

        // Convert into conditions.
        let mut conds = Conditions {
            provides: Vec::new(),
            requires: self.cfg.require.clone(),
        };

        for event in events {
            if let Some(exp) = self.event(&event)? {
                conds.provides.push(exp);
            }
        }

        Ok(conds)
    }

    fn event(&mut self, event: &Event<B>) -> anyhow::Result<Option<SpecExpr>> {
        match event {
            Event::Smt(def, attr, ..) if !attr.is_uninteresting() => self.smt(def),

            Event::ReadReg(name, acc, val) => {
                if !acc.is_empty() {
                    anyhow::bail!("register read accessors unimplemented");
                }

                if let Val::Symbolic(sym) = val {
                    self.read_reg(name, sym)
                } else {
                    anyhow::bail!("non-symbolic register reads are unimplemented");
                }
            }

            Event::WriteReg(name, acc, val) => {
                if !acc.is_empty() {
                    anyhow::bail!("register write accessors unimplemented");
                }

                if let Val::Symbolic(sym) = val {
                    self.write_reg(name, sym)
                } else {
                    anyhow::bail!("non-symbolic register writes are unimplemented");
                }
            }

            _ => anyhow::bail!("unsupported event type: {:?}", event),
        }
    }

    fn read_reg(&mut self, name: &Name, sym: &Sym) -> anyhow::Result<Option<SpecExpr>> {
        // Map to a string name.
        let reg = self.lookup_name(name);

        // Expect to find a configured mapping for this register read.
        let mapped_var = match self.cfg.reg_read.get(&reg) {
            Some(v) => v.clone(),
            None => anyhow::bail!("no mapping for read of register {}", reg),
        };

        // Expect register to be read only once.
        if self.reg_reads.contains(&reg) {
            anyhow::bail!("multiple reads of register {}", reg);
        }
        self.reg_reads.insert(reg.clone());

        // Emit expression binding the ISLA variable to mapped ISLE term.
        Ok(Some(spec_eq(spec_var(mapped_var), self.sym(sym))))
    }

    fn write_reg(&mut self, name: &Name, sym: &Sym) -> anyhow::Result<Option<SpecExpr>> {
        // Map to a string name.
        let reg = self.lookup_name(name);

        // Expect to find a configured mapping for this register write.
        let mapped_var = match self.cfg.reg_write.get(&reg) {
            Some(v) => v.clone(),
            None => anyhow::bail!("no mapping for write of register {}", reg),
        };

        // Expect register to be written only once.
        if self.reg_writes.contains(&reg) {
            anyhow::bail!("multiple writes of register {}", reg);
        }
        self.reg_writes.insert(reg.clone());

        // Emit expression binding the ISLA variable to mapped ISLE term.
        Ok(Some(spec_eq(spec_var(mapped_var), self.sym(sym))))
    }

    fn smt(&mut self, def: &smtlib::Def) -> anyhow::Result<Option<SpecExpr>> {
        match def {
            smtlib::Def::DefineConst(v, exp) => {
                let ty = self.infer(exp).expect("SMT expression was badly-typed");
                self.ty.insert(*v, ty.clone());

                Ok(Some(spec_eq(self.sym(v), self.exp(exp))))
            }

            smtlib::Def::DeclareConst(v, ty) => {
                self.ty.insert(*v, ty.clone());
                Ok(None)
            }

            smtlib::Def::DeclareFun(v, params, ret) => {
                self.funty.insert(*v, (params.clone(), ret.clone()));
                Ok(None)
            }

            smtlib::Def::DefineEnum(..) => Ok(None),

            _ => anyhow::bail!("unsupported smt def: {:?}", def),
        }
    }

    fn exp(&self, exp: &smtlib::Exp<Sym>) -> SpecExpr {
        use smtlib::Exp::*;
        match exp {
            Var(v) => self.sym(v),
            Bits(bits) => spec_bits(bits),
            Bits64(bv) => spec_const_bit_vector(
                bv.lower_u64().try_into().unwrap(),
                bv.len().try_into().unwrap(),
            ),
            // Enum(EnumMember),
            Bool(b) => spec_const_bool(*b),
            // Neq(Box<Exp<V>>, Box<Exp<V>>),
            Not(x) | Bvnot(x) => spec_unary(exp_spec_op(exp), self.exp(x)),

            // Bvnot(Box<Exp<V>>),
            // Bvnand(Box<Exp<V>>, Box<Exp<V>>),
            // Bvnor(Box<Exp<V>>, Box<Exp<V>>),
            // Bvxnor(Box<Exp<V>>, Box<Exp<V>>),
            // Bvneg(Box<Exp<V>>),
            Eq(lhs, rhs)
            | And(lhs, rhs)
            | Or(lhs, rhs)
            | Bvand(lhs, rhs)
            | Bvor(lhs, rhs)
            | Bvxor(lhs, rhs)
            | Bvadd(lhs, rhs)
            | Bvsub(lhs, rhs)
            | Bvmul(lhs, rhs)
            | Bvshl(lhs, rhs)
            | Bvlshr(lhs, rhs)
            | Concat(lhs, rhs) => spec_binary(exp_spec_op(exp), self.exp(lhs), self.exp(rhs)),

            // Bvudiv(Box<Exp<V>>, Box<Exp<V>>),
            // Bvsdiv(Box<Exp<V>>, Box<Exp<V>>),
            // Bvurem(Box<Exp<V>>, Box<Exp<V>>),
            // Bvsrem(Box<Exp<V>>, Box<Exp<V>>),
            // Bvsmod(Box<Exp<V>>, Box<Exp<V>>),
            // Bvult(Box<Exp<V>>, Box<Exp<V>>),
            // Bvslt(Box<Exp<V>>, Box<Exp<V>>),
            // Bvule(Box<Exp<V>>, Box<Exp<V>>),
            // Bvsle(Box<Exp<V>>, Box<Exp<V>>),
            // Bvuge(Box<Exp<V>>, Box<Exp<V>>),
            // Bvsge(Box<Exp<V>>, Box<Exp<V>>),
            // Bvugt(Box<Exp<V>>, Box<Exp<V>>),
            // Bvsgt(Box<Exp<V>>, Box<Exp<V>>),
            Extract(i, j, exp) => spec_ternary(
                SpecOp::Extract,
                spec_const_int(*i),
                spec_const_int(*j),
                self.exp(exp),
            ),

            ZeroExtend(n, x) | SignExtend(n, x) => match self.infer(x).unwrap() {
                smtlib::Ty::BitVec(w) => {
                    spec_binary(exp_spec_op(exp), spec_const_int(n + w), self.exp(x))
                }
                _ => panic!("extension applies to bitvector types"),
            },

            // Bvlshr(Box<Exp<V>>, Box<Exp<V>>),
            // Bvashr(Box<Exp<V>>, Box<Exp<V>>),
            Ite(c, t, e) => spec_ternary(SpecOp::If, self.exp(c), self.exp(t), self.exp(e)),

            // App(Sym, Vec<Exp<V>>),
            // Select(Box<Exp<V>>, Box<Exp<V>>),
            // Store(Box<Exp<V>>, Box<Exp<V>>, Box<Exp<V>>),
            // Distinct(Vec<Exp<V>>),
            // FPConstant(FPConstant, u32, u32),
            // FPRoundingMode(FPRoundingMode),
            // FPUnary(FPUnary, Box<Exp<V>>),
            // FPRoundingUnary(FPRoundingUnary, Box<Exp<V>>, Box<Exp<V>>),
            // FPBinary(FPBinary, Box<Exp<V>>, Box<Exp<V>>),
            // FPRoundingBinary(FPRoundingBinary, Box<Exp<V>>, Box<Exp<V>>, Box<Exp<V>>),
            // FPfma(Box<Exp<V>>, Box<Exp<V>>, Box<Exp<V>>, Box<Exp<V>>),
            _ => todo!("expression: {:?}", exp),
        }
    }

    fn sym(&self, s: &Sym) -> SpecExpr {
        spec_var(format!("v{}", s))
    }

    fn infer(&self, exp: &smtlib::Exp<Sym>) -> Option<smtlib::Ty> {
        exp.infer(&self.ty, &self.funty)
    }

    fn lookup_name(&self, name: &Name) -> String {
        zencode::decode(self.iarch.shared_state.symtab.to_str(name.clone()))
    }
}

fn exp_spec_op(exp: &smtlib::Exp<Sym>) -> SpecOp {
    use smtlib::Exp::*;
    match exp {
        // Bits(Vec<bool>),
        // Bits64(B64),
        // Enum(EnumMember),
        // Bool(bool),
        Eq(..) => SpecOp::Eq,
        // Neq(Box<Exp<V>>, Box<Exp<V>>),
        And(..) => SpecOp::And,
        Or(..) => SpecOp::Or,
        Not(..) => SpecOp::Not,
        Bvnot(..) => SpecOp::BVNot,
        Bvand(..) => SpecOp::BVAnd,
        Bvor(..) => SpecOp::BVOr,
        Bvxor(..) => SpecOp::BVXor,
        // Bvnand(Box<Exp<V>>, Box<Exp<V>>),
        // Bvnor(Box<Exp<V>>, Box<Exp<V>>),
        // Bvxnor(Box<Exp<V>>, Box<Exp<V>>),
        // Bvneg(Box<Exp<V>>),
        Bvadd(..) => SpecOp::BVAdd,
        Bvsub(..) => SpecOp::BVSub,
        Bvmul(..) => SpecOp::BVMul,
        // Bvudiv(Box<Exp<V>>, Box<Exp<V>>),
        // Bvsdiv(Box<Exp<V>>, Box<Exp<V>>),
        // Bvurem(Box<Exp<V>>, Box<Exp<V>>),
        // Bvsrem(Box<Exp<V>>, Box<Exp<V>>),
        // Bvsmod(Box<Exp<V>>, Box<Exp<V>>),
        // Bvult(Box<Exp<V>>, Box<Exp<V>>),
        // Bvslt(Box<Exp<V>>, Box<Exp<V>>),
        // Bvule(Box<Exp<V>>, Box<Exp<V>>),
        // Bvsle(Box<Exp<V>>, Box<Exp<V>>),
        // Bvuge(Box<Exp<V>>, Box<Exp<V>>),
        // Bvsge(Box<Exp<V>>, Box<Exp<V>>),
        // Bvugt(Box<Exp<V>>, Box<Exp<V>>),
        // Bvsgt(Box<Exp<V>>, Box<Exp<V>>),
        ZeroExtend(..) => SpecOp::ZeroExt,
        SignExtend(..) => SpecOp::SignExt,
        Bvshl(..) => SpecOp::BVShl,
        Bvlshr(..) => SpecOp::BVLshr,
        // Bvashr(Box<Exp<V>>, Box<Exp<V>>),
        Concat(..) => SpecOp::Concat,
        // Ite(Box<Exp<V>>, Box<Exp<V>>, Box<Exp<V>>),
        // App(Sym, Vec<Exp<V>>),
        // Select(Box<Exp<V>>, Box<Exp<V>>),
        // Store(Box<Exp<V>>, Box<Exp<V>>, Box<Exp<V>>),
        // Distinct(Vec<Exp<V>>),
        // FPConstant(FPConstant, u32, u32),
        // FPRoundingMode(FPRoundingMode),
        // FPUnary(FPUnary, Box<Exp<V>>),
        // FPRoundingUnary(FPRoundingUnary, Box<Exp<V>>, Box<Exp<V>>),
        // FPBinary(FPBinary, Box<Exp<V>>, Box<Exp<V>>),
        // FPRoundingBinary(FPRoundingBinary, Box<Exp<V>>, Box<Exp<V>>, Box<Exp<V>>),
        // FPfma(Box<Exp<V>>, Box<Exp<V>>, Box<Exp<V>>, Box<Exp<V>>),
        _ => todo!("spec op: {:?}", exp),
    }
}

fn spec_const_int<I>(x: I) -> SpecExpr
where
    i128: From<I>,
{
    SpecExpr::ConstInt {
        val: x.try_into().unwrap(),
        pos: Pos::default(),
    }
}

fn spec_const_bool(b: bool) -> SpecExpr {
    SpecExpr::ConstBool {
        val: if b { 1 } else { 0 },
        pos: Pos::default(),
    }
}

fn spec_const_bit_vector(val: i128, width: i8) -> SpecExpr {
    assert!(width >= 0);
    SpecExpr::ConstBitVec {
        val,
        width,
        pos: Pos::default(),
    }
}

fn spec_bits(bits: &[bool]) -> SpecExpr {
    // TODO(mbm): verify endianness assumption about Vec<bool> and test multi-chunk case
    bits.chunks(64)
        .map(|chunk| {
            let mut val: i128 = 0;
            for (i, bit) in chunk.iter().enumerate() {
                if *bit {
                    val |= 1 << i;
                }
            }
            spec_const_bit_vector(val, 64)
        })
        .rev()
        .reduce(|acc, bv| spec_binary(SpecOp::Concat, acc, bv))
        .unwrap()
}

fn spec_and(args: Vec<SpecExpr>) -> SpecExpr {
    spec_op(SpecOp::And, args)
}

fn spec_or(args: Vec<SpecExpr>) -> SpecExpr {
    spec_op(SpecOp::Or, args)
}

fn spec_unary(op: SpecOp, x: SpecExpr) -> SpecExpr {
    spec_op(op, vec![x])
}

fn spec_binary(op: SpecOp, x: SpecExpr, y: SpecExpr) -> SpecExpr {
    spec_op(op, vec![x, y])
}

fn spec_eq(x: SpecExpr, y: SpecExpr) -> SpecExpr {
    spec_binary(SpecOp::Eq, x, y)
}

fn spec_ternary(op: SpecOp, x: SpecExpr, y: SpecExpr, z: SpecExpr) -> SpecExpr {
    spec_op(op, vec![x, y, z])
}

fn spec_op(op: SpecOp, args: Vec<SpecExpr>) -> SpecExpr {
    SpecExpr::Op {
        op,
        args: args,
        pos: Pos::default(),
    }
}

fn spec_enum(name: String, variant: String) -> SpecExpr {
    SpecExpr::Enum {
        name: spec_ident(format!("{}.{}", name, variant)),
    }
}

fn spec_var(id: String) -> SpecExpr {
    SpecExpr::Var {
        var: spec_ident(id),
        pos: Pos::default(),
    }
}

fn spec_ident(id: String) -> Ident {
    Ident(id, Pos::default())
}
