use clap::Parser as ClapParser;
use cranelift_codegen::isa::aarch64::inst::{
    vreg, writable_vreg, writable_xreg, xreg, ALUOp, ALUOp3, BitOp, Cond, Inst, OperandSize,
    VecALUOp, VectorSize,
};
use cranelift_isle::printer;
use cranelift_isle_veri_aslp::ast::Block;
use cranelift_isle_veri_aslp::client::Client;
use tracing::debug;

use cranelift_isle_veri_isaspec::aarch64;
use cranelift_isle_veri_isaspec::constraints::Translator;
use cranelift_isle_veri_isaspec::semantics::inst_semantics;

#[derive(ClapParser)]
#[command(version, about)]
struct Args {
    /// Server URL
    #[arg(long = "server", required = true)]
    server: String,

    /// Print debugging output (repeat for more detail)
    #[arg(short = 'd', long = "debug", action = clap::ArgAction::Count)]
    debug_level: u8,
}

fn main() -> anyhow::Result<()> {
    let args = Args::parse();

    // Setup tracing output.
    tracing_subscriber::fmt()
        .with_timer(tracing_subscriber::fmt::time::uptime())
        .with_level(true)
        .with_target(false)
        .with_max_level(match args.debug_level {
            0 => tracing::Level::WARN,
            1 => tracing::Level::INFO,
            2 => tracing::Level::DEBUG,
            _ => tracing::Level::TRACE,
        })
        .init();

    // ASLp client.
    let client = Client::new(reqwest::blocking::Client::new(), args.server)?;

    // Conversion.
    let insts = define_insts();
    for inst in &insts {
        println!("-------------------------------------");
        let opcode = aarch64::opcode(inst);
        let asm = aarch64::assembly(inst);
        println!("inst = {inst:#?}");
        println!("opcode = {opcode:08x}");
        println!("asm = {asm}");
        println!("----");
        let block = inst_semantics(inst, &client)?;
        convert_block(&block)?;
        println!("-------------------------------------");
    }

    Ok(())
}

// Define instructions to test.
fn define_insts() -> Vec<Inst> {
    let mut insts = Vec::new();

    // AluRRR
    let alu_ops = vec![
        ALUOp::Add,
        ALUOp::Sub,
        ALUOp::Orr,
        ALUOp::OrrNot,
        ALUOp::And,
        ALUOp::AndNot,
        ALUOp::Eor,
        ALUOp::EorNot,
        ALUOp::AddS,
        ALUOp::SubS,
        ALUOp::SMulH,
        ALUOp::UMulH,
        ALUOp::Adc,
        ALUOp::Sbc,
        ALUOp::AdcS,
        ALUOp::SbcS,
        ALUOp::Lsr,
        ALUOp::Asr,
        ALUOp::Lsl,
        ALUOp::RotR,
        ALUOp::SDiv,
        ALUOp::UDiv,
    ];
    for alu_op in alu_ops {
        insts.push(Inst::AluRRR {
            alu_op,
            size: OperandSize::Size64,
            rd: writable_xreg(4),
            rn: xreg(5),
            rm: xreg(6),
        });
    }

    // AluRRRR
    let alu_ops = vec![ALUOp3::MAdd, ALUOp3::MSub, ALUOp3::UMAddL, ALUOp3::SMAddL];
    for alu_op in alu_ops {
        insts.push(Inst::AluRRRR {
            alu_op,
            size: OperandSize::Size32,
            rd: writable_xreg(4),
            rn: xreg(1),
            rm: xreg(2),
            ra: xreg(3),
        });
    }

    // BitRR
    let ops = vec![
        BitOp::RBit,
        BitOp::Clz,
        BitOp::Cls,
        BitOp::Rev16,
        BitOp::Rev32,
        BitOp::Rev64,
    ];
    for op in ops {
        insts.push(Inst::BitRR {
            op,
            size: OperandSize::Size64,
            rd: writable_xreg(2),
            rn: xreg(1),
        });
    }

    // CSel
    let conds = vec![
        Cond::Eq,
        Cond::Ne,
        Cond::Hs,
        Cond::Lo,
        Cond::Mi,
        Cond::Pl,
        Cond::Vs,
        Cond::Vc,
        Cond::Hi,
        Cond::Ls,
        Cond::Ge,
        Cond::Lt,
        Cond::Gt,
        Cond::Le,
        Cond::Al,
        Cond::Nv,
    ];
    for cond in conds.clone() {
        insts.push(Inst::CSel {
            rd: writable_xreg(3),
            cond,
            rn: xreg(1),
            rm: xreg(2),
        });
    }

    // CSNeg
    for cond in conds.clone() {
        insts.push(Inst::CSNeg {
            rd: writable_xreg(3),
            cond,
            rn: xreg(1),
            rm: xreg(2),
        });
    }

    // VecRRR
    let alu_ops = vec![
        VecALUOp::Cmeq,
        VecALUOp::Cmge,
        VecALUOp::Cmgt,
        VecALUOp::Cmhs,
        VecALUOp::Cmhi,
        VecALUOp::And,
        VecALUOp::Bic,
        VecALUOp::Orr,
        VecALUOp::Umaxp,
        VecALUOp::Add,
        VecALUOp::Sub,
        VecALUOp::Mul,
        VecALUOp::Sshl,
        VecALUOp::Ushl,
        VecALUOp::Umin,
        VecALUOp::Smin,
        VecALUOp::Umax,
        VecALUOp::Smax,
        VecALUOp::Urhadd,
        VecALUOp::Addp,
        VecALUOp::Zip1,
        VecALUOp::Zip2,
        VecALUOp::Uzp1,
        VecALUOp::Uzp2,
        VecALUOp::Trn1,
        VecALUOp::Trn2,
        // TODO: 128-bit bitvector literal
        // VecALUOp::Eor,
        // TODO: boolean literals
        // VecALUOp::Sqadd,
        // VecALUOp::Uqadd,
        // VecALUOp::Sqsub,
        // VecALUOp::Uqsub,
        // VecALUOp::Sqrdmulh,
        // TODO: floating point.
        // VecALUOp::Fcmeq,
        // VecALUOp::Fcmgt,
        // VecALUOp::Fcmge,
        // VecALUOp::Fadd,
        // VecALUOp::Fsub,
        // VecALUOp::Fdiv,
        // VecALUOp::Fmax,
        // VecALUOp::Fmin,
        // VecALUOp::Fmul,
    ];
    for alu_op in alu_ops {
        insts.push(Inst::VecRRR {
            alu_op,
            rd: writable_vreg(3),
            rn: vreg(1),
            rm: vreg(2),
            size: VectorSize::Size32x4,
        });
    }

    insts
}

// Convert a semantics block and print the result.
fn convert_block(block: &Block) -> anyhow::Result<()> {
    // Translation.
    let mut translator = Translator::new(aarch64::state(), "v".to_string());
    translator.translate(block)?;

    // Report.
    let global = translator.global();
    debug!("scope: {global:#?}");

    let bindings = global.bindings();

    for r in global.reads() {
        println!("read:\t{r}\t{:?}", bindings[r]);
    }

    for w in global.writes() {
        println!("write:\t{w}\t{:?}", bindings[w]);
    }

    println!();

    for constraint in global.constraints() {
        printer::dump(constraint).unwrap();
        println!();
    }

    Ok(())
}
