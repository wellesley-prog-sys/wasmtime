use std::collections::HashMap;

use clap::Parser as ClapParser;
use cranelift_codegen::isa::aarch64::inst::{writable_xreg, xreg, ALUOp, Inst, OperandSize};
use cranelift_isle::printer;
use cranelift_isle_veri_aslp::client::Client;
use itertools::Itertools;

use cranelift_isle_veri_isaspec::builder::{Builder, InstConfig, Mapping, SpecConfig};
use cranelift_isle_veri_isaspec::{aarch64, spec::*};

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
    let cfg = define();
    let builder = Builder::new(cfg, client);
    let defs = builder.build()?;

    // Output.
    printer::dump(&defs).unwrap();

    Ok(())
}

/// Define specificiation to generate.
fn define() -> SpecConfig {
    // ALUOp
    let alu_ops = [
        ALUOp::Add,
        ALUOp::Sub,
        ALUOp::Orr,
        ALUOp::OrrNot,
        ALUOp::And,
        ALUOp::AndNot,
        ALUOp::Eor,
        ALUOp::EorNot,
        ALUOp::SMulH,
        ALUOp::UMulH,
        ALUOp::SDiv,
        ALUOp::UDiv,
        // --------------
        // Shift variable is 6-bits:
        // ALUOp::Lsr,
        // ALUOp::Asr,
        // ALUOp::Lsl,
        // ALUOp::RotR,
        //
        // Flags:
        // ALUOp::AddS,
        // ALUOp::SubS,
        // ALUOp::Adc,
        // ALUOp::Sbc,
        // ALUOp::AdcS,
        // ALUOp::SbcS,
    ];

    // OperandSize
    let sizes = vec![OperandSize::Size32, OperandSize::Size64];

    // AluRRR
    let alu_rrr = SpecConfig {
        // Spec signature.
        term: "MInst.AluRRR".to_string(),
        args: ["alu_op", "size", "rd", "rn", "rm"]
            .map(String::from)
            .to_vec(),

        cases: alu_ops
            .iter()
            .copied()
            .cartesian_product(sizes)
            .filter(|(alu_op, size)| is_alu_op_size_supported(*alu_op, *size))
            .map(|(alu_op, size)| InstConfig {
                // Instruction to generate specification from.
                inst: Inst::AluRRR {
                    alu_op,
                    size,
                    rd: writable_xreg(4),
                    rn: xreg(5),
                    rm: xreg(6),
                },

                // Requires.
                require: vec![
                    spec_eq(
                        spec_var("alu_op".to_string()),
                        spec_enum("ALUOp".to_string(), format!("{alu_op:?}")),
                    ),
                    spec_eq(
                        spec_var("size".to_string()),
                        spec_enum("OperandSize".to_string(), format!("{size:?}")),
                    ),
                ],

                // Mappings from concrete registers to specification parameters.
                mappings: HashMap::from([
                    (aarch64::gpreg(4), Mapping::write("rd".to_string())),
                    (aarch64::gpreg(5), Mapping::read("rn".to_string())),
                    (aarch64::gpreg(6), Mapping::read("rm".to_string())),
                ]),
            })
            .collect(),
    };

    alu_rrr
}

fn is_alu_op_size_supported(alu_op: ALUOp, size: OperandSize) -> bool {
    match alu_op {
        ALUOp::SMulH | ALUOp::UMulH | ALUOp::SDiv | ALUOp::UDiv => size == OperandSize::Size64,
        _ => true,
    }
}
