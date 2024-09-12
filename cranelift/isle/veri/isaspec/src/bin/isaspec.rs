use clap::Parser as ClapParser;
use cranelift_codegen::isa::aarch64::inst::{writable_xreg, xreg, ALUOp, Inst, OperandSize};
use cranelift_isle::printer;
use cranelift_isle_veri_aslp::client::Client;
use cranelift_isle_veri_isaspec::aarch64::pstate_field;
use itertools::Itertools;

use cranelift_isle_veri_isaspec::builder::{Builder, InstConfig, Mapping, Mappings, SpecConfig};
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
        ALUOp::AddS,
        ALUOp::Adc,
        // --------------
        // Not supported yet:
        // ALUOp::Eor,
        // ALUOp::EorNot,
        // ALUOp::SMulH,
        // ALUOp::UMulH,
        //
        // Generates unimplemented function:
        // ALUOp::SDiv,
        // ALUOp::UDiv,
        //
        // Shift variable is 6-bits:
        // ALUOp::Lsr,
        // ALUOp::Asr,
        // ALUOp::Lsl,
        // ALUOp::RotR,
        //
        // Flag ops not required yet:
        // ALUOp::SubS,
        // ALUOp::Sbc,
        // ALUOp::AdcS,
        // ALUOp::SbcS,
    ];

    // OperandSize
    let sizes = vec![OperandSize::Size32, OperandSize::Size64];

    // AluRRR
    let mut mappings = flags_mappings();
    mappings.writes.insert(
        aarch64::gpreg(4),
        Mapping::require(spec_var("rd".to_string())),
    );
    mappings.reads.insert(
        aarch64::gpreg(5),
        Mapping::require(spec_var("rn".to_string())),
    );
    mappings.reads.insert(
        aarch64::gpreg(6),
        Mapping::require(spec_var("rm".to_string())),
    );

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

                // Mappings from state to specification parameters.
                mappings: mappings.clone(),
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

fn flags_mappings() -> Mappings {
    // Instruction model is the MInst value itself, which is considered the result of the variant term.
    let inst = spec_var("result".to_string());

    // Input and output flags of the instruction are fields of the MInst model.
    let flags_in = spec_field("flags_in".to_string(), inst.clone());
    let flags_out = spec_field("flags_out".to_string(), inst.clone());

    // Construct read and write mappings for each NZCV field.
    let mut mappings = Mappings::default();
    for field in &["N", "Z", "C", "V"] {
        // Read
        mappings.reads.insert(
            pstate_field(field),
            Mapping::allow(spec_field(field.to_string(), flags_in.clone())),
        );

        // Write
        mappings.writes.insert(
            pstate_field(field),
            Mapping::allow(spec_field(field.to_string(), flags_out.clone())),
        );
    }

    mappings
}
