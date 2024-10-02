use std::io;
use std::io::Write;
use std::path::{Path, PathBuf};

use anyhow::Result;
use clap::Parser as ClapParser;
use cranelift_codegen::ir::MemFlags;
use cranelift_codegen::{Reg, Writable};
use cranelift_isle_veri_isaspec::memory::ReadEffect;
use itertools::Itertools;

use cranelift_codegen::isa::aarch64::inst::{
    writable_xreg, xreg, ALUOp, ALUOp3, AMode, BitOp, ExtendOp, Inst, OperandSize,
};
use cranelift_isle::ast::{Def, SpecOp};
use cranelift_isle::printer;
use cranelift_isle_veri_aslp::client::Client;
use cranelift_isle_veri_isaspec::aarch64::{self, pstate_field};
use cranelift_isle_veri_isaspec::builder::{
    Arm, Builder, Case, Cases, InstConfig, Mapping, MappingBuilder, Mappings, Match, SpecConfig,
};
use cranelift_isle_veri_isaspec::spec::{
    spec_binary, spec_const_int, spec_eq, spec_eq_bool, spec_var,
};

#[derive(ClapParser)]
#[command(version, about)]
struct Args {
    /// Server URL
    #[arg(long = "server", required = true)]
    server: String,

    // Output directory.
    #[arg(long, required = true)]
    output: PathBuf,

    /// Print debugging output (repeat for more detail)
    #[arg(short = 'd', long = "debug", action = clap::ArgAction::Count)]
    debug_level: u8,
}

fn main() -> Result<()> {
    let args = Args::parse();

    // Setup tracing output.
    tracing_subscriber::fmt()
        .with_writer(io::stderr)
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
    let http_client = reqwest::blocking::Client::new();
    let client = Client::new(&http_client, args.server)?;

    // Conversion.
    let file_configs = define();
    for file_config in file_configs {
        // Generate specs.
        let mut defs = Vec::new();
        for spec_config in file_config.specs {
            let builder = Builder::new(spec_config, &client);
            let def = builder.build()?;
            defs.push(def);
        }

        // Output.
        let path = args.output.join(file_config.name);
        write_spec(&path, &defs)?;
    }

    Ok(())
}

fn write_spec(path: &Path, defs: &Vec<Def>) -> Result<()> {
    let mut output = std::fs::File::create(path)?;

    // Code generation warning.
    writeln!(output, ";; GENERATED BY `isaspec`. DO NOT EDIT!!!")?;
    writeln!(output)?;

    // Format with ISLE printer.
    printer::print(defs, 78, &mut output)?;

    Ok(())
}

/// Configuration for an ISLE specification file to generate.
struct FileConfig {
    name: PathBuf,
    specs: Vec<SpecConfig>,
}

/// Define specifications to generate.
fn define() -> Vec<FileConfig> {
    vec![
        FileConfig {
            name: "alu_rrr.isle".into(),
            specs: vec![define_alu_rrr()],
        },
        FileConfig {
            name: "alu_rrrr.isle".into(),
            specs: vec![define_alu_rrrr()],
        },
        FileConfig {
            name: "bit_rr.isle".into(),
            specs: vec![define_bit_rr()],
        },
        FileConfig {
            name: "loads.isle".into(),
            specs: define_loads(),
        },
        FileConfig {
            name: "extend.isle".into(),
            specs: vec![define_extend()],
        },
    ]
}

// MInst.AluRRR specification configuration.
fn define_alu_rrr() -> SpecConfig {
    let alu_ops = [
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
        ALUOp::SDiv,
        ALUOp::UDiv,
        ALUOp::Adc,
        // --------------
        // Shift variable is 6-bits:
        // ALUOp::Lsr,
        // ALUOp::Asr,
        // ALUOp::Lsl,
        // ALUOp::RotR,
        //
        // Flag ops not required yet:
        // ALUOp::Sbc,
        // ALUOp::AdcS,
        // ALUOp::SbcS,
    ];

    // OperandSize
    let sizes = [OperandSize::Size32, OperandSize::Size64];

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

    SpecConfig {
        // Spec signature.
        term: "MInst.AluRRR".to_string(),
        args: ["alu_op", "size", "rd", "rn", "rm"]
            .map(String::from)
            .to_vec(),

        cases: Cases::Match(Match {
            on: "size".to_string(),
            arms: sizes
                .iter()
                .rev()
                .map(|size| Arm {
                    variant: format!("{size:?}"),
                    args: Vec::new(),
                    body: Cases::Match(Match {
                        on: "alu_op".to_string(),
                        arms: alu_ops
                            .iter()
                            .filter(|alu_op| is_alu_op_size_supported(**alu_op, *size))
                            .map(|alu_op| Arm {
                                variant: format!("{alu_op:?}"),
                                args: Vec::new(),
                                body: Cases::Instruction(InstConfig {
                                    inst: Inst::AluRRR {
                                        alu_op: *alu_op,
                                        size: *size,
                                        rd: writable_xreg(4),
                                        rn: xreg(5),
                                        rm: xreg(6),
                                    },

                                    // Mappings from state to specification parameters.
                                    mappings: mappings.clone(),
                                }),
                            })
                            .collect(),
                    }),
                })
                .collect(),
        }),
    }
}

fn is_alu_op_size_supported(alu_op: ALUOp, size: OperandSize) -> bool {
    match alu_op {
        ALUOp::SMulH | ALUOp::UMulH | ALUOp::SDiv | ALUOp::UDiv => size == OperandSize::Size64,
        _ => true,
    }
}

// MInst.AluRRRR specification configuration.
fn define_alu_rrrr() -> SpecConfig {
    // ALUOp3
    let alu3_ops = [ALUOp3::MAdd, ALUOp3::MSub, ALUOp3::UMAddL, ALUOp3::SMAddL];

    // OperandSize
    let sizes = [OperandSize::Size32, OperandSize::Size64];

    let mut mappings = Mappings::default();
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
    mappings.reads.insert(
        aarch64::gpreg(7),
        Mapping::require(spec_var("ra".to_string())),
    );

    SpecConfig {
        // Spec signature.
        term: "MInst.AluRRRR".to_string(),
        args: ["alu_op", "size", "rd", "rn", "rm", "ra"]
            .map(String::from)
            .to_vec(),

        cases: Cases::Match(Match {
            on: "size".to_string(),
            arms: sizes
                .iter()
                .rev()
                .map(|size| Arm {
                    variant: format!("{size:?}"),
                    args: Vec::new(),
                    body: Cases::Match(Match {
                        on: "alu_op".to_string(),
                        arms: alu3_ops
                            .iter()
                            .filter(|alu3_op| is_alu3_op_size_supported(**alu3_op, *size))
                            .map(|alu_op| Arm {
                                variant: format!("{alu_op:?}"),
                                args: Vec::new(),
                                body: Cases::Instruction(InstConfig {
                                    inst: Inst::AluRRRR {
                                        alu_op: *alu_op,
                                        size: *size,
                                        rd: writable_xreg(4),
                                        rn: xreg(5),
                                        rm: xreg(6),
                                        ra: xreg(7),
                                    },
                                    mappings: mappings.clone(),
                                }),
                            })
                            .collect(),
                    }),
                })
                .collect(),
        }),
    }
}

fn is_alu3_op_size_supported(alu3_op: ALUOp3, size: OperandSize) -> bool {
    match alu3_op {
        ALUOp3::UMAddL | ALUOp3::SMAddL => size == OperandSize::Size32,
        _ => true,
    }
}

// MInst.BitRR specification configuration.
fn define_bit_rr() -> SpecConfig {
    // BitRR
    let bit_ops = [
        BitOp::Cls,
        // --------------
        // BitOp::RBit,
        // BitOp::Clz,
        // BitOp::Rev16,
        // BitOp::Rev32,
        // BitOp::Rev64,
    ];

    // OperandSize
    let sizes = [OperandSize::Size32, OperandSize::Size64];

    let mut mappings = Mappings::default();
    mappings.writes.insert(
        aarch64::gpreg(4),
        Mapping::require(spec_var("rd".to_string())),
    );
    mappings.reads.insert(
        aarch64::gpreg(5),
        Mapping::require(spec_var("rn".to_string())),
    );

    SpecConfig {
        // Spec signature.
        term: "MInst.BitRR".to_string(),
        args: ["op", "size", "rd", "rn"].map(String::from).to_vec(),

        cases: Cases::Match(Match {
            on: "size".to_string(),
            arms: sizes
                .iter()
                .rev()
                .map(|size| Arm {
                    variant: format!("{size:?}"),
                    args: Vec::new(),
                    body: Cases::Match(Match {
                        on: "op".to_string(),
                        arms: bit_ops
                            .iter()
                            .map(|op| Arm {
                                variant: format!("{op:?}"),
                                args: Vec::new(),
                                body: Cases::Instruction(InstConfig {
                                    inst: Inst::BitRR {
                                        op: *op,
                                        size: *size,
                                        rd: writable_xreg(4),
                                        rn: xreg(5),
                                    },
                                    mappings: mappings.clone(),
                                }),
                            })
                            .collect(),
                    }),
                })
                .collect(),
        }),
    }
}

fn define_extend() -> SpecConfig {
    // Extend
    let signed = [false, true];
    let bits = [8u8, 16u8, 32u8, 64u8];

    let mut mappings = Mappings::default();
    mappings.writes.insert(
        aarch64::gpreg(4),
        Mapping::require(spec_var("rd".to_string())),
    );
    mappings.reads.insert(
        aarch64::gpreg(5),
        Mapping::require(spec_var("rn".to_string())),
    );

    SpecConfig {
        // Spec signature.
        term: "MInst.Extend".to_string(),
        args: ["rd", "rn", "signed", "from_bits", "to_bits"]
            .map(String::from)
            .to_vec(),
        cases: Cases::Cases(
            bits.iter()
                .cartesian_product(&bits)
                .filter(|(from_bits, to_bits)| from_bits < to_bits)
                .cartesian_product(&signed)
                .map(|((from_bits, to_bits), signed)| Case {
                    conds: vec![
                        spec_eq_bool(spec_var("signed".to_string()), *signed),
                        spec_eq(
                            spec_var("from_bits".to_string()),
                            spec_const_int((*from_bits).into()),
                        ),
                        spec_eq(
                            spec_var("to_bits".to_string()),
                            spec_const_int((*to_bits).into()),
                        ),
                    ],
                    cases: Cases::Instruction(InstConfig {
                        // Instruction to generate specification from.
                        inst: Inst::Extend {
                            rd: writable_xreg(4),
                            rn: xreg(5),
                            signed: *signed,
                            from_bits: *from_bits,
                            to_bits: *to_bits,
                        },

                        // Mappings from state to specification parameters.
                        mappings: mappings.clone(),
                    }),
                })
                .collect(),
        ),
    }
}

fn define_loads() -> Vec<SpecConfig> {
    // ULoad8
    let uload8 = define_load("MInst.ULoad8", 8, |rd, mem, flags| Inst::ULoad8 {
        rd,
        mem,
        flags,
    });

    // SLoad8
    let sload8 = define_load("MInst.SLoad8", 8, |rd, mem, flags| Inst::SLoad8 {
        rd,
        mem,
        flags,
    });

    // ULoad16
    let uload16 = define_load("MInst.ULoad16", 16, |rd, mem, flags| Inst::ULoad16 {
        rd,
        mem,
        flags,
    });

    // SLoad16
    let sload16 = define_load("MInst.SLoad16", 16, |rd, mem, flags| Inst::SLoad16 {
        rd,
        mem,
        flags,
    });

    // ULoad32
    let uload32 = define_load("MInst.ULoad32", 32, |rd, mem, flags| Inst::ULoad32 {
        rd,
        mem,
        flags,
    });

    // SLoad32
    let sload32 = define_load("MInst.SLoad32", 32, |rd, mem, flags| Inst::SLoad32 {
        rd,
        mem,
        flags,
    });

    // ULoad64
    let uload64 = define_load("MInst.ULoad64", 64, |rd, mem, flags| Inst::ULoad64 {
        rd,
        mem,
        flags,
    });

    vec![uload8, sload8, uload16, sload16, uload32, sload32, uload64]
}

fn define_load<F>(term: &str, size_bits: usize, inst: F) -> SpecConfig
where
    F: Fn(Writable<Reg>, AMode, MemFlags) -> Inst,
{
    // Mappings.
    let mut mappings = Mappings::default();

    // Destination register.
    mappings.writes.insert(
        aarch64::gpreg(4),
        Mapping::require(spec_var("rd".to_string())),
    );

    // ISA load state mapped to read effect.
    let read_effect = ReadEffect::new();
    static ISA_LOAD: &str = "isa_load";
    static LOADED_VALUE: &str = "loaded_value";
    mappings.writes.insert(
        read_effect.active,
        MappingBuilder::state(ISA_LOAD).field("active").build(),
    );
    mappings.writes.insert(
        read_effect.addr,
        MappingBuilder::state(ISA_LOAD).field("addr").build(),
    );
    mappings.writes.insert(
        read_effect.size_bits,
        MappingBuilder::state(ISA_LOAD).field("size_bits").build(),
    );
    mappings.reads.insert(
        read_effect.value,
        Mapping::require(spec_binary(
            SpecOp::ConvTo,
            spec_const_int(size_bits.try_into().unwrap()),
            spec_var(LOADED_VALUE.to_string()),
        )),
    );

    // RegReg
    let mut reg_reg_mappings = mappings.clone();
    reg_reg_mappings.reads.insert(
        aarch64::gpreg(5),
        Mapping::require(spec_var("rn".to_string())),
    );
    reg_reg_mappings.reads.insert(
        aarch64::gpreg(6),
        Mapping::require(spec_var("rm".to_string())),
    );

    let reg_reg = Arm {
        variant: "RegReg".to_string(),
        args: ["rn", "rm"].map(String::from).to_vec(),
        body: Cases::Instruction(InstConfig {
            inst: inst(
                writable_xreg(4),
                AMode::RegReg {
                    rn: xreg(5),
                    rm: xreg(6),
                },
                MemFlags::new(),
            ),
            mappings: reg_reg_mappings,
        }),
    };

    // RegScaled
    let mut reg_scaled_mappings = mappings.clone();
    reg_scaled_mappings.reads.insert(
        aarch64::gpreg(5),
        Mapping::require(spec_var("rn".to_string())),
    );
    reg_scaled_mappings.reads.insert(
        aarch64::gpreg(6),
        Mapping::require(spec_var("rm".to_string())),
    );

    let reg_scaled = Arm {
        variant: "RegScaled".to_string(),
        args: ["rn", "rm"].map(String::from).to_vec(),
        body: Cases::Instruction(InstConfig {
            inst: inst(
                writable_xreg(4),
                AMode::RegScaled {
                    rn: xreg(5),
                    rm: xreg(6),
                },
                MemFlags::new(),
            ),
            mappings: reg_scaled_mappings,
        }),
    };

    // RegScaledExtended
    let extendops = [
        // Not supported by assembler: UXTB, UXTH, UXTX, SXTB, SXTH
        ExtendOp::UXTW,
        ExtendOp::SXTW,
        ExtendOp::SXTX,
    ];
    let mut reg_scaled_extended_mappings = mappings.clone();
    reg_scaled_extended_mappings.reads.insert(
        aarch64::gpreg(5),
        Mapping::require(spec_var("rn".to_string())),
    );
    reg_scaled_extended_mappings.reads.insert(
        aarch64::gpreg(6),
        Mapping::require(spec_var("rm".to_string())),
    );

    let reg_scaled_extended = Arm {
        variant: "RegScaledExtended".to_string(),
        args: ["rn", "rm", "extendop"].map(String::from).to_vec(),
        body: Cases::Match(Match {
            on: "extendop".to_string(),
            arms: extendops
                .into_iter()
                .map(|extendop| Arm {
                    variant: format!("{extendop:?}"),
                    args: Vec::new(),
                    body: Cases::Instruction(InstConfig {
                        inst: inst(
                            writable_xreg(4),
                            AMode::RegScaledExtended {
                                rn: xreg(5),
                                rm: xreg(6),
                                extendop,
                            },
                            MemFlags::new(),
                        ),
                        mappings: reg_scaled_extended_mappings.clone(),
                    }),
                })
                .collect(),
        }),
    };

    // RegExtended
    let mut reg_extended_mappings = mappings.clone();
    reg_extended_mappings.reads.insert(
        aarch64::gpreg(5),
        Mapping::require(spec_var("rn".to_string())),
    );
    reg_extended_mappings.reads.insert(
        aarch64::gpreg(6),
        Mapping::require(spec_var("rm".to_string())),
    );

    let reg_extended = Arm {
        variant: "RegExtended".to_string(),
        args: ["rn", "rm", "extendop"].map(String::from).to_vec(),
        body: Cases::Match(Match {
            on: "extendop".to_string(),
            arms: extendops
                .into_iter()
                .map(|extendop| Arm {
                    variant: format!("{extendop:?}"),
                    args: Vec::new(),
                    body: Cases::Instruction(InstConfig {
                        inst: inst(
                            writable_xreg(4),
                            AMode::RegExtended {
                                rn: xreg(5),
                                rm: xreg(6),
                                extendop,
                            },
                            MemFlags::new(),
                        ),
                        mappings: reg_extended_mappings.clone(),
                    }),
                })
                .collect(),
        }),
    };

    SpecConfig {
        term: term.to_string(),
        args: ["rd", "mem", "flags"].map(String::from).to_vec(),
        cases: Cases::Match(Match {
            on: "mem".to_string(),
            arms: vec![reg_reg, reg_scaled, reg_scaled_extended, reg_extended],
        }),
    }
}

fn flags_mappings() -> Mappings {
    // Instruction model is the MInst value itself, which is considered the result of the variant term.
    let inst = MappingBuilder::var("result").allow();

    // Input and output flags of the instruction are fields of the MInst model.
    let flags_in = inst.clone().field("flags_in");
    let flags_out = inst.clone().field("flags_out");

    // Construct read and write mappings for each NZCV field.
    let mut mappings = Mappings::default();
    for field in &["N", "Z", "C", "V"] {
        // Read
        mappings
            .reads
            .insert(pstate_field(field), flags_in.clone().field(field).build());

        // Write
        mappings
            .writes
            .insert(pstate_field(field), flags_out.clone().field(field).build());
    }

    mappings
}
