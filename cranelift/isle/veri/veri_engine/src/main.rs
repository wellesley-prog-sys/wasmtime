//! Prototype verification tool for Cranelift's ISLE lowering rules.

use clap::{ArgAction, Parser};
use std::env;
use std::path::PathBuf;
use veri_engine_lib::verify::verify_rules;
use veri_engine_lib::{build_clif_lower_isle, Config};

#[derive(Parser)]
#[clap(about, version, author)]
struct Args {
    /// Sets the input file
    #[clap(short, long)]
    input: Option<String>,

    /// Which LHS root to verify
    #[clap(short, long, default_value = "lower")]
    term: String,

    /// Which width types to verify
    #[clap(long)]
    widths: Option<Vec<String>>,

    /// Which named rule to verify
    #[clap(long)]
    names: Option<Vec<String>>,

    /// Don't use the prelude ISLE files
    #[clap(short, long, action=ArgAction::SetTrue)]
    noprelude: bool,

    /// Include the aarch64 files
    #[clap(short, long, action=ArgAction::SetTrue)]
    aarch64: bool,

    /// Include the x64 files
    #[clap(short, long, action=ArgAction::SetTrue)]
    x64: bool,

    /// Don't check for distinct possible models
    #[clap(long, action=ArgAction::SetTrue)]
    nodistinct: bool,

    /// Allow dynamic widths for the solver query
    #[clap(short, long, action=ArgAction::SetTrue)]
    dynwidths: bool,
}

fn main() {
    env_logger::init();

    let cur_dir = env::current_dir().expect("Can't access current working directory");

    let args = Args::parse();
    let mut inputs = vec![];

    let valid_widths = ["I8", "I16", "I32", "I64"];

    if let Some(widths) = &args.widths {
        for w in widths {
            let w_str = w.as_str();
            if !valid_widths.contains(&w_str) {
                panic!("Invalid width type: {}", w);
            }
        }
    }

    if !args.noprelude {
        // Build the relevant ISLE prelude using the meta crate
        inputs.push(build_clif_lower_isle());

        // TODO: clean up path logic
        inputs.push(cur_dir.join("../../../codegen/src").join("inst_specs.isle"));
        inputs.push(cur_dir.join("../../../codegen/src").join("prelude.isle"));
        inputs.push(
            cur_dir
                .join("../../../codegen/src")
                .join("prelude_lower.isle"),
        );
    }

    if args.aarch64 {
        inputs.push(
            cur_dir
                .join("../../../codegen/src/isa/aarch64")
                .join("inst.isle"),
        );
        inputs.push(
            cur_dir
                .join("../../../codegen/src/isa/aarch64")
                .join("lower.isle"),
        );
        if args.input.is_some() {
            panic!("Cannot specify both input file and aarch64 mode.")
        }
    } else if args.x64 {
        inputs.push(
            cur_dir
                .join("../../../codegen/src/isa/x64")
                .join("inst.isle"),
        );
        inputs.push(
            cur_dir
                .join("../../../codegen/src/isa/x64")
                .join("lower.isle"),
        );
        if args.input.is_some() {
            panic!("Cannot specify both input file and x64 mode.")
        }
    } else {
        if let Some(i) = args.input {
            inputs.push(PathBuf::from(i));
        } else {
            panic!("Missing input file in non-aarch64 mode");
        }
    }

    let names = if let Some(names) = args.names {
        let mut names = names;
        names.sort();
        names.dedup();
        Some(names)
    } else {
        None
    };

    let config = Config {
        dyn_width: args.dynwidths,
        term: args.term,
        names: names,
        distinct_check: !args.nodistinct,
        custom_verification_condition: None,
        custom_assumptions: None,
    };

    verify_rules(inputs, &config, &args.widths)
}
