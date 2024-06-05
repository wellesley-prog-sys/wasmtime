use cranelift_codegen_meta as meta;
use cranelift_isle as isle;
use easy_smt::SExpr;
use isle::compile::create_envs;
use isle::sema::{TermEnv, TypeEnv};
use meta::isa::Isa;
use std::path::PathBuf;

pub mod annotations;
pub mod interp;
pub mod solver;
pub mod termname;
pub mod type_inference;
pub mod verify;

pub const REG_WIDTH: usize = 64;

// Use a distinct with as the maximum width any value should have within type inference
pub const MAX_WIDTH: usize = 2 * REG_WIDTH;

pub const FLAGS_WIDTH: usize = 4;

pub const WIDTHS: [usize; 4] = [8, 16, 32, 64];

pub struct Config {
    pub term: String,
    pub names: Option<Vec<String>>,
    pub dyn_width: bool,
    pub distinct_check: bool,

    // Closure arguments: SMT context, arguments to the term, lhs, rhs
    pub custom_verification_condition:
        Option<Box<dyn Fn(&easy_smt::Context, Vec<SExpr>, SExpr, SExpr) -> SExpr>>,
    // Closure arguments: SMT context, arguments to the term
    pub custom_assumptions: Option<Box<dyn Fn(&easy_smt::Context, Vec<SExpr>) -> SExpr>>,
}

impl Config {
    pub fn with_term_and_name(term: &str, name: &str) -> Self {
        Config {
            dyn_width: false,
            term: term.to_string(),
            distinct_check: true,
            custom_verification_condition: None,
            custom_assumptions: None,
            names: Some(vec![name.to_string()]),
        }
    }
}

/// Given a file, lexes and parses the file to an ISLE term and type environment tuple
pub fn isle_files_to_terms(files: &Vec<PathBuf>) -> (TypeEnv, TermEnv) {
    let lexer = isle::lexer::Lexer::from_files(files).unwrap();
    parse_isle_to_terms(lexer)
}

/// Produces the two ISLE-defined structs with type and term environments
pub fn parse_isle_to_terms(lexer: isle::lexer::Lexer) -> (TypeEnv, TermEnv) {
    // Parses to an AST, as a list of definitions
    let defs = isle::parser::parse(lexer).expect("should parse");

    // Produces environments including terms, rules, and maps from symbols and
    // names to types
    create_envs(&defs).unwrap()
}

pub fn build_clif_lower_isle() -> PathBuf {
    // Build the relevant ISLE prelude using the meta crate
    let out_dir = std::path::Path::new(&"veri-isle-clif-gen");
    let isle_dir = std::path::Path::new(&out_dir);

    if isle_dir.is_dir() {
        let clif_lower_isle = isle_dir.join("clif_lower.isle");
        if clif_lower_isle.is_file() {
            return clif_lower_isle;
        }
    }
    std::fs::create_dir_all(isle_dir)
        .expect("Could not create directory for CLIF ISLE meta-generated code");

    // For now, build ISLE files for x86 and aarch64
    let isas = vec![Isa::X86, Isa::Arm64];

    if let Err(err) = meta::generate(&isas, &out_dir, isle_dir) {
        panic!("Meta generate error: {}", err);
    }

    PathBuf::from(isle_dir.join("clif_lower.isle"))
}
