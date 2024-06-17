//! Verification Intermediate Representation for relevant types, eventually to
//! be lowered to SMT. The goal is to leave some freedom to change term
//! encodings or the specific solver backend.
//!
//! Note: annotations use the higher-level IR in annotation_ir.rs.
pub mod annotation_ir;

use std::collections::HashMap;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct TypeContext {
    pub tyvars: HashMap<Expr, u32>,
    pub tymap: HashMap<u32, Type>,
    pub tyvals: HashMap<u32, i128>,
    // map of type var to set index
    pub bv_unknown_width_sets: HashMap<u32, u32>,
}

// Used for providing concrete inputs to test rule semantics
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConcreteInput {
    // SMT-LIB-formatted bitvector literal
    pub literal: String,
    pub ty: Type,
}
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct ConcreteTest {
    pub termname: String,
    // List of name, bitvector literal, widths
    pub args: Vec<ConcreteInput>,
    pub output: ConcreteInput,
}

/// A bound variable, including the VIR type
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct BoundVar {
    pub name: String,
    pub tyvar: u32,
}

/// Verification type
#[derive(Clone, Debug, PartialEq, Eq, Hash, Copy)]
pub enum Type {
    /// The expression is a bitvector, currently modeled in the
    /// logic QF_BV https://SMT-LIB.cs.uiowa.edu/version1/logics/QF_BV.smt
    /// This corresponds to Cranelift's Isle type:
    /// (type Value (primitive Value))
    BitVector(Option<usize>),

    /// The expression is a boolean. This does not directly correspond
    /// to a specific Cranelift Isle type, rather, we use it for the
    /// language of assertions.
    Bool,

    /// The expression is an Isle type. This is separate from BitVector
    /// because it allows us to use a different solver type (e.h., Int)
    //. for assertions (e.g., fits_in_64).
    /// This corresponds to Cranelift's Isle type:
    /// (type Type (primitive Type))
    Int,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub struct TermSignature {
    pub args: Vec<Type>,
    pub ret: Type,

    // Which type varies for different bitwidth Values, that is, the type that
    // is used as a key for testing for that type.
    pub canonical_type: Option<Type>,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Terminal {
    Var(String),

    // Literal SMT value, for testing (plus type variable)
    Literal(String, u32),

    // Value, type variable
    Const(i128, u32),
    True,
    False,
    Wildcard(u32),
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum UnaryOp {
    // Boolean operations
    Not,

    // Bitvector operations
    BVNeg,
    BVNot,
}

#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum BinaryOp {
    // Boolean operations
    And,
    Or,
    Imp,
    Eq,
    Lte,
    Lt,

    // Bitvector operations
    BVSgt,
    BVSgte,
    BVSlt,
    BVSlte,
    BVUgt,
    BVUgte,
    BVUlt,
    BVUlte,

    BVMul,
    BVUDiv,
    BVSDiv,
    BVAdd,
    BVSub,
    BVUrem,
    BVSrem,
    BVAnd,
    BVOr,
    BVXor,
    BVRotl,
    BVRotr,
    BVShl,
    BVShr,
    BVAShr,

    BVSaddo,
}

/// Expressions (combined across all types).
#[derive(Clone, Debug, PartialEq, Eq, Hash)]
pub enum Expr {
    // Terminal nodes
    Terminal(Terminal),

    // Opcode nodes
    Unary(UnaryOp, Box<Expr>),
    Binary(BinaryOp, Box<Expr>, Box<Expr>),

    // Count leading zeros
    CLZ(Box<Expr>),
    A64CLZ(Box<Expr>, Box<Expr>),
    CLS(Box<Expr>),
    A64CLS(Box<Expr>, Box<Expr>),
    Rev(Box<Expr>),
    A64Rev(Box<Expr>, Box<Expr>),

    BVPopcnt(Box<Expr>),

    BVSubs(Box<Expr>, Box<Expr>, Box<Expr>),

    // ITE
    Conditional(Box<Expr>, Box<Expr>, Box<Expr>),

    // Switch
    Switch(Box<Expr>, Vec<(Expr, Expr)>),

    // Conversions
    // Extract specified bits
    BVExtract(usize, usize, Box<Expr>),

    // Concat bitvectors
    BVConcat(Vec<Expr>),

    // Convert integer to bitvector with that value
    BVIntToBV(usize, Box<Expr>),

    // Convert bitvector to integer with that value
    BVToInt(Box<Expr>),

    // Zero extend, with static or dynamic width
    BVZeroExtTo(usize, Box<Expr>),
    BVZeroExtToVarWidth(Box<Expr>, Box<Expr>),

    // Sign extend, with static or dynamic width
    BVSignExtTo(usize, Box<Expr>),
    BVSignExtToVarWidth(Box<Expr>, Box<Expr>),

    // Conversion to wider/narrower bits, without an explicit extend
    BVConvTo(Box<Expr>),
    BVConvToVarWidth(Box<Expr>, Box<Expr>),

    WidthOf(Box<Expr>),

    Load(Box<Expr>, Box<Expr>, Box<Expr>),
    Store(Box<Expr>, Box<Expr>, Box<Expr>, Box<Expr>),
}

/// To-be-flushed-out verification counterexample for failures
#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Counterexample {}

/// To-be-flushed-out verification result
#[derive(Clone, Debug, PartialEq, Eq)]
pub enum VerificationResult {
    InapplicableRule,
    Success,
    Failure(Counterexample),
    Unknown,
    // Optional: heuristic that a rule is bad if there is only
    // a single model with distinct bitvector inputs
    NoDistinctModels,
}
