use cranelift_codegen::isa::aarch64::inst::Inst;
use cranelift_isle_veri_aslp::{ast::Block, client::Client};
use tracing::debug;

use crate::aarch64;

// Fetch semantics for the given Cranelift instruction.
pub fn inst_semantics(inst: &Inst, client: &Client) -> anyhow::Result<Block> {
    // Assemble instruction.
    let opcode = aarch64::opcode(inst);

    // Debugging.
    let asm = aarch64::assembly(inst);

    debug!("inst = {inst:#?}");
    debug!("opcode = {opcode:08x}");
    debug!("asm = {asm}");

    // Fetch semantics.
    client.opcode(opcode)
}
