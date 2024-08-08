//! Disassembly tests.

use pulley_interpreter::*;

fn encoded(ops: &[Op]) -> Vec<u8> {
    let mut encoded = vec![];
    for op in ops {
        op.encode(&mut encoded);
    }
    log::trace!("encoded: {encoded:?}");
    encoded
}

fn assert_disas(ops: &[Op], expected: &str) {
    let expected = expected.trim();
    eprintln!("=== expected ===\n{expected}");

    let bytecode = encoded(ops);

    let actual = disas::Disassembler::disassemble_all(&bytecode).expect("decoding failed");
    let actual = actual.trim();
    eprintln!("=== actual ===\n{actual}");

    assert_eq!(expected, actual);
}

#[test]
fn simple() {
    let x0 = XReg::new(0).unwrap();
    let x1 = XReg::new(1).unwrap();
    let x31 = XReg::new(31).unwrap();

    assert_disas(
        &[
            // Prologue.
            Op::Xconst8(Xconst8 {
                dst: x31,
                imm: -16i8,
            }),
            Op::Xadd32(Xadd32 {
                dst: XReg::SP,
                src1: XReg::SP,
                src2: x31,
            }),
            Op::Store64Offset8(Store64Offset8 {
                ptr: XReg::SP,
                offset: 8,
                src: XReg::LR,
            }),
            Op::Store64(Store64 {
                ptr: XReg::SP,
                src: XReg::FP,
            }),
            Op::Xmov(Xmov {
                dst: XReg::FP,
                src: XReg::SP,
            }),
            // Function body.
            Op::Xadd32(Xadd32 {
                dst: x0,
                src1: x0,
                src2: x1,
            }),
            // Epilogue.
            Op::Xmov(Xmov {
                dst: XReg::SP,
                src: XReg::FP,
            }),
            Op::Load64Offset8(Load64Offset8 {
                dst: XReg::LR,
                ptr: XReg::SP,
                offset: 8,
            }),
            Op::Load64(Load64 {
                dst: XReg::FP,
                ptr: XReg::SP,
            }),
            Op::Xconst8(Xconst8 { dst: x31, imm: 16 }),
            Op::Xadd32(Xadd32 {
                dst: XReg::SP,
                src1: XReg::SP,
                src2: x31,
            }),
            Op::Ret(Ret {}),
        ],
        r#"
       0: 0e 1f f0                        xconst8 x31, -16
       3: 12 20 20 1f                     xadd32 sp, sp, x31
       7: 29 20 08 21                     store64_offset8 sp, 8, lr
       b: 27 20 22                        store64 sp, fp
       e: 0b 22 20                        xmov fp, sp
      11: 12 00 00 01                     xadd32 x0, x0, x1
      15: 0b 20 22                        xmov sp, fp
      18: 25 21 20 08                     load64_offset8 lr, sp, 8
      1c: 22 22 20                        load64 fp, sp
      1f: 0e 1f 10                        xconst8 x31, 16
      22: 12 20 20 1f                     xadd32 sp, sp, x31
      26: 00                              ret
        "#,
    );
}
