# Formally Verifying Cranelift Instruction Lowering in Wasmtime

> This is the README file of the final project done for the class **CS340: Modelling Computer Systems** in Spring 2025.

[![slides](https://img.shields.io/badge/slides–Canva-blue)](https://www.canva.com/design/DAGm1Yk1pi4/w7E4R6LufXC2QbM3v_mwzg/edit)

---

## 📖 Overview

This project extends the Wasmtime/Cranelift codegen pipeline by integrating ISLE-based specifications for x86-64 instruction-lowering rules, checked with an SMT solver. We:

1. **Identify** critical instruction-lowering rules (e.g., loads, stores, shifts, ALU ops).
2. **Specify** each rule and helper in ISLE `spec` format.
3. **Verify** semantic equivalence between high-level IR and lowered instructions using Z3.

We formally verified correctness for the following rules:

- `load_narrow`
- `store_narrow`
- `Xor_mem_reg`
- `left_shift_small`

---

## 🚀 Getting Started

### Prerequisites

- Rust ≥ 1.68  
- [Z3 SMT Solver](https://github.com/Z3Prover/z3) in your system `PATH`

### Clone Repository

```bash
git clone https://github.com/Winniezyt/wasmtime.git
cd wasmtime
```

### Running the Verifier  

Navigate to the verifier tool:

```bash 
cd cranelift/isle/veri/veri 
```

Run verification for a specific rule (replace `<rule_name>` accordingly):

```bash 
./script/veri.sh -a x64 -- --filter include:rule:<rule_name> --solver z3
```

Successful verification will display:

``` text
type solution status = solved
applicability = applicable
verification = success
```
**Note**: It is normal to see `inapplicableConflict` messages due to enumerating combinations of input types; these can be ignored.

## 🐞 Debugging Verification Issues 

When you run the Cranelift‑ISLE verifier on a rule, you may encounter three different outcomes indicating a problem. Here’s how to recognize and diagnose each one:

---

### 1. **Concrete Type Error**

```text
Error: concrete type error between types:
        bv 16
        int
```
**Cause**: Mismatched operand types (e.g., bitvector and integer).

**Fix**: Use conversion functions like conv_to or zero_ext to match types.

### 2. **Rule Is 'Inapplicable'** 

```
<rule_name>
                type solution status = inapplicable
```
**Cause**: Rule conditions are mutually exclusive or unsatisfiable.

**Fix**: Use implications instead of conjunctions for optional checks. Review `require/provide` block. 

### 3. **Counterexample (Verification Failure)** 

```
#928    <rule_name>
    ...
        type solution status = solved
        applicability = applicable
        verification = failure

model:
  ...

Error: ... 
Caused by: verification failed
```
**Cause**: Rule matched, but LHS ≠ RHS.

**Fix**: Trace model step-by-step and compare helper output to find divergence. 

### Debugging tool 
Enable detailed logging with:

``` bash
RUST_BACKTRACE=1 RUST_LOG=DEBUG ./script/veri.sh -a x64 -- --filter include:rule:load_narrow --solver z3 --debug 
```

---

## 📚 Resources

- **ISLE language reference**
https://github.com/bytecodealliance/wasmtime/blob/main/cranelift/isle/docs/language-reference.md

- **Instruction specs in Cranelift**
`cranelift/codegen/meta/src/shared/instructions.rs`

- VeriISLE aarch64 Paper (OOPSLA’24): https://dl.acm.org/doi/10.1145/3617232.3624862

---
## 📓 Development Journal

### 1. `load_narrow` (Rule ID: –4) 

``` isle
 (rule load_narrow -4 
    (lower 
        (has_type 
            (and 
                (fits_in_32 ty) (is_gpr_type _)) 
        (load _ flags address offset)))
    (x64_movzx 
        (ext_mode
            (ty_bits_u16 ty) 64) 
        (to_amode flags address offset)))
```

For this rule, I wrote specs for the following declaration statements:

- write spec for the following declaration statements: 
    1. `is_gpr_type`
    2. `ty_bits_u16` 
    3. `ext_mode` 
    4. `synthetic_amode_to_reg_mem` 
    5. `to_amode` (non-extern, chaining)
    6. `GprMem` (model)
    7. `reg_mem_to_gpr_mem`
    8. `MovzxRmR` 


### 2. `store_narrow` (Rule ID: -2)

```

(rule store_narrow -2
      (lower (store flags
                    value @ (value_type (is_gpr_type ty))
                    address
                    offset))
      (side_effect
             (x64_movrm ty
                        (to_amode flags address offset)
                        value)))
```

- write spec for the following declaration statements:

1. `raw_operand_size_of_type`
2. `MInst.MovRM` 

### 3. `Xor_mem_reg` (Rule ID:3)

```
(rule Xor_mem_reg 3
      (lower
           (store flags
                  (has_type (ty_32_or_64 ty)
                            (bxor _ (and
                                        (sinkable_load sink)
                                        (load _ flags addr offset))
                                    src2))
                  addr
                  offset))
      (let
          ((_ RegMemImm sink))
          (side_effect
                 (x64_xor_mem ty
                              (to_amode flags addr offset)
                              src2))))
```

- write spec for the following declaration statements:
1. `SinkableLoad` (model)
2. `sinkable_load`
3. `MInst.AluRm`
4. `sink_load`
5. `RegMemImm.Mem`

### 4. `left_shift_small` (Rule ID: -1)
```
 (rule left_shift_small -1
        (lower
                (has_type
                      (fits_in_64 ty)
                      (ishl _ src amt)))
        (x64_shl ty
                 src
                (put_masked_in_imm8_gpr amt ty)))
```

- write spec for the following declaration statements:

1. `Imm8Gpr` (model)
2. `put_masked_in_imm8_gpr` ( chain)
3. `const_to_type_masked_imm8`
4. `x64_shl`
5. `gpr_to_imm8_gpr`
6. `shift_mask`
7. `RegMemImm.Imm`

**Note**: `x64_shl` is non-extern. Writing a full chain may cause solver timeouts.
