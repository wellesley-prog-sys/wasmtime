# Crocus: An SMT-based ISLE verification tool

This directory contains Crocus, a tool for verifying instruction lowering and transformation rules written in ISLE. Crocus uses an underlying SMT solver to model ISLE rules in as logical bitvectors; searching over all possible inputs to find potential soundness counterexamples.

Currently[^1], Crocus requires every ISLE term uses within a rule to have a user-provided specification, or `spec`, that provides the logical preconditions and effects of the term (`require` and `provide` blocks).
The syntax for these specs is embedded as an optional extension to ISLE itself: specs are written in the ISLE source files. 

[^1]: We have work in progress to lower this annotation burden.

## Running on an individual rule

The easiest way to run Crocus on an individual ISLE rule is to give that rule a name. 

For example, to verify the following `aarch64` rule:

```
(rule -1 (lower (has_type (fits_in_64 ty) (band x y)))
    (alu_rs_imm_logic_commutative (ALUOp.And) ty x y))
```

We can add a name (before the priority):
```
(rule band_fits_in_64 -1 (lower (has_type (fits_in_64 ty) (band x y)))
      (alu_rs_imm_logic_commutative (ALUOp.And) ty x y))
```

We also require that the relevant (outermost) CLIF term on the left hand side has a "type instantiation" to specify the types, e.g. bitwidths, we are interested in verifying. In this case, this is provided with:

```
(form
  bv_binary_8_to_64
  ((args (bv  8) (bv  8)) (ret (bv  8)) (canon (bv  8)))
  ((args (bv 16) (bv 16)) (ret (bv 16)) (canon (bv 16)))
  ((args (bv 32) (bv 32)) (ret (bv 32)) (canon (bv 32)))
  ((args (bv 64) (bv 64)) (ret (bv 64)) (canon (bv 64)))
)

(instantiate band bv_binary_8_to_64)
```


We can then invoke the rule with the following, using `-t` or `--term` to specify the relevant CLIF instruction and `--names` to specify the name of the rule:

```
cargo run -- --aarch64 -t band --names band_fits_in_64
```

With the expected output:

```
Writing generated file: /Users/avh/research/wasmtime/cranelift/isle/veri/veri_engine/output/clif_opt.isle
Writing generated file: /Users/avh/research/wasmtime/cranelift/isle/veri/veri_engine/output/clif_lower.isle
Verification succeeded for band_fits_in_64, width 8
Verification succeeded for band_fits_in_64, width 16
Verification succeeded for band_fits_in_64, width 32
Verification succeeded for band_fits_in_64, width 64
```

If the rule was unsound, this will report counterexamples. For instance, if we change the rule to the following:

```
(rule band_fits_in_64 -1 (lower (has_type (fits_in_64 ty) (band x y)))
      (alu_rs_imm_logic_commutative (ALUOp.Or) ty x y))
```

Then the output would include counterexamples, like so:

```
Verification failed for band_fits_in_64, width 8
Counterexample summary
(lower (has_type (fits_in_64 [ty|8]) (band [x|#x01|0b00000001] [y|#x00|0b00000000])))
=>
(output_reg (alu_rs_imm_logic_commutative (ALUOp.Orr) [ty|8] [x|#x01|0b00000001] [y|#x00|0b00000000]))

#x00|0b00000000 =>
#x01|0b00000001

Failed condition:
(= ((_ extract 7 0) lower__13) ((_ extract 7 0) output_reg__16))
```

## The annotation language

The annotation maps closely to [SMT-LIB](https://smt-lib.org) theories of bitvectors and booleans, with a several added conveniences. 

### Top-level constructs

We extend the ISLE parser with the following top-level constructs:

- `model` specifies how an ISLE type maps to an SMT type. For example, the follow ISLE type definitions along with their models specify how booleans and `u8`s are modeled:
```
(model u8 (type (bv 8)))
(type u8 (primitive u8))
(model bool (type Bool))
(type bool (primitive bool))
```

Models can be `Bool`, `Int`, or `(bv)` with or without a specific bitwidth. If the bitwidth is not provided, Crocus type inference will verify the rule with all possible inferred widths 

- As in the example above, `instantiate` and `form` specify what type instantiations should be considered for a verification. 

- `spec` terms provide specifications for ISLE declarations, which can correspond to ISLE instructions, ISA instructions, external constructors/extractors defined in Rust, or transient, ISLE-only terms.

### General SMT-LIB terms

The following terms exactly match their general SMT-LIB meaning:

- `=`: equality
- `and`: boolean and
- `or`: boolean or
- `not`: boolean negation
- `=>`: boolean implication

### Integer operations

The following terms exactly match the  [SMT-LIB theories `Int`](https://smt-lib.org/theories-Ints.shtml).

- `<`
- `<=`
- `>`
- `>=`

In specs, integer operations are primarily used for comparing the number of bits in an ISLE type.

### Bitvector operations

The following terms exactly match [SMT-LIB theory `FixedSizeBitVectors`](https://smt-lib.org/theories-FixedSizeBitVectors.shtml). 

There operations are typically used in specs for any operations on ISLE `Value`s.

- `bvnot`
- `bvand`
- `bvor`
- `bvxor`
- `bvneg`
- `bvadd`
- `bvsub`
- `bvmul`
- `bvudiv`
- `bvurem`
- `bvsdiv`
- `bvsrem`
- `bvshl`
- `bvlshr`
- `bvashr`
- `bvsaddo`
- `bvule`
- `bvult`
- `bvugt`
- `bvuge`
- `bvslt`
- `bvsle`
- `bvsgt`
- `bvsge`

### Custom bitvector operations

- `int2bv`
- `bv2int`
- `extract`
- `zero_ext`
- `sign_ext`
- `rotr`
- `rotl`
- `concat`
- `widthof`
- `subs`
- `popcnt`
- `rev`
- `cls`
- `clx`
- `convto`

### Custom memory operations

- `load_effect`
- `store_effect`


### Custom control operation

- `switch`

## Testing

To see an all of our current output, run tests without capturing standard out:
```bash
cargo test -- --nocapture
```

To run a specific test, you can provide the test name (most rules are tested in `cranelift/isle/veri/veri_engine/tests/veri.rs`). Set `RUST_LOG=DEBUG` to see more detailed output on test cases that expect success.

```bash
RUST_LOG=DEBUG cargo test test_named_band_fits_in_64 -- --nocapture  
```

To see the x86-64 CVE repro, run:

```bash
RUST_LOG=debug cargo run -- --noprelude -t amode_add -i examples/x86/amode_add_uextend_shl.isle
```

To see the x86-64 CVE variant with a 32-bit address, run:
```bash
RUST_LOG=debug cargo run -- --noprelude -t amode_add -i examples/x86/amode_add_shl.isle
```