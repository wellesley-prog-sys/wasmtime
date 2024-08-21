# Crocus: An SMT-based ISLE verification tool

This directory contains Crocus, a tool for verifying instruction lowering and transformation rules written in ISLE. Crocus uses an underlying SMT solver to model ISLE rules in as logical bitvectors; searching over all possible inputs to find potential soundness counterexamples.

Currently[^1], Crocus requires every ISLE term uses within a rule to have a user-provided specification, or `spec`, that provides the logical preconditions and effects of the term (`require` and `provide` blocks).
The syntax for these specs is embedded as an optional extension to ISLE itself: specs are written in the ISLE source files. 

[^1]: We have work in progress to lower this annotation burden.

## Running on a file


## Testing

To see an examples of our current output, run tests without capturing standard out:
```bash
cargo test -- --nocapture
```

To run a specific rule, you can provide the test name (most rules are tested in `cranelift/isle/veri/veri_engine/tests/veri.rs`):

```bash
cargo test test_named_band_64 -- --nocapture  
```

To see the x86-64 CVE repro, run:

```bash
cargo run -- --noprelude -t amode_add -i examples/x86/amode_add_uextend_shl.isle
```

To see the x86-64 CVE variant with a 32-bit address, run:
```bash
cargo run -- --noprelude -t amode_add -i examples/x86/amode_add_shl.isle
```