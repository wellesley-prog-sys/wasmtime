# VeriISLE

VeriISLE is an in-development research prototype verifier for the ISLE language.
It builds upon the Crocus verifier, adding support for automatic rule chaining
and authoritative ISA semantics.

## Dependencies

To run the verifier you will need a backend SMT2 solver installed. The `cvc5`
solver is currently preferred.

On MacOS, you can install via homebrew:

```
brew install cvc5/homebrew-cvc5/cvc5
```

Alternatively, on Linux or MacOS you can install from Github release with:

```
./veri/script/install/cvc5.sh -i <install_path>
```

If you use this method, ensure that `<install_path>/bin` is on your `$PATH`.

## Running

To run the verifier, from the `cranelift/isle/veri/veri` directory run:

```
./script/veri.sh
```

This will run verification on the default AArch64 backend. To run on the X64
backend, add the `-a x64` option.  The tool uses the presence of rule names as
an indication of whether to attempt verification.  Specifically, it will run
verification for all rule expansions such that the first expanded rule has a
name.

During development you may want to focus on a subset of expansions. The
following command can be used to limit to all expansions that involve named rule
`<rule>`:

```
./script/veri.sh -- --rule <rule>
```

## ISA Specifications

Where possible we derive ISA specifications in VeriISLE format from
authoritative specifications distributed by vendors. Currently this is only
possible for the AArch64 backend, with specifications derived from ARM's Machine
Readable Specification in Architecture Specification Language (ASL). We rely on
the [ASLp](https://github.com/UQ-PAC/aslp) tool to assist with distilling down
the original verbose specifications to usable semantics for verification.

The resulting ISA specifications are checked in to the repository, so there is
no requirement to install ASLp unless you want to alter existing or derive more
specifications with it.

### Generating ISA Specifications

To run ISA specification generation, you will first need to install ASLp:

1.  [Install `opam`](https://opam.ocaml.org/doc/Install.html), the OCaml Package
    Manager. The "Binary distribution" method is recommended.
2.  Install ASLp with `./veri/script/install/aslp.sh -i <aslp_install_path>`.
3.  Ensure ASLp tools are available by adding `<aslp_install_path>/bin` to your
    `PATH`.

To run ISA specification generation, from the `isaspec` directory run:

```
./script/generate.sh -l
```

This will:

1.  Launch an instance of the `aslp-server`. Communicating with ASLp over a
    server connection allows us to pay the initialization cost of reading the
    large ASL specification once.
2.  Build and execute the `isaspec` tool.
3.  Write outputs to the `cranelift/codegen/src/isa/aarch64/spec/` directory.

On a clean checkout this should be a no-op, and in fact we have CI tests that
verify this.
