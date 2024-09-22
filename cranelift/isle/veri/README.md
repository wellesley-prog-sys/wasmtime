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
