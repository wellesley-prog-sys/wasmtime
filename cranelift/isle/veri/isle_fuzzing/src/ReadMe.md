# Wellesley Programming System Lab and CS 340 Final Project: Creating a Fuzzer to randomly generate clif code from isle rules

Written by Annette Chau and Becky Chen

## Code Structure

- **expr.rs**
- **instructions.txt**
- **big.isle**: An Example Isle File to show that it works. This can be replaced with any ISLE file. 

## Enums and Structs

- **Expr**: Enumeration representing different types of expressions, including variables, integers, instructions, and non-instructions.
- **Ident**: Structure representing an identifier with its position.
- **Inst**: Structure representing an instruction with its name and arguments.
- **NotAnInst**: Structure representing a non-instruction with its name and arguments.

## Functions

- **ident_string**: Helper function to convert an identifier to a string.
- **convert_pattern**: Converts ISLE patterns into expressions based on the type environment.
- **lines_from_file**: Reads lines from a file and returns them as a vector of strings.
- **convert_rules**: Parses ISLE file into rules and converts them into expressions.
- **to_clif_list**: Converts expressions to CLIF instructions.
- **format_output**: Formats the CLIF program output.

## Usage
To use this code, run: cargo run --bin expr. Be sure the filepath looks similar to: wasmtime/cranelift/isle/veri/isle_fuzzing/src
The imports in the Cargo.toml file include: 
- **cranelift-isle = { path = "../../isle" }**
- **rand = "0.8.4"**
-**regex = "1"**

To test the fuzzer, go back to wasmtime/cranelift/isle and run this command in the terminal: 
cargo run -- compile --target aarch64 -D {custompath}/wasmtime/cranelift/isle/veri/isle_fuzzing/src/test.clif

Note that this only works for Macs with the ARM Chips (M1, M2, M3 or M4), and test.clif is manually created, where the user has copied and pasted the output of the code into a clif file. 

## General Problem

When instruction lowering from clif to a low level language (ARM or x86), guided by ISLE, occasionally ISLE rules guide to lower have errors, leading to vulnerabilities, such as CVE-2023-26489: https://nvd.nist.gov/vuln/detail/CVE-2023-26489

A fuzzer is one way to find inputs that the ISLE rules guide has difficulty lowering accurately by randomly generating outputs that are not always expected. This can help find bugs that were not anticipated. 

## Tradeoffs and Assumptions

We tried different parsing methods (seen in the misc folder/main.rs), where we attempted to find rules through parenthesis counting instead of through the patterns given. 

Currently our fuzzer inefficiently, where we must input the name of the file into the code and the user must copy and paste the generated clif file from the terminal and run it manually through Cranelift. In the future, we want to have it run automatically and take in multiple files. This was our reach. 

This fuzzer is also not foolproof-- it occasionally generates invalid clif programs due to the extension (.i16, .i32, .i64) not always being compatible throghout the program. 

## Next Steps
We want to be able to detect when the program can be lowered successfully.
We also want to generate multiple programs at a time. 
We also want to automate the entire process. 