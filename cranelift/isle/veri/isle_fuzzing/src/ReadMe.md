# Wellesley Programming System Lab and CS 340 Final Project: Creating a Fuzzer to randomly generate clif code from isle rules

Written by Annette Chau and Becky Chen

## Code Structure

- **expr.rs**: Code for parsing ISLE rule and randomly generating clif module that matches the input ISLE rule. Our main file.
- **instructions.txt**: A list of instructions
- **amod_unextended.isle**: The target ISLE rule we are trying to fuzz. The main input to expr.rs.
- **automate_script.sh** (in isle_fuzzing): The shell script to automate the process of generating an clif matching the input ISLE rule, and running said clif file through Cranelift to get the lowered ARM code. 

## Enums and Structs

- **Expr**: Enumeration representing different types of expressions, including variables, integers, instructions, and non-instructions.
- **Ident**: Structure representing an identifier.
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
First, install and setup RUST following this link: https://www.rust-lang.org/tools/install.

**Manual Use**

To use expr.rs for clif module generation, run this command in the terminal: 
```
cargo run --bin expr
```
This will write to the terminal an (may or may not) executable clif module. This uncertainty is normal and natural, as it is one of the characteristics of random generation. Keep running the command until you get a clif module that is executable following the instructions below.

Be sure the filepath you are in looks similar to: wasmtime/cranelift/isle/veri/isle_fuzzing/src
The imports in the Cargo.toml file include: 
- **cranelift-isle = { path = "../../isle" }**
- **rand = "0.8.4"**
-**regex = "1"**

Copy this generated code into a new file named 'test.clif', in wasmtime/cranelift/isle/veri/isle_fuzzing/src.

To test the fuzzer and run the code generated above, go back to wasmtime/cranelift/isle and run this command in the terminal: 
```
cargo run -- compile --target aarch64 -D {custompath}/wasmtime/cranelift/isle/veri/isle_fuzzing/src/test.clif
```

**Automation**

Make sure you are in wasmtime/cranelift/isle/veri/isle_fuzzing/.

First, update the paths at the top of automate_script.sh with your local paths:

- **EXPR_RS_PATH** is where expr.rs is located. 
- **EXPR_DIR** is the folder where expr.rs is located
- **TEST_CLIF_PATH** should be the path to the generated code stored in testauto.clif file in wasmtime/cranelift/ folder
- **DIR_PATH** should be the path to your local wasmtime/cranelift/ folder

Then, run this command in the terminal to make sure the script is executable:
```
chmod +x automate_script.sh
```
Last, run the script with:
```
./automate_script.sh
```
This will automatically go through the steps described in the manual use section, store the generated clif module in wasmtime/cranelift/isle/veri/isle_fuzzing/testauto.clif, and keep generating clif and run it through cranelift until an executable one is generated. This might take slightly more time, due to the nature of random generation generating invalid and unexecutable code. Please wait patiently until everything stops.

Then, you will see the ISLE-guided, lowered ARM module in the terminal. This result can then be used to manually check whether there were errors in the ISLE rule that led to inaccurate lowering. 


**Limitation**

Note that this only works for Macs with the ARM Chips (M1, M2, M3 or M4).

## General Problem

When instruction lowering from clif to a low level language (ARM or x86), guided by ISLE, occasionally ISLE rules guide to lower have errors, leading to vulnerabilities, such as CVE-2023-26489: https://nvd.nist.gov/vuln/detail/CVE-2023-26489

A fuzzer is one way to find inputs that the ISLE rules guide has difficulty lowering accurately by randomly generating outputs that are not always expected. This can help find bugs that were not anticipated. 

## Tradeoffs, Limitations, Assumptions

We tried different parsing methods (seen in the misc folder/main.rs), where we attempted to find rules through parenthesis counting instead of through the patterns given. 

Currently our fuzzer has a limited scope, where we can only generate one clif module that matches one single input ISLE rule at once. In the future, we want to have it take in multiple files of ISLE rules. This will speed up the fuzzing process and generate code with more variety and sophisticated patterns. 

This fuzzer is also not foolproof-- it occasionally generates invalid clif programs due to the extension (.i16, .i32, .i64) not always being compatible throughout the program. This is the tradeoff of using random generation. Our automatic approach partially tackles this issue. However, it still takes more than the ideal amount of time to generate an executable clif module.

## Next Steps
We want to be able to detect when the program can be lowered successfully. This would be more effective from a fuzzing perspective, to have the output efficiently checked against expected output.

We also want to take in and generate multiple programs at a time, hopefully through more advanced automation.

## Goals and Reflection
Our goals did not change from our proposal. Quite the opposite, we were able to have a much clearer train of thought after receiving feedback for our proposal. We clarified our goals and tried our best to execute them. 

We have completed most of our reach goals. 
