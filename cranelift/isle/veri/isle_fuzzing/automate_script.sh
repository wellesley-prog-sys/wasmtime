#!/bin/bash

EXPR_RS_PATH="C:/Users/becky/Documents/GitHub/wasmtime/cranelift/isle/veri/isle_fuzzing/src/expr.rs"

TEST_CLIF_PATH="C:/Users/becky/Documents/GitHub/wasmtime/cranelift/isle/veri/isle_fuzzing/src/testauto.clif"

DIR_PATH="C:/Users/becky/Documents/GitHub/wasmtime/cranelift/"

# compile and run expr.rs, and get the output
TEST_CLIF=$(cargo run --bin expr)

# write the output program to testauto.clif file in the cranelift dir
echo "$TEST_CLIF" > "$TEST_CLIF_PATH"


# cd back to cranelift dir
cd "$DIR_PATH"

cargo run -- compile --target aarch64 -D testauto.clif




# to run this script
# first chmod +x automate_script.sh to make sure it is executable
# run the script: ./automate_script.sh