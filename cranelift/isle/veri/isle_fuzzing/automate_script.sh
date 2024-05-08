EXPR_RS_PATH="/Users/ajchau/Documents/GitHub/wasmtime/cranelift/isle/veri/isle_fuzzing/src/expr.rs"

TEST_CLIF_PATH="/Users/ajchau/Documents/GitHub/wasmtime/cranelift/testauto.clif"

DIR_PATH="/Users/ajchau/Documents/GitHub/wasmtime/cranelift/"

# compile and run expr.rs, and get the output
TEST_CLIF=$(cargo run --bin expr)

# write the output program to testauto.clif file in the cranelift dir
echo "$TEST_CLIF" > "$TEST_CLIF_PATH"


# cd back to cranelift dir
cd "$DIR_PATH"

cargo run -- compile --target aarch64 -D testauto.clif



# to run this script
# first substitute the paths with your local paths. 
# EXPR_RS_PATH is where expr.rs is located. 
# TEST_CLIF_PATH should be the path to the generated code stored in testauto.clif file in cranelift folder
# DIR_PATH should be the path to your local cranelift folder                                              
# then chmod +x automate_script.sh to make sure it is executable
# run the script: ./automate_script.sh
