EXPR_RS_PATH="/Users/ajchau/Documents/GitHub/wasmtime/cranelift/isle/veri/isle_fuzzing/src/expr.rs"
EXPR_DIR="/Users/ajchau/Documents/GitHub/wasmtime/cranelift/isle/veri/isle_fuzzing/src/"
TEST_CLIF_PATH="/Users/ajchau/Documents/GitHub/wasmtime/cranelift/isle/veri/isle_fuzzing/testauto.clif"
DIR_PATH="/Users/ajchau/Documents/GitHub/wasmtime/cranelift/"

while true; do

    # Run expr.rs and save the output to testauto.clif under isle_fuzzing
    TEST_CLIF=$(cargo run --bin expr)
    echo "$TEST_CLIF" > "$TEST_CLIF_PATH"

    cd "$DIR_PATH"

    # Run testauto.clif through cranelift (only works on M series Mac)
    OUTPUT=$(cargo run -- compile --target aarch64 -D "$TEST_CLIF_PATH")
    echo "Output:"
    echo "$OUTPUT"

    # Check if the output starts with ".byte", which means the generated clif is executable
    if [[ "$OUTPUT" == .byte* ]]; then
        echo "Condition met: The output starts with '.byte'"
        break
    fi

    # Wait for a while before trying again
    sleep 1

    # go back to the dir with expr.rs to build and run again
    cd "$EXPR_DIR"


done

echo "complete. Found executable random gen .clif and output ARM"


# to run this script
# first update the paths with your local paths. 
    # EXPR_RS_PATH is where expr.rs is located. 
    # EXPR_DIR is the folder where expr.rs is located
    # TEST_CLIF_PATH should be the path to the generated code stored in testauto.clif file in cranelift folder
    # DIR_PATH should be the path to your local cranelift folder                                              
# then chmod +x automate_script.sh to make sure it is executable
# run the script: ./automate_script.sh
