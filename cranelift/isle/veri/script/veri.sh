#!/usr/bin/env bash

set -exuo pipefail

function veri() {
    cargo run --bin veri -- \
        --codegen-crate-dir ../../codegen/ \
        --work-dir /tmp \
        "$@"
}

# Clean.
rm -f output/*.{veri,smt2}

# Verify some rules.
arch="aarch64"
rules=(
    # iadd
    iadd_base_case
    iadd_imm12_right
    iadd_imm12_left
    iadd_imm12_neg_right
    iadd_imm12_neg_left
    iadd_ishl_right
    iadd_ishl_left
    iadd_imul_right
    iadd_imul_left

    # isub
    isub_base_case
    isub_imm12
    isub_imm12_neg
    isub_ishl
    isub_imul
)

for rule in "${rules[@]}"; do
    name="${arch}_${rule}"
    veri --name "${arch}" --rule "${rule}" --smt2-replay-path "output/${name}.smt2" --timeout 10 | tee "output/${name}.veri"
done
