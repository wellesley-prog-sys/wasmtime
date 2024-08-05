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
    "iadd_base_case"
    "iadd_imm12_right"
    "iadd_imm12_left"
    "iadd_imm12_neg_right"
    "iadd_imm12_neg_left"
    "iadd_ishl_right"
)

for rule in "${rules[@]}"; do
    name="${arch}_${rule}"
    veri --name "${arch}" --rule "${rule}" --smt2-replay-path "output/${name}.smt2" | tee "output/${name}.veri"
done
