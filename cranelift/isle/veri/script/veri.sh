#!/usr/bin/env bash

set -exuo pipefail

# Clean.
rm -f output/*.{veri,smt2}

# Verify some rules.
arch="aarch64"
cargo run --bin veri -- \
    --codegen-crate-dir ../../codegen/ \
    --work-dir /tmp \
    --name "${arch}" \
    --smt2-replay-path "output/${arch}.smt2" \
    --timeout 10 \
    | tee "output/${arch}.veri"
