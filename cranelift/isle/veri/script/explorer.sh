#!/usr/bin/env bash

set -exuo pipefail

output_dir="output/explorer"
cargo run --bin explorer -- \
    --codegen-crate-dir ../../codegen/ \
    --work-dir /tmp \
    --name aarch64 \
    --output-dir "${output_dir}"
