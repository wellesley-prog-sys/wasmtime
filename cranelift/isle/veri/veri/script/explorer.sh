#!/usr/bin/env bash

set -exuo pipefail

export RUST_LOG=info

output_dir="tmp/explorer"
rm -rf "${output_dir}"

for arch in aarch64 x64; do
    cargo run --bin explorer -- \
        --codegen-crate-dir ../../../codegen/ \
        --work-dir /tmp \
        --name "${arch}" \
        --output-dir "${output_dir}/${arch}"
done
