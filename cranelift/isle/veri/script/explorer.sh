#!/usr/bin/env bash

set -exuo pipefail

output_dir=$(mktemp -d)
port="5050"

cargo run --bin explorer -- \
    --codegen-crate-dir ../../codegen/ \
    --work-dir /tmp \
    --name aarch64 \
    --output-dir "${output_dir}"

miniserve \
    --port "${port}" \
    --index index.html \
    --disable-indexing \
    "${output_dir}"
