#!/usr/bin/env bash

set -exuo pipefail

export RUST_LOG=info

# Options
function usage() {
    echo "Usage: ${0} [-h] [-o <output_dir>]"
    exit 2
}

output_dir="${ISLE_EXPLORER_OUTPUT_DIR}"
while getopts "o:h" opt; do
    case "${opt}" in
        o) output_dir="${OPTARG}" ;;
        h) usage ;;
        *) usage ;;
    esac
done

if [[ ! -d "${output_dir}" ]]; then
    echo "output directory does not exist"
    exit 1
fi

# Generate explorer.
for arch in aarch64 x64; do
    arch_dir="${output_dir:?}/${arch:?}"
    rm -rf "${arch_dir}"
    cargo run --bin explorer -- \
        --codegen-crate-dir ../../../codegen/ \
        --work-dir /tmp \
        --name "${arch}" \
        --output-dir "${arch_dir}"
done
