#!/usr/bin/env bash

set -euo pipefail

# Options
function usage() {
    echo "Usage: ${0} [-h] [-a <arch>] [-t <tmp_dir>] [-o <output_dir>]"
    exit 2
}

arch="aarch64"
tmp_dir="${TMPDIR}"
output_dir="output"
while getopts "a:t:o:h" opt; do
    case "${opt}" in
        a) arch="${OPTARG}" ;;
        t) tmp_dir="${OPTARG}" ;;
        o) output_dir="${OPTARG}" ;;
        h) usage ;;
        *) usage ;;
    esac
done
shift $((OPTIND-1))

# Setup output.
mkdir -p "${output_dir}"

# Run.
cargo run --bin veri -- \
    --codegen-crate-dir ../../codegen/ \
    --work-dir "${tmp_dir}" \
    --name "${arch}" \
    --smt2-replay-path /dev/null \
    --timeout 10 \
    "$@" \
    | tee "${output_dir}/${arch}.veri"
