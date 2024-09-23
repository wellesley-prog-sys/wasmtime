#!/usr/bin/env bash

set -exuo pipefail

# Options
function usage() {
    echo "Usage: ${0} [-h] [-s <aslp_server>]"
    exit 2
}

aslp_server="${ASLP_SERVER:-}"
output_path="../../../codegen/src/isa/aarch64/spec/"
while getopts "s:o:h" opt; do
    case "${opt}" in
        s) aslp_server="${OPTARG}" ;;
        o) output="${OPTARG}" ;;
        h) usage ;;
        *) usage ;;
    esac
done

# Generate
cargo run --bin isaspec \
    -- \
    --server "${aslp_server}" \
    --output "${output_path}"
