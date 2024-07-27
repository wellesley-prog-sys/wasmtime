#!/usr/bin/env bash

set -exuo pipefail

output_dir="output/explorer"
port="5050"

miniserve \
    --port "${port}" \
    --index index.html \
    --disable-indexing \
    "${output_dir}"
