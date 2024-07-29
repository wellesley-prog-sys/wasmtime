#!/usr/bin/env bash

set -exuo pipefail

output_dir="tmp/explorer"
port="5050"

miniserve \
    --port "${port}" \
    --index index.html \
    --disable-indexing \
    --verbose \
    "${output_dir}"
