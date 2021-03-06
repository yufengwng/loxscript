#!/usr/bin/env bash
#
# Run all benchmarking files, piping to stdout.

set -euo pipefail

cargo build --release

for file in ./bench/*.lox ; do
  echo "# $file"
  (time ./target/release/loxscript $file) 2>&1
done
