#!/bin/bash

build_dir="$(dirname "$("$(dirname "$BASH_SOURCE")/readlink-dirname.sh" "$BASH_SOURCE")")"

export PATH=$PATH:"$build_dir/ccl"

"$build_dir/scripts/run-lisp.pl" "$@"
