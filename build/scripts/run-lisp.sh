#!/bin/bash

build_dir="$(dirname "$("$(dirname "$BASH_SOURCE")/readlink-dirname.sh" "$BASH_SOURCE")")"

export PATH=$PATH:"$build_dir/ccl"

if [[ -z $("$build_dir/scripts/run-lisp.pl" "$1" --load "$build_dir/scripts/quicklisp-existsp.lisp" | grep 'Quicklisp exists? yes') ]]; then
    "$build_dir/scripts/get-quicklisp.sh" "$1"
fi
"$build_dir/scripts/run-lisp.pl" "$@"
