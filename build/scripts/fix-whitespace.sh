#!/bin/bash

repo_dir="$(dirname "$(dirname "$("$(dirname "$BASH_SOURCE")/readlink-dirname.sh" "$BASH_SOURCE")")")"

find "$repo_dir/src" -name '*.lisp' | while read f; do sed 's/[ 	]*$//' $f > $f.2 && mv $f.2 $f; done
