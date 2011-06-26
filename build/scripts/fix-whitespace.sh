#!/bin/bash

repo_dir="$(dirname "$(dirname "$("$(dirname "$BASH_SOURCE")/readlink-dirname.sh" "$BASH_SOURCE")")")"

# fix-file <filename>
function fix-file () {
    file="$1"
    dos2unix -q "$file" &&
    sed 's/[ 	]*$//' "$file" > "$file.2" &&
    mv "$file.2" "$file"
}

find "$repo_dir/src" -name '*.lisp' | while read f; do fix-file $f; done
