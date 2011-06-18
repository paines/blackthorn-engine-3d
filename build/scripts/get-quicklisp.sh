#!/bin/bash
#### Blackthorn -- Lisp Game Engine
####
#### Copyright (c) 2011, Elliott Slaughter <elliottslaughter@gmail.com>
####
#### Permission is hereby granted, free of charge, to any person
#### obtaining a copy of this software and associated documentation
#### files (the "Software"), to deal in the Software without
#### restriction, including without limitation the rights to use, copy,
#### modify, merge, publish, distribute, sublicense, and/or sell copies
#### of the Software, and to permit persons to whom the Software is
#### furnished to do so, subject to the following conditions:
####
#### The above copyright notice and this permission notice shall be
#### included in all copies or substantial portions of the Software.
####
#### THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
#### EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
#### MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
#### NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
#### HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
#### WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
#### OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
#### DEALINGS IN THE SOFTWARE.
####

build_dir="$(dirname "$("$(dirname "$BASH_SOURCE")/readlink-dirname.sh" "$BASH_SOURCE")")"

lisp="$1"

quicklisp_file="quicklisp.lisp"
quicklisp_url="http://beta.quicklisp.org/$quicklisp_file"

echo "Attempting to install Quicklisp..."

pushd "$build_dir" >& /dev/null
"$build_dir/scripts/download.sh" "$quicklisp_url"
if [ ! "$(echo $?)" -eq 0 ]; then
    echo "Failed to download Quicklisp."
    echo
    echo "Please go to the following URL and install it manually:"
    echo "$quicklisp_url"
else
    "$build_dir/scripts/run-lisp.pl" "$lisp" --load "$quicklisp_file" --eval "(quicklisp-quickstart:install :path \"quicklisp/\")" --eval '#-allegro (quit) #+allegro (exit)'
fi
popd >& /dev/null
