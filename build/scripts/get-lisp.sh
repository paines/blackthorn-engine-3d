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

# FIXME: Mac OS X readlink doesn't recognize the -f parameter.
build_dir="$(dirname "$(dirname "$(readlink -f "$BASH_SOURCE")")")"

# get-lisp-for-windows
function get-lisp-for-windows () {
    if [[ ! -d $build_dir/ccl ]]; then
        echo "Downloading Clozure CL..."
        if [ ! "$(which svn >& /dev/null; echo $?)" -eq 0 ]; then
            echo "Please install SVN from the following URL:"
            echo "http://sourceforge.net/projects/win32svn/files/1.6.16/Setup-Subversion-1.6.16.msi/download"
            echo "Then log out and log back in and try again."
            echo "Failed to download Clozure CL."
            return 1
        else
            pushd "$build_dir" >& /dev/null
            svn co http://svn.clozure.com/publicsvn/openmcl/trunk/windows/ccl
            popd >& /dev/null
            echo "Done downloading Clozure CL."
        fi
    fi
    which sbcl >& /dev/null
    if [ ! "$(echo $?)" -eq 0 ]; then
        echo "Downloading SBCL..."
        pushd "$build_dir" >& /dev/null
        "$build_dir/scripts/download.sh" "http://prdownloads.sourceforge.net/sbcl/sbcl-1.0.49-x86-windows-binary.msi"
        if [ ! "$(echo $?)" -eq 0 ]; then
            echo "Failed to download SBCL."
            echo
            echo "Please go to the following URL and install it manually:"
            echo "http://prdownloads.sourceforge.net/sbcl/sbcl-1.0.49-x86-windows-binary.msi"
        else
            echo "Running SBCL installer..."
            cmd "/C cd scripts && install.bat .. sbcl-1.0.49-x86-windows-binary.msi "
            popd >& /dev/null
            echo "Please close and reopen the terminal."
        fi
        return 1
    fi
}

if [[ $(uname) == CYGWIN* ]]; then
    get-lisp-for-windows
elif [[ $(uname) == MINGW* ]]; then
    get-lisp-for-windows
else
    echo "Unable to download Lisp for your OS."
fi
