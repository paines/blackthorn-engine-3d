#!/usr/bin/env python
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

import os, subprocess, sys

def _path():
    return os.environ['PATH'].split(os.pathsep)

def _is_executable(filename):
    return os.path.isfile(filename) and os.access(filename, os.X_OK)

_windows = sys.platform in ('win32', 'cygwin')
def _try_executable(filename):
    if _is_executable(filename):
        return filename
    filename_exe = '%s.exe' % filename
    if _windows and _is_executable(filename_exe):
        return filename_exe
    return None

def which(filename, path = None):
    if path is None:
        path = _path()
    if os.path.isabs(filename):
        return _try_executable(filename)
    for directory in path:
        result = _try_executable(os.path.join(directory, filename))
        if result is not None:
            return result
