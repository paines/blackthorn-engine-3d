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

import os, re
from scripts.find_lisp import find_lisp
from scripts.get_lisp import get_lisp
from scripts.get_quicklisp import get_quicklisp
from scripts.run_lisp import run_lisp_output

_build_dir = os.path.dirname(os.path.realpath(__file__))

_quicklisp_exists = re.compile(r'^Quicklisp exists[?] yes$', re.M)
if __name__ == '__main__':
    lisp = find_lisp()
    if lisp is None:
        get_lisp()
    lisp = find_lisp()
    if lisp is None:
        print('Failed to install Lisp')
    output = run_lisp_output(
        'sbcl',
        '--load', os.path.join(_build_dir, 'scripts', 'quicklisp-existsp.lisp'))
    if re.search(_quicklisp_exists, output) is None:
        get_quicklisp(lisp)
