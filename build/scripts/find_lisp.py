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

import os, subprocess
from .which import which

_cached_filename = '.find-lisp'
_lisps = (
    ('sbcl', 'sbcl'),
    ('ccl', 'clozure'),
    ('alisp', 'allegro'),
    ('clisp', 'clisp'),
    ('ecl', 'ecl'),
)
def find_lisp(cache_filename = _cached_filename):
    if os.path.isfile(cache_filename):
        with open(cache_filename) as f:
            lisp = f.read()
            # 2.x returns list, 3.x returns iterator
            if lisp in next(iter(zip(*_lisps))): return lisp
    for command, alias in _lisps:
        if which(command) is not None:
            with open(cache_filename, 'w') as f:
                f.write(alias)
            return alias
