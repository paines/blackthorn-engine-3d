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

import os, sys
from build.scripts.find_lisp import find_lisp
from build.scripts.run_lisp import run_lisp

# Lisp Implementation
_lisp = find_lisp()

# Drivers
_load = 'build/scripts/load.lisp'
_quicklisp_setup = 'build/scripts/quicklisp-setup.lisp'

# ASDF System
_system = 'lkcas'

# Network
_localhost = 'localhost'
_port = '12345'

def load(lisp = _lisp, driver = _load, system = _system, args = []):
    run_lisp(lisp,
             '--eval', '(defparameter *driver-system* "%s")' % system,
             '--load', _quicklisp_setup,
             '--load', driver,
             '--', *args)

def server(host = _localhost, port = _port, args = [], **rest):
    load(args = ['--server=%s --port=%s' % (host, port)] + args, **rest)

def client(host = _localhost, port = _port, args = [], **rest):
    load(args = ['--client=%s --port=%s' % (host, port)] + args, **rest)

if __name__ == '__main__':
    server()
