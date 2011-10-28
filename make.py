#!/usr/bin/env python

import os, sys
from build.scripts.run_lisp import run_lisp

_lisp = 'sbcl' # TODO: find lisp
_load = 'build/scripts/load.lisp'
_quicklisp_setup = 'build/scripts/quicklisp-setup.lisp'
_system = 'lkcas'

def load(lisp = _lisp, driver = _load, system = _system, args = []):
    run_lisp(lisp,
             '--eval', '(defparameter *driver-system* "%s")' % system,
             '--load', _quicklisp_setup,
             '--load', driver,
             '--', *args)

if __name__ == '__main__':
    load(args = ['--server=127.0.0.1', '--port=12345'])
