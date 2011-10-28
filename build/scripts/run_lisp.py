#!/usr/bin/env python

import subprocess, sys

def _no_transformation(*args): return args
def _simple_transformer(param_map):
    lambda param, value: (param_map[param], value)

def _clozure_command():
    if sys.platform in ('win32', 'cygwin'):
        return 'wx86cl'
    return 'ccl'

def _clisp_transform_arg(param, value):
    if param == '--load':
        param, value = '--eval', '(load "%s")' % value
    if param == 'eval':
        return ('-x', value)
    raise Exception('Unknown key "%s"' % param)

def _allegro_reorder_args(*args):
    if len(args) >= 2:
        command, filename = args[-2:]
        if command == '-L':
            return ('+s', filename) + args[:-2] + ('+B',)
    return args + ('+B',)

_impls = {
    'sbcl': {
        'cmd': 'sbcl',
        'transform_arg': _no_transformation,
        'reorder_args': _no_transformation,
    },
    'ccl': {
        'cmd': _clozure_command(),
        'transform_arg': _no_transformation,
        'reorder_args': _no_transformation,
    },
    'clozure': {
        'cmd': _clozure_command(),
        'transform_arg': _no_transformation,
        'reorder_args': _no_transformation,
    },
    'ecl': {
        'cmd': 'ecl',
        'transform_arg': _simple_transformer({'--load': '-load',
                                              '--eval': '-eval'}),
        'reorder_args': _no_transformation,
    },
    'clisp': {
        'cmd': 'clisp',
        'transform_arg': _clisp_transform_arg,
        'reorder_args': _no_transformation,
    },
    'allegro': {
        'cmd': 'alisp',
        'transform_arg': _simple_transformer({'--load': '-L',
                                              '--eval': '-e'}),
        'reorder_args': _allegro_reorder_args,
    },
}

def run_lisp(impl_name, *args):
    impl = _impls[impl_name]
    impl_cmd = impl['cmd']
    out_args = []
    iter_args = iter(args)
    try:
        while True:
            in_arg = iter_args.next()
            if in_arg in ('--load', '--eval'):
                out_args.extend(impl['transform_arg'](in_arg, iter_args.next()))
            else:
                out_args.append(in_arg)
            if in_arg == '--':
                break
    except StopIteration:
        pass
    out_args = list(impl['reorder_args'](*out_args))
    out_args.extend(iter_args)
    return subprocess.call([impl_cmd] + out_args)
