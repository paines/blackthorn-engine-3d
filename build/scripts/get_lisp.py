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

from .download import download
from .which import which

_build_dir = os.path.dirname(os.path.dirname(os.path.realpath(__file__)))
_svn_url = \
    "http://prdownloads.sourceforge.net/win32svn/Setup-Subversion-1.6.16.msi"
_clozure_url = \
    "http://svn.clozure.com/publicsvn/openmcl/release/1.7/windows/ccl"
_sbcl_url = \
    "http://prdownloads.sourceforge.net/sbcl/sbcl-1.0.52-x86-windows-binary.msi"

def get_lisp_for_windows():
    previous_cwd = os.getcwd()
    os.chdir(_build_dir)
    if not os.path.exists(os.path.join(_build_dir, 'ccl')):
        print('Downloading Clozure CL...')
        if which('svn') is None:
            print("If you want to install Clozure CL you'll need SVN.")
            print('You can install SVN from the following URL:')
            print(_svn_url)
            print('Install the file, then log out, log back in and try again.')
            print('Failed to download Clozure CL.')
        else:
            subprocess.check_call(['svn', 'co', _clozure_url])
            print('Done downloading Clozure CL.')
    if which('sbcl') is None:
        print('Downloading SBCL...')
        filename = download(_sbcl_url)
        if filename is None:
            print("Failed to download SBCL.")
            print("Please go to the following URL and install it manually:")
            print(_sbcl_url)
        else:
            print("Running SBCL installer...")
            subprocess.check_call(['msiexec', '/i', filename])
            raise Exception("Please log out and log back in.")
    os.chdir(previous_cwd)

def get_lisp():
    if sys.platform in ('win32', 'cygwin'):
        return get_lisp_for_windows()
    raise Exception('Unable to download Lisp for your OS')
