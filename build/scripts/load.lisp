;;;; Blackthorn -- Lisp Game Engine
;;;;
;;;; Copyright (c) 2007-2011, Elliott Slaughter <elliottslaughter@gmail.com>
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use, copy,
;;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;;; of the Software, and to permit persons to whom the Software is
;;;; furnished to do so, subject to the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;;; DEALINGS IN THE SOFTWARE.
;;;;

(defvar *driver-system* :blackthorn3d)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (find-package :swank)
    (pushnew :blt-debug *features*)))

#-quicklisp
(require :asdf)

;; Inject library paths for cffi:
#+quicklisp
(ql:quickload :cffi)
#-quicklisp
(asdf:operate 'asdf:load-op :cffi)

(pushnew
 (merge-pathnames (make-pathname :directory '(:relative "lib")))
 cffi:*foreign-library-directories* :test #'equal)

;; Load and run main:
#+quicklisp
(ql:quickload *driver-system*)
#-quicklisp
(asdf:operate 'asdf:load-op *driver-system*)

(blt3d-user:main)
