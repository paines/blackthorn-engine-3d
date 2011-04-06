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

(defpackage :blackthorn3d-asd
  (:use :cl :asdf))

(in-package :blackthorn3d-asd)

(defsystem blackthorn3d
    :name "blackthorn3d"
    :author "Elliott Slaughter <elliottslaughter@gmail.com>"
    :version "0.0"
    :components ((:module src
                          :components
                          ((:module blackthorn
                                    :components
                                    ((:module utils
                                              :components
                                              ((:file "package")
                                               (:file "utils")
                                               (:file "resources")
                                               (:file "math"))
                                              :serial t)
                                     (:module graphics
                                              :components
                                              ((:file "package")
                                               (:file "draw"))
                                              :serial t)
                                     (:file "package")
                                     (:file "library")
                                     (:file "main"))
                                    :serial t))))
    :depends-on (;; Utilities
                 :alexandria
                 :trivial-features
                 :command-line-arguments
                 :cl-fad
                 :iterate
                 :cl-containers
                 :mt19937

                 ;; Networking and Serialization
                 :usocket
                 :cl-store

                 ;; Graphics and Sound:
                 :lispbuilder-sdl
                 :lispbuilder-sdl-image
                 :lispbuilder-sdl-mixer
                 :cl-opengl))
