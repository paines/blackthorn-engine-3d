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
                          ((:module blackthorn3d
                                    :components
                                    ((:module utils
                                              :components
                                              ((:file "package")
                                               (:file "anaphora")
                                               (:file "library"))
                                              :serial t)
                                     (:module math
                                              :components
                                              ((:file "package")
                                               (:file "vector")
                                               (:file "matrix")
                                               (:file "quaternion")
                                               (:file "utils"))
                                              :serial t)
                                     (:module resources
                                              :components
                                              ((:file "package")
                                               (:file "locate"))
                                              :serial t)
                                     (:module network
                                              :components
                                              ((:file "package"))
                                              :serial t)
                                     (:module entity
                                              :components
                                              ((:file "package")
                                               (:file "serializer")
                                               (:file "entity")
                                               (:file "event"))
                                              :serial t)
                                     (:module graphics
                                              :components
                                              ((:file "package")
                                               (:file "mesh")
                                               (:file "xmls-helpers")
                                               (:file "dae-loader")
                                               (:file "draw")
                                               (:file "frustum")
                                               (:file "camera")
                                               (:file "render"))
                                              :serial t)
                                     (:module input
                                              :components
                                              ((:file "package")
                                               (:file "input-control")
                                               #+(or win32 windows)
                                               (:file "xbox360"))
                                              :serial t)
                                     (:module main
                                              :components
                                              ((:file "package")
                                               (:file "server")
                                               (:file "main"))
                                              :serial t)
                                     (:file "package"))
                                    :serial t))))
    :depends-on (;; Utilities
                 :alexandria
                 :trivial-features
                 :command-line-arguments
                 :cl-fad
                 :iterate
                 :cl-containers
                 :mt19937
                 :cxml

                 ;; Networking
                 :usocket

                 ;; Serialization
                 :userial

                 ;; Graphics and Sound
                 :lispbuilder-sdl
                 :lispbuilder-sdl-image
                 :lispbuilder-sdl-mixer
                 :cl-opengl))
