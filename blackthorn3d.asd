;;;; Blackthorn -- Lisp Game Engine
;;;;
;;;; Copyright (c) 2011-2012, Elliott Slaughter <elliottslaughter@gmail.com>
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
  :license "MIT"
  :description "3D game engine for Common Lisp"
  :components ((:module src
                        :components
                        ((:module blackthorn3d
                                  :components
                                  ((:module utils
                                            :components
                                            ((:file "package")
                                             (:file "library"))
                                            :serial t)
                                   (:module math
                                            :components
                                            ((:file "package")
                                             (:file "vector")
                                             (:file "matrix")
                                             (:file "quaternion")
                                             (:file "utils")
                                             (:file "curves"))
                                            :serial t)
                                   (:module resources
                                            :components
                                            ((:file "package")
                                             (:file "locate")
                                             (:file "files")
                                             (:file "graphics"))
                                            :serial t)
                                   (:module network
                                            :components
                                            ((:file "package")
                                             (:file "serializer")
                                             (:file "socket")
                                             (:file "message"))
                                            :serial t)
                                   (:module animation
                                            :components
                                            ((:file "package")
                                             (:file "channels")
                                             (:file "animation")
                                             (:file "animation-controller"))
                                            :serial t)
                                   (:module entity
                                            :components
                                            ((:file "package")
                                             (:file "entity")
                                             (:file "event"))
                                            :serial t)
                                   (:module input
                                            :components
                                            ((:file "package")
                                             (:file "server-control")
                                             (:file "input-control")
                                               
                                             #+(or win32 windows)
                                             (:file "xbox360"))
                                            :serial t)
                                   (:module physics
                                            :components
                                            ((:file "package")
                                             (:file "shapes")
                                             (:file "blt-mesh")
                                             (:file "blt-model")
                                             (:file "camera")
                                             (:file "intersect")
                                             (:file "ray-trace")
                                             (:file "static-collision")
                                             (:file "collision")
                                             (:file "motion")
                                             (:file "skeleton")
                                             (:file "octree"))
                                            :serial t)
                                   (:module sector
                                            :components
                                            ((:file "package")
                                             (:file "sector")
                                             (:file "portal"))
                                            :serial t)
                                   (:module graphics
                                            :components
                                            ((:file "package")
                                             (:file "scene-graph")
                                             (:file "texture")
                                             (:file "framebuffer")
                                             (:file "shader")
                                             (:file "light")
                                             (:file "material")
                                             (:file "mesh")
                                             (:file "skin")
                                             (:file "model")
                                             (:file "draw")
                                             (:file "vao")
                                             (:file "billboards")
                                             (:file "particles")
                                             (:file "effects")
                                             (:file "frustum")
                                             (:file "ui")
                                             (:file "init"))
                                            :serial t)
                                   (:module import
                                            :components
                                            ((:file "package")
                                             (:file "xmls-helpers")
                                             (:file "dae-general")
                                             (:file "loaded-dae")
                                             (:file "dae-geometry")
                                             (:file "dae-material")
                                             (:file "dae-scene")
                                             (:file "dae-animation")
                                             (:file "dae-controller")
                                             (:file "dae-loader"))
                                            :serial t)
                                   (:module renderer
                                            :components
                                            ((:file "package")
                                             (:file "scene-manager")
                                             (:file "render")
                                             (:file "deferred-render")
                                             (:file "old-render"))
                                            :serial t)
                                     
                                   (:module sound
                                            :components
                                            ((:file "package")
                                             (:file "sound")
                                             (:file "sound-event"))
                                            :serial t)
                                   (:module main
                                            :components
                                            ((:file "package")
                                             (:file "pos-edge-react")
                                             (:file "base")
                                             (:file "level-01")
                                             (:file "player")
                                             (:file "simple-monster")
                                             (:file "fire-beast")
                                             (:file "game")
                                             (:file "alarm")
                                             (:file "server")
                                             (:file "client")
                                             (:file "main"))
                                            :serial t)
                                   (:file "package"))
                                  :serial t))))
  :depends-on ( ;; Utilities
               :alexandria
               :trivial-features
               :command-line-arguments
               :cl-fad
               :iterate
               :cl-containers
               :mt19937
               :cxml
               :spatial-trees

               ;; Networking
               :usocket

               ;; Serialization
               :userial

               ;; Graphics and Sound
               :lispbuilder-sdl
               :lispbuilder-sdl-image
               :lispbuilder-sdl-mixer
               :cl-opengl))
