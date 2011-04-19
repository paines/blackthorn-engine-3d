;;;; Blackthorn -- Lisp Game Engine
;;;;
;;;; Copyright (c) 2011, Robert Gross <r.gross.3@gmail.com>
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

(in-package :cl-user)

(defpackage :blackthorn3d-graphics
  (:nicknames :blt3d-gfx)
  (:use :iter :cl :blt3d-utils :blt3d-math)
  (:export

   ;; draw.lisp
   :draw-triangle
   :draw-cube
   :make-cube
   :draw-vert-array
   :make-vao-cube
   :draw-vao-cube
   :gfx-init
   :gfx-draw
   :draw-object

   ; should go somewhere else
   :init
   :prepare-scene
   :*main-cam*
   :*main-cam-quat*
   :render-frame

   ;; camera.lisp
   :camera-matrix
   :camera-inverse
   :camera
   :cam-pos
   :cam-dir
   :cam-up
   :camera-move!
   :camera-rotate!
   :camera-lookat!
   :camera-orbit!
   :update-fp-camera
   :update-tp-camera

   ;; mesh.lisp
   :mesh

   ;; dae-loader.lisp
   :load-dae
   ))
        
        
        