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
  (:use :iter :cl :blt3d-utils :blt3d-math :blt3d-phy :blt3d-ent :blt3d-ani)
  (:export

   ;; init.lisp
;   :init

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

   ;; frustum.lisp
   :frustum
   :make-frstm
   :load-frstm

   ;; camera.lisp
   :camera-matrix
   :camera-inverse
   :camera
   :target
   :mode
   :update-camera
   :move-player

   ;; texture.lisp
   :load-image
   :image->texture2d

   ;; mesh.lisp
   :mesh
   
   ;; model.lisp
   :model-shape
   :load-obj->models
   :controller
   :update-model

   ;; light.lisp
   :light
   :init-light
   :make-light

   ;; shader.lisp
   :make-shader
   :use-shader
  
   ))
        
        
        