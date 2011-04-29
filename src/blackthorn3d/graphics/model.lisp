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

(in-package :blackthorn3d-graphics)

;;;
;;; Models 
;;; 
;;; a model encapsulates mesh and material data in a
;;; tree like structure that chains matrices and meshes
;;; with the textures and shaders they require to render
;;;

;; However, for now let's just get a mesh with transform
(defclass model-shape ()  
  ((mesh
    :accessor model-mesh
    :initarg :mesh
    :documentation "The 3d geometry data comprising the model")
   (matrix
    :accessor model-matrix
    :initarg :matrix
    :initform nil
    :documentation "the matrix that transforms from the model space
                    to the space of the parent (world, unless a child 
                    in a scenegraph")
   (material
    :accessor model-mat
    :initarg :material
    :initform nil
    :documentation "the material to use for the mesh")))

(defmethod draw-object ((this model-shape))
  (with-slots (mesh matrix material) this
    (gl:with-pushed-matrix
      (when matrix (gl:mult-matrix matrix))
      (when material (use-material material))
      (draw-object mesh))))