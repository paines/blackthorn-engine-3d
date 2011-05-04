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
  ((mesh-graph
    :accessor model-mesh-graph
    :initarg :mesh-graph
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
  (with-slots (mesh-graph matrix material) this
    (gl:with-pushed-matrix
      (when matrix   (gl:mult-matrix matrix))
      (when material (use-material material))
      (iter (for node in mesh-graph)
            (aif (node-xform node) (gl:mult-matrix it))
            (draw-object (node-obj node))))))

(defparameter +mesh-components+ '(:vertex :normal :tex-coord))

(defun indices->gl-array (indices)
  (let* ((count (length indices))
         (gl-array (gl:alloc-gl-array :unsigned-short count)))
    (iter (for i below count)
          (setf (gl:glaref gl-array i) (aref indices i)))
    gl-array))

(defun elem->gl-elem (element)
  (make-instance
   'elem
   :indices (indices->gl-array (elem-indices element))
   ;; TODO: load textures to open-gl
   :material (elem-material element)))

;; temp behavior is to only load the first element, until I
;; change the way mesh works
(defmethod load-obj->models ((this load-object))
  (format t "loading object to models~%")
  (make-instance 
   'model-shape
   :mesh-graph 
   (iter (for blt-mesh in (lo-meshes this))
         (let* ((interleaved (interleave
                              (vertex-streams blt-mesh)
                              #+disabled
                              (organize-streams (vertex-streams blt-mesh)
                                                +mesh-components+)))
                (elements
                 (iter (for elt in (elements blt-mesh))
                       (collect (elem->gl-elem elt))))
                (mesh
                 (make-instance
                  'mesh
                  :id (id blt-mesh)
                  :vert-data (vnt-array->gl-array interleaved)
                  :elements elements
                  :array-format 'blt-vnt-mesh)))
           (collect 
            (make-instance 'node
                           :xform (transform blt-mesh)
                           :obj mesh))))))