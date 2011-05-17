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


(defclass model-shape ()  
  ((mesh-graph
    :accessor model-mesh-graph
    :initarg :mesh-graph
    :documentation "The 3d geometry data comprising the model")
   (controller
    :accessor controller
    :initarg :controller
    :documentation "the animation-clips, for now. should be a controller")
   (matrix
    :accessor model-matrix
    :initarg :matrix
    :initform nil
    :documentation "the matrix that transforms from the model space
                    to the space of the parent (world, unless a child 
                    in a scenegraph")))

(defvar *material-array* nil)

(defmethod update-model((this blt-model) time)
  (aif (animations this)
       (update-anim-controller it time)))

(defmethod draw-object ((this blt-model))
  (with-slots (mesh-nodes) this
    (iter (for node in mesh-nodes)
          (draw-object node))))

(defmethod draw-object ((this model-node))
  
  (gl:with-pushed-matrix
      (aif (transform this) (gl:mult-matrix it))

   
    (let ((*material-array* (material-array this)))
      (draw-object (mesh this))
      (iter (for node in (child-nodes this))
            (draw-object node))))
   (draw-bounding-sphere (node-bounding-volume this)))

(defparameter +mesh-components+ '(:vertex :normal :tex-coord))
(defparameter mesh-format '((:vertex 3) (:normal 3) (:texcoord 2)))
(defparameter skin-format (append mesh-format
                                  '((:joint-index 4) (:joint-weight 4))))

(defun indices->gl-array (indices)
  (let* ((count (length indices))
         (gl-array (gl:alloc-gl-array :unsigned-short count)))
    (iter (for i below count)
          (setf (gl:glaref gl-array i) (aref indices i)))
    gl-array))

(defun elem->gl-elem (element)
  (make-element
   :indices (indices->gl-array (element-indices element))
   ;; TODO: load textures to open-gl
   :material (element-material element)))

(defvar *animator* nil)

(defmethod get-mesh-format ((this blt-mesh))
  mesh-format)

(defmethod get-mesh-format ((this blt-skin))
  skin-format)

(defvar *accessor* nil)

;; Changed to return the same, albeit modified, blt-model
(defmethod load-obj->models ((this blt-model))
  (setf *animator* (animations this))
  (format t "MODEL ANIMATIONS: ~a~%" (animations this))
  (with-slots (mesh-nodes animations) this
    (iter (for node in mesh-nodes)
          (with-slots (mesh transform) node
            (multiple-value-bind (interleaved accessor)
                (interleave
                 (vertex-streams mesh)
                 (get-mesh-format mesh))
              (setf *accessor* accessor)
              (setf mesh (convert-to-ogl mesh
                                         interleaved
                                         (elements mesh)))))))
  this)

(defmethod convert-to-ogl ((mesh blt-mesh) interleaved elements)
  (make-instance
   'mesh
   :id (id mesh)
   :vert-data (vnt-array->gl-array interleaved)
   :elements (iter (for elt in (elements mesh))
                         (collect (elem->gl-elem elt)))
   :array-format 'blt-vnt-mesh))

(defmethod convert-to-ogl ((skin blt-skin) interleaved elements)
  (make-instance
   'skin
   :id (id skin)
   :vert-data interleaved ;(vntiw-array->gl-array interleaved)
   :elements elements
   :array-format 'blt-vntiw-mesh
   :bind-skeleton (bind-skeleton skin)
   :bind-shape-matrix (bind-shape-matrix skin)))