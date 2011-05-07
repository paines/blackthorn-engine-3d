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
;;; Mesh -
;;;  A mesh object contains the geometry data and information
;;;  necessary to render it.
;;;

;; The formats meshes can use:
;; Note that I would like to sometime write a macro that 
;; will create component information, and maybe even
;; accessors to make things nice and easy 
(gl:define-gl-array-format blt-vnt-mesh
  (gl:vertex :type :float :components (px py pz))
  (gl:normal :type :float :components (nx ny nz))
  (gl:tex-coord :type :float :components (u v)))
(defparameter *blt-mesh-components* '(px py pz nx ny nz u v))

;; the class
(defclass mesh ()
  ;; For now we're going to assume we can interleave all the data
  ;; allowing us to keep everything neatly in one gl-vertex-array
  ((id 
    :accessor mesh-id
    :initarg :id)
   (vert-data 
    :accessor mesh-vert-data
    :initarg :vert-data)
   (elements
    :accessor mesh-elements
    :initarg :elements
    :documentation "A list of elem objects containing index and material data
                    for drawing a portion, or all, of the mesh")
   (array-format 
    :accessor mesh-array-format
    :initarg :array-format)
   ;; Also assuming we only have one primitive per mesh
   (primitive-type 
    :accessor mesh-primitive-type
    :initarg :primitive)))

(defun vnt-array->gl-array (array)
  (let* ((count (array-dimension array 0))
         (vertex-size (array-dimension array 1))
         (gl-array (gl:alloc-gl-array 'blt-vnt-mesh count)))
    (iter (for i below count)
          (iter (for j below vertex-size)
                (for c in *blt-mesh-components*)
                (setf (gl:glaref gl-array i c) (aref array i j))))
    gl-array))

(defmethod draw-object ((m mesh))
  (with-slots (vert-data elements primitive-type) m
    (gl:enable-client-state :vertex-array)
    (gl:enable-client-state :normal-array)
    (gl:enable-client-state :texture-coord-array)
    (gl:bind-gl-vertex-array vert-data)
    (iter (for elt in elements)
          (when (and (elem-material elt) *material-array*) 
            (use-material (aref *material-array* (car (elem-material elt)))))
          (gl:draw-elements :triangles (elem-indices elt)))))