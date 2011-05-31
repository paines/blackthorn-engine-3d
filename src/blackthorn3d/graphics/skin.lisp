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
;;; Skin - like a mesh but with skin
;;;

(defvar skin-shader nil)
(defvar joint-indices-loc nil)
(defvar joint-weights-loc nil)
(defvar joint-mats-loc nil)

;; The formats meshes can use:
;; Note that I would like to sometime write a macro that 
;; will create component information, and maybe even
;; accessors to make things nice and easy 
(gl:define-gl-array-format blt-vntiw-skin
  (gl:vertex :type :float :components (px py pz))
  (gl:normal :type :float :components (nx ny nz))
  (gl:tex-coord :type :float :components (u v))
  (gl:vertex-attrib :type :float 
                    :index (get-attribute-loc skin-shader "jointIndices")
                    :components (i0 i1 i2 i3))
  (gl:vertex-attrib :type :float 
                    :index (get-attribute-loc skin-shader "jointWeights")
                    :components (w0 w1 w2 w3)))
(defparameter *blt-skin-components* '(px py pz 
                                      nx ny nz 
                                      u v
                                      i0 i1 i2 i3
                                      w0 w1 w2 w3))

(defun vntiw-array->gl-array (array)
  (let* ((count (array-dimension array 0))
         (vertex-size (array-dimension array 1))
         (gl-array (gl:alloc-gl-array 'blt-vntiw-skin count)))
    (iter (for i below count)
          (iter (for j below vertex-size)
                (for c in *blt-skin-components*)
                (setf (gl:glaref gl-array i c) (float (aref array i j)))))
    gl-array))

;; the class
(defclass skin (mesh)
  ((mesh
    :accessor skin-mesh
    :initarg :mesh)
   (bind-skeleton
    :accessor bind-skeleton
    :initarg :bind-skeleton
    :documentation "the skeleton this skin is bound to")
   (bind-shape-matrix
    :accessor bind-shape-matrix
    :initarg :bind-shape-matrix
    :initform (make-identity-matrix))))

(defparameter bone-mat (make-blt-material
                        :ambient #(1.0 1.0 1.0 1.0)
                        :diffuse #(1.0 1.0 1.0 1.0)))


(defmethod draw-object ((this skin))
  (with-slots (vert-data elements bind-skeleton bind-shape-matrix)
      this

    (gl:point-size 5.0)
    ;; Set up the skeleton array
    (update-skeleton bind-skeleton)

    #+disabled
    (labels ((skele-drawer (joint)
               (let ((point (matrix-multiply-v (joint-matrix joint)
                                               +origin+)))
                 (gl:with-primitives :lines
                   (gl:vertex 0.0 0.0 0.0)
                   (gl:vertex (x point) (y point) (z point)))
                 (gl:with-primitives :points
                   (gl:vertex (x point) (y point) (z point)))
                 (gl:with-pushed-matrix
                     (gl:mult-matrix (joint-matrix joint))
                   (iter (for c in (child-joints joint))
                         (skele-drawer c))))))

      (use-material bone-mat)
      (let ((joint-mats (get-joint-matrices bind-skeleton)))
        (iter (for mat in-vector joint-mats)
              (gl:with-pushed-matrix
                  (gl:mult-matrix mat)
                (gl:with-primitives :points
                  (gl:vertex 0.0 0.0 0.0)))))

      (skele-drawer (root-joint bind-skeleton)))

   ; #+disabled
    (progn
      (enable-shader skin-shader)
      (gl:enable-client-state :vertex-array)
      (gl:enable-client-state :normal-array)
      (gl:enable-client-state :texture-coord-array)
      (gl::enable-vertex-attrib-array 
       (get-attribute-loc skin-shader "jointIndices"))
      (gl::enable-vertex-attrib-array
       (get-attribute-loc skin-shader "jointWeights")))
 

    ;#+disabled
    (progn
      (gl:with-pushed-matrix 
      ;    (gl:mult-matrix bind-shape-matrix)
        (gl:uniform-matrix 
         (get-uniform-loc skin-shader "jointMats") 4
         (get-joint-matrices bind-skeleton) nil)

        (gl:bind-gl-vertex-array vert-data)
        (iter (for elt in elements)
              (when (and (element-material elt) *material-array*) 
                (use-material (aref *material-array* 
                                    (car (element-material elt)))))
              (gl:draw-elements :triangles (element-indices elt)))
          
        (disable-attributes skin-shader)))))