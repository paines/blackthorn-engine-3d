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

;; The formats meshes can use:
;; Note that I would like to sometime write a macro that 
;; will create component information, and maybe even
;; accessors to make things nice and easy 
(gl:define-gl-array-format blt-vntwi-skin
  (gl:vertex :type :float :components (px py pz))
  (gl:normal :type :float :components (nx ny nz))
  (gl:tex-coord :type :float :components (u v))
  (gl:vertex-attrib :type :float :index 3 :components (i0 i1 i2 i3))
  (gl:vertex-attrib :type :float :index 4 :components (w0 w1 w2 w3)))
(defparameter *blt-skin-components* '(px py pz 
                                      nx ny nz 
                                      u v
                                      i0 i1 i2 i3
                                      w0 w1 w2 w3))

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
    :initarg :bind-shape-matrx
    :initform (make-identity-matrix))))

