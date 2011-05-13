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

(defpackage :blackthorn3d-math
  (:nicknames :blt3d-math)
  (:use :cl :alexandria :iter :blt3d-utils)
  (:export
   
   ;; vector.lisp
   :make-vector4
   :make-vector3
   :make-point3
   :make-vec3
   :make-color
   :x :y :z :w
   :r :g :b :a   
   :dot
   :cross 
   :cross3
   :vec4+ :vec4- :vec4* :vec4/
   :vec3+ :vec3- :vec3* :vec3/
   :vec-scale4 
   :vec-scale3
   :vec-neg4
   :vec-neg3
   :mag
   :sq-mag
   :sq
   :norm4
   :norm3
   :magnitude
   :spherical->cartesian
   :tri-centroid
   
   ;; matrix.lisp
   :make-matrix
   :make-matrix3x3
   :make-matrix4x4
   :make-identity-matrix
   :make-translate
   :make-scale
   :make-x-rot :make-y-rot :make-z-rot
   :make-ortho-basis
   :make-inv-ortho-basis
   :make-projection
   :set-mat
   :col
   :row
   :inner-product
   :outer-product
   :matrix-multiply-v
   :matrix-multiply-m
   :vector-multiply-m
   :transpose
   :reshape
   :look-at-matrix
   :look-dir-matrix
   :rt-inverse
   :vec-cols->matrix
   :vec-rows->matrix
   :extract-scale
   :extract-translate

   ;; quaternion.lisp
   :make-quat
   :make-quat-from-vw
   :vec3->quat
   :axis-rad->quat
   :quat+ :quat*
   :quat-norm
   :quat-identity
   :quat-conjugate
   :quat-rotate-vec
   :quat->matrix

   ;; utils.lisp
   :deg->rad

   ;; curves.lisp
   :calc-1d-hermite-coefs
   :eval-1d-curve

   ))
