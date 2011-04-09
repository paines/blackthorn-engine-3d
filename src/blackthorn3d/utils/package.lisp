;;;; Blackthorn -- Lisp Game Engine
;;;;
;;;; Copyright (c) 2011, Elliott Slaughter <elliottslaughter@gmail.com>
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

(defpackage :blackthorn3d-utils
  (:nicknames :blt3d-utils)
  (:use :cl :alexandria :iter)
  (:export

   ;; utils.lisp
   :aif
   :acond
   :it
   
   ;; math.lisp
   :make-vector4
   :make-vector3
   :make-point3
   :make-vec3
   :x :y :z :w
   :r :g :b :a   
   :dot
   :cross 
   :cross3
   :vec+ :vec- :vec* :vec/
   :vec-scale 
   :vec-scale3
   :vec-neg
   :mag
   :sq
   :norm
   :magnitude
   
   :make-matrix
   :make-matrix3x3
   :make-matrix4x4
   :make-identity
   :make-translate
   :make-scale
   :make-x-rot :make-y-rot :make-z-rot
   :set-col :get-col
   :set-row :get-row
   :inner-product
   :outer-product
   :matrix-multiply-v
   :matrix-multiply-m
   :vector-multiply-m
   :transpose

   ;; library.lisp
   :load-dlls

   ))
