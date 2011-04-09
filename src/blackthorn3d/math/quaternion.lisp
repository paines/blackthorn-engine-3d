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

(in-package :blackthorn3d-math)

;;;;
;;;; Quaternions
;;;;   Do not seek to understand them, the denizens of Quaternia, for they are
;;;;   far beyond mortal ken.  Know only well that they do a mean twist.
;;;;
;;;; for our purposes, we will represent quaternions in the same form as
;;;; points <x y z w> where <x y z> == qv and w == qw.  This will make it easy
;;;; to do operations on quaternions and convert points to quaternions for
;;;; rotation (though ideally this should be done on hardware with matrices,
;;;; unless copying matrices onto the hardware proves too expensive)


(defun qv (q)
  (make-vector3 (x q) (y q) (z q)))
  
(defmacro qw (q)
  `(w ,q))
  
(defun make-quaternion (x y z w)
  (make-vector4 x y z w))

(defun make-quaternion-from-vw (v w)
  (make-vector4 (x v) (y v) (z v) w))
  
(defun vec3->quaternion (p)
  (make-quaternion (x p) (y p) (z p) 0.0))
  
(defun axis-rad->quaternion (axis rad)
  (make-quaternion-from-vw (vec-scale axis (sin rad)) (cos rad)))
  
(defun quaternion-identity ()
  (make-vector4 0.0 0.0 0.0 1.0))

(defun quat-norm (q)
  (normalize q))
  
(defun quat+ (q r)
  (make-quaternion
    (+ (x q) (x r))
    (+ (y q) (y r))
    (+ (z q) (z r))
    (+ (w q) (w q))))

(defun quat* (q r)
  (let ((q-v (qv q)) (q-w (qw q))
        (r-v (qv r)) (r-w (qw r)))
    (make-quaternion-from-rw
      (vec+ (vec+ (cross3 q-v r-v)
                  (vec-scale q-v r-w))
            (vec-scale r-v q-w))
       (- (* q-w r-w) (dot q-v r-v)))))

(defun quat-conjugate (q)
  (make-quaternion-from-vw (vec-neg (qv q)) (qw q)))
  
(defun quaternion-rotate-vec (q v)
  "Rotates a vector or point v by quaternion q.
   @arg[q]{quaternion to rotate by}
   @arg[v]{point or vector being rotated}
   @return{a point}"
  (quat* q (quat* (vec3->quaternion v) (quat-conjugate q))))

(defun quaternion->matrix (q)
  "Convert a quaternion into a matrix (if this weren't lisp, i'd be more exited
   at the lack of trig functions)
   @return{a matrix that can be used to rotate objects according to the quaternion}"
  (let ((m11 (- 1.0 (* 2.0 (+ (sq (y q)) (sq (z q))))))
        (m12 (* 2.0 (- (* (x q) (y q)) (* (w q) (z q)))))
        (m13 (* 2.0 (+ (* (x q) (z q)) (* (w q) (y q)))))
        (m21 (* 2.0 (+ (* (x q) (z q)) (* (w q) (z q)))))
        (m22 (- 1.0 (* 2.0 (+ (sq (x q)) (sq (z q))))))
        (m23 (* 2.0 (- (* (y q) (z q)) (* (w q) (x q)))))
        (m31 (* 2.0 (- (* (x q) (z q)) (* (w q) (y q)))))
        (m32 (* 2.0 (+ (* (y q) (z q)) (* (w q) (x q)))))
        (m33 (- 1.0 (* 2.0 (+ (sq (x q)) (sq (y q)))))))
  (make-matrix4x4
    `((,m11 ,m21 ,m31 0.0)
      (,m12 ,m22 ,m32 0.0)
      (,m13 ,m23 ,m33 0.0)
      (0.0  0.0  0.0  1.0)))))