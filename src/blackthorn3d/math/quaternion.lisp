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
  
(defun make-quat (x y z w)
  (make-vector4 x y z w))

(defun make-quat-from-vw (v w)
  (make-vector4 (x v) (y v) (z v) w))
  
(defun vec3->quat (p)
  (make-quat (x p) (y p) (z p) 0.0))
  
(defun axis-rad->quat (axis rad)
  (make-quat-from-vw (vec-scale4 axis (sin rad)) (cos rad)))
  
(defun euler->quat (roll pitch yaw)
  "Takes euler angles (roll pitch and yaw and converts them
   to a quaternion.  Assumes order is r->p->y"
  (let* ((cos-r (cos (/ roll 2.0)))
         (cos-p (cos (/ pitch 2.0)))
         (cos-y (cos (/ yaw 2.0)))
         (sin-r (sin (/ roll 2.0)))
         (sin-p (sin (/ pitch 2.0)))
         (sin-y (sin (/ yaw 2.0)))
         (cpcy (* cos-p cos-y))
         (spsy (* sin-p sin-y)))
    (make-quat (+ (* cos-r cpcy) (* sin-r spsy))
               (- (* sin-r cpcy) (* cos-r spsy))
               (+ (* cos-r sin-p cos-y) (* sin-r cos-p sin-y))
               (- (* cos-r cos-p sin-y) (* sin-r sin-p cos-y)))))

(defun quat-identity ()
  (make-vector4 0.0 0.0 0.0 1.0))

(defun quat-norm (q)
  (normalize q))
  
(defun quat+ (q r)
  (make-quat
    (+ (x q) (x r))
    (+ (y q) (y r))
    (+ (z q) (z r))
    (+ (w q) (w q))))

(defun quat* (q r)
  (let ((q-v (qv q)) (q-w (qw q))
        (r-v (qv r)) (r-w (qw r)))
    (make-quat-from-vw
     (vec3+ (vec3+ (cross3 q-v r-v)
                   (vec-scale3 q-v r-w))
            (vec-scale3 r-v q-w))
     (- (* q-w r-w) (dot q-v r-v)))))

(defun quat-scale (q s)
  (make-quat (* s (x q))
             (* s (y q))
             (* s (z q))
             (* s (w q))))

(defun quat-conjugate (q)
  (make-quat-from-vw (vec-neg3 (qv q)) (qw q)))

(defun quat-rotate-vec (q v)
  "Rotates a vector or point v by quaternion q.
   Calculated Q' = QVQ* where V = <v, 0.0>
   @arg[q]{quaternion to rotate by}
   @arg[v]{point or vector being rotated}
   @return{a point}"
  (quat* q (quat* (vec3->quat v) (quat-conjugate q))))

(defun quat->matrix (q)
  "Convert a quaternion into a matrix (if this weren't lisp, i'd be more exited
   at the lack of trig functions)
   @return{a matrix that can be used to rotate objects according to
   the quaternion}"
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

(defun quat-rotate-to-vec (srcVec destVec)
  "Creates a quaternion that will reorient a vector pointing in
   srcVec to point in destVec"
  (let* ((ns (norm4 srcVec))
         (nt (norm4 destVec))
         (u (cross ns nt))
         (e (dot ns nt))
         (radical (sqrt (* 2.0 (+ 1.0 e)))))
    (make-quat-from-vw   (vec-scale4 u (/ 1.0 radical)) ; qv
                         (/ radical 2.0))))             ; qw

(defun quat-slerp (q1 q2 s)
  (let* ((phi (iter (for a in-vector q1)
                    (for b in-vector q2)
                    (sum (* a b))))
         (sin-phi (sin phi)))
    (quat+ (quat-scale q1 (/ (sin (* phi (- 1 s))) 
                             sin-phi))
           (quat-scale q2 (/ (sin (* phi s)) 
                             sin-phi)))))