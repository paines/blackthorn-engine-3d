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
;;;; Vector Stuff
;;;;

(defmacro gen-vec-accessors (&rest names)
  (labels ((vec-accessor (n p)
             (with-gensyms (v) `(defmacro ,n (,v) `(svref ,,v ,,p)))))
    `(progn
       ,@(iter (for i in names)
               (for j below (length names))
               (collect (vec-accessor i j))))))
        
;;; Create aliases for accessing different elements
;;; of vectors
(gen-vec-accessors x y z w)
(gen-vec-accessors r g b a)

(defun make-vector4 (x y z w)
  "@short{Creates a vector of length 4 of floats}
   @arg[x]{element at index 0}
   @arg[y]{element at index 1}
   @arg[z]{element at index 2}
   @arg[w]{element at index 3}"
  (make-array 4 :element-type 'float :initial-contents
    (list x y z w)))


(defun make-vector3 (x y z)
  "@short{Creates a vector of length 3 of floats}
   @arg[x]{element at index 0}
   @arg[y]{element at index 1}
   @arg[z]{element at index 2}"
  (make-array 3 :element-type 'float :initial-contents
    (list x y z)))

(defun make-point3 (x y z)
  "@short{Create a point in 3d space; automatically sets w to 1.0}"
  (make-vector4 x y z 1.0))

(defun make-vec3 (x y z)
  "@short{Create a vector in 3d space; sets w to 0.0}"
  (make-vector4 x y z 0.0))

(defun dot (a b)
  (iter (for i from 0 below 3)
        (sum (* (aref a i) (aref b i)))))

(defun cross (a b)
  (make-vector4
    (- (* (y a) (z b)) (* (z a) (y b)))
    (- (* (z a) (x b)) (* (x a) (z b)))
    (- (* (x a) (y b)) (* (y a) (x b)))
    (w a)))

(defun cross3 (a b)
  (make-vector3
    (- (* (y a) (z b)) (* (z a) (y b)))
    (- (* (z a) (x b)) (* (x a) (z b)))
    (- (* (x a) (y b)) (* (y a) (x b)))))

(defmacro vector-elt-wise (fn a b)
  `(map 'vector ,fn ,a ,b))

(defmacro vec4-3elt-op (fn a b)
  (with-gensyms (v1 v2)
    (let ((v1 a) (v2 b))
      `(make-vector4
        (,fn (x ,v1) (x ,v2))
        (,fn (y ,v1) (y ,v2))
        (,fn (z ,v1) (z ,v2))
        (w ,v1)))))

(defmacro vec3-3elt-op (fn a b)
  (with-gensyms (v1 v2)
    (let ((v1 a) (v2 b))
      `(make-vector3
        (,fn (x ,v1) (x ,v2))
        (,fn (y ,v1) (y ,v2))
        (,fn (z ,v1) (z ,v2))))))

(defun vec4+ (a b)
  (vec4-3elt-op + a b))

(defun vec3+ (a b)
  (vec3-3elt-op + a b))
  
(defun vec4- (a b)
  (vec4-3elt-op - a b))

(defun vec3- (a b)
  (vec3-3elt-op - a b))

(defun vec4* (a b)
  (vec4-3elt-op * a b))

(defun vec3* (a b)
  (vec3-3elt-op * a b))

(defun vec4/ (a b)
  (vec4-3elt-op / a b))

(defun vec3/ (a b)
  (vec3-3elt-op / a b))

(defun vec-scale4 (v s)
  (make-vector4 
    (* s (x v))
    (* s (y v))
    (* s (z v))
    (w v)))

(defun vec-scale3 (v s)
  (make-vector3
    (* s (x v))
    (* s (y v))
    (* s (z v))))

(defun vec-neg4 (v)
  (make-vector4 
    (- (x v))
    (- (y v))
    (- (z v))
    (w v)))

(defun vec-neg3 (v)
  (make-vector3 
    (- (x v))
    (- (y v))
    (- (z v))))

(defun sq (x) (* x x))

(defun mag (v)
  (sqrt (+ (+ (sq (x v))
              (sq (y v)))
           (sq (z v)))))

(defun sq-mag (v)
  (+ (+ (sq (x v))
        (sq (y v))
     (sq (z v)))))

(defun norm4 (v)
  "Normalize a vector4, see normalize for general normalize fn"
  (let ((magv (mag v)))
    (unless (zerop magv) (vec-scale4 v (/ magv)))))

(defun norm3 (v)
  "Normalize a vector3."
  (let ((magv (mag v)))
    (unless (zerop magv) (vec-scale3 v (/ magv)))))

(defun normalize (v)
  "Normalize a vector of any length"
  (let ((magv (magnitude v)))
    (unless (zerop magv)
      (map 'vector #'(lambda (x) (/ x magv)) v))))

(defun homogenize (v)
  (unless (zerop (w v)) (vec-scale v (/ (w v)))))

(defun min-axis (v)
  (iter (for i below 4)
        (finding i minimizing (svref v i))))

(defun make-perp (v)
  (let ((min-a (min-axis v)))
    (case min-a
      (0 (norm (make-vec3 0.0 (-(z v)) (y v))))
      (1 (norm (make-vec3 (-(z v)) 0.0 (x v))))
      (2 (norm (make-vec3 (-(y v)) (x v) 0.0))))))