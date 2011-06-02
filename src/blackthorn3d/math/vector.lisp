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

(defvar +zero-vec+ #(0.0 0.0 0.0 0.0))
(defvar +origin+ #(0.0 0.0 0.0 1.0))
(defvar +x-axis+ #(1.0 0.0 0.0 0.0))
(defvar +y-axis+ #(0.0 1.0 0.0 0.0))
(defvar +z-axis+ #(0.0 0.0 1.0 0.0))

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

(defun to-float (vec)
  (iter (for e in-vector vec)
        (collect (float e) result-type 'vector)))

(defun make-color (r g b &optional (a 1.0))
  "@short{Create a vector4 for color}
   @arg[r]{red component, float [0 1]}
   @arg[g]{green component, float [0 1]}
   @arg[b]{blue component, float [0 1]}
   @arg[a]{optional alpha defaults to 1}"
  (make-vector4 r g b a))

(defun make-vector4 (x y z w)
  "@short{Creates a vector of length 4 of floats}
   @arg[x]{element at index 0}
   @arg[y]{element at index 1}
   @arg[z]{element at index 2}
   @arg[w]{element at index 3}"
  (make-array 4 :element-type 'float :initial-contents
              (list (float x) (float y) (float z) (float w))))


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

(defun vec3->point (vec3)
  (concatenate 'vector vec3 #(1.0)))

(defun vec3->vec (vec3)
  (concatenate 'vector vec3 #(0.0)))

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

#+disabled
(defmacro element-wise-fn (v-lst fn)
  (with-gensysm (i vs)
    (let (vs v-list)
      `(iter (for ,i below (length (car ,vs)))
             (collect (apply ,fn (mapcar #'(lambda (v) (svref v ,i))
                                         vs))
                      result-type 'vector)))))

(defun vec- (&rest vs)
  (iter (for i below (length (car vs)))
        (collect
         (apply #'- (mapcar #'(lambda (v) (svref v i)) vs))
         result-type 'vector)))

(defun vec+ (&rest vs)
  (iter (for i below (length (car vs)))
        (collect
         (apply #'+ (mapcar #'(lambda (v) (svref v i)) vs))
         result-type 'vector)))

(defun vec-scale3 (v s)
  (make-vector3
    (* s (x v))
    (* s (y v))
    (* s (z v))))

(defun vec-scale (v s)
  (iter (for e in-vector v)
        (collect (* e s) result-type 'vector)))

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

(defun pt-dist (p1 p2)
  (mag (vec3- p2 p1)))

(defun mid-point (p1 p2)
  (vec3->point
   (vec3+ p1 (vec-scale3 
              (vec3- p2 p1) 0.5))))

(defun sq-mag (v)
  (+ (+ (sq (x v))
        (sq (y v))
     (sq (z v)))))

(defun norm4 (v)
  "Normalize a vector4, see normalize for general normalize fn"
  (let ((magv (mag v)))
    (if (zerop magv) v
        (vec-scale4 v (/ magv)))))

(defun norm3 (v)
  "Normalize a vector3."
  (let ((magv (mag v)))
    (if (zerop magv) v
        (vec-scale3 v (/ magv)))))

(defun normalize (v)
  "Normalize a vector of any length"
  (let ((magv (magnitude v)))
    (unless (zerop magv)
      (map 'vector #'(lambda (x) (/ x magv)) v))))

(defun set-length3 (v len)
  (vec-scale3 (norm3 v) len))

(defun set-length4 (v len)
  (vec-scale4 (norm4 v) len))

(defun homogenize (v)
  (unless (zerop (w v)) (vec-scale4 v (/ (w v)))))

(defun min-axis (v)
  (iter (for i below 4)
        (finding i minimizing (svref v i))))

(defun make-perp (v)
  (let ((min-a (min-axis v)))
    (case min-a
      (0 (norm (make-vec3 0.0 (-(z v)) (y v))))
      (1 (norm (make-vec3 (-(z v)) 0.0 (x v))))
      (2 (norm (make-vec3 (-(y v)) (x v) 0.0))))))

(defun spherical->cartesian (sphere-coords &optional vec-p)
  (let* ((phi   (elt sphere-coords 0))
         (theta (elt sphere-coords 1))
         (r     (elt sphere-coords 2))
         (sin-theta (sin (- (/ pi 2) theta))))
    (make-vector4 (* r (sin phi) sin-theta)
                  (* r (cos (- (/ pi 2) theta)))
                  (* r (cos phi) sin-theta)
                  (if vec-p 0.0 1.0))))

(defun get-perpendicular (vec)
  (if (or (not (zerop (x vec)))
          (not (zerop (y vec))))
      (norm4 (make-vec3 (y vec) (- (x vec)) 0.0))
      (norm4 (make-vec3 (z vec) 0.0 (- (x vec))))))

(defun zero-vec-p (vec)
  (and (zerop (x vec))
       (zerop (y vec))
       (zerop (z vec))))

(defun tri-centroid (v0 v1 v2)
  (make-vector3 
   (/ (+ (x v0) (x v1) (x v2)) 3.0)
   (/ (+ (y v0) (y v1) (y v2)) 3.0)
   (/ (+ (z v0) (z v1) (z v2)) 3.0)))

;; TODO: fix this later
#+disabled 
(defun centroid (&rest vecs)
  (let ((v-len (length (car vecs))))
    (iter (for v in vecs)
          (counting t into len)
          (iter (for i below v-len)
                (collect )))))
                
(defun vector-sum (vectors)
  (reduce #'vec4+ vectors :initial-value +zero-vec+))

(defun max-axis (vector)
  (iter (with max-axis = 0)
        (with max = most-negative-single-float)
        (for i below 3)
        (when (> (svref vector i) max)
          (setf max-axis i))
        (finally (return max-axis))))


(defvar +directions+
  (list :north (vec-neg4 +z-axis+)
        :east  +x-axis+
        :south +z-axis+
        :west (vec-neg4 +x-axis+)
        :up +y-axis+
        :down (vec-neg4 +y-axis+)))



(defun get-direction (vector)
  "returns the direction (axis) the vector points most in"
  (let ((max-axis (max-axis vector))
        axis)
    (case max-axis
      (0 (if (< (x vector) 0.0) :west :east))
      (1 (if (< (y vector) 0.0) :down :up))
      (2 (if (< (z vector) 0.0) :north :south)))))

(defmethod opposite-dir ((vec symbol))
  (case vec
    (:east :west)
    (:west :east)
    (:south :north)
    (:north :south)
    (:up :down)
    (:down :up)))

(defmethod opposite-dir ((vec vector))
  (vec-neg4 vec))

(defun to-vec4 (point)
  (make-vec3 (x point) (y point) (z point)))

(defun to-point4 (vec)
  (make-point3 (x vec) (y vec) (z vec)))
  
(defun vec-eql (v1 v2)
  (let ((epsilon 0.001))
    (and (< (+ (abs (x v1)) (abs (x v2))) epsilon)
         (< (+ (abs (y v1)) (abs (y v2))) epsilon)
         (< (+ (abs (z v1)) (abs (z v2))) epsilon))))