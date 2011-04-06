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

(in-package :blackthorn3d-utils)

;;;
;;; Vector Stuff
;;;

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

(defun dot (a b)
  (iter (for i from 0 below 4)
        (sum (* (aref a i) (aref b i)))))

(defun cross (a b)
  (make-array float 4 :initial-elements
              (list (- (* (y v1) (z v2)) (* (z v1) (y v2)))
                    (- (* (x v1) (z v2)) (* (z v1) (x v2)))
                    (- (* (x v1) (y v2)) (* (y v1) (x v2)))
                    (w a))))

(defun vec+ (&rest vecs)
  (apply #'map 'vector #'+ vecs))

(defun vec- (&rest vecs)
  (apply #'map 'vector #'- vecs))

(defun vec* (&rest vecs)
  (apply #'map 'vector #'* vecs))
  
(defun vec/ (&rest vecs)
  (apply #'map 'vector #'/ vecs))

(defun vec-scale (v s)
  (make-array float 4 :initial-contents
    (iter (for i below 4)
              (collect (* s (svref v i))))))
  
(defun sq (x) (* x x))

(defun mag (v)
  (sqrt (+ (sq (x v))
           (sq (y v))
                   (sq (z v)))))
                   
(defun norm (v)
  (v/ v mag(v)))
  
(defun homogenize (v)
  (vec-scale v (/ (w v))))
  
(defun min-axis (v)
  (iter (for i below 4)
        (finding i minimizing (svref v i))))
                
(defun make-perp (v)
  (let ((min-a (min-axis v)))
    (case min-a
      (0 (norm (vector 0.0 -(z v) (y v))))
      (1 (norm (vector -(z v) 0.0 (x v))))
      (2 (norm (vector -(y v) (x v) 0.0))))))

;;;
;;; Matrices
;;;

(defun make-matrix (size &optional (init-contents nil))
  (if (init-contents)
    (make-array size :element-type 'float
                     :initial-contents init-contents)
    (make-array size :element-type 'float
                     :initial-element 0.0)))
  
(defun make-matrix4x4 (&optional (init-contents nil)) 
  (make-matrix '(4 4) init-contents))
                                
(defun make-matrix3x3 (&optional (init-contents nil))
  (make-matrix '(3 3) init-contents))

(defun get-col (mat col)
  (let ((nrows (array-dimension mat 0)))
    (make-array nrows :element-type 'float :initial-elements
                (iter (for i below nrows)
                      (collect (aref mat i col))))))
 
(defun set-col (mat col vec)
  (let ((nrows (array-dimension mat 0)))
    (iter (for i below (min (length vec) nrows))
          (setf (aref mat i col) (svref vec i)))
    mat))
 
(defun get-row (mat row)
  (let ((ncols (array-dimension mat 1)))
    (make-array nrows :element-type 'float :initial-elements
      (iter (for i below ncols)
                (collect (aref mat row i))))))
        
(defun set-row (mat row vec)
  (let ((ncols (array-dimension mat 1)))
    (iter (for i below (min (length vec) ncols))
          (setf (aref mat row i) (svref vec i)))
    mat))
        
;;;
;;; General Matrix and Vector Math
;;;

(defun magnitude (v)
  (sqrt (iter (for i in-vector v) (sum (sq i)))))

;; Computes the inner product of two vectors
;; Returns a float value
(defun inner-product (a b)
  (let ((len (min (length a) (length b))))
    (the float
      (iter (for i below len)
            (sum (the float (* (aref a i) (aref b i))) into s)
            (declare (float s))
            (finally (return s))))))
                  
;; Computes the outer product of two vectors (of the same size)
;; defined as:
;; a (x) b'
;; returns a matrix of size (len(a), len(a))
(defun outer-product (a b)
  (let* ((len (min (length a) (length b)))
         (new-mat (make-matrix (list len len))))
    (iter (for i below len)
          (iter (for j below len)
                (setf (aref new-mat i j) (* (aref a i) (aref b j)))))))
                
;; Calculate the transpose of a matrix m
(defun transpose (m &key size)
  (let* ((n-rows (if size (first size) (array-dimension m 0)))
         (n-cols (if size (second size) (array-dimension m 1)))
         (new-mat (make-matrix (list n-cols n-rows) :element-type 'float)))
    (iter (for i below n-rows)
          (iter (for j below n-cols)
                (setf (aref new-mat j i) (aref m i j))))))
  
;; Matrix-Vector multiplication M * v
(defun matrix-multipy-v (mat vec)
  (let* ((nrows (min (length vec) (array-dimension mat 0)))
         (ncols (array-dimension mat 1))
         (new-vec (make-array nrows :element-type 'float)))
    (iter (for i below nrows)
          (setf (aref new-vec i)
                (inner-product (get-row mat i) v)))
    new-vec))
  
;; Not sure what we would need this for...
;; column vectors have little use
(defun vector-multiply-m (v A)
  (let* ((n-rows (array-dimension mat 0))
         (n-cols (min (length v) (array-dimension mat 1)))
         (new-cvec (make-array `(1 ,n-rows) :element-type 'float)))
    (iter (for i below n-cols)
          (setf (aref new-cvec 1 i) 
                (inner-product v (get-col A i))))))

(defun matrix-multiply-m (A B)
  (let* ((n-rows (array-dimension A 0))
         (n-cols (array-dimension A 1))
         (new-mat (make-matrix (list n-rows n-cols))))
    (iter (for i below n-cols)
          (iter (for j below n-rows)
                (setf (aref new-mat j i)
                      (inner-product (get-row A j)
                                     (get-col B i)))))))

(defun place-one (index len)
  (iter (from i below len)
        (collect (if (= i index) 1.0 0.0))))

(defun make-identity (&optional (size 4))
  (make-matrix (size)
               (iter (from i below size)
                     (collect (place-one (i size))))))

(defun make-translate (v)
  (set-col (make-identity) 3 v))

(defun make-scale (v)
  (matrix-multiply-v (make-identity) v))
