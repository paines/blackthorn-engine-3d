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

(defun vec+ (a b)
  (vector-elt-wise #'- a b))
  
(defun vec- (a b)
  (vector-elt-wise #'- a b))

(defun vec* (a b)
  (vector-elt-wise #'* a b))

(defun vec/ (a b)
  (vector-elt-wise #'/ a b))

(defun vec-scale (v s)
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

(defun vec-neg (v)
  (make-vector4 
    (- (x v))
    (- (y v))
    (- (z v))
    (w v)))

(defun sq (x) (* x x))

(defun mag (v)
  (sqrt (+ (+ (sq (x v))
              (sq (y v)))
           (sq (z v)))))

(defun sq-mag (v)
  (+ (+ (sq (x v))
        (sq (y v))
     (sq (z v)))))

(defun norm (v)
  "Normalize a vector4, see normalize for general normalize fn"
  (let ((magv (mag v)))
    (unless (zerop magv) (vec-scale v (/ magv)))))


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

;;;
;;; Matrices
;;;


(defun transpose-lists (lsts)
  (apply #'mapcar #'list lsts))

(defun make-matrix (size &optional init-contents)
  (if init-contents
    (make-array size :element-type 'float
                     :initial-contents init-contents)
    (make-array size :element-type 'float
                     :initial-element 0.0)))

(defun make-matrix4x4 (&optional init-contents) 
  (make-matrix '(4 4) init-contents))

(defun make-matrix3x3 (&optional init-contents)
  (make-matrix '(3 3) init-contents))
  
(defun get-nrows (mat)
  (array-dimension mat 1))
  
(defun get-ncols (mat)
  (array-dimension mat 0))

(defun copy-matrix (mat)
  "@return{a matrix with the same elements as mat}"
  (let* ((n-row (get-nrows mat))
         (n-col (get-ncols mat))
         (copy  (make-matrix (list n-row n-col))))
    (iter (for i below (* n-row n-col))
         (setf (row-major-aref copy i) (row-major-aref mat i)))))

(defun col (mat c)
  "@arg[mat]{matrix of any size}
   @arg[col]{zero-index column into mat}
   @return{the column of mat as a simple-vector}"
  (let ((nrows (get-nrows mat)))
    (make-array nrows :element-type 'float :initial-contents
      (iter (for i below nrows)
            (collect (aref mat c i))))))

(defun (setf col) (vec mat c)
  "@arg[mat]{matrix of any size mxn}
   @arg[col]{zero-index column into mat}
   @arg[vec]{the vector of length l to replace col. 
             If l < m. the elements in range [l m) in mat not modified.
             If l > m, only the [0 m) elements of vec are copied}
   @return{the modified matrix.  Modifies argument mat}"
  (let ((nrows (get-nrows mat)))
    (iter (for i below (min (length vec) nrows))
          (setf (aref mat c i) (svref vec i)))
    vec))

(defun row (mat r)
  "@arg[mat]{matrix of any size}
   @arg[row]{zero-index row into mat}
   @return{the row of mat as a simple-vector}"
  (let ((ncols (get-ncols mat)))
    (make-array ncols :element-type 'float :initial-contents
      (iter (for i below ncols)
                (collect (aref mat i r))))))

(defun (setf row) (vec mat r)
  "@arg[mat]{matrix of any size mxn}
   @arg[row]{zero-index row into mat}
   @arg[vec]{the vector of length l to replace row of mat. 
             If l < n. the elements in range [l n) in mat not modified.
             If l > n, only the [0 n) elements of vec are copied}
   @return{the modified matrix.  Modifies argument mat}"
  (let ((ncols (get-ncols mat)))
    (iter (for c below (min (length vec) ncols))
          (setf (aref mat c r) (svref vec c)))
    vec))

;;;
;;; General Matrix and Vector Math
;;;

(defun magnitude (v)
  "@short{the magnitude of a vector of any length}"
  (sqrt (iter (for i in-vector v) (sum (sq i)))))

(defun inner-product (a b)
  "Computes the inner product of two vectors
   @returns{float result of a . b}"
  (let ((len (min (length a) (length b))))
      (iter (for i below len)
            (sum (* (aref a i) (aref b i))))))

(defun outer-product (a b)
  "Computes the outer product of two vectors (of the same size)
   defined as:
   a (x) b'
   @return{a matrix of size (len(a), len(b))}"
  (let* ((len (min (length a) (length b)))
         (new-mat (make-matrix (list len len))))
    (iter (for i below len)
          (iter (for j below len)
                (setf (aref new-mat i j) (* (aref a j) (aref b i)))))))

;; Calculate the transpose of a matrix m
(defun transpose (m)
  "@return{the transpose of matrix m}"
  (let* ((n-rows (get-nrows m))
         (n-cols (get-ncols m))
         (new-mat (make-matrix (list n-cols n-rows))))
    (iter (for i below n-rows)
          (iter (for j below n-cols)
                (setf (aref new-mat j i) (aref m i j))))
    new-mat))
  
(defun matrix-multiply-v (mat vec)
  "Multiply matrix mat by column vector vec
   @arg[mat]{a matrix of size m x n}
   @arg[vec]{a vector of size n}
   @return{the result of multiplying mat by vec, a vector of size m}"
  (let* ((nrows (min (length vec) (get-nrows mat)))
         (new-vec (make-array nrows :element-type 'float)))
    (iter (for i below nrows)
          (setf (aref new-vec i)
                (inner-product (row mat i) vec)))
    new-vec))

;; Not sure what we would need this for...
;; row vectors have little use
(defun vector-multiply-m (v A)
  "Multiply a row vector by a matrix
   @arg[v]{a vector of size m}
   @arg[A]{a matrix of size m x n}
   @return{the result of multiplying v by A, a row vector of size n}"
  (let* ((n-rows (get-nrows A))
         (n-cols (min (length v) (get-ncols A)))
         (new-cvec (make-array `(1 ,n-rows) :element-type 'float)))
    (iter (for i below n-cols)
          (setf (aref new-cvec 1 i) 
                (inner-product v (col A i))))
	new-cvec))

(defun matrix-multiply-m (A B)
  "Multiply a matrix A by matrix B
   @arg[A]{Matrix on left-side of multiplication. Should be size m x n}
   @arg[B]{Matrix on right-side of multiplication.  Should be size n x k}
   @return{the m x k matrix that results from performing A * B}"
  (let* ((n-rows (get-nrows A))
         (n-cols (get-ncols B))
         (new-mat (make-matrix (list n-rows n-cols))))
    (iter (for i below n-cols)
          (iter (for j below n-rows)
                (setf (aref new-mat j i)
                      (inner-product (row A j)
                                     (col B i)))))
	new-mat))

(defun place-one (index len)
  (iter (for i below len)
        (collect (if (= i index) 1.0 0.0))))

(defun make-identity (&optional (size 4))
  "@return{a square identity matrix}"
  (make-matrix (list size size)
               (iter (for i below size)
                     (collect (place-one i size)))))

;(defun make-ortho-basis (u v w)
;  "@arg[u]{the axis the x-axis will map to}
;   @arg[v]{the axis the y-axis will map to}
;   @arg[z]{the axis the z-axis will map to}
;   @return{a 4x4 matrix that will convert coordinates from the default
;           basis to the one defined by u v w}"
;  (let* ((basis (make-matrix4x4))
;         (z (norm (vec-neg d)))
;         (x (norm (cross up z)))
;         (y (cross z x)))
;    (set-col cam 0 x) 
;    (set-col cam 1 y) 
;    (set-col cam 2 z) 
;    (set-col cam 3 e)
;    (set-row cam 3 (make-vector4 0.0 0.0 0.0 1.0))))

(defun make-translate (v)
  "@return{a translation matrix that will translate points by v}"
  (setf (col (make-identity) 3) v))

(defun make-scale (v)
  "@return{a scaling matrix that will scale points by v}"
  (matrix-multiply-v (make-identity) v))

(defun make-x-rot (r)
  "@arg[r]{radians to rotate around x-axis (counter-clockwise)}
   @return{a 4x4 rotation matrix}"
  (let ((cosr (cos r))
        (sinr (sin r)))
    (make-matrix4x4 
      (transpose-lists  `((1.0 0.0 0.0 0.0)
                          (0.0 ,cosr ,(- sinr) 0.0)
                          (0.0 ,sinr ,cosr 0.0)
                          (0.0 0.0 0.0 1.0))))))

(defun make-y-rot (r)
  "@arg[r]{radians to rotate around y-axis (counter-clockwise)}
   @return{a 4x4 rotation matrix}"
  (let ((cosr (cos r))
        (sinr (sin r)))
    (make-matrix4x4 
      (transpose-lists `((,cosr 0.0 ,sinr 0.0)
                         (0.0 1.0 0.0 0.0)
                         (,(- sinr) 0.0 ,cosr 0.0)
                         (0.0 0.0 0.0 1.0))))))

(defun make-z-rot (r)
  "@arg[r]{radians to rotate around z-axis (counter-clockwise)}
   @return{a 4x4 rotation matrix}"
  (let ((cosr (cos r))
        (sinr (sin r)))
    (make-matrix4x4 
      (transpose-lists `((,cosr ,(- sinr) 0.0 0.0)
                         (,sinr ,cosr 0.0 0.0)
                         (0.0 0.0 1.0 0.0)
                         (0.0 0.0 0.0 1.0))))))
