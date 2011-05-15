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

(defun set-mat (A B)
  "Sets contents of a to contents of b"
  (let ((size (array-dimensions A)))
    (iter (for i below (* (first size) (second size)))
          (setf (row-major-aref A i) (row-major-aref B i)))
    A))

(defun copy-matrix (mat)
  "@return{a matrix with the same elements as mat}"
  (let* ((n-row (get-nrows mat))
         (n-col (get-ncols mat))
         (copy  (make-matrix (list n-row n-col))))
    (iter (for i below (* n-row n-col))
         (setf (row-major-aref copy i) (row-major-aref mat i)))
    copy))

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
                (setf (aref new-mat i j)
                      (inner-product (row A j)
                                     (col B i)))))
	new-mat))

(defun place-one (index len)
  (iter (for i below len)
        (collect (if (= i index) 1.0 0.0))))

(defun make-identity-matrix (&optional (size 4))
  "@return{a square identity matrix}"
  (make-matrix (list size size)
               (iter (for i below size)
                     (collect (place-one i size)))))

(defun make-ortho-basis (u v w)
  "@arg[u]{the axis the x-axis will map to}
   @arg[v]{the axis the y-axis will map to}
   @arg[z]{the axis the z-axis will map to}
   @return{a 4x4 matrix that will convert coordinates from the default
           basis to the one defined by u v w}"
  (make-matrix4x4 
   (iter (for i below 4) 
         (collect
             (if (< i 3)
                 (list (svref u i) (svref v i) (svref w i) 0.0)
                 (list 0.0 0.0 0.0 1.0))))))

(defun make-inv-ortho-basis (u v w)
  "@return{a 4x4 matrix that will convert coordinates of the basis
   formed by uvw to the standard basis"
  (transpose (make-ortho-basis u v w)))

(defun make-translate (v)
  "@return{a translation matrix that will translate points by v}"
  (let ((mat (make-identity-matrix)))
    (setf (col mat 3) v)
    (setf (aref mat 3 3) 1.0)
    mat))

(defun make-scale (v)
  "@return{a scaling matrix that will scale points by v}"
  (make-matrix4x4
   `((,(x v) 0.0 0.0 0.0)
     (0.0 ,(y v) 0.0 0.0)
     (0.0 0.0 ,(z v) 0.0)
     (0.0 0.0 0.0 1.0))))

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

(defun reshape (mat size)
  (let ((new-sz (apply #'* size))
        (mat-sz (apply #'* (array-dimensions mat))))
    (unless (/= new-sz mat-sz)
      (let ((new-mat (make-matrix size)))
        (iter (for i below new-sz)
              (setf (row-major-aref new-mat i) (row-major-aref mat i)))
        new-mat))))

(defun make-projection (left right top bottom near far)
  (if (and (= top (- bottom))
           (= left (- right)))
      ;; Symetrical frustum (yay!)
      (make-matrix4x4
       `(( ,(/ near right) 0.0 0.0 0.0 )
         ( 0.0 ,(/ near top) 0.0 0.0 )
         ( 0.0 0.0 ,(/ (- (+ far near)) (- far near)) -1.0 )
         ( 0.0 0.0 ,(/ (- (* 2 far near)) (- far near)) 0.0 )))
      ;; Significantly less yay...
      (let ((n2 (* near 2))
            (r-l (- right left))
            (t-b (- top bottom))
            (f-n (- far near)))
        (make-matrix4x4
         `(( ,(/ n2 r-l) 0.0 0.0 0.0)
           ( 0.0 ,(/ n2 t-b) 0.0 0.0)
           ( ,(/ (+ right left) r-l) 
              ,(/ (+ top bottom) t-b) 
              ,(/ (- (+ far near)) f-n) 
              -1.0)
           ( 0.0 0.0 ,(/ (- (* 2 far near)) f-n) 0.0))))))

(defun look-at-matrix (from to up)
  "Returns a matrix that transforms points and vectors to a coordinate
   system that looks at TO from FROM with UP as the up orientation"
  (let* ((dir (vec4- from to)))
    (setf (w dir) 0.0)
    (let* ((w (norm4 dir))
           (u (norm4 (cross up w)))
           (v (cross w u))
           (la-mat (make-ortho-basis u v w)))
      (setf (col la-mat 3) (matrix-multiply-v la-mat (vec-neg4 from)))
      la-mat)))

(defun look-dir-matrix (from dir up)
  "Returns a matrix that transforms points and vectors to a coordinate
   system that looks from FROM in direction DIR with UP as up"
  (let* ((w (vec-neg4 (norm4 dir)))
         (u (norm4 (cross up w)))
         (v (cross w u))
         (ld-mat (make-ortho-basis u v w)))
    (setf (col ld-mat 3) (matrix-multiply-v ld-mat (vec-neg4 from)))
    ld-mat))

(defun rt-inverse (mat)
  (let ((Rmat (copy-matrix mat))
        (Tvec (vec-neg4 (col mat 3))))
    (setf (col Rmat 3) (make-point3 0.0 0.0 0.0))
    (setf Rmat (transpose Rmat))
    (setf (col Rmat 3) (matrix-multiply-v Rmat Tvec))
    Rmat))

(defun vec-cols->matrix (vecs)
  (let* ((n-rows (iter (for vec in-vector vecs)
                       (minimizing (length vec))))
         (n-cols (length vecs))
         (n-mat (make-matrix (list n-cols n-rows))))
    (iter (for i below (* n-rows n-cols) by n-rows)
          (for vec in-vector vecs)
          (iter (for j below n-rows)
                (setf (row-major-aref n-mat (+ i j)) (svref vec j))))
    n-mat))

(defun vec-rows->matrix (vecs)
  (let* ((n-cols (iter (for vec in-vector vecs)
                       (minimizing (length vec))))
         (n-rows (length vecs))
         (n-mat (make-matrix (list n-cols n-rows))))
    (iter (for i below (* n-rows n-cols) by n-rows)
          (iter (for j below n-rows)
                (for vec in-vector vecs)
                (setf (row-major-aref n-mat (+ i j)) (svref vec j))))
    n-mat))

#+disabled
(defun Q-R-decomp (mat)
  (labels ((proj (e a) (vec-scale 
                        e (/ (inner-product e a)
                             (inner-product e e)))))
    (destructuring-bind (e-lst R-lst)
      (iter (for k in (n-cols mat))
            (for a-k = (col mat k))
            (for u-k first a-k 
                 then (iter (for e in e-lst)
                            (sum (proj e a-k) into s)
                            (finally (return (- a-k s)))))
            (collect u-k into u-lst)
            (collect (normalize u-k) into e-lst)
            (collect 
             (append 
              (iter (for e-i in e-lst)
                    (collect (inner-product e-i a-k)))
              (iter (for i from (length e-lst) below (- (n-rows mat) 
                                                        (length e-lst)))
                    (collect 0.0))) into R-mat)
            (finally (return (list e-lst R-mat))))
      (values (apply #'vec-cols->matrix e-lst)
              (make-matrix (list (n-cols mat) (n-rows mat))
                           R-lst)))))
;#+disabled
(defun extract-scale (mat)
  "Extract the scale factors of an affine transform
   @return{a vec3 with the scale along each axis}"
  (list
   (mag (row mat 0))
   (mag (row mat 1))
   (mag (row mat 2))))

(defun extract-translate (mat)
  (make-vec3 (aref mat 3 0)
             (aref mat 3 1)
             (aref mat 3 2)
             0.0))