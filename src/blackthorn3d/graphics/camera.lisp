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

;;;;
;;;; Yay cameras!
;;;;

(defun set-cam-pos (cam pos)
  (set-col cam 3 pos))
  
(defun get-cam-pos (cam)
  (get-col cam 3))
  
(defun translate-cam (cam vec)
  "translates a camera by the vector in vec.  Modifies it's paramter"
  (iter (for i below 3)
        (setf (aref cam i 3) (+ (aref cam i 3) (svref vec i)))))

(defun move-cam (cam pos)
  (set-cam-pos cam pos))

(defun mult-cam (cam matrix)
  (matrix-mulitply-m cam matrix))

(defun make-camera-matrix (e d up)
   "@return{A 4x4 matrix representing a camera's location and direction"
  (let* ((cam (make-matrix4x4))
         (z (norm (vec-neg d)))
         (x (norm (cross up z)))
         (y (cross z x)))
    (set-col cam 0 x) 
    (set-col cam 1 y) 
    (set-col cam 2 z) 
    (set-col cam 3 e)
    (set-row cam 3 (make-vector4 0.0 0.0 0.0 1.0))))
  
;;; Get the inverse matrix for the modelview matrix
;;; This is done by computing the inverse of the 3x3 
;;; rotation part of the matrix (the same as its 
;;; transpose and then inverting the translate
;;; (negating the values) and putting in the final
;;; matrix
(defun cam-inverse (cam-mat)
  (set-cam-pos
     (set-row (transpose cam-mat) 3 (make-vec3 0.0 0.0 0.0))
     (vec-neg (get-cam-pos cam-mat))))

