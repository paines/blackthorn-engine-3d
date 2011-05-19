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

(in-package :blackthorn3d-test)

(def-suite blackthorn3d-math :in blackthorn3d)

(in-suite blackthorn3d-math)

(test make-vector4
  (let ((vec (make-vector4 1.0 2.0 3.0 4.0)))
    (is (= (x vec) 1.0))
    (is (= (y vec) 2.0))
    (is (= (z vec) 3.0))
    (is (= (w vec) 4.0))))

(test scale-factor
  (let* ((scale-vec (make-vec3 5.0 5.0 5.0))
         (scale-mat (make-scale scale-vec))
         (ex-scale (extract-scale scale-mat)))
    (is (= (x ex-scale) (x scale-vec)))
    (is (= (y ex-scale) (y scale-vec)))
    (is (= (z ex-scale) (z scale-vec)))))

(test quat-rotate-to-vec
      (let* ((vec1 +x-axis+)
             (vec2 +x-axis+)
             (quat (quat-rotate-to-vec vec1 vec2))
             (vec3 (quat-rotate-vec quat (make-point3 5.0 0.0 5.0))))
        (format t "~%~%### OUR RESULT vec: ~a~%" vec3)
        (is (and
             (= 5.0 (x vec3))
             (= 0.0 (y vec3))
             (= 5.0 (z vec3))))))

#+disabled
(test q-r-decomp
      (let* ((scale-mat (make-scale #(2.0 2.0 2.0))))
        (multiple-value-bind (q r) (q-r-decomp scale-mat)
          (format t "~%Q: ~a~%R:~a~%" q r)
          (is (not (null 5))))))