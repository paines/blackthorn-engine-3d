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
;;; 1D curves
;;;

(defvar herm-mat
  #2A ((2.0 -3.0 0.0 1.0)
       (-2.0 3.0 0.0 0.0)
       (1.0 -2.0 1.0 0.0)
       (1.0 -1.0 0.0 0.0)))

(defmacro inv-lerp (time t0 t1)
  (once-only (time t0 t1)
             `(/ (- ,time ,t0) (- ,t1 ,t0))))

#+disabled
(defmacro t-vec (time)
  (once-only (time)
             `#(,(expt time 3)
                ,(expt time 2)
                ,time
                1.0)))

;; Returns #(a b c d)
(defun calc-1d-hermite-coefs (p0 p1 v0 v1 dt)
  (let ((right-vec (make-vector4 p0
                                 p1
                                 (* dt v0)
                                 (* dt v1))))
    (matrix-multiply-v herm-mat right-vec)))

;; Returns a float
(defun eval-1d-cubic (coefs time)
  (let ((a (svref coefs 0))
        (b (svref coefs 1))
        (c (svref coefs 2))
        (d (svref coefs 3)))
    ;; optimized dot product type deal
    ;; [t^3 t^2 t 1].[a b c d]
    (+ d (* time (+ c (* time (+ b (* u a))))))))
