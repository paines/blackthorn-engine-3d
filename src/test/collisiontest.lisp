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

;(defclass sphere ()
;  ((x :accessor x :initarg :x :initform 0)
;   (y :accessor y :initarg :y :initform 0)
;   (z :accessor z :initarg :z :initform 0)
;   (r :accessor r :initarg :r :initform 0)))

;(defmethod collide-p ((s1 sphere) (s2 sphere))
;  (if (< (sqrt (+ (expt (- (x s1) (x s2)) 2)
;                  (expt (- (y s1) (y s2)) 2)
;                  (expt (- (z s1) (z s2)) 2)))
;       (+ (r s1) (r s2)))
;    t nil))

(in-package :blackthorn3d-test)

(def-suite blackthorn3d-phy :in blackthorn3d)

(in-suite blackthorn3d-phy)

(test collide-p
  (let ((s1 (make-instance 'sphere :pos (make-vector3 0 0 0) :rad 5))
        (s2 (make-instance 'sphere :pos (make-vector3 1 1 1) :rad 2)))
    (is (eql (collide-p s1 s2) t)))
    ; expect T
  (let ((s1 (make-instance 'sphere :pos (make-vector3 0 0 0) :rad 1))
        (s2 (make-instance 'sphere :pos (make-vector3 3 3 3) :rad 2)))
    (is (eql (collide-p s1 s2) nil)))
    ; expect NIL
  (let ((s1 (make-instance 'sphere :pos (make-vector3 0 0 0) :rad 1))
        (s2 (make-instance 'sphere :pos (make-vector3 0 2 0) :rad 1)))
    (is (eql (collide-p s1 s2) nil)))
    ; expect NIL
  (let ((s1 (make-instance 'sphere :pos (make-vector3 0 0 0)   :rad 1))
        (s2 (make-instance 'sphere :pos (make-vector3 0 1.9 0) :rad 1)))
    (is (eql (collide-p s1 s2) t))))
    ; expect T

(test find-bounding-points
      (let* ((vert-array (vector (make-vector3 1 1 1)
                                 (make-vector3 0 2 2)
                                 (make-vector3 3 0 3)
                                 (make-vector3 3 1 0)))
             (pos-list (find-bounding-points vert-array)))
        (is (equalp (aref pos-list 0) #(0 0 0)))
	(is (equalp (aref pos-list 1) #(3 2 3)))) )
	