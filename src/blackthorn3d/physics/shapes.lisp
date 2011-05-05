;;;; Blackthorn -- Lisp Game Engine
;;;;
;;;; Copyright (c) 2011, Michael Matthews <iismichaels@gmail.com>
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

(in-package :blackthorn3d-physics)

(defclass bounding-shape ()
  ((pos :accessor pos :initarg :pos)))

(defclass bounding-sphere (bounding-shape)
  ((rad :accessor rad :initarg :rad))
  (:documentation "sphere object, used in collision detection"))

(defclass aa-bounding-box (bounding-shape)
  ((a-min
    :accessor a-min
    :initarg :a-min
    :initform (make-point3 -1.0 -1.0 -1.0))
   (a-max
    :accessor a-max
    :initarg :a-max
    :initform (make-point3 1.0 1.0 1.0))))

(defmethod initialize-instance :after ((b-box aa-bounding-box) &key)
  (with-slots (a-min a-max) b-box
    (setf pos (vec-scale4 (vec4+ a-min a-max) 2))))

;#+disabled
(defclass o-bounding-box ()
  ((center
    :accessor center
    :initarg :center
    :initform (make-point3 0.0 0.0 0.0))
   (axes
    :accessor axes
    :initarg :axes
    :initform (make-identity-matrix))
   (half-lengths
    :accessor half-lengths
    :initarg :half-lengths
    :initform (make-vector3 1.0 1.0 1.0))))

(defmethod find-bounding-points (vect-array)
  (iter (for i in-vector vect-array)
	(maximizing (aref i 0) into max-x)
	(maximizing (aref i 1) into max-y)
	(maximizing (aref i 2) into max-z)
	(minimizing (aref i 0) into min-x)
	(minimizing (aref i 1) into min-y)
	(minimizing (aref i 2) into min-z)
	(finally (return (vector (make-point3 min-x min-y min-z) 
				 (make-point3 max-x max-y max-z))))))

(defmethod make-bounding-box (vect-array)
  (let* ((pos-list (find-bounding-points vect-array))
	 (min-point (aref pos-list 0))
	 (max-point (aref pos-list 1)))
    (make-instance 'aa-bounding-box :a-min min-point :a-max max-point)))

(defmethod make-bounding-sphere (vect-array)
  (let* ((pos-list (find-bounding-points vect-array))
	 (min-point (aref pos-list 0))
	 (max-point (aref pos-list 1))
	 (rad-vector (vec-scale3 (vec3- max-point min-point) 0.5)))
    (make-instance 'bounding-sphere :pos (vec4+ min-point rad-vector)
		           :rad (mag rad-vector))))

(defmethod make-bounding-volume (vect-array)
  (make-bounding-sphere vect-array))

#+disabled
(defmethod transform-bounding-volume ((this bounding-sphere) xform)
  (with-slots (rad pos) this
    (setf pos (matrix-multiply-v xform pos)
          rad (* rad scale-factor xform))))
