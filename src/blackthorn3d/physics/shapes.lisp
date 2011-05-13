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

;; returns a list (lows highs)
(defgeneric shape-bounds (shape))

;; May need to add triangle class

(defclass bounding-shape ()
  ((pos :accessor pos :initarg :pos)))

(defclass bounding-sphere (bounding-shape)
  ((rad :accessor rad :initarg :rad))
  (:documentation "sphere object, used in collision detection"))

(defmethod shape-bounds ((sphere bounding-sphere))
  (with-slots (rad pos) sphere
    (let ((disp-vec (make-vec3 rad rad rad)))
      (list (vec3+ pos disp-vec)
            (vec3- pos disp-vec)))))

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

(defmethod move-bounding-volume ((bv bounding-sphere) move-vec)
  (with-slots (pos rad) bv
    (make-instance 'bounding-sphere
		   :pos (vec4+ pos move-vec)
		   :rad rad)))

(defmethod move-bounding-volume (a move-vec) nil)

(defmethod move-bounding-volume-set ((bv bounding-shape) move-vec)
  (setf (pos bv) (vec4+ (pos bv) move-vec)))

;; By Robert - note, changed to defun, as there isn't a reason for it to
;;             be a method
(defun find-bounding-points (vect-array)
  (iter (for i in-vector (subseq vect-array 0 3))
	(maximizing (aref i 0) into max-x)
	(maximizing (aref i 1) into max-y)
	(maximizing (aref i 2) into max-z)
	(minimizing (aref i 0) into min-x)
	(minimizing (aref i 1) into min-y)
	(minimizing (aref i 2) into min-z)
	(finally (return (vector (make-point3 min-x min-y min-z) 
				 (make-point3 max-x max-y max-z))))))

;; for trianles only!!! even though it allows all simple vectors
(defmethod shape-bounds ((tri simple-vector))
  (let ((bounds (find-bounding-points tri)))
    (list (svref bounds 0) (svref bounds 1))))

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

(defmethod distance (vec1 vec2)
  (mag (vec4- vec1 vec2)))

#+disabled
(defun combine-spheres (sph1 sph2)
  (with-slots ((p1 pos) (r1 rad)) sph1
    (with-slots ((p2 pos) (r2 rad)) sph2
      (let* ((t-vec (vec4- p2 p1))
             (d (mag t-vec))
             (r (/ (+ (mag t-vec) r1 r2)
                   2.0)))
        (make-instance 'bounding-sphere
                       :pos (vec4+ (pos sph1) 
                                   (vec-scale4 t-vec (/ (rad sph2) 
                                                        (rad sph1))))
                       :rad r)))))

#+disabled
(defmethod combine-bounding-volumes (list-bv)
  (labels ())
  (let ((mid-point (iter (for bv in list-bv)
		     (with-slots (pos) bv
		       (reducing pos by vec4+ into sum-vec))
		     (finally (vec-scale sum-vec (/ 1 (length list-bv)))))))
    (iter (for bv in list-bv)
      (maximizing ))))

#+disabled
(defmethod transform-bounding-volume ((this bounding-sphere) xform)
  (with-slots (rad pos) this
    (setf pos (matrix-multiply-v xform pos)
          rad (* rad scale-factor xform))))