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

(defun swept-sphere->aabb (sphere vel)
  ;; box encompassing a swept sphere
  (let* ((norm-v (norm3 vel))
         (c1 (pos sphere))
         (c2 (vec3+ c1 vel))
         (min-p (make-point3 (min (x c1) (x c2))
                             (min (y c1) (y c2))
                             (min (z c1) (z c2))))
         (max-p (make-point3 (max (x c1) (x c2))
                             (max (y c1) (y c2))
                             (max (z c1) (z c2))))
         (rad-vec (make-vec3 (rad sphere)
                             (rad sphere)
                             (rad sphere))))
    (make-instance 'aa-bounding-box
                   :a-min (vec3->point (vec3- min-p rad-vec))
                   :a-max (vec3->point (vec3+ max-p rad-vec)))))

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
    (setf pos (vec-scale4 (vec4+ a-min a-max) 0.5))))

(defmethod rad ((object aa-bounding-box))
  (with-slots (a-min a-max) b-box
    (mag (vec-scale4 (vec4+ a-min a-max) 0.5))))

(defmethod shape-bounds ((box aa-bounding-box))
  (list (a-min box) (a-max box)))

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
  (iter (for i in-vector vect-array)
	(maximizing (x i) into max-x)
	(maximizing (y i) into max-y)
	(maximizing (z i) into max-z)
	(minimizing (x i) into min-x)
	(minimizing (y i) into min-y)
	(minimizing (z i) into min-z)
	(finally (return (vector (make-point3 min-x min-y min-z) 
				 (make-point3 max-x max-y max-z))))))

;; for triangles only!!! even though it allows all simple vectors
(defmethod shape-bounds ((tri vector))
  (let ((bounds (find-bounding-points (subseq tri 0 3))))
    (format t 
"### tri-bounds: ~a
                ~a~%" (x bounds) (y bounds))
    (list (svref bounds 0) (svref bounds 1))))

(defmethod make-bounding-box (vect-array)
  (let* ((pos-list (find-bounding-points vect-array))
	 (min-point (aref pos-list 0))
	 (max-point (aref pos-list 1)))
    (make-instance 'aa-bounding-box :a-min min-point :a-max max-point)))

(defun find-min-max-points (vec-array)
  (let ((mins (iter (for i below 3)
                    (collect 
                     (make-point3 most-positive-single-float
                                  most-positive-single-float
                                  most-positive-single-float))))
        (maxes (iter (for i below 3)
                     (collect 
                      (make-point3 most-negative-single-float
                                   most-negative-single-float
                                   most-negative-single-float)))))
    (iter (for v in-vector vec-array)
          (iter (for axis below 3)
                (for min in mins)
                (for max in maxes)
                (when (< (svref v axis) (svref min axis))
                  (setf (elt mins axis) (to-float v)))
                (when (> (svref v axis) (svref max axis))
                  (setf (elt maxes axis) (to-float v)))))
    (mapcar #'cons mins maxes)))

(defmethod make-bounding-sphere (vect-array)
  ;; tighter spheres this way, but not super good...
  ;; now maybe even better
  (when vect-array
    (let 
        ((bv1
          (let* ((pairs (find-min-max-points vect-array))
                 (axis (iter (with max-dist = 0.0)
                             (with axis = nil)
                             (for i below 3)
                             (for pair in pairs)
                             (when (> (pt-dist (car pair) (cdr pair)) max-dist)
                               (setf axis i))
                             (finally (return axis))))
                 (pair (elt pairs axis))
                 (pos (mid-point (car pair) (cdr pair)))
                 (radius (* 0.5 (pt-dist (car pair) (cdr pair)))))
             
            ;; Now iterate over ever vertices and check if it's outside
            ;; the sphere.  If it is, expand the sphere
            (labels ((sphere-contains (c r2 pt)
                       (<= (mag (vec3- pt c)) r2)))

              (iter (with r2 = (sq radius))
                    (for v in-vector vect-array)
                    (unless (sphere-contains pos radius v)
                      (let ((dist (pt-dist v pos))
                            (dir (norm3 (vec3- v pos))))
                        (setf pos 
                              (vec3+ 
                               pos 
                               (vec-scale3 dir (* 0.5 (- dist radius)))))
                        (setf radius
                              (* 0.5 (+ dist radius))))))
              (list radius pos))))
         (bv2
          (let ((mean-point
                 (iter (for v in-vector vect-array)
                       (sum (x v) into sumx)
                       (sum (y v) into sumy)
                       (sum (z v) into sumz)
                       (finally 
                        (return (vec-scale4 (make-point3 sumx sumy sumz) 
                                            (/ 1 (length vect-array))))))))
            (iter (for v in-vector vect-array)
                  (for sq-radius 
                       initially 0.0
                       then (max sq-radius (sq-mag (vec3- v mean-point))))
                  (finally 
                   (return (list (sqrt sq-radius) mean-point)))))))

      ;; return the one with the smallest radius -> smallest surface area
      (if (< (car bv1) (car bv2))
          (make-instance 'bounding-sphere
                         :rad (car bv1)
                         :pos (vec3->point (cadr bv1)))
          (make-instance 'bounding-sphere
                         :rad (car bv2)
                         :pos (vec3->point (cadr bv2))))))

  #+disabled
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


(defmethod combine-bounding-spheres (list-bv)
  (labels ())
  (let ((mid-point (iter (for bv in list-bv)
		     (with-slots (pos) bv
		       (reducing pos by #'vec4+ into sum-vec 
                                 initial-value +zero-vec+))
		     (finally 
                      (return (vec-scale4 sum-vec 
                                          (/ 1 (length list-bv))))))))
    (iter (for bv in list-bv)
          (with-slots (pos rad) bv
            (maximizing (+ (sq-mag (vec4- pos mid-point)) (* rad rad)) 
                        into radius))
          (finally (return (make-instance 
                            'bounding-sphere
                            :pos mid-point
                            :rad radius))))))

(defmethod transform-bounding-volume ((this bounding-sphere) xform
                                      &optional ignore-r)
  (with-slots (rad pos) this
    (make-instance 'bounding-sphere
                 :pos (matrix-multiply-v xform pos)
                 :rad (if ignore-r
                          rad
                          (mag (matrix-multiply-v 
                                xform
                                (make-vec3 rad 0.0 0.0)))))))