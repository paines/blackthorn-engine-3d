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
  (let ((s1 (make-instance 'bounding-sphere
                           :pos (make-point3 0.0 0.0 0.0)
                           :rad 5.0))
        (s2 (make-instance 'bounding-sphere
                           :pos (make-point3 1.0 1.0 1.0)
                           :rad 2.0)))
    (is (eql (collide-p s1 s2) t))
    )
    ; expect T
  (let ((s1 (make-instance 'bounding-sphere
                           :pos (make-point3 0.0 0.0 0.0)
                           :rad 1.0))
        (s2 (make-instance 'bounding-sphere
                           :pos (make-point3 3.0 3.0 3.0)
                           :rad 2.0)))
    (is (eql (collide-p s1 s2) nil))
    )
    ; expect NIL
  (let ((s1 (make-instance 'bounding-sphere
                           :pos (make-point3 0.0 0.0 0.0)
                           :rad 1.0))
        (s2 (make-instance 'bounding-sphere
                           :pos (make-point3 0.0 2.0 0.0)
                           :rad 1.0)))
    (is (eql (collide-p s1 s2) nil))    )
    ; expect NIL
  (let ((s1 (make-instance 'bounding-sphere
                           :pos (make-point3 0.0 0.0 0.0)
                           :rad 1.0))
        (s2 (make-instance 'bounding-sphere
                           :pos (make-point3 0.0 1.9 0.0)
                           :rad 1.0)))
    (is (eql (collide-p s1 s2) t)))
    ; expect T
  (let ((s1 (make-instance 'bounding-sphere
                           :pos (make-point3 0.0 0.0 0.0)
                           :rad 1.0))
	(s2 (make-instance 'aa-bounding-box :a-min (make-point3 -1.0 -1.0 -1.0) 
			                    :a-max (make-point3 1.0 1.0 1.0))))
    (is (eql (collide-p s1 s2) t)))
    ; expect T
  (let ((s1 (make-instance 'bounding-sphere
                           :pos (make-point3 0.0 0.0 0.0)
                           :rad 1.0))
	(s2 (make-instance 'aa-bounding-box :a-min (make-point3 1.1 0.0 0.0) 
			                    :a-max (make-point3 1.1 2.0 0.0))))
    (is (eql (collide-p s1 s2) nil)))
  (let ((s1 (make-instance 'bounding-sphere 
                           :pos (make-point3 0.0 0.0 0.0)
                           :rad 1.0)))
    (is (eql (collide-p s1 nil) nil))
    (is (eql (collide-p nil s1) nil)))
  (let* ((s1 (make-instance 'bounding-sphere 
                           :pos (make-point3 1.0 0.0 0.0)
                           :rad 1.0))
	 (e1 (make-instance 'entity-server :pos (make-point3 0.0 1.0 0.0)
			    :bv s1))
	 (s2 (make-instance 'bounding-sphere 
                           :pos (make-point3 0.0 2.0 0.0)
                           :rad 1.1))
	 (e2 (make-instance 'entity-server :pos (make-point3 1.0 0.0 0.0)
			    :bv s2))
    (is (eql (collide-p e1 e2) t)))))
    ; expect NIL


(test point-line-sq-distance
      (let* ((line (cons #(0.0 0.0 0.0)
                         #(1.0 0.0 0.0)))
             (point #(1.0 1.0 0.0))
             (test1 (point-line-sq-distance point line)))
        (is (= test1 1.0))
        (is (=
             (point-line-sq-distance
              #(0.0 1.0 0.0) (cons #(-1.0 0.0 0.0) #(1.0 0.0 0.0)))             
             1.0))
        (is (=
             (point-line-sq-distance
              #(0.0 0.0 0.0) (cons #(-1.0 0.0 0.0) #(1.0 0.0 0.0)))
             0.0))))

(test sphere-triangle-intersection
      (let* ((tri (make-triangle (make-point3 -1.0 -1.0 0.0)
                                 (make-point3 1.0 -1.0 0.0)
                                 (make-point3 0.0 1.0 0.0)))
             (sphere (make-instance 'bounding-sphere
                                    :pos (make-point3 0.0 0.0 2.0)
                                    :rad 1.0))
             (test-res (sphere-triangle-intersection sphere tri)))
    ;    (format t "~%~%WE RETURNED: ~a~%~%" test-res)
        (is (not (eql test-res
                      nil)))))


(test move-bounding-volume-set
  (let* ((s1 (make-instance 'bounding-sphere 
                           :pos (make-point3 0.0 0.0 0.0)
                           :rad 1.0))
	 (vec1 (make-vector3 1.0 2.0 3.0))
	 (s2 (move-bounding-volume s1 vec1)))
    (is (equalp (pos s2) (make-point3 1 2 3)))))

(test move-bounding-volume-set
  (let ((s1 (make-instance 'bounding-sphere 
                           :pos (make-point3 0.0 0.0 0.0)
                           :rad 1.0))
	(vec1 (make-vector3 1.0 2.0 3.0)))
    (move-bounding-volume-set s1 vec1)
    (is (equalp (pos s1) (make-point3 1 2 3)))))
    

(test swept-sphere-collide
      (let* ((sph1 (make-instance 'bounding-sphere
                                  :pos (make-point3 2.0 0.0 0.0)
                                  :rad 1.0))
             (sph2 (make-instance 'bounding-sphere
                                  :pos (make-point3 -2.0 0.0 0.0)
                                  :rad 1.0))
             (vel1 (make-vec3 0.0 0.0 0.0))
             (vel2 (make-vec3 2.1 0.0 0.0)))
        (format t "result is ~a~%" 
                (swept-sphere-collide sph1 vel1 sph2 vel2))
        (is (swept-sphere-collide sph1 vel1 sph2 vel2))))

(test rt-inverse
      (let* ((base-mat (matrix-multiply-m
                        (make-translate #(0.0 1.0 0.0 1.0))
                        (matrix-multiply-m
                         (make-x-rot 1.892)
                         (matrix-multiply-m
                          (make-y-rot (/ pi 2))
                          (make-translate #(1.0 0.0 0.0 1.0))))))
             (inv-mat (rt-inverse base-mat))
             (point (make-point3 5.0 -3.0 15.0))
             (xfer-pt (matrix-multiply-v base-mat point)))
        (is (= 5 5))))

(test point-in-triangle-p
      (let* ((tri (vector (make-point3 -1.0 -1.0 0.0)
                          (make-point3 1.0 -1.0 0.0)
                          (make-point3 0.0 1.0 0.0)))
             (point (make-point3 0.0 0.0 0.0)))
        (is (not (null (point-in-triangle-p point tri))))))


(test quadratic
      (let ((res (quadratic 1 -1 -6 100)))
        (is (= 2 res))))

(test sphere-edge-intersection
      (let* ((sphere (make-instance 'bounding-sphere
                                    :pos (make-point3 2.344 0.0 4.0)
                                    :rad 1.0))
             (vel (make-vec3 -0.065 0.0 -0.0753))
             (edge-o (make-point3 1.5 -1.5 3.0))
             (edge-e (make-vec3 0.0 3.0 0.0))
             
             (result 
              (sphere-edge-intersection sphere vel edge-o edge-e 1.0)))
        (format t "~%SPHERE-EDGE RESULT: ~a~%" result)
        (is (not (null result)))))

(test sphere-point-intersection
      (let* ((point (make-point3 3.0 1.0 0.0))
             (sphere (make-instance 'bounding-sphere
                                    :pos (make-point3 0.0 0.0 0.0)
                                    :rad 1.0))
             (velocity (make-vec3 3.0 0.0 0.0))
             (test-res (sphere-point-intersection sphere velocity point 2.0)))
        (format t "~%sphere-point result: ~a~%" test-res)
        (is (not (null test-res)))))

(test swept-sphere-intersection
      (let* ((sph1 (make-instance 'bounding-sphere
                                  :rad 1.0
                                  :pos +origin+))
             (sph2 (make-instance 'bounding-sphere
                                  :rad 1.0
                                  :pos (make-point3 4.0 0.0 0.0)))
             (vel1 (make-vec3 2.5 0.0 0.0))
             (vel2 (make-vec3 0.0 0.0 0.0))
             (test-res (swept-sphere-collide sph1 vel1 sph2 vel2))
             (test-res2 (sphere-point-intersection 
                         (make-instance 'bounding-sphere
                                        :rad 2.0
                                        :pos +origin+)
                         (make-vec3 2.5 0.0 0.0)
                         (pos sph2) 5.0)))
        (format t "~%swept-sphere result: ~a~%" test-res)
        (format t "the point test: ~a~%" test-res2)
        (is (not (null test-res)))))

(test find-bounding-points
      (let* ((vect-array (vector (make-vector3 1.0 1.0 1.0)
                                 (make-vector3 0.0 2.0 2.0)
                                 (make-vector3 3.0 0.0 3.0)
                                 (make-vector3 3.0 1.0 0.0)))
             (pos-list (find-bounding-points vect-array)))
        (is (equalp (aref pos-list 0) (make-point3 0.0 0.0 0.0)))
	(is (equalp (aref pos-list 1) (make-point3 3.0 2.0 3.0)))))

#+disabled
(test make-bounding-box
      (let* ((vect-array (vector (make-vector3 1.0 1.0 1.0)
                                 (make-vector3 0.0 3.0 2.0)
                                 (make-vector3 3.0 0.0 3.0)
                                 (make-vector3 3.0 1.0 0.0)))
             (b-box (make-bounding-box vect-array)))
        (is (equalp (a-min b-box) (make-point3 0.0 0.0 0.0)))
	(is (equalp (a-max b-box) (make-point3 3.0 3.0 3.0)))))

(test make-bounding-sphere
      (let* ((vect-array (vector (make-vector3 1.0 1.0 4.0)
                                 (make-vector3 0.0 3.0 2.0)
                                 (make-vector3 4.0 0.0 3.0)
                                 (make-vector3 3.0 4.0 0.0)))
             (b-sphere (make-bounding-sphere vect-array)))
	(is (equalp (blt3d-phy::pos b-sphere) (make-point3 2.0 2.0 2.0)))
	(is (equalp (blt3d-phy::rad b-sphere) (sqrt 12)))))

