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

(in-package :blackthorn3d-physics)

(defclass sphere ()
  ((pos :accessor pos :initarg :pos)
   (rad :accessor rad :initarg :rad))
  (:documentation "sphere object, used in collision detection"))

(defmethod collide-p ((s1 sphere) (s2 sphere))
  "@short{tests if two spheres intersect}
   @arg[s1]{first sphere}
   @arg[s2]{second sphere}"
  ;(with-slots ((x1 x) (y1 y) (z1 z) (r1 r)) s1
   ; (with-slots ((x2 x) (y2 y) (z2 z) (r2 r)) s2
     ; (if (< (sqrt (+ (expt (- x1 x2) 2)
     ;                 (expt (- y1 y2) 2)
     ;                 (expt (- z1 z2) 2)))
  (with-slots ((pos1 pos) (r1 rad)) s1
    (with-slots ((pos2 pos) (r2 rad)) s2
      (if (< (sqrt (+ (expt (- (x pos1) (x pos2)) 2)
                      (expt (- (y pos1) (y pos2)) 2)
                      (expt (- (z pos1) (z pos2)) 2)))
             (+ r1 r2))
          t nil))))

(defmethod find-bounding-points (vert-array)
  (iter (for i in-vector vert-array)
	(maximizing (aref i 0) into max-x)
	(maximizing (aref i 1) into max-y)
	(maximizing (aref i 2) into max-z)
	(minimizing (aref i 0) into min-x)
	(minimizing (aref i 1) into min-y)
	(minimizing (aref i 2) into min-z)
	(finally (return (vector (vector min-x min-y min-z) 
			       (vector max-x max-y max-z))))))

(defmethod make-bounding-volume (vert-array)
  (let* ((pos-array (find-bounding-points vert-array))
	 (min-point (aref pos-list 0))
	 (max-point (aref pos-list 1))
	 (rad-vector (vec-scale3 (vec- max-point min-point) 0.5)))
    (make-instance 'sphere :pos (vec3+ min-point rad-vector)
		           :rad (mag rad-vector))))
				 
				 
		    