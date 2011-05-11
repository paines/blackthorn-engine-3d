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
(defvar +eps+ 10e-5)





(defun make-ray (e d)
  (cons e d))
(defun ray-d (ray)
  (cdr ray))
(defun ray-e (ray)
  (car ray))
(blt3d-math::gen-vec-accessors tri-v0 tri-v1 tri-v2 tri-n tri-c)

(defun triangle-triangle-intersection (tri1 tri2)
  "Detect whether two triangles intersect or not. returns nil for
   false, otherwise true (may return collision info later)"
  )



(defun ray-triangle-intersection (ray tri)
  "Detect whether a ray (e-vec . d-vec) intersects a triangle
   returns nil if no intersection occurs, or (P s) where P
   is the point on the triangle and s is the distance, 
   or time (in e + s*d).  Returns nil or values (s u v)"
  (let* ((e1 (vec3- (svref tri 1) (svref tri 0)))
         (e2 (vec3- (svref tri 2) (svref tri 0)))
         (p (cross3 (ray-d ray) e2))
         (alpha (dot e1 p)))
    (when (range alpha (- +eps+) +eps+)
      (return-from ray-triangle-intersection nil))
    (let* ((f (/ 1 alpha))
           (s (vec3- (ray-e ray) (svref tri 0)))
           (u (* f (dot s p))))
      (when (or (< u 0.0) (> u 1.0))
        (return-from ray-triangle-intersection nil))
      (let* ((q (cross3 s e1))
             (v (* f (dot d q))))
        (when (or (< v 0.0) (> v 1.0))
          (return-from ray-triangle-intersection nil))
        (values (* f (dot e2 q)) 
                u 
                v)))))

(defun make-plane (n d)
  (cons n d))
(defun plane-n (plane)
  (car plane))
(defun plane-dist (plane point)
  (+ (dot (car plane) point) (cdr plane)))
(defun get-triangle-plane (tri)
  (make-plane (tri-n tri)
              (dot (tri-c tri) (tri-n tri))))

(defun point-in-triangle-p (point tri)
  (let* ((e1 (vec3- (svref tri 1) (svref tri 0)))
         (e2 (vec3- (svref tri 2) (svref tri 0)))
         (a (dot e1 e1))
         (b (dot e1 e2))
         (c (dot e2 e2))
         (p-vec (vec3- point (tri-v0 tri)))
         (d (dot p-vec e1))
         (e (dot p-vec e2))
         (x (- (* d c) (* e b)))
         (y (- (* e a) (* d b)))
         (z (- (+ x y) (- (* a c) (* b b)))))
    (and (minusp z) (plusp x) (plusp y))))

(defun sphere-plane-intersection (sphere plane)
  "Returns nil if sphere does not intersect plane
   otherwise returns (list pt dist) where pt is
   the pt in the sphere on the plane that forms the
   center of the intersection.  dist is the distance
   of this point from the sphere"
  (with-slots ((sph-rad rad) (sph-pos pos)) sphere
    (let* ((center-dist (plane-dist plane sph-pos)))
      (if (< center-dist sph-rad)
          (list (vec3- sph-pos (vec-scale3 (plane-n plane) center-dist)) center-dist)
          nil))))

(defun point-line-dist (point line))

#+disabled
(defun sphere-triangle-intersection (sphere tri)
  (with-slots ((sph-rad rad) (sph-pos pos)) sphere
    (let ((tri-plane (get-triangle-plane tri))
          plane-pt dist)
      (aif (sphere-plane-intersection sphere tri-plane)
           (setf plane-pt (first it)
                 dist (second it)) 
           ;; If sphere is too far form triangle plane then we can reject 
         (return-from sphere-triangle-intersection nil))
  
      ;; Here's mah theory.  If we enlarge the triangle by the radius of the
      ;; circle intersecting the tri-plane, we can do point-in-triangle test
      (let* (;; Circle radius is rad^2-dist^2
             (circle-sq-rad (- (* sph-rad sph-rad) (* dist dist)))
             (big-tri (expand-triangle tri circle-rad))))
      ))
  )

#+disabled
(defun moving-sphere-triangle-intersection (sphere tri velocity)
  (with-slots ((sph-rad rad) (sph-pos pos)) sphere
    (let* ((tri-plane (make-plane (tri-n tri) 
                                  (- (dot (tri-c tri) (tri-n tri)))))
           (n.vel (dot (tri-n tri) velocity))
           (basepoint-dist (plane-dist tri-plane sph-pos))
           t0 t1)
      ;; check for velocity parallel to triangle
      (if (= 0.0 n.vel)
          (cond 
            ;; Case: the sphere never touches the plane
            ((< sph-rad basepoint-dist) 
             (return-from moving-sphere-triangle-intersection nil))
            ;; Case: the sphere moves through the plane
            ((>= sph-rad basepoint-dist)
             (setf t0 0.0 t1 1.0)))
          ;; General case here, where sphere is not moving parallel to plane
          (let ((one/n.vec (/ 1 n.vec)))
            (setf t0 (* one/n.vec (- sph-rad basepoint-dist))
                  t1 (+ t0 (* 2 one/n.vec)))))

      ;; Check if t0 and t1 are between 0 and 1
      (unless (or (range t0 0 1) (range t1 0 1))
        (return-from moving-sphere-triangle-intersection nil))
      
      ;; TEST 1: check if the sphere intersects with the surface of tri
      (let* ((plane-intersection (vec3+ (vec3- sph-pos (tri-n tri))
                                        (vec-scale3 velocity t0)))
             )))))
