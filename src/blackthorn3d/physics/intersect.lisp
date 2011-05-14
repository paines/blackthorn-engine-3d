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
(defun make-plane-abcd (a b c d)
  (cons (vector a b c) d))

(defun plane-n (plane)
  (car plane))
(defun plane-dist (plane point)
  (+ (dot point (car plane)) (cdr plane)))
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
          (list (vec3- sph-pos (vec-scale3 (plane-n plane) center-dist)) 
                center-dist)
          nil))))

(defun point-line-sq-distance (point line)
  "Returns (values sq-dist t-val) where dist is the squared distance of 
   point from line segment line, t-val is the value along B-A that point
   occurs at"
  (let* ((e (vec3- (cdr line) (car line)))
         (d1 (dot (vec3- point (car line)) e))
         (d2 (dot (vec3- point (cdr line)) (vec-neg3 e))))
    ;; If p is 'behind' the beginning of line
    (if (minusp d1)
        (values (mag (vec3- point (car line))) 
                (/ d1 (sq-mag e)))
        ;; If p is 'after' the end of the line
        (if (minusp d2)
            (values (sq-mag (vec3- point (cdr line))) 
                    (/ d1 (sq-mag e)))
            ;; If p is in between
            (values
             (/ (sq-mag (cross3 e (vec3- (car line) point)))
                (sq-mag e))
             (/ d1 (sq-mag e)))))))


(defun sphere-triangle-intersection (sphere tri)
  "Reports sphere-triangle intersections. Returns nil
   if no intersection occurs. Returns the point of intersection
   if one does occur"
  (with-slots ((sph-rad rad) (sph-pos pos)) sphere
    (let ((tri-plane (get-triangle-plane tri))
          (rad-sq (* sph-rad sph-rad))
          plane-pt dist)
      (aif (sphere-plane-intersection sphere tri-plane)
           (setf plane-pt (first it)
                 dist (second it)) 
           ;; If sphere is too far form triangle plane then we can reject 
         (return-from sphere-triangle-intersection nil))
  
      ;; DEBUG
      ;(format t "~%Plane-pt: ~a~%" plane-pt)

      ;; Check if point is inside triangle
      (when (point-in-triangle-p plane-pt tri)
        ;(format t "~%WE IZ UP HERR 'N DIS TRI!~%")
        (return-from sphere-triangle-intersection plane-pt))

      ;; Check the sides of the triangle
      (labels ((side-test (v0 v1)
                 (multiple-value-bind (dist t-val)
                     (point-line-sq-distance plane-pt (cons v0 v1))
                   (when (>= rad-sq dist)
                    ; (format t "~%dist=~a~%t-val=~a~%" dist t-val)
                     ;; Do hit herr
                     (list dist 
                           (vec3+ v0 (vec-scale3 (vec3- v1 v0) t-val)))
                     #+disabled
                     (return-from
                      sphere-triangle-intersection
                       (vec3+ v0 (vec-scale3 (vec3- v1 v0) t-val)))))))
        (let (min-d)
          ;; Side v0->v1
          (setf min-d (side-test (tri-v0 tri) (tri-v1 tri)))
          
          ;; Side v0->v2
          (let ((t2 (side-test (tri-v0 tri) (tri-v2 tri))))
            (if (and min-d t2 (< (car t2) (car min-d)))
                (setf min-d t2)))
          
          ;; Side v1->v
          (let ((t3 (side-test (tri-v1 tri) (tri-v2 tri))))
            (if (and min-d t3 (< (car t3) (car min-d)))
                (setf min-d t3)))
          
          min-d)))))


(defun quadratic (a b c)
  (if (and (zerop a) (/= 0 b)) (/ (- c) b)
      (let* ((det (- (* b b) (* 4 a c)))
             (denom (* 0.5 a)))
        (if (< 0 det)
            nil
            (let ((r1 (* (- (- b) (sqrt det))))
                  (r2 (* (+ (- b) (sqrt det)))))
              (if (<= r1 r2) 
                  (values r1 r2)
                  (values r2 r1)))))))

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
          (let ((one/n.vel (/ 1 n.vel)))
            (setf t0 (* one/n.vel (- sph-rad basepoint-dist))
                  t1 (+ t0 (* 2 one/n.vel)))))

      ;; Check if t0 and t1 are between 0 and 1
      (unless (or (range t0 0 1) (range t1 0 1))
        (return-from moving-sphere-triangle-intersection nil))
      
      ;; TEST 1: check if the sphere intersects with the surface of tri
      (let* ((plane-intersection (vec3+ (vec3- sph-pos (tri-n tri))
                                        (vec-scale3 velocity t0))))
        (if (point-in-triangle plane-intersection tri)
            (return-from moving-sphere-triangle-intersection
              (list plane-intersection (* t0 (mag velocity))))))
      
      ;; TEST 2: test the vertices
      (let* ((sq-rad (* sph-rad sph-rad))
             (vel-sqlen (sq-mag velocity))
             (mag-vel (sqrt vel-sqlen))
             (hit (nil . nil)))
        (iter (with min-t = 1.0e+INF)
              (for i below 3)
              (for v = (svref tri i))
              (for tp = 
                   (quadratic (dot velocity velocity)
                              (* 2 (dot velocity (vec3- sph-pos v)))
                              (- (sq-mag (vec3- v sph-pos)) sq-rad)))
              (when (< tp min-t)
                (setf min-t tp)
                (setf hit (list tp v))))

        ;; TEST 3: test the edges
        (let ((edges (list (cons (tri-v1 tri) (tri-v0 tri))
                           (cons (tri-v2 tri) (tri-v0 tri))
                           (cons (tri-v2 tri) (tri-v1 tri)))))
          (iter (with min-t = (or (car hit) 1.0e+INF))
                (for edge-pair in edges)
                (let* ((edge (vec3- (car edge-pair) (cdr edge-pair)))
                       (s-to-p (vec3- sph-pos (cdr edge-pair)))
                       (e-sqlen (sq-mag edge))
                       (e.v (dot edge velocity))
                       (e.stp (dot e s-to-p)))
                  (for x0 = 
                       (quadratic (+ (* e-sqlen (- vel-sqlen)) 
                                     (sq e.v))
                                  (- (* e-sqlen (* 2 (dot velocity s-to-p)))
                                     (* 2 e.v e.stp))
                                  (+ (* e-sqlen (- sq-rad (sq-mag b-to-p))) 
                                     (sq e.stp))))
                  (for f0 = (/ (- (* x0 e.v) e.stp) 
                               e-sqlen))
                  (when (and (range f0 0 1)
                             (< x0 min-t))
                    (setf min-t x0)
                    (setf hit (list x0 (vec3+ (car edge-pair)
                                              (vec-scale3 edge f0))))))))

        ;; At this point, if we have a collision, hit is not (nil . nil)
        ;; but some (x0 . p).  turn into (d . p)
        (if (car hit)
            (cons (* (car hit) mag-vel) (cdr hit))
            nil)))))
