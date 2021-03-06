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



(blt3d-math::gen-vec-accessors tri-v0 tri-v1 tri-v2 tri-n tri-c)


(defun make-plane (n d)
  (cons n d))
(defun make-plane-abcd (a b c d)
  (cons (vector a b c) d))
(defun point-normal->plane (point normal)
  (cons normal (- (dot point normal))))

(defun plane-n (plane)
  (car plane))
(defun plane-d (plane)
  (cdr plane))
(defun plane-dist (plane point)
  (+ (dot point (car plane)) (cdr plane)))
(defun get-triangle-plane (tri)
  (make-plane (tri-n tri)
              (- (dot (tri-c tri) (tri-n tri)))))

;; basically same as below...but less hacks so i can debug
;; uses barycentric coords
(defun point-in-triangle-p (point tri)
  (let* ((v0 (vec3- (tri-v1 tri) (tri-v0 tri)))
         (v1 (vec3- (tri-v2 tri) (tri-v0 tri)))
         (v2 (vec3- point (tri-v0 tri)))
         (dot00 (dot v0 v0))
         (dot01 (dot v0 v1))
         (dot02 (dot v0 v2))
         (dot11 (dot v1 v1))
         (dot12 (dot v1 v2))
         (denom (- (* dot00 dot11)
                   (* dot01 dot01))))
    (unless (zerop denom)
      (let* ((inv-denom (/ 1.0 denom))
             (u (* inv-denom
                   (- (* dot11 dot02) (* dot01 dot12))))
             (v (* inv-denom
                   (- (* dot00 dot12) (* dot01 dot02)))))
        (and (> u 0) (> v 0) (< (+ u v) 1.0))))))

#+disabled
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


(defun sphere-edge-intersection (sphere velocity
                                 O E tmax)
  ;(break)
; #+disable
  (let* ((pos (pos sphere))
         (r (rad sphere))
         (r2 (* r r))
         (e2 (sq-mag E))
         (H (vec3- pos O))
         ;; cylinder equation related
         (V-x-E (cross3 velocity E))
         (H-x-E (cross3 H E))
         (Q pos)

         ;; quadratic cooefs
         (qa (dot V-x-E V-x-E))
         (qb (* 2.0 (dot V-x-E H-x-E)))
         (qc (- (dot H-x-E H-x-E) (* r2 e2)))

         (t0 (quadratic qa qb qc tmax)))

    #+disabled
    (when (and (> a 0.0) (< c 0.0))
      (let ((d (- (sq b) (* 4 a c))))
        (when (>= 0.0 d)
          (setf d (sqrt d))
          (let* ((denom (1/ (* 2 a)))
                 (t0 (* denom (- (- b) d)))
                 (t1 (* denom (+ (- b) d))))))))

    (when t0
      ;(format t "We have a ray-cylinder collision~%")
      ;; The ray, Q = P + Vt that intersects the cylinder
      (setf Q (vec3+ Q (vec-scale4 velocity t0)))

      ;; Now check if the point (Q) is part of the edge,
      ;; the start, or end
      (setf H (vec3- Q O))
      (let ((f0 (dot H E)))
        ;; f0 must be below e2, or it's past the end of the edge
        ;; and greater than 0.0
        ;; NOTE that we could do the vertex tests here...
        (when (and (<= f0 e2) (>= f0 0.0))
          ;; set h to be perpendicular to E
          (setf H (vec3- H (vec-scale3 E (/ f0 e2))))
          ;(format t "T0: ~a hit: ~a~%" t0 (vec3- Q H))
          ;; Return the result.
          ;; We need the time (t0) of the intersection
          ;; and the location....mebbe some other stuff
          (list t0 (vec3- Q H)))))))


(defun sphere-point-intersection (sphere velocity pt tmax)
  (quadratic (dot velocity velocity)
             (* 2 (dot velocity (vec3- (pos sphere) pt)))
             (- (sq-mag (vec3- pt (pos sphere))) (sq (rad sphere)))
             tmax))

;; Returns (x0 p) (and maybe other info later) where x0
;; is how far along velocity sphere goes before hitting the
;; triangle, and p is the point of intersection
;; from http://www.peroxide.dk/papers/collision/collision.pdf
(defun moving-sphere-triangle-intersection (sphere tri velocity)
  (with-slots ((sph-rad rad) (sph-pos pos)) sphere
    (let* ((tri-plane (make-plane (tri-n tri)
                                  (- (dot (tri-c tri) (tri-n tri)))))
           (n.vel (dot (tri-n tri) velocity))
           (basepoint-dist (plane-dist tri-plane sph-pos))
           (t-max 1.0)
           t0 t1)


      ;; check for backfacing
      ;#+disabled
      (when (> n.vel 0)
       ; (format t " b ")
        (return-from moving-sphere-triangle-intersection nil))

      ;; check for velocity parallel to triangle
      (if (= 0.0 n.vel)
          (progn ; (format t "WE IS PARALLEL!~%")
            (cond
              ;; Case: the sphere never touches the plane
              ((< sph-rad basepoint-dist)
               (return-from moving-sphere-triangle-intersection nil))
              ;; Case: the sphere moves through the plane
              ((>= sph-rad basepoint-dist)
               (setf t0 0.0 t1 1.0))))
          ;; General case here, where sphere is not moving parallel to plane
          (let ((one/n.vel (/ 1 n.vel)))
            (setf t0 (* one/n.vel (- (- sph-rad) basepoint-dist))
                  t1 (* one/n.vel (- sph-rad basepoint-dist)))))

      (let ((temp t0))
        (setf t0 (min t0 t1))
        (setf t1 (max temp t1)))


      ;; Check if t0 and t1 are between 0 and 1
      #+DISABLED
      (when (or (< t1 0) (> t0 1.0))
        (return-from moving-sphere-triangle-intersection nil))

      ;; Check for and handle the t0 < 0 case
      ;; (we start inside the triangle!)
      #+disabled
      (when (< t0 0)
        (return-from moving-sphere-triangle-intersection
          (list t0 (vec3+ (vec3- sph-pos (tri-n tri))
                          (vec-scale3 velocity t0)))))

      ;; clamp t1, we don't care what happens after 1.0
      (setf ;t0 (clamp t0 0 1)
            t1 (clamp t1 0 1))

      ;; TEST 1: check if the sphere intersects with the surface of tri
      (let* ((plane-intersection (vec3+ (vec3- sph-pos (tri-n tri))
                                        (vec-scale3 velocity t0))))
        (if (point-in-triangle-p plane-intersection tri)
            (return-from moving-sphere-triangle-intersection
              (list t0 plane-intersection))))


      ;; TEST 2: test the vertices
      (let* ((sq-rad (* sph-rad sph-rad))
             (vel-sqlen (sq-mag velocity))
             (mag-vel (sqrt vel-sqlen))
             (hit '(nil nil)))
        (iter (for i below 3)
              (for v = (svref tri i))
              (for tp = (sphere-point-intersection sphere velocity v t-max)
                   #+disabled
                   (quadratic (dot velocity velocity)
                              (* 2 (dot velocity (vec3- sph-pos v)))
                              (- (sq-mag (vec3- v sph-pos)) sq-rad)
                              t-max))
              (when tp
                (setf t-max tp)
                (setf hit (list tp v))))

        ;; TEST 3: test the edges
        ;; edges are (p2 . p1)
        (let ((edges (list (cons (tri-v1 tri) (tri-v0 tri))
                           (cons (tri-v2 tri) (tri-v1 tri))
                           (cons (tri-v0 tri) (tri-v2 tri)))))
          (iter (for edge-pair in edges)
                (for e0 = (cdr edge-pair))
                (for ev = (vec3- (car edge-pair) e0))
                (for edge-hit = (sphere-edge-intersection sphere velocity
                                                          e0 ev t-max))
                (when edge-hit
                  (setf t-max (car edge-hit))
                  (setf hit edge-hit))))

        ;; At this point, if we have a collision, hit is not (nil . nil)
        ;; but some (x0 . p).  turn into (new-pos hit-point)
        (if (car hit)
            hit
            nil)))))


(defun sphere-triangle-intersection (sphere tri)
  "Reports sphere-triangle intersections. Returns nil
   if no intersection occurs. Returns the point of intersection
   if one does occur"
  (with-slots ((sph-rad rad) (sph-pos pos)) sphere
   ; (format t "Testing sphere ~a ~a~%" sph-pos sph-pos)
    (let ((tri-plane (get-triangle-plane tri))
          (rad-sq (* sph-rad sph-rad))
          plane-pt dist)
      (if-let (it (sphere-plane-intersection sphere tri-plane))
              (setf plane-pt (first it)
                    dist (second it))
              ;; If sphere is too far form triangle plane then we can reject
              (return-from sphere-triangle-intersection nil))


      ;; Check if point is inside triangle
      (when (point-in-triangle-p plane-pt tri)
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


(defun slide-sphere (sphere velocity hit)
  "@return{the new position and velocity of the sphere as
           (new-pos  new-vel plane-normal)}"
  (let* ((x0 (car hit))
         (hit-pt (second hit))
         (new-bp (pos sphere))
         (dest (vec4+ new-bp velocity))
         (closest-dist (mag (vec-scale4 velocity x0))))

  ;  (format t "#####~%hit: ~a~%" hit)

    ;; only move the base point if we aren't already
    ;; very close to the hit
    (if (>= closest-dist 0.00001)
        (setf new-bp
              (vec4+ new-bp
                     (set-length4 velocity
                                  (- closest-dist 0.00001)))
              hit-pt (vec4- hit-pt (set-length4 velocity
                                                0.00001))))

    ;; make sliding plane
    (let* ((plane-normal (norm4 (vec4- new-bp hit-pt)))
           (plane-origin hit-pt)
           (sliding-plane (point-normal->plane plane-origin
                                               plane-normal))
           (new-dest (vec4-
                      dest
                      (vec-scale4
                       plane-normal
                       (plane-dist sliding-plane dest))))
           (new-vel (vec4- new-dest hit-pt)))

   ;   (format t "~3Told-dest: ~a~%~3Tnew-dest: ~a~%" dest new-dest)
   ;   (format t "~3Tplane-n: ~a~%" plane-normal)
   ;   (format t "~3Tplane-d: ~a~%" (plane-d sliding-plane))
  ;    (format t "~3Tplane-dist: ~a~%" (plane-dist sliding-plane dest))


      (list new-bp new-vel plane-normal))))
