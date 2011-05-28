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


;;;
;;; Ray tracing related stuff.  Here so i don't mess other things up
;;; feel free to refactor elsewhere
;;;


(defun make-ray (e d)
  (cons e d))
(defun ray-d (ray)
  (cdr ray))
(defun ray-e (ray)
  (car ray))
(blt3d-math::gen-vec-accessors tri-v0 tri-v1 tri-v2 tri-n tri-c)

(defun ray-sphere-intersection (ray sphere t-max)
  (let* ((r2 (sq (rad sphere)))
         (L (vec3- (pos sphere) (ray-e ray)))
         (l2 (sq-mag L))
         (s (dot L (ray-d ray))))
    (when (and (< s 0) (> l2 r2))
      (return-from ray-sphere-intersection nil))
    
    (let ((m2 (- l2 (sq s))))
      (when (> m2 r2)
        (return-from ray-sphere-intersection nil))
      (let* ((q (sqrt (- r2 m2)))
             (t0 (if (> l2 r2) (- s q) (+ s q))))
        (list t0 (vec4+ (ray-e ray) (vec-scale4 (ray-d ray) t0)))))))

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

(defun ray-aabb-intersection (ray aabb)
  (with-slots (a-min a-max) aabb
    (let ((o (ray-e ray))
          (d (ray-d ray))
          (t-min most-positive-single-float)
          (t-max most-negative-single-float)
          t0 t1)

      (iter (for i below 3)
            (for one/d = (/ 1 (svref d i)))
            (if (plusp (svref d i))
                ;; If di is positive
                (setf t0 (* (- (svref a-min i) (svref o i)) one/d)
                      t1 (* (- (svref a-max i) (svref o i)) one/d))
                ;; If di is negative
                (setf t1 (* (- (svref a-min i) (svref o i)) one/d)
                      t0 (* (- (svref a-max i) (svref o i)) one/d)))
            
            (if (< t0 t1)
                (setf t-min (max t0 t-min)
                      t-max (min t1 t-max))

                (setf t-min (max t1 t-min)
                      t-max (min t0 t-max)))
            (when (or (> t-min t-max)
                      (< t-max 0)) 
              (return-from ray-aabb-intersection nil)))
      (if (> t-min 0) 
          t-min t-max))))


(defun ray-rect-intersection (ray rect)
  (with-slots (a-min a-max) aabb
    (let ((o (ray-e ray))
          (d (ray-d ray))
          (t-min most-positive-single-float)
          (t-max most-negative-single-float)
          t0 t1)

      (iter (for i below 3)
            (for min-e in (lows rect))
            (for max-e in (highs rect))
            (for one/d = (/ 1 (svref d i)))
            (if (plusp (svref d i))
                ;; If di is positive
                (setf t0 (* (- min-e (svref o i)) one/d)
                      t1 (* (- max-e (svref o i)) one/d))
                ;; If di is negative
                (setf t1 (* (- min-e (svref o i)) one/d)
                      t0 (* (- max-e (svref o i)) one/d)))
            
            (if (< t0 t1)
                (setf t-min (max t0 t-min)
                      t-max (min t1 t-max))

                (setf t-min (max t1 t-min)
                      t-max (min t0 t-max)))
            (when (or (> t-min t-max)
                      (< t-max 0)) 
              (return-from ray-rect-intersection nil)))
      (if (> t-min 0) 
          t-min t-max))))

(defmethod search (ray (tree r-tree))
  (labels ((%search (r node)
             (cond
               ((typep node 'spatial-tree-leaf-node)
                (let (result)
                  (dolist (entry (records node) (nreverse result))
                    (when (ray-rect-intersection 
                           r (leaf-node-entry-rectangle entry))
                      (push (leaf-node-entry-datum entry) result)))))
               (t
                (let (result)
                  (dolist (child (children node) result)
                    (when (ray-rect-intersection
                           r (mbr child tree))
                      (setq result (append (%search r child) result)))))))))
    (let ((root (root-node tree)))
      (%search r root))))