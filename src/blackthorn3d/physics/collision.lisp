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

(defmethod collide-p ((s1 bounding-shape) a) nil)
(defmethod collide-p (a (s1 bounding-shape)) nil)
(defmethod collide-p (a b) nil)

;; sphere-sphere collisions
(defmethod collide-p ((s1 bounding-sphere) (s2 bounding-sphere))
  "@short{tests if two spheres intersect}
   @arg[s1]{first sphere}
   @arg[s2]{second sphere}"
  (with-slots ((pos1 pos) (r1 rad)) s1
    (with-slots ((pos2 pos) (r2 rad)) s2
      (if (< (sqrt (+ (expt (- (x pos1) (x pos2)) 2)
                      (expt (- (y pos1) (y pos2)) 2)
                      (expt (- (z pos1) (z pos2)) 2)))
             (+ r1 r2))
          t nil))))

;; Box-Box intersection
(defmethod collide-p ((bv1 aa-bounding-box) (bv2 aa-bounding-box))
  (with-slots (a-min a-max) bv1
    (with-slots ((b-min a-min) (b-max a-max)) bv2
      (iter (for i below 3)
            (if (or (> (svref a-min i) (svref b-max i))
                    (> (svref b-min i) (svref a-max i)))
                (return-from collide-p nil)))
      t)))

;; Box-Sphere intersection
(defmethod collide-p ((aabb aa-bounding-box) (sph bounding-sphere))
  (with-slots ((c pos) (r rad)) sph
    (with-slots (a-min a-max) aabb
      (let ((d 0))
        (iter (for i below 3)
              (let ((e1 (- (svref c i) (svref a-min i)))
                    (e2 (- (svref c i) (svref a-max i))))
                (if (< e1 0)
                    (if (< e1 (- r)) 
                        (return-from collide-p nil)
                        (incf d (sq e1)))
                    (if (> e2 0)
                        (if (> e2 r) 
                            (return-from collide-p nil)
                            (incf d (sq e2)))))))
        (<= d (sq r))))))

(defmethod collide-p ((sph bounding-sphere) (aabb aa-bounding-box))
  (collide-p aabb sph))


(defmethod collide-p ((e1 entity-server) (e2 entity-server))
  (collide-p (move-bounding-volume (blt3d-ent:bounding-volume e1) (pos e1))
	     (move-bounding-volume (blt3d-ent:bounding-volume e2) (pos e2))))

;;;
;;; Oriented Bounding Box
;;;

(defun axis-proj (obb axis)
    (iter (for i below 3)
     (sum (* (svref (half-lengths obb) i)
             (abs (dot (col (axes obb) i) axis))))))

(defun separating-axis-p (axis obb1 obb2)
  (> (abs (dot Tvec axis)) 
     (+ (axis-proj bv1 axis)
        (axis-proj bv2 axis))))

;; o-bounding-boxes
;; slow(er) way of doing things (for now)
;#+disabled
(defmethod collide-p ((bv1 o-bounding-box) (bv2 o-bounding-box))
  (let* ((Rmat (make-ortho-basis (vec4- (u bv2) (u bv1))
                                 (vec4- (v bv2) (v bv1))
                                 (vec4- (w bv2) (w bv1))))
        (Tvec (vec4- (center bv2) (center bv1))))
    (labels (;; used to test an axis of separation parallel to
             ;; one of the basis axes of bv1 (A)
             (A-face-test (axis)
               (separating-axis-p axis bv1 bv2))

             ;; used to test an axis if separation parallel to
             ;; one of the basis axes of bv2 (B)
             (B-face-test (axis)
               (separating-axis-p axis bv1 bv2))
             
             ;; used for testing axes parallel to the cross product
             ;; of an edge of A and an edge of B
             (AB-edge-test (i j)
               (separating-axis-p (cross (col (axes bv1) i) (col (axes bv2) j))
                                  bv1 bv2)))

      ;; First we test the faces of A
      (iter (for i below 3)
            (if (A-face-test (col (axes bv1) i))
                (return-from collide-p nil)))

      ;; Next the faces of B
      (iter (for i below 3)
            (if (B-face-test (col (axes bv2) i))
                (return-from collide-p nil)))
      
      ;; Finally the edges, all 9 of them
      (iter (for i below 3)
            (iter (for j below 3)
                  (if (AB-edge-test i j)
                      (return-from collide-p nil))))

      ;; If they all intersect, then we return true
      t)))