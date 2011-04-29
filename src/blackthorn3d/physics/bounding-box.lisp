;;;; Blackthorn -- Lisp Game Engine
;;;;
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

(defclass aa-bounding-box ()
  ((a-min
    :accessor a-min
    :initarg :a-min
    :initform (make-point3 -1.0 -1.0 -1.0))
   (a-max
    :accessor a-max
    :initarg :a-max
    :initform (make-point3 1.0 1.0 1.0))))

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
(defmethod collide-p ((aabb aa-bounding-box) (sph sphere))
  (with-slots ((c pos) (r rad)) sph
    (with-slots (a-min a-max) aabb
      (let ((d 0))
        (iter (for i below 3)
              (let ((e1 (- (svref c i) (svref a-min i)))
                    (e2 (- (svref c i) (svref a-max i))))
                (if (< e1 0)
                    (if (< e1 (- r)) (return-from collide-p nil)
                        (incf d (sq e1)))
                    (if (> e2 0)
                        (if (> e2 r) (return-from collide-p nil)
                            (incf d (sq e2)))))))
        (< (sq r) d)))))

(defclass o-bounding-box ()
  ((center
    :accessor center
    :initarg :center
    :initform (make-point3 0.0 0.0 0.0))
   (axes
    :accessor axes
    :initarg :axes
    :initform (make-identity-matrix)
    (half-lengths
    :accessor half-lengths
    :initarg :half-lengths
    :initform (make-vector3 1.0 1.0 1.0)))))

;; slow(er) way of doing things (for now)
(defmethod collide-p ((bv1 o-bounding-box) (bv2 o-bounding-box))
  (let (#+disabled(Rmat (make-ortho-basis (vec4- (u bv2) (u bv1))
                                (vec4- (v bv2) (v bv1))
                                (vec4- (w bv2) (w bv1))))
        (Tvec (vec4- (center bv2) (center bv1))))
    (labels ((axis-proj (obb axis)
               (iter (for i below 3)
                     (sum (* (svref (half-lengths obb) i)
                             (abs (dot (col (axes obb) i) axis))))))
             ;; used to test an axis of separation parallel to
             ;; one of the basis axes of bv1 (A)
             (A-face-test (axis)
               (> (abs (dot Tvec axis)) 
                  (+ (axis-proj bv1 axis)
                     (axis-proj bv2 axis))))

             ;; used to test an axis if separation parallel to
             ;; one of the basis axes of bv2 (B)
             (B-face-test (axis)
               (A-face-test axis))
             
             ;; used for testing axes parallel to the cross product
             ;; of an edge of A and an edge of B
             (AB-edge-test (i j)
               (A-face-test (cross (col (axes bv1) i) (col (axes bv2) j)))))

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
                      (return-from collide-p nil)))))))