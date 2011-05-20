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

(in-package :blackthorn3d-graphics)

;; what is a viewport? 
;;  - the size of the render target (/ screen)
;;  - the projection matrix / frustum (not sure which...)
;;  - 
(defclass viewport ()
  ((size
    :accessor view-size
    :initarg :size
    :documentation "The size of the viewport. the viewport
                    will be sent to gl as (0 0 (car size) (cdr size)")
   (aspect-ratio
    :accessor view-ratio
    :initarg :aspect-ratio)
   (fov
    :accessor view-fov
    :initarg :fov)
   (framebuffer
    :initarg :fbo
    :initform nil
    :documentation "if not nil, is a fbo that the viewport
                    will render to. More options to come")
   (vfrustum
    :accessor view-frustum
    :initarg :frustum)))

(defun create-viewport (size near far
                        &key
                        aspect-ratio
                        framebuffer
                        (fov (/ pi 2))
                        (type :projection))
  (let ((aspect-ratio
         (aif aspect-ratio it (/ (car size) (cadr size)))))
    (make-instance 'viewport
                   :size size
                   :fov fov
                   :aspect-ratio aspect-ratio
                   :fbo framebuffer
                   :frustum
                   (make-frstm near far aspect-ratio fov :type type))))

(defmethod set-viewport ((this viewport))
  (format t "starting set-viewport~%")
  (with-slots (size vfrustum framebuffer) this
    (format t "setting viewport of size ~a~%" size)
    (gl:viewport 0 0 (car size) (cadr size))
    (format t "load-frustum~%")
    (load-frstm vfrustum)
    (when framebuffer
      ;; do framebuffer stuff
      )))

;; notes- uses similar description for a frustum as opengl
;;        there is nothing forcing the left and right plane
;;        to be mirrored
(defclass frustum ()
  ((near-dist
    :accessor frstm-near
    :initarg :near)
   (far-dist
    :accessor frstm-far
    :initarg :far)
   (top-left
    :accessor frstm-top-left
    :initarg :top-left)
   (bottom-right
    :accessor frstm-bottom-right
    :initarg :bottom-right)
   (proj-matrix
    :initarg :proj-matrix)))

(defun make-frstm (near far aspect fov &key (type :projection))
  (let* ((width (/ (calc-width near fov) 2.0))
         (height (* width (/ aspect))))
    (make-instance 'frustum
                   :near near
                   :far far
                   :top-left (vector height (- width))
                   :bottom-right (vector (- height) width)
                   :proj-matrix 
                   (case type
                     (:projection 
                      (make-projection (- width) width
                                       height (- height)
                                       near far))
                     (:orthographic 
                      (make-orthographic (- width) width
                                         height (- height)
                                         near far))))))

(defun calc-width (near fov)
  (* near 
     (tan (/ fov 2))))

(defmethod load-frstm ((this frustum))
  (with-slots ((near near-dist)
               (far far-dist)
               (tl top-left)
               (br bottom-right)) this
    (gl:push-attrib :transform-bit)
    (gl:matrix-mode :projection)
    (gl:load-matrix (slot-value this 'proj-matrix))
    (gl:pop-attrib)
    #+disabled(gl:matrix-mode :modelview)))

(defmethod frustum-projection-matrix ((this frustum))
  (with-slots ((near near-dist)
               (far far-dist)
               (tl top-left)
               (br bottom-right)) this
    (let ((left (svref tl 1))
          (right (svref br 1))
          (top (svref tl 0))
          (bottom (svref br 0)))
      (make-projection left right top bottom near far))))

(defvar +planes+ '(:left :right :bottom :top :near :far))

(defun get-plane (pmat plane)
  (labels ((plane-maker (row sign)
             (make-plane (vec3+ (row pmat 3) 
                                (vec-scale3 (row pmat row) sign))
                         (+ (aref pmat 3 3) (* sign (aref pmat 3 row))))))
    (case plane
      (:top    (plane-maker 1 -1))
      (:bottom (plane-maker 1 1))
      (:left   (plane-maker 0 1))
      (:right  (plane-maker 0 -1))
      (:near   (plane-maker 2 1))
      (:far    (plane-maker 2 -1))
      (otherwise nil))))

;; We can do frustum culling at a viewport level...
;; generate the planes first, then process every sphere
;; but....what are objects? Do I want to pass in a sphere hierarchy?
;; could pass a list of nodes, and cull by those spheres...however
;; we're missing the translation info from entity =(
;; but lets go with that...we can work out details at a higher leve
;; it makes more sense than anything else i can think of
;; 
;; Really I need some sort of real scene-management heirarchy
;;   entity
;;    -> blt-model
;;         -> list of nodes w/bounding spheres
;;
;;  blt-model for level
;;    -> list of nodes w/bounding spheres


(defun cull-frustum (frstm view-matrix objects)
  (with-slots ((P proj-matrix)) frstm
    (let* ((M (matrix-multiply-m P view-matrix))
           (planes (iter (for i in +planes+) 
                         (collect (get-plane M i)))))
      )))

(defun frustum-planes-intersect-test (planes b-sphere)
  (with-slots (pos rad) b-sphere
    (iter (for plane in planes)
          (for dist = (plane-dist plane pos))
          (when (> dist rad)
            (return-from frustum-planes-intersect-test :outside))
          (maximizing dist into max-dist)
          (finally 
           (if (< max-dist (- rad)) 
               (return :inside)
               (return :intersect))))))