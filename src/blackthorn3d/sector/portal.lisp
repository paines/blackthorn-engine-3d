;;;; Blackthorn -- Lisp Game Engine
;;;;
;;;; Copyright (c) 2011 Chris McFarland <askgeek@gmail.com>
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

(in-package :blackthorn3d-sector)

(defvar *default-count* 0)

(defclass portal (entity-server)
  ((id
    :accessor portal-id
    :initarg :id)
   (direction
    :accessor portal-direction
    :initarg :direction
    :documentation "World direction (:east :north etc) of the portal
                    this is different from the dir slot inherited from
                    entity in that dir is a local-to-sector direction
                    that is used to test if we've crossed the portal's
                    edge.  this direction is more useful for linking")
   (links-to-sector
    :accessor links-to-sector
    :initform nil
    :initarg :links-to-sector)
   (links-to-portal
    :accessor links-to-portal
    :initform nil
    :initarg  :links-to-portal)))

;(defun make-portal (level-name portal-name portal-model-name)
;    (let ((portal-model (find-portal-model level-name portal-model-name)))
;        (make-entity-server 'portal
;            :


(defun make-portal (portal-name pos dir bv)
  (make-instance 'portal
                 :id portal-name
                 :pos pos
                 :dir dir
                 :bv bv))

(defmethod link-portals ((s1 sector) (p1 portal)
                         (s2 sector) (p2 portal))
  (setf (links-to-portal p1) p2
        (links-to-sector p1) s2
        (links-to-portal p2) p1
        (links-to-sector p2) s1))

(defmethod transform-portal ((this portal) xform)
  (get-direction (dir this))
  (with-slots (pos dir direction) this
    (setf (pos this) (matrix-multiply-v xform pos)
          (dir this) (norm4 pos)
          direction (get-direction dir)
          (dir this) (getf +directions+ direction))
    this))

;;;
;;; Portal Collision
;;;

;; Returns true if an entity (assumed to have already intersected
;; the bv) has crossed the portal
(defmethod crosses-portal-p ((sph bounding-sphere) (p portal))
  (let ((p-plane (point-normal->plane (pos p) (dir p))))
    (plusp (plane-dist p-plane (pos sph)))))

(defmethod crosses-portal-p ((obj entity-server) (p portal))
  (crosses-portal-p
   (move-bounding-volume (bounding-volume obj) (pos obj))
   p))


;; Sphere should be a sphere in the sector coordinates
(defmethod collide-p ((sphere bounding-sphere) (p portal))
  (with-slots (pos dir bounding-volume) p
    ;; move the sphere into the coordinates of the aabb
    ;; p' = p - p_aabb_rotated
    ;; p_abb_rotated = the pos of portal rotated back to the -z direction
    (let ((moved-sphere (move-bounding-volume
                         sphere
                         (quat-rotate-vec
                          (quat-rotate-to-vec
                           dir
                           (vec-neg4 +z-axis+))
                          (vec-neg4 pos)))))

      (crosses-portal-p sphere p)
      #+disabled
      (when (collide-p moved-sphere bounding-volume)
        (format t "We hit the protal!! id: ~a~%" (portal-id p))
        (crosses-portal-p moved-sphere p)))))

(defmethod ray-cast (ray (portal portal))
  (when (links-to-sector portal)
    (with-slots (pos dir bounding-volume) portal
      (when (plusp (dot (ray-d ray) dir))
        (let* ((rot-quat (quat-rotate-to-vec
                          dir (vec-neg4 +z-axis+)))
               (x-ray (make-ray
                       (quat-rotate-vec
                        rot-quat
                        (vec4- (ray-e ray) pos))
                       (quat-rotate-vec
                        rot-quat
                        (ray-d ray))))
               (res (ray-aabb-intersection x-ray
                                           bounding-volume
                                           most-positive-single-float)))
          res)))))
