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


;;;
;;; Spatial trees.
;;;  all spatial trees require that the primitives being used
;;;  be able to provide the following values:
;;;    Min/Max for each axis or Extents
;;;    Centroid
;;;

;;;
;;; Octree
;;;

;; For dynamic collision detection, and potentially more??
;; Octree is contructed as cube

(defclass octree-node ()
  ((threshold :accessor threshold :initarg :threshold :initform 1)
   (max-depth :accessor max-depth :initarg :max-depth :initform 0)
   (depth :accessor depth :initarg :depth :initform 0)
   (growth-k :accessor growth-k :initarg :growth-k :initform 2)
   (width :accessor width :initarg :width :initform 8) ;width is 1/2 full box
   (octree-children :accessor octree-children :initarg :octree-children :initform nil)
   (objects :accessor objects :initarg :objects
                   :initform (make-array 0 :fill-pointer 0 :adjustable t))
   (center :accessor center :initarg :center :initform nil)
   ))

(defclass octree (octree-node)())

(defmethod make-octree (center width max-depth)
  (make-instance 'octree :center center :width width
		         :max-depth max-depth))

(defmethod make-child-node ((node octree-node) i)
  (with-slots (width max-depth growth-k depth center) node
    (make-instance 'octree-node :width (/ width 2) :max-depth max-depth
		   :depth (+ depth 1) :growth-k growth-k
		   :center (vec4+ center (vec-scale4 (case i
                                                     (0 (make-point3 -1 -1 -1))
                                                     (1 (make-point3 -1 -1 +1))
                                                     (2 (make-point3 -1 +1 -1))
                                                     (3 (make-point3 -1 +1 +1))
                                                     (4 (make-point3 +1 -1 -1))
                                                     (5 (make-point3 +1 -1 +1))
						     (6 (make-point3 +1 +1 -1))
						     (7 (make-point3 +1 +1 +1)))
						  (/ width 2))))))

(defmethod make-children ((node octree-node))
  ;(format t "~%~%~%~a~%~%~%"  (setf (octree-children node)
;	(iter (for i below 8)
;	      (collect (make-child-node node i) result-type 'vector))))
  (with-slots (octree-children) node
    (setf octree-children
	  (iter (for i below 8)
		(collect (make-child-node node i) result-type 'vector)))
  ;(when (< (depth node) (max-depth node))
  ;  (iter (for i below 8)
  ;    (make-children (aref (octree-children node) i))))
))

(defmethod initialize-instance :after ((octree-node octree-node) &key)
  (when (< (depth octree-node) (max-depth octree-node))
    (make-children octree-node)))

(defmethod octree-insert ((node octree-node) (object blt3d-ent:entity-server))
  (with-slots (width center octree-children) node
    (let* ((moved-object
             (move-bounding-volume (blt3d-ent:bounding-volume object)
                                 (pos object)))
	   (object-pos (pos moved-object))
           (object-rad (rad (blt3d-ent:bounding-volume object)))
           (good-node (find-appropiate-node node object-pos object-rad))
           (good-objects (objects good-node)))
      ;insert into children
      (vector-push-extend object good-objects)
      ;(format t "~%~%~% POS ~a" (center good-node))
      ;(format t "~% WIDTH ~a" (width good-node))
)))

(defmethod octree-insert ((node octree-node) not-important)
) ;catch nil at end of list

(defmethod octree-delete ((node octree-node) (object blt3d-ent:entity-server))
  (with-slots (width center octree-children) node
    (let* ((moved-object
           (move-bounding-volume (blt3d-ent:bounding-volume object)
                                 (pos object)))
	   (object-pos (pos moved-object))
           (object-rad (rad (blt3d-ent:bounding-volume object)))
           (good-node (find-appropiate-node node object-pos object-rad))
           (good-objects (objects good-node)))
      ;(format t "~%~% HERE ~%~%")
      ;(format t "~%~%~% ~a ~%" (objects good-node))
      (setf good-objects (cl:delete object good-objects :test #'equalp))
      ;(delete object good-objects :test #'equalp)
      ;(format t "~%~%~% ~a ~%" (objects good-node))
       )))

(defmethod octree-query ((node octree-node) (object blt3d-ent:entity-server))
  (octree-query node (bounding-volume object)))

(defmethod octree-query ((node octree-node) (object bounding-shape))
  (let ((good-node (find-appropiate-node node (pos object)
					      (rad object))))
    ;(format t "~%~%HERE ~a" good-node)
    ;(format t "~%~% query ~a" (center good-node))
    ;(format t "~%~% query ~a" (width good-node))
    ;(format t "~%~% query ~a" (pos object))
    ;(format t "~%~% query ~a" (rad object))
    (gather-objects good-node)))

(defmethod gather-objects ((node octree-node))
  (let ((object-list (make-array 0 :fill-pointer 0 :adjustable t)))
    ;(format t "~%~% HERE ~a" node)
    ;(format t "~%~% descending ~a" (center node))
    (iter (for object in-vector (objects node))
      ;(format t "~%~% gather1 ~a" object)
      (vector-push-extend object object-list))
    (iter (for child in-vector (octree-children node))
      (iter (for object in-vector (gather-objects child))
        ;(format t "~%~% gather2 ~a" object)
	(vector-push-extend object object-list)))
    (return-from gather-objects object-list)))

#+disabled
(defmethod find-appropiate-node ((node octree-node) obj-center obj-width)
  (with-slots (width center octree-children) node
      (let* ((width3 (make-point3 width width width))
             (min-point (vec4- center width3))
             (max-point (vec4+ center width3)))
        (progn
          ;check if radius inside
          (iter (for i below 3)
            (when (or (> (svref min-point i) (svref obj-center i))
                      (< (svref max-point i) (svref obj-center i)))
              (return-from find-appropiate-node nil)))
          ;check if appropiate size
;	  (format t "~%~% obj-width ~a" obj-width)
;	  (format t "~%~% width ~a" width)
          (when (< obj-width (/ width 2))
            ;descend
            (iter (for child in-vector octree-children)
        	(find-appropiate-node child obj-center obj-width)))
	  (when (< obj-width width)
	    ;this node is good
	    (progn
;	      (format t "~%~% HERE")
	      (return-from find-appropiate-node node)))))))
;	      node))))))


(defmethod find-appropiate-node ((node octree-node) obj-center obj-width)
  (with-slots (width center octree-children) node
    ;(format t "~%~%~%~%~% HERE ~a" octree-children)
    (if (and (< obj-width (/ width 2)) octree-children)
      ;decide which child to descend into
      (iter (for child in-vector octree-children)
	(let* ((child-width (width child))
	       (child-center (center child))
	       (width3 (make-point3 child-width child-width child-width))
	       (min-point (vec4- child-center width3))
	       (max-point (vec4+ child-center width3))
	       (inside-child t))
          (iter (for i below 3)
            (when (or (> (svref min-point i) (svref obj-center i))
                      (< (svref max-point i) (svref obj-center i)))
	      ;not inside this child
	      (setf inside-child nil)))
	  (when inside-child
	      ;descend
	    ;(format t "~%~%~%~% obj ~a" obj-center)
	    ;(format t "~%~% obj ~a" obj-width)
	    ;(format t "~%~% node ~a" (center node))
	    ;(format t "~%~% node ~a~%" (width node))
	      (return-from find-appropiate-node
			   (find-appropiate-node child obj-center obj-width)))))
    (when (< obj-width width)
    ;(progn
      ;this node is good
      ;(format t "~%~%~% good")
      ;(format t "~%~% obj ~a" obj-center)
      ;(format t "~%~% obj ~a" obj-width)
      ;(format t "~%~% good-node ~a" (center node))
      ;(format t "~%~% good-node ~a~%" (width node))
      (return-from find-appropiate-node node)))))
