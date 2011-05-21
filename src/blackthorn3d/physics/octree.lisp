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
   (max-depth :accessor max-depth :initarg :max-depth :initform 3)
   (depth :accessor depth :initarg :depth :initform 0)
   (growth-k :accessor growth-k :initarg :growth-k :initform 2)
   (width :accessor width :initarg :width :initform 8) ;width is 1/2 full box
   (children :accessor children :initarg :children :initform nil)
   (objects :accessor objects :initarg :objects :initform nil)
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
  (setf (children node) 
    (iter (for i below 8)
      (collect (make-child-node node i) result-type 'vector)))
  (when (< (depth node) (max-depth node))
    (iter (for i below 8)
      (make-children (aref (children node) i)))))

(defmethod initialize-instance :after ((octree octree) &key)
  (make-children octree))
      
(defmethod octree-insert ((node octree-node) (object blt3d-ent:entity-server))
  (with-slots (width center children) node
    (let* ((object-pos 
           (move-bounding-volume (blt3d-ent:bounding-volume object) 
                                 (pos object)))
           (object-rad (rad (blt3d-ent:bounding-volume object)))
           (good-node (find-appropiate-node node object-pos object-rad))
           (good-objects (objects good-node)))
      ;insert into children
      (setf good-children (vconcat good-children object)))))

(defmethod octree-delete ((node octree-node) (object blt3d-ent:entity-server))
  (with-slots (width center children) node
    (let* ((object-pos 
           (move-bounding-volume (blt3d-ent:bounding-volume object) 
                                 (pos object)))
           (object-rad (rad (blt3d-ent:bounding-volume object)))
           (good-node (find-appropiate-node node object-pos object-rad))
           (good-objects (objects good-node)))
      (setf good-objects (delete object good-objects :test #'equalp)))))

(defmethod octree-query ((node octree-node)(object blt3d-ent:entity-server))
  (octree-query node node (bounding-volume object)))

(defmethod octree-query ((node octree-node) (object bounding-shape))
  (let ((good-node (find-appropiate-node node obj-center obj-width)))
    (gather-objects good-node)))

(defmethod gather-objects ((node octree-node))
  (let ((object-list (objects node)))
    (iter (for child in-vector (children node))
      (setf object-list (vconcat object-list gather-objects child)))))

(defmethod find-appropiate-node ((node octree-node) obj-center obj-width)
  (with-slots (width center children) node
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
        ;  (when (< obj-width (/ width 2))
        ;    ;descend
        ;    (iter (for child in-vector children)
        ;	(find-appropiate-node child obj-center obj-width)))
	;  (when (< obj-width width)
	;    ;this node is good
	;    (return-from find-appropiate-node node))))))
	  ))))
	
	   
	  
