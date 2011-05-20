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

(defclass octree (octree-node))

(defmethod make-octree (center width max-depth growth-k)
  (make-instance 'octree :center center :width width 
		         :max-depth max-depth :growth-k growth-k))

(defmethod make-child-node ((node octree-node) i)
  (with-slots (width max-depth growth-k depth center) node
    (make-instance octree-node :width (/ width 2) :max-depth max-depth
		   :depth (- depth 1) :growth-k growth-k
		   :center (vec3+ center (vec-scale4 (/ width 2) 
					             (case i 
                                                           (0 #(-1 -1 -1))
                                                           (1 #(-1 -1 +1))
                                                           (2 #(-1 +1 -1))
                                                           (3 #(-1 +1 +1))
                                                           (4 #(+1 -1 -1))
                                                           (5 #(+1 -1 +1))
							   (6 #(+1 +1 -1))
							   (7 #(+1 +1 +1))))))))

(defmethod make-children ((node octree-node))
  (setf (children node) 
    (iter (for i below 8)
      (collect (make-child-node node i) result-type 'vector)))
  (when (> (depth node) 0)
    (iter (for i below 8)
      (make-children (aref (children node) i)))))

(defmethod initialize-instance :after ((octree octree) &key)
  (make-children octree))

(defmethod insert ((node octree-node) (object blt3d-ent:entity-server))
  (with-slots (width center children) node
    (let ((object-pos 
	   (move-bounding-volume (blt3d-ent:bounding-volume object) 
				 (pos object)))
	  (object-rad (rad (blt3d-ent:bounding-volume object)))))))
      
      ;check if radius inside
      ;check if appropiate size
      ;insert into children
      
