;;;; Blackthorn -- Lisp Game Engine
;;;;
;;;; Copyright (c) 2011 Chris McFarland <askgeek@gmail.com>
;;;;               2011 Robert Gross <r.gross.3@gmail.com>
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

(defvar *sector-table* (make-hash-table))

(defclass sector ()
  ((transform
    :reader transform
    :initarg :transform
    :initform (make-identity-matrix)
    :documentation "transform matrix from sector coords to world")
   (inverse-transform
    :reader inverse-transform
    :documentation "transform matrix from world coords to sector")
   (origin
    :accessor origin
    :initarg :origin
    :documentation "the world location of the origin of this sector")
   (portals
    :accessor portals
    :initarg :portals
    :initform ())
   (contents
    :accessor contents
    :initform (make-hash-table))
   (geometry
    :accessor geometry
    :initarg :geometry
    :initform nil
    :documentation "contains the r-trees of the sectors geometry")))
    

(defmethod initialize-instance :after ((this sector) &key)
  (setf (slot-value this 'inverse-transform)
        (rt-inverse (transform this))))


(defun clone-table (table)
  (let ((clone (make-hash-table)))
    (maphash #'(lambda (k v) (setf (gethash k clone) v)) table)
    clone))
    
(defmethod foreach-in-sector ((a-sector sector) func)
  (let ((new-table (clone-table (contents a-sector))))
    (maphash #'(lambda (k v) 
                 (declare (ignore k))
                 (funcall func v))  
             new-table)))
             
(defmethod foreach-in-sector ((a-sector symbol) func)
  (foreach-in-sector (lookup-sector a-sector) func))
    
(defmethod add-to-sector (an-entity (a-sector sector))
  (setf (gethash (oid an-entity) (contents a-sector)) an-entity)
  (setf (current-sector an-entity) a-sector))
  
(defmethod add-to-sector (an-entity (a-sector symbol))
  (add-to-sector an-entity (lookup-sector a-sector)))
  
(defun remove-from-sector (an-entity)
  (let ((the-sector (current-sector an-entity)))
    (remhash (oid an-entity) (contents the-sector))
    (setf (current-sector an-entity) nil)))
    
(defmethod update ((a-sector sector))
  (foreach-in-sector a-sector #'(lambda (an-entity) (update an-entity))))

(defun lookup-sector (name-symbol)
  (gethash name-symbol *sector-table*))
  


(defun new-sector (name-symbol geometry
                   &key
                   portals
                   (origin +origin+) 
                   (orientation (quat-identity)))

  (let ((the-sector 
         (make-instance 'sector 
                        :geometry geometry
                        :portals portals)))

    (setf (gethash name-symbol *sector-table*) the-sector)))
    


(defun update-sectors ()
  (maphash #'(lambda (sym a-sector) (declare (ignore sym)) (update a-sector))
           *sector-table*))
           
(defmethod transform-to-sector (pos-or-vec (a-sector sector))
  (matrix-multiply-v (inverse-transform a-sector) pos-or-vec))

(defmethod collide-sector ((obj entity-server) (a-sector sector)
                           &optional depth)
  (with-slots (pos bounding-volume velocity) obj
    (with-slots (geometry) a-sector
      (let ((test-sphere (copy-sphere (bounding-volume obj)))
            (test-vel velocity))
        ;; transform into sector coordinates
        (setf (pos test-sphere)
              (transform-to-sector pos a-sector)

              test-vel
              (transform-to-sector test-vel a-sector))

        ;; test against the geometry
        (blt3d-phy:collide-with-world 
         test-sphere velocity geometry depth)))))


;; where the heck should this method go???
(defun kill-entity (e)
  (when (current-sector e)
    (remove-from-sector e))
  (remove-entity e))
