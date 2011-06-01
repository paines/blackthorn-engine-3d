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

(defvar +sector-size+ 30.0)

(defclass sector ()
  ((id
    :reader sector-id
    :initarg :id)
   (origin
    :accessor origin
    :initarg :origin
    :documentation "the world location of the origin of this sector")
   (orientation
    :accessor orientation
    :initarg :orientation
    :initform (quat-identity)
    :documentation "quatenion representing the rotation of the sector
                    Is expected to be in increments of 90 degrees")
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
    
#+disabled
(defmethod initialize-instance :after ((this sector) &key)
  (setf (slot-value this 'inverse-transform)
        (rt-inverse (transform this))))


(defun clone-table (table)
  (let ((clone (make-hash-table)))
    (maphash #'(lambda (k v) (setf (gethash k clone) v)) table)
    clone))
    

(defun lookup-sector (name-symbol)
  (gethash name-symbol *sector-table*))
  
(defun new-sector (name-symbol geometry
                   &key
                   portals
                   (origin +origin+) 
                   (orientation (quat-identity)))
  (let ((the-sector 
         (make-instance 'sector 
                        :id name-symbol
                        :origin origin
                        :orientation orientation
                        :geometry geometry
                        :portals portals)))

    (format t "~%CREATING SECTOR ~a with geometry ~a ~%~3Tand portals ~a~%"
            name-symbol geometry portals)

    ;; we really ought to update portals here
    (iter (for p in (portals the-sector))
          (setf (portal-direction p)
                (get-direction 
                 (quat-rotate-vec orientation (dir p))))
          (format t "~2TPORTAL ~a's DIRECTION:   ~a~%"
                  (portal-id p) (portal-direction p)))

    (setf (gethash name-symbol *sector-table*) the-sector)))



;;;
;;; Setting up sectors in world space
;;;

(defmethod find-portal-in-direction ((s sector) direction)
  (find direction (portals s) :key #'portal-direction))


(defmethod add-sector-relative ((rel-sector sector) direction (sector sector))
  (with-slots ((base-origin origin)) rel-sector
    (with-slots ((new-origin origin)) sector
      (setf new-origin 
            (vec4+  base-origin
                    (vec-scale4
                     (getf +directions+ direction)
                     +sector-size+))))))

(defmethod add-sector-relative ((rel-sector symbol) direction (sector sector))
  (add-sector-relative (lookup-sector rel-sector) direction sector))

(defmethod link-sectors ((sec1 sector) (sec2 sector))
  (format t "~%Linking sectors ~a and ~a~%"
          (sector-id sec1) (sector-id sec2))

  (with-slots ((o1 origin)) sec1
    (with-slots ((o2 origin)) sec2
      (let* ((o2-o1 (vec4- o2 o1))
             (direction (get-direction o2-o1)))
        (format t "DIRECTION from ~a to ~a is ~a~%" 
                (sector-id sec1) (sector-id sec2) direction)
        ;; First check if they are next to each other
        (format t "Distance from ~a to ~a is ~a~%"
                (sector-id sec1) (sector-id sec2) (mag o2-o1))

        (unless (> (mag o2-o1) +sector-size+)
          ;; Find if they share a portal
          (format t "s1 portals: ~a~%" (portals sec1))
          (format t "s2 portals: ~a~%" (portals sec2))
          (let ((p1 (find-portal-in-direction sec1 direction))
                (p2 (find-portal-in-direction 
                     sec2 (opposite-dir direction))))
            (when (and p1 p2)
              (link-portals sec1 p1 sec2 p2))))))))

(defmethod link-sectors ((sec1 symbol) (sec2 symbol))
  (link-sectors (lookup-sector sec1) (lookup-sector sec2)))


;;;
;;;  Updates
;;;

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
  (setf (current-sector an-entity) (sector-id a-sector)))
  
(defmethod add-to-sector (an-entity (a-sector symbol))
  (add-to-sector an-entity (lookup-sector a-sector)))
  
(defun remove-from-sector (an-entity)
  (let ((the-sector (lookup-sector (current-sector an-entity))))
    (remhash (oid an-entity) (contents the-sector))
    (setf (current-sector an-entity) nil)))
    
(defmethod update ((a-sector sector))
  (foreach-in-sector a-sector #'(lambda (an-entity) (update an-entity))))

(defun update-sectors ()
  (maphash #'(lambda (sym a-sector) (declare (ignore sym)) 
                     (update a-sector))
           *sector-table*))           


;;;
;;; Collisions
;;;

(defmethod transform-to-sector (pos-or-vec (a-sector sector))
  (with-slots (origin orientation) a-sector
    (let ((xformed
           #+disabled
           (matrix-multiply-v
            (matrix-multiply-m
             (quat->matrix orientation)
             (make-translate (vec-neg4 origin)))
            pos-or-vec)
          ;  #+disabled
           (quat-rotate-vec 
            (quat-inverse orientation)       ; use the inverse
            (if (zerop (w pos-or-vec)) 
                pos-or-vec
                (vec4- pos-or-vec origin)))))
      (setf (w xformed) (w pos-or-vec))
      xformed)))

(defmethod transform-to-world (pos-or-vec (a-sector sector))
  (with-slots (origin orientation) a-sector
    (let ((xformed
           (quat-rotate-vec
            orientation
            pos-or-vec)))
      (if (zerop (w pos-or-vec))
          xformed
          (to-point4 (vec4+ xformed origin))))))

(defmethod get-transform-to-world ((this sector))
  (with-slots (origin orientation) this
    (matrix-multiply-m 
     (make-translate origin)
     (quat->matrix orientation))))

(defmethod get-transform-to-sector ((this sector))
  (with-slots (origin orientation) this
    (matrix-multiply-m
     (rt-inverse (quat->matrix orientation))
     (make-translate (vec-neg4 origin)))))

(defmethod collide-sector ((obj entity-server) (a-sector sector)
                           &optional depth)
  (with-slots (pos bounding-volume velocity) obj
    (with-slots (geometry portals) a-sector
      (let ((test-sphere (move-bounding-volume bounding-volume pos))
            (test-vel (to-vec4 velocity)))

        ;; transform into sector coordinates
        (setf (pos test-sphere)
              (transform-to-sector pos a-sector)

              test-vel
              (transform-to-sector test-vel a-sector))

        ;; test against the geometry (and transform the result back)
        (transform-to-world
         (to-vec4
          (blt3d-phy:collide-with-world
           test-sphere test-vel geometry depth))
         a-sector)))))

(defvar +p-eps+ 1e-3)
(defmethod collide-sector-portals ((obj entity-server) (a-sector sector))
  (with-slots (pos bounding-volume) obj
    (with-slots (portals) a-sector
      
      ;; Test against portals
      (iter (with test-sphere = (move-bounding-volume 
                                 bounding-volume 
                                 (transform-to-sector pos a-sector)))
            (for portal in portals)
            (when (and (links-to-sector portal) (collide-p test-sphere portal))
              (format t "Collided with portal: ~a @~a~%" 
                      (portal-id portal) (pos portal))
              ;; We need to change the sector of the object
              (format t "Setting sector to: ~a~%" 
                      (sector-id (links-to-sector portal)))
              (setf (pos obj) (vec4+ pos (vec-scale4 (dir portal) +p-eps+)))
              (setf (current-sector obj) 
                    (sector-id (links-to-sector portal))))))))

(defmethod ray-cast (ray (a-sector sector))
  (let ((x-ray (make-ray (transform-to-sector (ray-e ray) a-sector)
                         (transform-to-sector (ray-d ray) a-sector))))
    (with-slots (geometry portals) a-sector
      (aif (ray-cast x-ray geometry)
           it
           ;; also check for portals!!
           (iter (for portal in portals)
                 (when (ray-cast x-ray portal)
                   (aif (ray-cast ray (links-to-sector portal))
                        (return-from ray-cast it))))))))



;; where the heck should this method go???
(defun kill-entity (e)
  (when (current-sector e)
    (remove-from-sector e))
  (remove-entity e))
