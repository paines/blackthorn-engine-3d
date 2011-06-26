;;;; Blackthorn -- Lisp Game Engine
;;;;
;;;; Copyright (c) 2011, Elliott Slaughter <elliottslaughter@gmail.com>
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

(in-package :blackthorn3d-entity)

(defmacro track-modifed-slots (class &rest slots)
  (with-gensyms (value object modified)
    `(progn
       ,@(iter (for slot in slots)
               (collect
                   `(defmethod (setf ,slot) :after (,value (,object ,class))
                      (with-slots ((,modified modified)) ,object
                        (setf ,modified t))))))))

(defclass entity ()
  ((oid
    :accessor oid)
   (modified
    :accessor modified
    :initform nil)
   (pos
    :accessor pos
    :initarg :pos
    :initform (blt3d-math:make-point3 0.0 0.0 0.0))
   (dir
    :accessor dir
    :initarg :dir
    :initform (blt3d-math:make-vec3 1.0 0.0 0.0))
   (up
    :accessor up
    :initarg :up
    :initform (blt3d-math:make-vec3 0.0 1.0 0.0))
   (die-now
    :accessor die-now
    :initform :no
    :initarg :die-now)
   (shape-name
    :accessor shape-name
    :initform :none
    :initarg  :shape-name)
   (current-sector
    :accessor current-sector
    :initform nil)
   (shape
    :accessor shape
    :initarg :shape
    :initform nil)))

(track-modifed-slots entity pos dir up shape-name die-now)

(defclass entity-server (entity)
  ((oid
    :initform (make-server-oid)
    :initarg :oid)
   (velocity
    :accessor velocity
    :initarg :velocity
    :initform blt3d-math:+zero-vec+)
   (new-up
    :accessor new-up
    :initform +y-axis+)
   (displacers
    :accessor displacers
    :initform nil)
   (forces
    :accessor forces
    :initform nil)
   (is-jumping
        :accessor is-jumping
        :initform nil
        :documentation "Jumping flag is t when you are jumping")
   (bounding-volume
    :accessor bounding-volume
    :initform nil
    :initarg :bv)
   (team
    :accessor team
    :initform nil
    :initarg :team)))


(defclass entity-client (entity)
  ((oid
    :initarg :oid)))



(defgeneric update (a-server-entity))
(defmethod update ((e entity-server))
  (declare (ignore e)))

(defgeneric transform-entity (a-entity xform))
(defmethod transform-entity ((e entity) xform)
  (with-slots (pos dir up) e
    (setf (pos e) (matrix-multiply-v xform pos)
          (dir e) (matrix-multiply-v xform dir)
          (up e)  (matrix-multiply-v xform up))
    e))

(defmethod transform-entity :before ((e entity-server) xform)
  (setf (velocity e) (matrix-multiply-v xform (velocity e))))


(defvar *global-oid-table* (make-hash-table))
(defvar *recently-created-server-entities* nil)
(defvar *recently-removed-server-entities* nil)

(let ((next-oid 0))
  (defun make-server-oid ()
    (incf next-oid)))

(defun oid-in-use-p (oid)
  (multiple-value-bind (previous exists)
      (gethash oid *global-oid-table*)
    (declare (ignore previous))
    exists))

(defun lookup-entity (oid)
  (multiple-value-bind (object exists) (gethash oid *global-oid-table*)
    (unless exists (error "No object for oid ~a." oid))
    object))

(defun intern-entity (object)
  (with-slots (oid) object
    (assert (not (oid-in-use-p oid)))
    (setf (gethash oid *global-oid-table*) object)))

(defun unintern-entity (object)
  (with-slots (oid) object
    (assert (eql (lookup-entity oid) object))
    (remhash oid *global-oid-table*)))

(defun remember-created-server-entity (object)
  (push object *recently-created-server-entities*)
  object)

(defun remember-removed-server-entity (object)
  (push object *recently-removed-server-entities*)
  object)

(defun forget-server-entity-changes ()
  (setf *recently-created-server-entities* nil
        *recently-removed-server-entities* nil)
  (iter (for (nil entity) in-hashtable *global-oid-table*)
        (with-slots (modified) entity
          (setf modified nil))))

(defun make-server-entity (class &rest initargs)
  (remember-created-server-entity
   (intern-entity (apply #'make-instance class initargs))))

(defun make-client-entity (oid &rest initargs)
  (intern-entity (apply #'make-instance 'entity-client :oid oid initargs)))

(defun list-entities ()
  (iter (for (nil entity) in-hashtable *global-oid-table*)
        (collect entity)))

(defun remove-entity (object)
  (unintern-entity object)
  (remember-removed-server-entity object))

(defun lookup-and-remove-entity (oid)
  (remove-entity (lookup-entity oid)))

(make-alias-serializer :oid :uint32)

(make-vector-serializer :vec3 :single-float 3)
(make-vector-serializer :vec4 :single-float 4)

(make-key-slot-serializer (:entity-create entity
                           (:oid oid oid)
                           (make-client-entity oid))
                          :vec4 pos
                          :vec4 dir
                          :vec4 up
                          :symbol shape-name
                          :symbol die-now
                          :symbol current-sector)

(make-key-slot-serializer (:entity-update entity
                           (:oid oid oid)
                           (lookup-entity oid))
                          :vec4 pos
                          :vec4 dir
                          :vec4 up
                          :symbol shape-name
                          :symbol die-now
                          :symbol current-sector)

(make-key-slot-serializer (:entity-oid entity
                           (:oid oid oid)
                           (lookup-entity oid)))

(make-key-slot-serializer (:entity-remove entity
                           (:oid oid oid)
                           (lookup-and-remove-entity oid)))
