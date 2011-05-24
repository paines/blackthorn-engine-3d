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

(in-package :blackthorn3d-main)

(defvar *room-table* (make-hash-table))

(defclass room (entity-server)
  ((contents
    :accessor contents
    :initform (make-hash-table))
   (geometry
    :accessor geometry
    :initarg :geometry
    :initform nil)))
    
(defun clone-table (table)
  (let ((clone (make-hash-table)))
    (maphash #'(lambda (k v) (setf (gethash k clone) v)) table)
    clone))
    
(defmethod foreach-in-room ((a-room room) func)
  (let ((new-table (clone-table (contents a-room))))
    (maphash #'(lambda (k v) 
                 (declare (ignore k))
                 (funcall func v))  
             new-table)))
             
(defmethod foreach-in-room ((a-room symbol) func)
  (foreach-in-room (lookup-room a-room) func))
    
(defmethod add-to-room (an-entity (a-room room))
  (setf (gethash (oid an-entity) (contents a-room)) an-entity)
  (setf (current-room an-entity) a-room))
  
(defmethod add-to-room (an-entity (a-room symbol))
  (add-to-room an-entity (lookup-room a-room)))
  
(defun remove-from-room (an-entity)
  (let ((the-room (current-room an-entity)))
    (remhash (oid an-entity) (contents the-room))
    (setf (current-room an-entity) nil)))
    
(defmethod update ((a-room room))
  (foreach-in-room a-room #'(lambda (an-entity) (update an-entity))))

(defun lookup-room (name-symbol)
  (gethash name-symbol *room-table*))
  
(defun new-room (name-symbol geometry)
  (let ((the-room (make-server-entity 'room :geometry geometry)))
    (setf (gethash name-symbol *room-table*) the-room)))
    
(defun update-rooms ()
  (maphash #'(lambda (sym a-room) (declare (ignore sym)) (update a-room))
           *room-table*))
           
;; where the heck should this method go???
(defun kill-entity (e)
  (when (current-room e)
    (remove-from-room e))
  (remove-entity e))