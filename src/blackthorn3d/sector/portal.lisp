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

;; Returns true if an entity (assumed to have already intersected 
;; the bv) has crossed the portal
(defmethod crosses-portal-p ((obj entity-server) (p portal))
  (let ((p-plane (point-normal-plane (pos p) (dir p))))
    (plusp (plane-dist p-plane (pos entity)))))