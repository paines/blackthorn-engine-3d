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

(defvar *eyesight-radius* 10.0)

(defclass simple-monster (entity-server)
  ((chase-who
    :accessor chase-who
    :initform nil)
   (eyesight
    :accessor eyesight
    :initform nil)
  ))
  
(defclass eyesight (entity-server)
  ((follow
    :accessor follow
    :initarg  :follow)))
    
(defmethod update ((monster simple-monster))
  (if (chase-who monster)
    (monster-chase-who monster (chase-who monster))
    (monster-wander monster)))

(defun make-eye (the-monster)
  (make-server-entity 'eyesight
      :pos (make-point3 0.0 0.0 0.0)
      :dir (make-vec3 1.0 0.0 0.0)
      :up  (make-vec3 0.0 1.0 0.0)
      :follow the-monster
      :shape-name :none
      :bv (make-instance 'blackthorn3d-physics:bounding-sphere 
                    :pos (make-vec3 0.0 0.0 0.0)
                    :rad *eyesight-radius*)))
    
(defun make-monster (pos)
  (let ((the-monster (make-server-entity 'simple-monster
          :pos pos
          :dir (make-vec3 1.0 0.0 0.0)
          :up  (make-vec3 0.0 1.0 0.0)
          :shape-name :wedge
          :bv (make-instance 'blackthorn3d-physics:bounding-sphere
                :pos (make-vec3 0.0 0.0 0.0)
                :rad 1.0)
          )))
    (setf (eyesight the-monster) (make-eye the-monster))
    the-monster))
                    
(defmethod update ((eye eyesight))
  (if (not (is-alive-p (follow eye)))
    (remove-entity eye)
    (setf (pos eye) (pos (follow eye)))))
    
    
    
(defun monster-chase-who (self who)
  (format t "Chase ~a~%" who)
  (if (or (not (is-alive-p who))
          (not (blackthorn3d-physics:collide-p (eyesight self) who)))
    ;; then
    (setf (chase-who self) nil)
    ;; else
    (blt3d-phy:chase self who 0.08)))
      
(defun monster-wander (self)
  (declare (ignore self)))
  
(defmethod collide ((eye eyesight) (p player))
  (let ((monster (follow eye)))
    (when (eq nil (chase-who monster))
      (format t "Monster says: I SEE YOU, ~a! YOU LOOK TASTY!~%" p)
      (setf (chase-who monster) p))))
      
(defmethod collide ((p player) (eye eyesight))
  (collide eye p))
      
(defmethod collide ((p player) (m simple-monster))
  (format t "Om nom nom nom!~%")
  (setf (blackthorn3d-entity::die-now p) :yes)
  )
  
(defmethod collide ((m simple-monster) (p player))
  (collide p m))
      