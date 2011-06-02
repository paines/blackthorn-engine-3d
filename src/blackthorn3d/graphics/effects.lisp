;;;; Blackthorn -- Lisp Game Engine
;;;;
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

(in-package :blackthorn3d-graphics)

;;;
;;; Effects
;;; - mostly for managing particles

;; to be part of an 'effect' it needs a 'draw-object' method
;; as well as an 'update'
(defclass effect ()
  ((objects
    :accessor objects
    :initarg :objects
    :initform ())))

(defun add-to-effect (obj effect)
  (push obj (objects effect)))


;;
;; to define effects...
;; we need a mapping of ids to effects and how to play/or create them
;; so they need a 'handle' function that takes &rest args and does stuff
;; For now lets consider creating new effects every time.

;; 

(defvar *robot-laser-tex* nil)
(defvar *robot-laser-size* '(0.1 . 0.4))
(defvar *human-laser-tex* nil)
(defvar *human-laser-size* '(0.1 . 0.4))

(defclass laser ()
  ((start
    :initarg :start)
   (beam
    :initarg :beam)
   (color
    :initarg :color)
   (texture
    :initarg :texture)
   (size
    :initarg :size)))

(defun make-human-laser (start beam)
  (make-instance 'laser 
                 :start start
                 :beam beam
                 :color +aqua+
                 :texture *human-laser-tex*
                 :size *human-laser-size*))

(defun make-robot-laser (start beam)
  (make-instance 'laser
                 :start start
                 :beam beam
                 :color +purple+
                 :texture *robot-laser-tex*
                 :size *robot-laser-size*))