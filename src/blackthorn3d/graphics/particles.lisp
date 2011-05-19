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
;;; particles
;;;

;; states: :active :dead 

(defclass particle ()
  ((state)
   (energy)
   (position)
   (old-position)
   (velocity)
   (color)
   (size)))
 
(defclass emitter ()
  ((origin
    :accessor origin
    :initarg :origin)
   (spawn-rate
    :accessor spawn-rate
    :initarg :spawn-rate)
   (shader
    :accessor shader
    :initarg :shader)
   (texture
    :accessor texture
    :initarg :texture)
   (blend-mode
    :accessor blend-mode
    :initarg :blend-mode
    :initform '(:src-alpha :one-minus-source-alpha))
   (type
    :accessor type
    :initarg :type
    :initform :default)
   (particles
    :accessor particles
    :initarg :particles
    :documentation "the array of particles belonging to the system")
   (num-alive
    :accessor num-alive)
   
   ))