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

(defclass pos-edge-react (entity-server)
    ((state
        :accessor state
        :initform nil)
     (check-signal
        :accessor check-signal
        :initform #'(lambda () nil)
        :initarg :check-signal)
     (callback
        :accessor callback
        :initarg :callback)))
        
(defmethod update ((reactor pos-edge-react))
  (let ((new-state (funcall (check-signal reactor))))
    (when (and (not (state reactor)) new-state)
        (setf (state reactor) t)
        (funcall (callback reactor)))
    (when (and (state reactor) (not new-state))
        (setf (state reactor) nil))))
        
(defun make-pos-reactor (test action)
  (make-server-entity 'pos-edge-react
    :check-signal test
    :callback action))
    