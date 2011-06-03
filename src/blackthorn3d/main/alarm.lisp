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

(defun get-real-time ()
  "Real time in seconds."
  (/ (get-internal-real-time)
     internal-time-units-per-second))

(defclass alarm (entity-server)
  ((time-left
    :accessor time-left
    :initarg :time-left
    :documentation "How long until alarm is triggered, in seconds.")
   (then
    :accessor then
    :initform (get-real-time))
   (callback
    :accessor callback
    :initarg :callback
    :documentation "Called when alarm goes off.")))

(defmethod update ((a alarm))
  (let* ((now (get-real-time))
         (elapsed (- now (then a))))
   
    (when (> elapsed 0)
      (setf (then a) now)
      (setf (time-left a) 
            (- (time-left a) elapsed)))
              
    (when (< (time-left a) 0)
      (if (not (eq (callback a) nil))
          (funcall (callback a)))
      (setf (callback a) nil)
      (remove-entity a))))
       
(defclass cyclic-alarm (entity-server)
  ((alarm
    :accessor alarm
    :initarg :alarm)))

(defun make-cyclic-alarm (period callback)
  (let ((external-alarm (make-server-only 'cyclic-alarm :alarm nil)))
    (labels ((indirect-callback ()
               (setf (alarm external-alarm)
                     (make-server-only 'alarm
                                       :time-left period
                                       :callback #'indirect-callback))
               (funcall callback)))
      (setf (alarm external-alarm)
            (make-server-only 'alarm
                              :time-left period
                              :callback #'indirect-callback)))
    external-alarm))
