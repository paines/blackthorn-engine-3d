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

(in-package :blackthorn3d-network)

;;;
;;; Messages
;;;

(defvar *message-buffer* (make-buffer))

(defun message-send (destination message)
  (with-buffer *message-buffer*
    (buffer-rewind)
    (serialize :message message)
    (socket-send destination *message-buffer*)))

(defun message-receive-all (&key timeout)
  (let (messages)
    (labels ((read-message (src buffer size)
               (declare (ignore size))
               (with-buffer buffer
                 (push (list src (unserialize :message)) messages))))
      (socket-receive-all *message-buffer* #'read-message :timeout timeout)
      (nreverse messages))))

(defclass message ()
  ((type
    :accessor message-type
    :initarg :type)
   (value
    :accessor message-value
    :initarg :value)))

(defun make-message (type value)
  (make-instance 'message :type type :value value))

;; Note: The bogus enum entry was needed to make this take up more than 0 bits.
(make-enum-serializer :message-type (:string
                                     :event-entity-create
                                     :event-entity-update))

(defmethod serialize ((mtype (eql :message)) value &key (buffer *buffer*))
  (with-buffer buffer
    (with-slots (type value) value
      (serialize :message-type type)
      (serialize type value))))

(defmethod unserialize ((mtype (eql :message)) &key (buffer *buffer*))
  (let ((message (make-instance 'message)))
    (with-buffer buffer
      (with-slots (type value) message
        (setf type (unserialize :message-type))
        (setf value (unserialize type))
        message))))
