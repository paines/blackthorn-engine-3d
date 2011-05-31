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

(defun make-message-list (type &rest values)
  (make-instance 'message :type type :value values))

(define-serializer (:message value)
  (with-slots (type value) value
    (serialize :keyword type)
    (serialize type value)))

(define-unserializer (:message)
  (let ((message (make-instance 'message)))
    (with-slots (type value) message
      (setf type (unserialize :keyword))
      (setf value (unserialize type))
      message)))

(defmacro defmessage (type sender (&rest fields))
  (let ((field-vars (mapcar #'(lambda (field) (gensym (symbol-name field)))
                            fields))
        (dest (gensym (symbol-name 'dest))))
    `(progn
       (define-serializer (,type value)
         (destructuring-bind ,field-vars value
           ,@(mapcar #'(lambda (field var)
                         `(serialize ,field ,var))
                     fields
                     field-vars)))
       (define-unserializer (,type)
         (let ((value
                (list ,@(mapcar #'(lambda (field) `(unserialize ,field))
                                fields))))
           value))
       ,(when sender
          `(defun ,sender (,dest ,@field-vars)
             (message-send ,dest (make-message-list ,type ,@field-vars)))))))

(defun apply-message-handler (handler src message)
  (apply handler src (message-value message)))
