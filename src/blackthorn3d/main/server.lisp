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

(defclass ball (entity-server)
  ())

(defgeneric update (a-server-entity))

(defmethod update ((b ball))
  #+disabled
  (format t "I am ball #~a. I am at ~a~%" (oid b) (pos b))
  (incf (pos b)))

#+disabled ; TODO: Is this really what we want to do?
(defvar *living-things*
  (list
   (make-server-entity 'ball :pos 0)
   (make-server-entity 'ball :pos -1000)))

(defun next-frame ()
  "Reset the state of things to begin processing the next frame"
  (forget-server-entity-changes))

(defvar *client-count* 0)
(defvar *box-server-entity*)

(defun check-for-clients ()
  (let ((client (socket-server-connect :timeout 0)))
    (when client
      (incf *client-count*)
      (format t "Client ~a joined! (Total: ~a)~%" client *client-count*))
    client))

(defun read-string (msg)
  (message-value msg))

(defun send-string (dst str)
  (message-send dst (make-message :string str)))

(defun send-all-entities (destination)
  (message-send destination (make-event :entity-create :include-all t)))

(defun handle-message-server (src message)
  (ecase (message-type message)
    (:string
     (let ((str (read-string message)))
       (format t "The message from ~a was ~a~%" src str)
       (send-string src (concatenate 'string "ACK: " str))))
    (:event-input
     (let* ((inputs (message-value message))
            (z-amt (input-amount (find :x inputs :key #'input-type)))
            (x-amt (input-amount (find :y inputs :key #'input-type))))
       (setf (pos *box-server-entity*)
             (vec4+ (pos *box-server-entity*)
                    (make-vec3 (float x-amt) 0.0 (float z-amt))))))))

(defun handle-disconnect (client)
  (decf *client-count*)
  (format t "Client ~a disconnected. (Total: ~a)~%" client *client-count*))

(defun server-main (host port)
  (declare (ignore host))

  (when (not (socket-server-start port))
    (format t "Unable to start the server~%")
    (return-from server-main))
  (socket-disconnect-callback #'handle-disconnect)
  (format t "Server running on port ~a.~%" port)

  (setf *box-server-entity*
        (make-server-entity
         'entity-server 
         :pos (make-point3 0.0 0.0 0.0)
         :dir (make-vec3 1.0 0.0 0.0) 
         :up  (make-vec3 0.0 1.0 0.0)))

  (loop
     (next-frame)
     #+disabled             ; TODO: Is this really what we want to do?
     (iter (for thing in *living-things*)
           (update thing))
       
     ;; check for clients to join
     (let ((new-client (check-for-clients))
           #+disabled (camera nil)) ; TODO: Create a camera for new client.
       (when new-client (send-all-entities new-client))
       #+disabled
       (message-send new-client (make-event :camera camera)))

     ;; insert network code call here
     (iter (for (src message) in (message-receive-all :timeout 0))
           (handle-message-server src message))
     (message-send :broadcast (make-event :entity-create))
     (message-send :broadcast (make-event :entity-update))
     (message-send :broadcast (make-event :entity-remove))

     (sleep 1/120)))
