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

(defvar *living-things*
  (list
   (make-server-entity 'ball :pos 0)
   (make-server-entity 'ball :pos -1000)))

(defun next-frame ()
  "Reset the state of things to begin processing the next frame"
  (forget-server-entity-changes))

(defvar *client-count* 0)

(defun check-for-clients ()
  (let ((client (socket-server-connect :timeout 0)))
    (when client
      (incf *client-count*)
      (format t "Client ~a joined! (Total: ~a)~%" client *client-count*))))

(defvar *my-buffer*)

(defun read-string (msg)
  (message-value msg))

(defun send-string (dst str)
  (message-send dst (make-message :string str)))

(defvar *last*)

(defun handle-message (src message)
  (let ((str (read-string message)))
    (setf *last* src)
    (format t "The message from ~a was ~a~%" src str)
    (send-string src (concatenate 'string "ACK: " str))))

(defun handle-disconnect (client)
  (decf *client-count*)
  (format t "Client ~a disconnected. (Total: ~a)~%" client *client-count*))

(defun server-main ()
  ;; TODO: Customizable server port
  (format t "Please wait while the server starts up...~%")
  (when (not (socket-server-start 9001))
    (format t "Unable to start the server~%")
    (return-from server-main))
  (socket-disconnect-callback #'handle-disconnect)
  (format t "Join when ready.~%")

  (loop
     (next-frame)
     (iter (for thing in *living-things*)
           (update thing)) 

     ;; check for clients to join
     (check-for-clients)

     ;; insert network code call here
     (iter (for (src message) in (message-receive-all :timeout 0))
           (handle-message src message))

     ;; this causes all kinds of interesting problems!
     ;;(when (boundp '*last*)
     ;;    (send-string *last* "A message for you"))
     (sleep 1/120)))
