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
    ()
)

(defgeneric update (a-server-entity))

(defmethod update ((b ball))
    #+disabled
    (format t "I am ball #~a. I am at ~a~%" (oid b) (pos b))
    (incf (pos b))
    )


(defvar *living-things* (list
    (make-server-entity 'ball :pos 0)
    (make-server-entity 'ball :pos -1000)
))

(defun next-frame ()
    "Reset the state of things to begin processing the next frame"
    (forget-server-entity-changes)
)
    
    
(let ((client-count 0))
    (defun wait-for-clients ()
        (loop
            (when (eq client-count 2)  ; TODO: should not hard-code # of players
                (return t))
            ;(format t "Clients: ~a~%" client-count)
            (format t ".")
            (when (socket-server-connect :timeout 1.0)
                (format t "~%Client joined!~%")
                (incf client-count)
            ))))
    
(defvar *my-buffer*)

(defun read-string (b)
    (userial:with-buffer b
      (userial:unserialize :string)))

(let ((my-buffer (userial:make-buffer)))
    (defun send-string (dst str)
        (userial:with-buffer my-buffer
           (userial:buffer-rewind)
           (userial:serialize :string str))
        (socket-send dst my-buffer)))

(defvar *last*)
        
(defun handle-message (src b size)
  (let ((msg (read-string b)))
    (setf *last* src)
    (format t "The message from ~a was ~a~%" src msg)
    (send-string src (concatenate 'string "ACK: " msg))
    #+disabled
    (format t "A message of ~a bytes was received.~%" size)))

(defun server-main ()
    ; TODO: Customizable server port
    (format t "Please wait while the server starts up...~%")
    (setf *my-buffer* (userial:make-buffer 4096))    
    (when (not (socket-server-start 9001))
        (format t "Unable to start the server~%")
        (exit)
    )
    
    ; wait for clients to join
    (format t "Waiting for clients to join~%")
    (wait-for-clients)
    (format t "All clients joined. Beginning game...~%")
    
    (loop
        (next-frame)
        (iter (for thing in *living-things*)
           (update thing))
           
        ; insert network code call here
        (socket-receive-all *my-buffer* #'handle-message 
            :timeout 0)
            
        ; this causes all kinds of interesting problems!
        ;(when (boundp '*last*)
        ;    (send-string *last* "A message for you"))
        (sleep 1/120)
))
