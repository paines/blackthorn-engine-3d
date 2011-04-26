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

;(let ((my-buffer (userial:make-buffer)))
;    (defun send-string (dst str)
;        (userial:with-buffer my-buffer
;           (userial:buffer-rewind)
;           (userial:serialize :string str))
;        (socket-message-send dst my-buffer)))

(defvar *my-client-buffer*)

;(defun read-string (b)
;    (userial:with-buffer b
;        (userial:buffer-rewind)
;        (userial:unserialize :string)))
        
(defun handle-message-client (src b size)
  (let ((msg (read-string b)))
    (format t "Msg from ~a was ~a~%" src msg)))
        
(defvar counter 0)
        
(defun client-main ()

    (setf *my-client-buffer* (userial:make-buffer))
    (setf *random-state* (make-random-state t))
    
    
    ; TODO: Don't hard code connection information
    (handler-case 
        (socket-client-connect "127.0.0.1" 9001 :timeout 1.0)
      (usocket:connection-refused-error ()
        ; TODO: handle this more gracefully.
        (format t "Error: Connection refused.~%")
        (return-from client-main)))
        
    ;(userial:with-buffer *my-buffer*
    ;  (userial:buffer-rewind)
    ;  (userial:serialize :string "Hello, world!"))
    ;(socket-message-send :server *my-buffer*)
    
    (loop
      ;(format t "HELLO")
      (socket-message-receive-all *my-client-buffer* #'handle-message-client 
            :timeout 0)
      (sleep 1/60)
      (send-string :server (format nil "Msg #~a (rand: ~a)" counter (random 10)))
      (incf counter))
)
