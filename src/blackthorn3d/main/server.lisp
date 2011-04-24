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
            (when (eq client-count 4)  ; TODO: should not hard-code # of players
                (return t))
            (format t "Clients: ~a~%" client-count)
            (when (socket-server-connect :timeout 0)
                (format t "~%Client joined!~%")
                (incf client-count)
            ))))
    
(defun server-main ()
    ; TODO: Customizable server port
    (format t "Please wait while the server starts up...~%")
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
))