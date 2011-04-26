(ql:quickload :blackthorn3d)
(in-package :blt3d-net)

(defvar *port* 12345)
(defvar *expected-clients* 2)
(defvar *clients* nil)
(defvar *my-recv-buffer* (make-buffer))
(defvar *my-send-buffer* (make-buffer))

;; connect
(socket-server-start *port*)
(iter (repeat *expected-clients*)
      (let ((nid (socket-server-connect :timeout 30)))
        (format t "Connected to client ~a.~%" nid)
        (push nid *clients*)))

;; receive/send
(sleep 1)
(socket-receive-all
 *my-recv-buffer*
 #'(lambda (nid buf size)
     (format t "Message from ~a size ~a.~%" nid size)
     (format t "Message contents: ~s~%" (unserialize :string :buffer buf))
     (with-buffer *my-send-buffer*
       (buffer-rewind)
       (serialize :string "This is the server."))
     (socket-send nid *my-send-buffer*))
 :timeout 10)

(blt3d-main::exit)
