(ql:quickload :blackthorn3d)
(in-package :blt3d-net)
(defvar *port* 12345)
(defvar *expected-clients* 2)
(socket-server-start *port*)
(iter (repeat *expected-clients*)
      (format t "Connected to client ~a.~%"
              (socket-server-connect :timeout 10)))
(defvar *my-buffer* (make-buffer))
(sleep 1)
(socket-message-receive-all
 *my-buffer*
 #'(lambda (nid buf size)
     (format t "Message from ~a size ~a.~%" nid size)
     (format t "Message contents ~a.~%" (unserialize :string :buffer buf)))
 :timeout 0)
(blt3d-main::exit)
