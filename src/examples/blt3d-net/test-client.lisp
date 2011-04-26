(ql:quickload :blackthorn3d)
(in-package :blt3d-net)

(defvar *port* 12345)
(defvar *my-send-buffer* (make-buffer))
(defvar *my-recv-buffer* (make-buffer))

;; connect
(let ((server (socket-client-connect "localhost" *port* :timeout 10)))
  (if server
      (format t "Connected to server ~a.~%" server)
      (blt3d-main::exit)))

;; send/receive
(serialize :string "Hello world!" :buffer *my-send-buffer*)
(socket-send :server *my-send-buffer*)

(with-buffer *my-recv-buffer*
  (buffer-rewind))
(socket-receive-all
 *my-recv-buffer*
 #'(lambda (nid buf size)
     (format t "Message from ~a size ~a.~%" nid size)
     (format t "Message contents: ~s~%" (unserialize :string :buffer buf)))
 :timeout 10)

(blt3d-main::exit)
