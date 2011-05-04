(ql:quickload :blackthorn3d)
(in-package :blt3d-net)

(defvar *port* 12345)
(defvar *msg* (make-instance 'message :type :string :value "from client"))

;; connect
(let ((server (socket-client-connect "localhost" *port* :timeout 10)))
  (if server
      (format t "Connected to server ~a.~%" server)
      (blt3d-main::exit)))

;; send/receive
(message-send :server *msg*)

(let ((messages (message-receive-all :timeout 10)))
  (iter (for (src message) in messages)
        (format t "Received message: ~s~%" message)))

(blt3d-main::exit)
