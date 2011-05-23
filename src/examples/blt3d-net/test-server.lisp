(ql:quickload :blackthorn3d)
(in-package :blt3d-net)

(defvar *port* 12345)
(defvar *clients* 0)
(defvar *msg* (make-instance 'message :type :string :value "from server"))

;; connect
(socket-server-start *port*)

;; receive/send
(iter
 (let ((nid (socket-server-connect :timeout 0)))
   (when nid
     (incf *clients*)
     (format t "Connected to client ~a.~%" nid)))
 (unless (zerop *clients*)
   (let ((messages (message-receive-all :timeout 0)))
     (iter (for (src message) in messages)
           (format t "Received message: ~s~%" message)
           (message-send src *msg*))))
 (sleep 1/10))

(blt3d-main::exit)
