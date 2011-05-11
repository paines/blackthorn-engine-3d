;;;;
;;;; This example tests usocket with TCP and UDP. Any serialization library
;;;; could have been used, but for this example cl-store was chosen because
;;;; it takes little effort to set up.
;;;;
;;;; To try this, load this file
;;;;     (load "usocket.lisp")
;;;; and try out the various functions, e.g.
;;;;     (http-test)
;;;; Note that for some tests (e.g. server/client pairs), you will need
;;;; to start two Lisp consoles to run the test.
;;;;

(ql:quickload :usocket)
(ql:quickload :flexi-streams)
(ql:quickload :cl-store)

(defpackage :usocket-test
  (:use :cl :usocket :cl-store)
  (:export :http-test :tcp-server :tcp-client :udp-server :udp-client))
(in-package :cl-user)
(use-package :usocket-test)
(in-package :usocket-test)


;; example HTTP conversation with google.com
(defun http-test ()
  (with-client-socket (socket stream "google.com" 80)
    (format stream "GET / HTTP/1.1~%~%")
    (force-output stream)
    (read-line stream)))


;; example with server and client communicating using cl-store

;; TCP
(defun tcp-server ()
  (with-socket-listener (socket "127.0.0.1" 8888 :element-type '(unsigned-byte 8))
    (loop
       (with-connected-socket (connection (socket-accept socket))
         (format t "Server recieved ~a~%" (restore (socket-stream connection)))))))

(defun tcp-client ()
  (with-client-socket (socket stream "127.0.0.1" 8888 :element-type '(unsigned-byte 8))
    (store '(1 2 3 4 5) stream)
    (force-output stream)
    (format t "Client sent ~a~%" '(1 2 3 4 5))))

;; UDP
(defun udp-server ()
  (let* ((socket (socket-connect nil nil :protocol :datagram
                                 :local-host "127.0.0.1" :local-port 8888))
         (buffer (make-array 65536 :element-type '(unsigned-byte 8))))
    (multiple-value-bind (buffer size host port)
        (socket-receive socket buffer 65536)
      (flexi-streams:with-input-from-sequence (stream buffer :end size)
        (format t "Server received ~a~%" (restore stream))))))

(defun udp-client ()
  (let* ((socket (socket-connect "127.0.0.1" 8888 :protocol :datagram))
         (buffer (flexi-streams:with-output-to-sequence (stream)
                   (store '(1 2 3 4 5) stream)))
         (length (array-dimension buffer 0)))
    (format t "Client sent ~a~%" '(1 2 3 4 5))
    (socket-send socket buffer length)))
