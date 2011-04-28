;;;; Blackthorn -- Lisp Game Engine
;;;;
;;;; Copyright (c) 2011, Elliott Slaughter <elliottslaughter@gmail.com>
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

(in-package :blackthorn3d-network)

;;;
;;; Node ID Mappings
;;;

(defvar *nid->socket* (make-hash-table))
(defvar *socket->nid* (make-hash-table))

(defun add-nid (nid socket)
  (multiple-value-bind (value exists) (gethash nid *nid->socket*)
    (declare (ignore value))
    (assert (not exists)))
  (setf (gethash nid *nid->socket*) socket
        (gethash socket *socket->nid*) nid)
  nid)

(defun remove-nid (nid socket)
  (multiple-value-bind (value exists) (gethash nid *nid->socket*)
    (declare (ignore value))
    (assert exists))
  (remhash nid *nid->socket*)
  (remhash socket *socket->nid*))

(defun nid->socket (nid)
  (gethash nid *nid->socket*))

(defun socket->nid (socket)
  (gethash socket *socket->nid*))

;;;
;;; Connections
;;;

(defvar *socket-server-listen*)
(defvar *socket-client-connection*)
(defvar *socket-connections* nil)
(defvar *message-size-buffer* (make-buffer 4))

;; TODO: Bugs!
;; Handle client error due to connection failure.
;; Handle server error due to client disconnect.
;; Bug unserializing string on other side (client -> server).

(defun socket-server-start (port)
  "@short{Starts a server listening on the specified port.} Note that
   @fun{socket-server-connect} must be used to actually connect to a client.

   @arg[port]{A port number.}
   @return{@code{T} on success.}"
  (assert (not (boundp '*socket-server-listen*)))
  (setf *socket-server-listen*
        (socket-listen *wildcard-host* port :element-type '(unsigned-byte 8)))
  t)

(defun socket-server-connect (&key timeout)
  "@short{Accepts an incomming connection.} Leaves an open socket to the
   connecting client so that bidirectional communication can follow. Must
   be called once for each client that is to connect.

   @arg[timeout]{An integer number of seconds to wait for a connection.}
   @return{The node ID for the client that connected.}"
  (assert (boundp '*socket-server-listen*))
  (assert (realp timeout) (timeout) "Please specify an integral timeout.")
  (handler-case
      (when (wait-for-input *socket-server-listen*
                            :timeout timeout :ready-only t)
        (let ((connection (socket-accept *socket-server-listen*)))
          (push connection *socket-connections*)
          (add-nid (gensym (symbol-name 'client)) connection)))
    (socket-error (err)
      (values nil err))))

(defun socket-client-connect (host port &key timeout)
  "@short{Connects to the specified server.}

   @arg[host]{A host specifier. Either a string (e.g. @code{\"127.0.0.1\"}) or
              a vector (e.g. @code{#(127 0 0 1)}).}
   @arg[port]{A port number.}
   @arg[timeout]{An integer number of seconds to wait for a connection.}
   @return{The node ID for the server that connected.}"
  (assert (not (boundp '*socket-client-connection*)))
  (assert (realp timeout) (timeout) "Please specify an integral timeout.")
  (handler-case
      (let ((connection (socket-connect host port
                                        :protocol :stream
                                        :element-type '(unsigned-byte 8)
                                        :timeout timeout)))
        (setf *socket-client-connection* connection)
        (push connection *socket-connections*)
        (values (add-nid :server connection) nil))
    (socket-error (err)
      (values nil err))))

;;;
;;; Buffers
;;;

(defun socket-receive-buffer-size (connection)
  (with-buffer *message-size-buffer*
    (buffer-rewind)
    (buffer-advance :amount 4)
    (read-sequence *message-size-buffer* (socket-stream connection))
    (buffer-rewind)
    (unserialize :uint32)))

(defun socket-send-buffer-size (connection size)
  (with-buffer *message-size-buffer*
    (buffer-rewind)
    (serialize :uint32 size)
    (write-sequence *message-size-buffer* (socket-stream connection))))

(defun socket-receive-buffer (connection buffer)
  (let ((size (socket-receive-buffer-size connection)))
    (with-buffer buffer
      (buffer-rewind)
      (buffer-advance :amount size)
      (read-sequence buffer (socket-stream connection))
      (buffer-rewind)
      size)))

(defun socket-send-buffer (connection buffer)
  (with-buffer buffer
    (let ((size (buffer-length)))
      (socket-send-buffer-size connection size)
      (write-sequence buffer (socket-stream connection))
      (force-output (socket-stream connection)))))

(defun socket-receive-buffer-all (connections buffer callback timeout)
  (let ((ready (wait-for-input connections
                               :timeout timeout :ready-only t)))
    (iter (for connection in ready)
          (let ((size (socket-receive-buffer connection buffer)))
            (funcall callback (socket->nid connection) buffer size)))
    (length ready)))

(defun socket-receive-all (buffer callback &key timeout)
  "@short{Receives (and processes) all available messages.}

   @arg[buffer]{A userial buffer to store the incomming messages.
                See @a[http://nklein.com/software/unet/userial/#make-buffer]{make-buffer}.}
   @arg[callback]{A function to the called for each available message. The
                  function should take three arguments: the source node ID,
                  the buffer, and the length in bytes of the data written
                  into the buffer.}
   @arg[timeout]{An integer number of seconds to wait for any messages.}
   @return{The number of messages received.}"
  (assert *socket-connections*)
  (assert (realp timeout) (timeout) "Please specify a real number timeout.")
  (labels ((receive-all (timeout)
             (socket-receive-buffer-all
                               *socket-connections* buffer callback timeout)))
    (let ((initial (receive-all timeout)))
      (if (zerop initial)
          initial
          (+ initial (iter (for subsequent next (receive-all 0))
                           (summing subsequent)
                           (until (zerop subsequent))))))))

(defun socket-send (destination buffer)
  "@short{Sends a message.}

   @arg[destination]{A destination node ID. Use @code{:server} to send to
                     the server from a client, @code{:broadcast} to send to
                     all clients from the server, or a specific client's
                     ID to send to that client.}
   @arg[buffer]{A userial buffer with the outgoing message.
                See @a[http://nklein.com/software/unet/userial/#make-buffer]{make-buffer}.}"
  (acond ((eql destination :broadcast)
          (assert (boundp '*socket-server-listen*))
          (error "unimplemented"))
         ((nid->socket destination)
          (socket-send-buffer it buffer))
         (t (error "Unknown node ID ~s" destination))))
