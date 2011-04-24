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

(defvar *socket-server-listen*)
(defvar *socket-server-connections* nil)
(defvar *socket-client-connection*)
(defvar *message-size-buffer* (make-buffer 4))

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
   @return{@code{T} on success.}"
  (assert (boundp '*socket-server-listen*))
  (assert (integerp timeout) (timeout) "Please specify an integral timeout.")
  (when (wait-for-input *socket-server-listen* :timeout timeout)
    (push (socket-accept *socket-server-listen*) *socket-server-connections*)
    t))

(defun socket-client-connect (host port &key timeout)
  "@short{Connects to the specified server.}

   @arg[host]{A host specifier. Either a string (e.g. @code{\"127.0.0.1\"}) or
              a vector (e.g. @code{#(127 0 0 1)}).}
   @arg[port]{A port number.}
   @arg[timeout]{An integer number of seconds to wait for a connection.}
   @return{@code{T} on success.}"
  (assert (not (boundp '*socket-client-connection*)))
  (assert (integerp timeout) (timeout) "Please specify an integral timeout.")
  (setf *socket-client-connection*
        (socket-connect host port
                        :protocol :stream
                        :element-type '(unsigned-byte 8)
                        :timeout timeout))
  t)

(defun socket-receive-message-size (connection)
  (with-buffer *message-size-buffer*
    (buffer-rewind)
    (buffer-advance :amount 4)
    (read-sequence *message-size-buffer* (socket-stream connection))
    (buffer-rewind)
    (unserialize :uint32)))

(defun socket-send-message-size (connection size)
  (with-buffer *message-size-buffer*
    (buffer-rewind)
    (serialize :uint32 size)
    (write-sequence *message-size-buffer* (socket-stream connection))))

(defun socket-receive-message (connection buffer)
  (let ((size (socket-receive-message-size connection)))
    (with-buffer buffer
      (buffer-rewind)
      (buffer-advance :amount size)
      (read-sequence size (socket-stream connection))
      (buffer-rewind)
      size)))

(defun socket-send-message (connection buffer)
  (with-buffer buffer
    (let ((size (buffer-length)))
      (socket-send-message-size connection size)
      (write-sequence buffer (socket-stream connection))
      (force-output (socket-stream connection)))))

(defun socket-server-receive-message (buffer &key timeout)
  "@short{Receives a single message.}

   @arg[buffer]{A userial buffer to store the incomming message.
                See @a[http://nklein.com/software/unet/userial/#make-buffer]{make-buffer}.}
   @arg[timeout]{An integer number of seconds to wait for a message.}"
  (assert (boundp '*socket-server-listen*))
  (assert (integerp timeout) (timeout) "Please specify an integral timeout.")
  (let ((ready (wait-for-input *socket-server-connections*
                               :timeout timeout :ready-only t)))
    (if ready
        (socket-receive-message (first ready) buffer)
        0)))

(defun socket-server-receive-all-messages (buffer callback &key timeout)
  "@short{Receives (and processes) all available messages.}

   @arg[buffer]{A userial buffer to store the incomming messages.
                See @a[http://nklein.com/software/unet/userial/#make-buffer]{make-buffer}.}
   @arg[callback]{A function to the called for each available message. The
                  function should take two arguments: the buffer, and the
                  length in bytes of the data written into the buffer.}
   @arg[timeout]{An integer number of seconds to wait for any messages.}"
  (assert (boundp '*socket-server-listen*))
  (assert (integerp timeout) (timeout) "Please specify an integral timeout.")
  (let ((ready (wait-for-input *socket-server-connections*
                               :timeout timeout :ready-only t)))
    (iter (for connection in ready)
          (let ((size (socket-receive-message connection buffer)))
            (funcall callback buffer size)))))

(defun socket-client-receive-message (buffer &key timeout)
  "@short{Receives a single message.}

   @arg[buffer]{A userial buffer to store the incomming message.
                See @a[http://nklein.com/software/unet/userial/#make-buffer]{make-buffer}.}
   @arg[timeout]{An integer number of seconds to wait for a message.}"
  (assert (boundp '*socket-client-connection*))
  (assert (integerp timeout) (timeout) "Please specify an integral timeout.")
  (if (wait-for-input *socket-client-connection* :timeout timeout)
      (socket-receive-message *socket-client-connection* buffer)
      0))

(defun socket-client-send-message (buffer)
  "@short{Sends a message.}

   @arg[buffer]{A userial buffer with the outgoing message.
                See @a[http://nklein.com/software/unet/userial/#make-buffer]{make-buffer}.}"
  (assert (boundp '*socket-client-connection*))
  (socket-send-message *socket-client-connection* buffer))
