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

(defclass Player (entity-server)
  ((client
        :accessor player-client
        :initarg :client
        :documentation "The socket symbol for the player's client")))

(defvar *client->player* '())
(defun register-player (p c)
    (setf (getf *client->player* c) p))
    
(defun remove-player (client)
    (let ((player (getf *client->player* client)))
      (when player 
        (remove-entity player)))
    (remf *client->player* client))
    
(defun new-player (client-id)
    (let ((p (make-server-entity
         'Player
         :client client-id
         :pos (make-point3 0.0 0.0 0.0)
         :dir (make-vec3 1.0 0.0 0.0)
         :up  (make-vec3 0.0 1.0 0.0)
         :bv  (make-instance 'blackthorn3d-physics:bounding-sphere 
                :pos (make-point3 1.0 0.0 0.0)
                :rad 1.0)
         :shape-name :wedge
         )))
    (register-player p client-id)))
    
