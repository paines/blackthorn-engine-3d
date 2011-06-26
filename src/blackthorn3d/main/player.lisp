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

(defclass player (entity-server)
  ((client
        :accessor player-client
        :initarg :client
        :documentation "The socket symbol for the player's client")
   (health
        :accessor health
        :initform 1.0
        :documentation "Health in range [0.0, 1.0]")
   (camera
        :accessor attached-cam
        :initform nil
        :documentation "The camera that's associated with this player for motion")
   ))

(defmethod on-die ((self player))
  (send-play-explosion :broadcast :none (pos self))

  (let ((the-cam (attached-cam self)))
    (with-slots (pos velocity health up dir) self
      (setf health 1.0)

      (setf pos (make-point3 0.0 0.1 0.0))
      (setf dir (make-vec3 1.0 0.0 0.0))
      (setf up  (make-vec3 0.0 1.0 0.0))
      (setf velocity (vec-neg4 +y-axis+))
      (reinitialize-instance the-cam
        :pos (make-point3 0.0 0.0 0.0)
        :dir (make-vec3 1.0 0.0 0.0)
        :up  (make-vec3 0.0 1.0 0.0)
        :ideal-coord (list 0.0 (/ pi 6) 4.0))
  )))

(defmethod quickhit ((self player))
  (setf (health self) (- (health self) 0.1))
  (format t "Player health now: ~a~%" (health self)))

(defmethod try-die ((self player))
  (when (< (health self) 0.0)
    (on-die self)))

(defvar *client->player* '())
(defun register-player (p c)
    (setf (getf *client->player* c) p))

(defun remove-player (client)
    (let ((player (getf *client->player* client)))
      (when player
        (kill-entity player)))
    (remf *client->player* client))

(defun new-player (client-id)
    (let ((p (make-server-entity
              'player
              :client client-id
              :pos (make-point3 0.0 0.1 0.0)
              :dir (make-vec3 1.0 0.0 0.0)
              :up  (make-vec3 0.0 1.0 0.0)
              :bv  (make-instance 'blackthorn3d-physics:bounding-sphere
                                  :pos (make-point3 0.0 0.0 0.0)
                                  :rad 1.0)
              :shape-name :wedge
              :velocity (vec-neg4 +y-axis+)
              )))
      ;(push #'blackthorn3d-physics:gravity-mover (movers p))
      (push (blackthorn3d-physics::make-gravity-mover)
            (forces p))
      (push (blackthorn3d-physics::make-smarter-jump-mover client-id)
            (forces p))
      (setf (bounding-volume p) (expand-bounding-spheres
                    (blt3d-res:get-model (shape-name p))))
      (format t "bounding volume: ~a ~a~%"
              (blt3d-phy::pos (bounding-volume p))
              (blt3d-phy::rad (bounding-volume p)))

      (flet ((test () (not (eql 0.0 (s-input-move-x client-id))))
             (action () (format t "Client ~a started moving.~%" client-id)))
        (make-pos-reactor #'test #'action))

      (register-player p client-id)))
