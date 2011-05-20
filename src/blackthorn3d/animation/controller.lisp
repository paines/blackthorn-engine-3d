;;;; Blackthorn -- Lisp Game Engine
;;;;
;;;; Copyright (c) 2011, Robert Gross <r.gross.3@gmail.com>
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

(in-package :blackthorn3d-animation)



;; every animated model should have one of these...
(defclass anim-controller ()
  ((current-clip
    :accessor current-clip
    :initarg :current-clip
    :initform nil)
   (next-clip
    :initform nil)
   (elapsed
    :accessor elapsed
    :initform 0.0)
   (state
    :accessor state
    :initarg :state
    :initform :stop)
   (clips
    :initarg :clips)
   (channel-bindings
    :initarg :channel-bindings
    :documentation "to allow re-use of channels for multiple objects
                    we bind them to their targets at the controller 
                    level.  a channel binding is a tuple:
                    (channel target-fn last-frame) where last-frame
                    is an optimization for channel")))

(defun make-animation-controller (clips)
  (make-instance
   'anim-controller
   :current-clip (car clips)
   :state :stop
   :clips clips
   :channel-bindings 
   (iter (for c in clips)
         (append (iter (for ch in (channel-lst c))
                       (collect (list ch nil nil)))))))

(defmethod bind-channel ((this anim-controller) channel target-fn)
  (with-slots (channel-bindings) this
    (let ((ch-bind (find channel channel-bindings :key #'car)))
      (if ch-bind
          (setf (second ch-bind) target-fn
                (third ch-bind) nil)
          (push (make-binding channel target-fn nil) channel-bindings)))))

(defun set-next-clip (controller clip)
  (with-slots (current-clip next-clip) controller
    (if current-clip
        (setf next-clip clip)
        (setf current-clip clip))))

(defmethod play-clip ((this anim-controller) clip &optional (state :run))
  (set-next-clip this clip)
  (setf (state this) state))

(defun next-clip (controller)
  (with-slots (current-clip next-clip state) this
    (setf current-clip next-clip)
    (setf next-clip nil)
    (setf elapsed 0)))

;; Updates the animation state controlled by this controller
;; there are two states (possibly three later):
;;   :run - animation playes current-clip until it stops
;;          then will run next-clip if there is one
;;   :stop - does nothing
(defmethod update-anim-controller ((this anim-controller) dt)
  (with-slots (current-clip next-clip elapsed 
               t-start state 
               channel-bindings) this
    (incf elapsed dt)
    (when (> elapsed (end-time current-clip))
      (if (eql state :loop) 
          (setf elapsed 0.0)
          (next-clip this)))
    (case state
      ((:run 
        :loop) (if current-clip
                   (update-clip current-clip elapsed channel-bindings)
                   (setf state :stop)))
      (:stop   (setf elapsed 0.0))
      (:pause  nil)
      (:reset  (if current-clip
                   (update-clip current-clip 0.0))
               (setf state :stop)))))