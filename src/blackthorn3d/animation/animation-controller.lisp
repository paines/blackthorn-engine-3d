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

(defun make-anim-controller (clips)
  (make-instance
   'anim-controller
   :current-clip (car clips)
   :state :loop
   :clips clips))

(defun copy-anim-controller (controller)
  (with-slots (channel-bindings clips state elapsed current-clip next-clip)
      controller
    (make-instance 'animation-controller
                   :channel-bindings channel-bindings
                   :clips clips
                   :state state
                   :current-clip current-clip
                   :next-clip next-clip
                   :elapsed elapsed)))


(defmethod apply-transform ((this anim-controller) xform)
  (with-slots (clips) this
    (iter (for clip in clips)
          (iter (for channel in (channel-lst clip))
                (apply-transformation channel xform)))))

(defun set-next-clip (controller clip)
  (with-slots (current-clip next-clip state) controller
    (if current-clip
        (setf next-clip clip)
        (setf current-clip clip))))

(defmethod play-clip ((this anim-controller) clip)
  (set-next-clip this clip)
  (setf (state this) :run))

#+disabled
(defmethod play-clip ((this anim-controller)))

(defun next-clip (controller)
  (with-slots (current-clip next-clip state) controller
    (setf current-clip next-clip)
    (setf next-clip nil)
    (setf elapsed 0)))

;; Updates the animation state controlled by this controller
;; there are two states (possibly three later):
;;   :run - animation playes current-clip until it stops
;;          then will run next-clip if there is one
;;   :stop - does nothing
(defmethod update-anim-controller ((this anim-controller) dt)
  (with-slots (current-clip next-clip elapsed t-start state clips) this
    (incf elapsed dt)
    (when (> elapsed (end-time current-clip))
      (if (eql state :loop) 
          (setf elapsed 0.0)
          (next-clip this)))
    (case state
      ((:run :loop)    (if current-clip
                           (update-clip current-clip elapsed)
                           (play-clip this (car clips))
                           #+disabled
                           (setf state :stop)))
      (:stop   (setf elapsed 0.0))
      (:pause  nil)
      (:reset  (if current-clip
                   (update-clip current-clip 0.0))
               (setf state :stop)))))