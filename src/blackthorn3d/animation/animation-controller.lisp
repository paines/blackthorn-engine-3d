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
    :accessor clips
    :initarg :clips)
   (channel-bindings
    :initarg :channel-bindings
    :documentation "to allow re-use of channels for multiple objects
                    we bind them to their targets at the controller 
                    level.  a channel binding is a tuple:
                    (channel target-fn last-frame) where last-frame
                    is an optimization for channel")))

#+disabled
(defun make-anim-controller (clips)
  (make-instance
   'anim-controller
   :current-clip (car clips)
   :state :loop
   :clips clips))

;#+disabled
(defun make-anim-controller (channel-bindings &optional clips)
  (make-instance 'anim-controller
                 :channel-bindings channel-bindings
                 :state :loop
                 :clips 
                 (if clips
                     clips
                     (list 
                      :default 
                      (cons 
                       (get-start (mapcar #'first channel-bindings))
                       (get-end (mapcar #'first channel-bindings)))))))

(defun get-start (channels)
  (iter (for ch in channels)
        (minimizing (time-step (frames ch) 0))))

(defun get-end (channels)
  (iter (for ch in channels)
        (maximizing (slot-value ch 't-max))))

(defun copy-anim-controller (controller)
  (with-slots (channel-bindings clips state elapsed current-clip next-clip)
      controller
    (make-instance 'animation-controller
                   :channel-bindings (copy-list channel-bindings)
                   :clips clips
                   :state state
                   :current-clip current-clip
                   :next-clip next-clip
                   :elapsed elapsed)))

(defmethod add-clip ((this anim-controller) name start end)
  (setf (getf (slot-value this 'clips) :name)
        (cons start end)))

(defmacro start-time (clip)
  `(car ,clip))

(defmacro end-time (clip)
  `(cdr ,clip))

#+disabled
(defmethod apply-transform ((this anim-controller) xform)
  (with-slots (clips) this
    (iter (for clip in clips)
          (iter (for channel in (channel-lst clip))
                (apply-transformation channel xform)))))

(defun set-next-clip (controller clip)
  (with-slots (current-clip next-clip state clips) controller
    (if current-clip
        (setf next-clip clip)
        (setf current-clip clip))))

(defmethod play-clip ((this anim-controller) clip mode)
  (let ((new-clip (getf (clips this) clip)))
    (set-next-clip this new-clip)
    (setf (state this) mode)))


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
  (with-slots (current-clip next-clip elapsed t-start state 
                            channel-bindings clips) this
    (incf elapsed dt)
    (when current-clip
      (when (> elapsed (end-time current-clip))
        (if (eql state :loop) 
            (setf elapsed 0.0)
            (next-clip this))))
    (case state
      ((:run :loop)  
       (if current-clip
           (update-channels channel-bindings elapsed)
           (play-clip this (car clips))))
      (:stop  (setf elapsed 0.0))
      (:pause nil)
      (:reset (setf state :stop)))))

(defun update-channels (channel-bindings time)
  (iter (for binding in channel-bindings)
        (update-binding binding time)))