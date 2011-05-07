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

;;;
;;; Animation control objects
;;;


;; Notes and design considerations::
;;
;; An ANIMATION-CLIP is an abstraction for a collection of channels
;; and the targets they bind to (DC: this may be better placed as
;; part of the channel, will check collada spec for their choice)
;; 

(defclass animation-clip ()
  ((id
    :accessor id
    :initarg :id)
   (channel-lst
    :accessor channel-lst
    :initarg :channel-lst)
   (t-start
    :accessor start-time
    :initarg :t-start)
   (t-end
    :accessor end-time
    :initarg :t-end)
   (ref-time
    :documentation "the time used as a reference to game time 
                    may get abstracted to higher up")))


;; called to update the fields this clip controls.
;; time should be game-time
(defmethod update-clip ((this animation-clip) time)
  (with-slots (channel-lst t-start t-end ref-time) this
    ;(let ((c-time (+ t-start (- time ref-time)))))
    (iter (for ch in channel-lst)
          (with-slots (target) ch
            (funcall target (evaluate-channel ch time))))))


;; every animated model should have one of these...
(defclass anim-controller ()
  ((current-clip
    :accessor current-clip
    :initarg :current-clip)
   (next-clip
    :initform nil)
   (t0
    :accessor t0)
   (state
    :accessor state
    :initarg :state
    :initform :stop)))

(defun set-next-clip (controller clip)
  (with-slots (current-clip next-clip state) controller
    (if current-clip
        (setf next-clip clip)
        (setf current-clip clip))))

(defmethod play-clip ((this anim-controller) clip)
  (set-next-clip this clip)
  (setf (state this) :run))

;; Updates the animation state controlled by this controller
;; there are two states (possibly three later):
;;   :run - animation playes current-clip until it stops
;;          then will run next-clip if there is one
;;   :stop - does nothing
(defmethod update ((this anim-controller) time)
  (with-slots (current-clip next-clip t0 state) this
    (let ((c-time (- time t0)))
      (when (> c-time (end-time current-clip))
        (setf current-clip next-clip)
        (setf next-clip nil)
        (setf t0 time)
        (setf c-time 0.0))
      (case state
        (:run 
         (if current-clip
             (update-clip current-clip c-time)
             (setf state :stop)))
        (:stop nil)))))