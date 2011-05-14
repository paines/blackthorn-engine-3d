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
;;; Keyframing
;;;

(defstruct keyframe
  time
  value
  tan-in
  tan-out
  interp)

(defstruct span
  t0
  ;; this may not be needed, since it's in coefs (d @ index 3)
  value
  coefs
  t1-t0)

;;;
;;; Channels
;;;
;;; Interpolation Modes:
;;;  :none/nil  -> takes the last Ti such that Ti < t0
;;;  :linear    -> linearly interpolates between two frames by
;;;                Ti, Ti+1 with s = (t0 - Ti) / (Ti+1 - Ti)
;;;
;;; Extrapolation Modes:
;;;  :constant  -> before 0 evaluate to Ch(0)
;;;                after Tmax evaluate to Ch(Tmax)
;;;  :repeat    -> before 0 evaluate to Ch(mod t0 Tmax)
;;;             -> after Tmax evaluate to Ch(mod t0 Tmax)

;; Used for setting the previous frame so I don't have to add bookkeeping
;; code everywhere
(defvar *prev-frame* nil)

(defclass channel ()
  ((frames
    :accessor frames
    :initarg :frames)
   (t-max
    :initarg :t-max)
   (extrapolation
    :initarg :extrapolation
    :initform :constant)
   (interpolation
    :initarg :interpolation
    :initform :linear)
   (target
    :initarg :target)
   (eval-fn)))

(defun make-channel (&key times values target normalize)
  (let* ((len (min (length times) (length values)))
         (t-min (aref times 0))
         (t-max (aref times(1- (length times))))
         (t-d (- t-max 0.0))
         (frames 
          (make-array len
                      :initial-contents
                      (iter (for i below len)
                            (collect 
                                (cons
                                 (if normalize 
                                     (- (/ (aref times i) t-d) t-min)
                                     (aref times i))
                                 (aref values i)))))))
    (make-instance 
     'channel
     :frames frames
     :t-max t-d
     :target target)))


(defmacro time-step (frames index)
  `(car (aref ,frames ,index)))
(defmacro value (frames index)
  `(cdr (aref ,frames ,index)))

(defun map-time (t0 t-max extrapolation)
  (cond ((< t0 0.0)
         ;; TODO: min extrapolation
         (case extrapolation
           (:loop (mod t0 t-max))
           (otherwise 0.0)))
        ((> t0 t-max)
         ;; TODO: max extrapolation
         (case extrapolation
           (:loop (mod t0 t-max))
           (otherwise t-max)))
        (t t0)))

(defmethod evaluate-channel ((this channel) t0)
  (with-slots (frames t-max extrapolation interpolation) this
    (let* ((n-frames (length frames))
           (m-time (map-time t0 t-max extrapolation))
           (i
            (iter (for i from (or *prev-frame* 0) below n-frames)
                  (finding i #+disabled(value frames i) 
                           such-that (>= (time-step frames i) m-time)))))
      (case interpolation
        (:linear (linear-interpolate 
                  frames i 
                  (case extrapolation
                    (:loop (mod (1+ i) (length frames)))
                    (otherwise (clamp (1+ i) 0 (1- (length frames)))))
                  m-time))
        (otherwise (value frames i))))))

(defun linear-interpolate (frames i0 i1 t0)
  (let* ((v1 (value frames i0))
         (v2 (value frames i1))
         (denom (- (time-step frames i1) (time-step frames i0)))
         (s (if (= 0.0 denom)
                0.0 
                (/ (- t0 (time-step frames i0))
                   denom))))
   ; (format t "v1:  ~a~%v2:   ~a~%" v1 v2)
    (if (arrayp v1)
        (iter (with len = (length v1))
              (with arr = (make-array len))
              (for i below len)
              (setf (svref arr i)
                    (lerp s (svref v1 i) (svref v2 i)))
              (finally (return arr)))
        (lerp s v1 v2))))




;;;
;;; Channel Bindings
;;;

;; channel bindings are tuples of form (channel target-fn prev-frame)

(defun make-binding (channel target-fn &optional last-frame)
  (list channel target-fn last-frame))

(defun update-binding (binding t0)
  (destructuring-bind (channel fn prev-frame) binding
    (let ((*prev-frame* prev-frame))
      (funcall fn (evaluate-channel channel t0))
      ;; save the *prev-frame* (updated, ostensibly, by evaluate-channel)
      ;; into the binding
      (setf (third binding) *prev-frame*))))


#+disabled
(defun evalute-channel (channel t0)
  (funcall (eval-fn channel) t0))



;; takes 2 keyframes and spits out a span
#+disable
(defun bake-keyframe (key1 key2)
  (let ((dt (- (keyframe-time key2) (keyframe-time key1))))
    (make-span
     :t0 (keyframe-time key1)
     :t1-t0 dt
     :value (keyframe-value key1)
     :coefs (calc-1d-hermite-coefs (keyframe-value key1)
                                   (keyframe-value key2)
                                   (keyframe-tan-out key1)
                                   (keyframe-tan-in key2)
                                   dt))))

;; array of keyframes -> array of spans
#+disable
(defun bake-keyframes (key-arr &key 
                       (tmin-extrap :const)
                       (tmax-extrap :const))
  (let ((n-keys (length key-arr))
        (spans (make-array (1- n-keys))))
    (iter (for k below (1- n-keys))
          (setf (aref spans k)
                (bake-keyframe (aref key-arr k)
                               (aref key-arr (1+ k)))))
    ;; TODO- something about the extrapolation so we can has loop
    ))

#+disable
(defun evaluate-channel (c time)
  ;; For now we can do with sequential access
  ;; eventually we will want to cache most recent
  ;; span in the animating object
  (let ((span (iter (for span in-vector (channel-data c))
                    (finding span 
                             such-that (< time (+ (span-t0 span) 
                                                  (span-t1-t0 span)))))))
    ;; TODO- Handle two cases where time is outside the range of the
    ;; spans (ie, extrapolation cases
    (eval-1d-curve (span-coefs span) time)))
