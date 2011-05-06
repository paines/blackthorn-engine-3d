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

;; Channels
(defstruct span
  t0
  ;; this may not be needed, since it's in coefs (d @ index 3)
  value
  coefs
  t1-t0)

;; Used for setting the previous frame so I don't have to add bookkeeping
;; code everywhere
(defvar *prev-frame* nil)

(defclass channel ()
  ((frames
    :accessor frames
    :initarg :frames)
   (dt
    :initarg :dt)
   (extrapolation
    :initarg :extrapolation
    :initform :loop)
   (interpolation
    :initarg :interpolation
    :initform :linear)
   (target
    :initform :target)))

(defun make-channel (&key times values target normalize)
  (let* ((len (min (length times) (length values)))
         (t-min (aref times 0))
         (t-max (aref times(1- (length time))))
         (t-d (- t-max t-min))
         (frames (make-array len
                             :initial-contents
                             (iter (for i below len)
                                   (collect 
                                    (cons
                                     (if normalize 
                                       (- (/ (aref times i) t-d) t-min)
                                       (aref times i))
                                     (aref values i))))))))
  (make-instance 
   'channel
   :frames frames
   :dt dt
   :target target))

(defmacro time-step (frames index)
  `(car (aref ,frames ,index)))
(defmacro value (frames index)
  `(cdr (aref ,frames ,index)))

(defmethod map-time ((this channel) time)
  (with-slots (frames dt extrapolation) this
    (cond ((< time 0.0)
           ;; TODO: max extrapolation
           )
          ((> time dt)
           ;; TODO: min extrapolation
           )
          (t time))))

(defmethod evaluate-channel ((this channel) time)
  (with-slots (frames t-start t-end extrapolation) this
    (let ((n-frames (length frames))
          (m-time (map-time this time)))
      (iter (for i below n-frames)
            (finding (value frames i) 
                     such-that (> (time-step frames i) time))))))



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
