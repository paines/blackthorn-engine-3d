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

(in-package :blackthorn3d-graphics)

;;;
;;; And so pandora's box is opened
;;;

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

(defstruct channel
  data
  min-extrap-mode min-extrap-data
  max-extrap-mode max-extrap-data)

;; takes 2 keyframes and spits out a span
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

;;;
;;; Skeletons
;;;

;; A joint: (world-mat bind-mat <...> (child-joint-1 child-joint-2 ... ))
;; world-mat -> the most recently calculated world-mat for this joint
;; bind-mat -> static matrix that describes the transform from this 

(defun joint-update-r )

(defun joint-update (skeleton)
  (labels ((j-u-r (joint parent-matrix)
             (destructuring-bind (world-mat bind-mat children) joint
               (setf (car joint) (matrix-multipy-m parent-matrix bind-mat))
               (dolist (child children)
                 (j-u-r child (car joint))))))
    (j-u-r skeleton (make-identity-matrix))))

;;;
;;; Skinning
;;;