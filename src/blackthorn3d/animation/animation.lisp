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

(defclass channel-instance ()
  ((channel-ref)
   (prev-frame)
   (modify-fn)))

(defclass animation-clip ()
  ((channel-lst
    :accessor channel-lst
    :initarg :channel-lst)
   (ref-time
    :documentation "the time to use as a reference for getting channel
                    data.  channel-time = time - ref-time.
                    This may be abstracted up to a higher level object")))

(defun make-animation-clip (&key channels))

;; called to update the fields this clip controls.
;; time should be game-time
(defmethod update-clip ((this animation-clip) time)
  (let ((dt (- time ref-time)))
    (iter (for ch in channel-lst)
          (destructuring-bind (modify-fn channel prev-frame) ch
            (funcall modify-fn (evaluate-channel channel dt prev-frame))))))



