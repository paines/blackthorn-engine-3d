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