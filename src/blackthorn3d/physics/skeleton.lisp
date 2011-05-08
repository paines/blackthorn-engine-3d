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

(in-package :blackthorn3d-physics)

;;;
;;; Joints
;;;

(defclass joint ()
  ((model-matrix
    :accessor joint-model-mat
    :initform nil
    :documentation "Place to store the last computed local->model space mat")
   (inverse-bind-matrix
    :accessor joint-ibm
    :initarg :inverse-bind-matrix
    :documentation "matrix to bring points to joint space")
   (joint-matrix
    :accessor joint-mat
    :initarg :joint-matrix
    :documentation "The matrix that moves the joint, and what gets animated")
   (child-joints
    :accessor child-joints
    :initarg :child-joints)))

(defun make-joint (inverse-bind-matrix 
                   &key (joint-matrix (make-identity-matrix))
                        child-joints)
  (make-instance 'joint 
                 :inverse-bind-matrix inverse-bind-matrix
                 :joint-matrix joint-matrix
                 :child-joints child-joints))

(defun calc-joint-matrix (joint parent-matrix)
  "Return the local-to-model matrix for this joint"
  (matrix-multiply-m (joint-ibm joint) 
                     (matrix-multiply-m parent-matrix
                                        (joint-mat joint))))