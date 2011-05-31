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

;; The joint matrix will be stored in the transform field of node
(defclass joint (node)
  ((model-matrix
    :accessor joint-model-mat
    :initform nil
    :documentation "Place to store the last computed local->model space mat")
   (inverse-bind-matrix
    :accessor joint-ibm
    :initarg :inverse-bind-matrix
    :documentation "matrix to bring points to joint space")))

(defun joint-matrix (joint) (slot-value joint 'transform))
(defun (setf joint-matrix) (mat joint)
  (setf (slot-value joint 'transform) mat))

(defun child-joints (joint) (slot-value joint 'child-nodes))
(defun (setf child-joints) (children joint) 
  (setf (slot-value joint 'child-nodes) children))

(defun joint-id (joint) (id joint))
(defun make-joint (id inverse-bind-matrix 
                   &key (joint-matrix (make-identity-matrix))
                        child-joints)
  (make-instance 'joint 
                 :id id
                 :inverse-bind-matrix inverse-bind-matrix
                 :transform joint-matrix
                 :bounding-volume (make-instance 'bounding-sphere
                                                 :rad 0.33
                                                 :pos +origin+)
                 :child-nodes child-joints))

(defun calc-joint-matrix (joint parent-matrix)
  "Return the local-to-model matrix for this joint"
 ; (format t "joint ~a's local matrix: ~a~%" (id joint) (joint-matrix joint))
 ; (format t "~8TIBM: ~a~%~%" (joint-ibm joint))
  
  ;(make-identity-matrix)

  ;#+disabled
  (matrix-multiply-m 
   parent-matrix ;(joint-matrix joint)
   ;#+disabled
   (matrix-multiply-m (joint-matrix joint)
                      (joint-ibm joint))))

(defun update-joint-matrices (root)
  "Updates the cached joint->model space matrices for each joint
  recursively starting at ROOT"
  (labels ((%update-r (joint parent-m)
             (when (eql (find-class 'blt3d-phy:joint) (class-of joint))
               (setf (joint-model-mat joint) (calc-joint-matrix joint parent-m))
               (iter (with joint-world = 
                           (matrix-multiply-m parent-m (joint-matrix joint)))
                     (for child-j in (child-joints joint))
                     (%update-r child-j joint-world)))))
    (%update-r root (make-identity-matrix))))


;;;
;;; Skeletons
;;;

(defclass skeleton ()
  ((root-joint
    :accessor root-joint
    :initarg :root-joint)
   (joint-array
    :accessor joint-array
    :initarg :joint-array
    :documentation "Since vertices need to index the joints in fixed
                    order, we store them here. Root informs us which
                    is the first, and each one has a list of children")))

(defun make-skeleton (root-joint joint-arr)
  (make-instance 'skeleton :root-joint root-joint :joint-array joint-arr))

(defmethod get-joint-matrices ((this skeleton))
  "returns an array of matricies"
  (iter (for joint in-vector (joint-array this))
       ; (format t "joint ~a's ibm: ~a~%" (id joint) (joint-ibm joint))
       ; (format t "~5Tlocal: ~a~%" (joint-matrix joint))
        (collect (joint-model-mat joint) result-type 'vector)
      ;  (collect (make-identity-matrix) result-type 'vector)
        ))

(defmethod update-skeleton ((this skeleton))
  "updates the positions of all the joints"
  (update-joint-matrices (root-joint this)))