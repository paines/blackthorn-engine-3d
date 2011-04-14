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

;;;;
;;;; Yay cameras!
;;;;

(defclass camera ()
  ((pos :accessor cam-pos
        :initarg :position
        :initform (make-point3 0.0 0.0 0.0)
        :documentation "the current world location of the camera")
   (dir :accessor cam-dir
        :initarg :direction
        :initform (make-vec3 0.0 0.0 -1.0)
        :documentation "the current direction the camera is pointing. Used in 1st person mode")
   (up  :accessor cam-up
        :initarg :up
        :initform (make-vec3 0.0 1.0 0.0)
        :documentation "the current up direction of the camera")
   (target :accessor cam-target
           :initarg :target
           :initform (make-point3 0.0 0.0 0.0)
           :documentation "the desired target of the camera. Used in 3rd person mode")
   (mode :accessor cam-mode
         :initarg :mode
         :initform :first-person)
   (matrix :accessor cam-matrix)))


(defmethod update-fp-camera ((c camera)))

(defmethod update-tp-camera ((c camera)))

;; for now, we'll update the camera each time this method is called.
;; the ideal situation would be for the camera to only re-calculate 
;; the matrix when it changes
(defmethod update-camera ((c camera))
  (with-slots (mode) c
    (case mode
        (:first-person update-fp-camera c)
        (:third-person update-tp-camera c))))

(defmethod camera-matrix ((c camera))
  "@return{A 4x4 matrix representing a camera's location and direction}"
  (with-slots (pos dir up) c
    (let* ((cam-matrix (make-matrix4x4))
           (z (norm4 (vec-neg4 dir)))
           (x (norm4 (cross up z)))
           (y (cross z x)))
      (setf (col cam-matrix 0) x)
      (setf (col cam-matrix 1) y)
      (setf (col cam-matrix 2) z)
      (setf (col cam-matrix 3) pos)
      cam-matrix)))

(defmethod camera-inverse ((c camera))
  (with-slots (pos dir up) c
    (let* ((z (norm4 (vec-neg4 dir)))
           (x (norm4 (cross up z)))
           (y (cross z x))
           (cam-inv (make-ortho-basis x y z)))
      (setf (col cam-inv 3) (matrix-multiply-v cam-inv (vec-neg4 pos)))
      cam-inv)))

(defmethod camera-move! ((c camera) vec4)
  "translates the camera by vec3. This is sort of temporary, as once we
   have a scene graph the camera will have nodes and ways of updating those nodes
   or letting the game do it for it. (ie: the position is mounted on the player"
  (setf (cam-pos c) (vec4+ (cam-pos c) vec4)))

(defmethod camera-rotate! ((c camera) yaw pitch)
  "Rotates the view of the camera. Intended for first-person cameras.
   @arg[yaw]{the rotation amount in radians around the y axis
   @arg[pitch]{the rotation in radians around the x axis"
  (let* ((y-quat (axis-rad->quat (make-vec3 0.0 1.0 0.0) yaw))
         (p-quat (axis-rad->quat (make-vec3 1.0 0.0 0.0) pitch))
         (combined-quat (quat* y-quat p-quat)))
    (setf (cam-dir c) (quat-rotate-vec combined-quat (cam-dir c)))
    (setf (cam-up c) (quat-rotate-vec combined-quat (cam-up c)))))

(defmethod camera-lookat! ((c camera) point)
  "Points a camera to look at a point, takes current location into account.
   Does not slerp, intended for 3rd person cameras"
  (let* ((new-dir (norm4 (vec4- point (cam-dir c))))
         (quat (quat-rotate-to-vec (cam-dir c) new-dir)))
    (setf (cam-dir c) new-dir)
    (setf (cam-up c) (quat-rotate-vec quat (cam-up c)))))