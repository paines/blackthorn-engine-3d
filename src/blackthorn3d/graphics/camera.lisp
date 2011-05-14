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
;;; Yay cameras!
;;;

(defclass camera (entity-server)
  ((ideal-coord
    :accessor ideal-coord
    :initarg :ideal-coord
    :initform (list 0.0 (cos (/ pi 6.0)) 5.0 )
    :documentation "The ideal spherical coordinates of the camera relative to 
                    the target.  tuple is (phi theta dist)")
   (veloc
    :initform (make-vec3 0.0 0.0 0.0))
   (spring-k
    :accessor spring-k
    :initarg :spring-k
    :initform 10.0)
   (target
    :accessor target
    :initarg :target
    :documentation "the desired target(entity) of the camera. 
                    Used in 3rd person mode")
   (mode
    :accessor mode
    :initarg :mode
    :initform :first-person)
   (matrix 
    :accessor matrix)))

(defmethod update-fp-camera ((c camera) time input-vec)
  (setf (matrix c) (camera-inverse c)))

(defvar +phi-scale+ nil)
(defvar +theta-scale nil)
(setf +phi-scale+ -0.2)
(setf +theta-scale+ -0.2)

(defmethod update-tp-camera ((c camera) time input-vec)
  (with-slots (ideal-coord target pos veloc up dir spring-k) c
    (with-slots ((t-pos pos)
                 (t-dir dir)
                 (t-up up)) target
      ;; Update the ideal azimuth (phi) based on the camera's
      ;; position relative to the target
      ;; note2: this allows the camera to lazily rotate around
      ;;   the character, like in many platform games, as opposed
      ;;   to strafing with the character, as in many shooter games

      ;; look-at is an offset from the entity....hopefully we replace
      ;; this eventually
      (let ((look-at (vec4+ t-pos (make-vec3 0.0 3.0 0.0))))
        ;; set phi
        ;; TODO:- make this suck less
        ;; to make it suck less, use a quaternion?
        ;; the axes to rotate around are
        ;;    phi: (up target)
        ;;    theta: (cross (dir target) (up target))

        ;#+disabled
        (setf (elt ideal-coord 0)
              (aif (/= 0 (x input-vec))
                   (+ (elt ideal-coord 0) (* +phi-scale+ (x input-vec)))
                   (+ (atan (- (x pos) (x look-at))
                            (- (z pos) (z look-at))))))
        ;; set theta
        #+disabled
        (let* ((theta (* +theta-scale+ (y input-vec)))
               (phi  (* +phi-scale+ (x input-vec)))
               (disp-vec (vec-scale4 (norm4 (vec4- pos t-pos))
                                     (elt ideal-coord 2)))
               (cam-quat (quat* (axis-rad->quat t-up phi)
                                (axis-rad->quat (norm4 (cross t-dir t-up)) 
                                                (- theta)))))
          (setf (pos c) (vec4+ t-pos (quat-rotate-vec cam-quat disp-vec))
                (up c)  (quat-rotate-vec cam-quat up)
                (dir c) (quat-rotate-vec cam-quat dir)))
        
        ;#+disabled
        (aif (/= 0 (y input-vec))
             (setf (elt ideal-coord 1)
                   (+ (elt ideal-coord 1) (* +theta-scale+ (y input-vec)))))
        
        ;; calculate the camera's movement
       ; #+disabled
        (let* ((ideal-pos (vec4+ look-at (spherical->cartesian ideal-coord)))
               (displace-vec (vec4- pos ideal-pos))
               (spring-accel (vec4-
                              (vec-scale4 displace-vec (- spring-k))
                              (vec-scale4 veloc (* 2.0 (sqrt spring-k))))))
          (setf veloc (vec4+ veloc (vec-scale4 spring-accel time)))
         ; (setf (up c) )
          (setf (pos c) ideal-pos #+disabled(vec4+ pos (vec-scale4 veloc time)))
          (setf (dir c) (norm4 (vec4- look-at pos))))))))

;; for now, we'll update the camera each time this method is called.
;; the ideal situation would be for the camera to only re-calculate 
;; the matrix when it changes
(defmethod update-camera ((c camera) time input-vec)
  (with-slots (mode) c
    (case mode
      (:first-person (update-fp-camera c time input-vec))
      (:third-person (update-tp-camera c time input-vec)))))

(defmethod camera-matrix ((c camera))
  "@return{A 4x4 matrix representing a camera's location and direction}"
  (with-slots (pos dir up) c
    (let* ((z (norm4 (vec-neg4 dir)))
           (x (norm4 (cross up z)))
           (y (cross z x))
           (cam-matrix (make-matrix4x4)))
      (setf (col cam-matrix 0) x)
      (setf (col cam-matrix 1) y)
      (setf (col cam-matrix 2) z)
      (setf (col cam-matrix 3) (make-point3 0.0 0.0 0.0))
      (setf (col cam-matrix 3) (matrix-multiply-v cam-matrix pos))
      cam-matrix)))

(defmethod camera-inverse ((c camera))
  (with-slots (pos dir up) c
    (let* ((z (norm4 (vec-neg4 dir)))
           (x (norm4 (cross up z)))
           (y (cross z x))
           (cam-inv (make-ortho-basis x y z)))
      (setf (col cam-inv 3) (matrix-multiply-v cam-inv (vec-neg4 pos)))
      cam-inv)))

(defmethod generate-move-vector ((c camera) input-vec)
  (with-slots ((c-up up) (c-dir dir) target) c
    (with-slots ((t-up up) (t-dir dir)) target
      (let* ((x-axis (norm4 (cross c-dir t-up)))
             (y-axis (cross t-up x-axis)))
        (vec4+ (vec-scale4 x-axis (x input-vec))
               (vec-scale4 y-axis (y input-vec)))))))

(defmethod move-player ((c camera) input-vec)
  (with-slots (target (c-up up) (x2 dir) (c-pos pos)) c
    (with-slots ((t-up up) (x1 dir) (t-pos pos) client) target
      (let* ((z1 (cross x1 t-up))
             (z2 (cross x2 c-up))
             (move-vec (generate-move-vector c input-vec)))
        (setf (pos target)
              (vec4+ t-pos move-vec))
        (when (or (/= 0.0 (x input-vec)) (/= 0.0 (y input-vec)))
          (setf (dir target) (norm4 move-vec)))))))

(defmethod camera-move! ((c camera) vec4)
  "translates the camera by vec3. This is sort of temporary, as once we
   have a scene graph the camera will have nodes and ways of updating those nodes
   or letting the game do it for it. (ie: the position is mounted on the player"
  (setf (pos c) (vec4+ (pos c) vec4)))

(defmethod camera-rotate! ((c camera) yaw pitch)
  "Rotates the view of the camera. Intended for first-person cameras.
   @arg[yaw]{the rotation amount in radians around the y axis
   @arg[pitch]{the rotation in radians around the x axis"
  (let* ((y-quat (axis-rad->quat (make-vec3 0.0 1.0 0.0) yaw))
         (p-quat (axis-rad->quat (make-vec3 1.0 0.0 0.0) pitch))
         (combined-quat (quat* y-quat p-quat)))
    (setf (dir c) (quat-rotate-vec combined-quat (dir c)))
    (setf (up c) (quat-rotate-vec combined-quat (up c)))))

(defmethod camera-lookat! ((c camera) point)
  "Points a camera to look at a point, takes current location into account.
   Does not slerp, intended for 3rd person cameras"
  (let* ((new-dir (norm4 (vec4- point (dir c))))
         (quat (quat-rotate-to-vec (dir c) new-dir)))
    (setf (dir c) new-dir)
    (setf (up c) (quat-rotate-vec quat (up c)))))

(defmethod camera-orbit! ((c camera) phi theta dist)
  "Orbits the camera around its target by phi (horizontal axis) and theta 
   (vertical axis) at a radius dist"
  (with-slots (dir up pos target) c
    (when (< (abs (dot up dir)) 0.8)
      (let* ((x-axis (norm4 (cross dir up)))
             (y-axis up)
             (quat (quat* (axis-rad->quat x-axis theta)
                          (axis-rad->quat y-axis phi))))
        (setf dir (quat-rotate-vec quat dir))
        (setf pos (vec4+ (pos target) (vec-neg4 (vec-scale4 dir dist))))))))