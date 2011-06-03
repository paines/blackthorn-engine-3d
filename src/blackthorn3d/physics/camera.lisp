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
;;; Yay cameras!
;;;

(defclass camera (entity-server)
  ((ideal-coord
    :accessor ideal-coord
    :initarg :ideal-coord
    :initform (list 0.0 (cos (/ pi 6.0)) 5.0 )
    :documentation "The ideal spherical coordinates of the camera relative to 
                    the target.  tuple is (phi theta dist)")
   (ideal-coord2
    :accessor ideal-coord2
    :initarg :ideal-coord2
    :initform (list 0.0 (cos (/ pi 6.0)) 2.5))
   (veloc
    :accessor veloc
    :initarg :veloc
    :initform +zero-vec+)
   (spring-k
    :accessor spring-k
    :initarg :spring-k
    :initform 20.0)
   (spring-k2
    :accessor spring-k2
    :initarg :spring-k2
    :initform 95.0)
   (target
    :accessor target
    :initarg :target
    :documentation "the desired target(entity) of the camera. 
                    Used in 3rd person mode")
   (mode
    :accessor mode
    :initarg :mode
    :initform :first-person)
   (minor-mode
    :accessor minor-mode
    :initarg :minor-mode
    :initform :free)
   (matrix 
    :accessor matrix)))

(defmethod update-fp-camera ((c camera) time input-vec)
  (setf (matrix c) (camera-inverse c)))

(defvar +phi-scale+ nil)
(defvar +theta-scale nil)
(defvar +thresh+ 0.0001)
(defvar +pos-theta-limit+ (* 50 (/ pi 180)))
(defvar +neg-theta-limit+ (* -30 (/ pi 180)))
(setf +phi-scale+ -0.2)
(setf +theta-scale+ -0.2)

(defmethod move-camera ((c camera) vec)
  ;#+disabled
  (with-slots (target pos velocity minor-mode) c
    (with-slots ((t-pos pos) (t-up up)) target
      (let ((look-at (vec4+ t-pos (vec-scale4 t-up 0.42))))
        #+disabled
        (setf (pos c) (vec4+ pos vec)
              (dir c) (norm4 (vec4- look-at pos))
              (up c) t-up)
        #+disabled
        (when (eql minor-mode :strafe)
          (setf (dir target) (norm4 (cross t-up (cross (dir c) t-up)))))))))

(defmethod update-tp-camera ((c camera) time input-vec)
  (with-slots (ideal-coord 
               ideal-coord2 target 
               pos veloc up dir 
               spring-k spring-k2
               minor-mode) c

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
      (let* ((look-at (vec4+ t-pos (vec-scale4 t-up 0.42)))
             (t-right (cross t-dir t-up))
             (c-right (norm4 (cross dir up)))
             (up-quat (quat-rotate-to-vec +y-axis+ t-up))

             (basis (make-ortho-basis t-right t-up (vec-neg4 t-dir)))
             (inv-basis (transpose basis))
             
             (wc-pos (vec4- pos look-at))
             (tc-pos (matrix-multiply-v basis wc-pos))

            (sphere-coord (cartesian->spherical tc-pos))
        ;     ideal-pos c-look

             (ks (ecase minor-mode
                   (:free spring-k)
                   (:strafe spring-k2))))

        ;#+disabled
        (progn
          (setf (elt sphere-coord 2) (elt ideal-coord 2))
          
          #+disabled
          (incf (elt sphere-coord 0) 
                (* +phi-scale+ (x input-vec)))
          (setf (elt sphere-coord 0) 0.0)
          (setf (elt sphere-coord 1) 
                (clamp (+ (elt sphere-coord 1)
                          (* +theta-scale+ (y input-vec)))
                       +neg-theta-limit+
                       +pos-theta-limit+))

          ;; Step1: find the ideal pos for the camera
          (setf ideal-pos
                (vec4+ look-at (matrix-multiply-v 
                                inv-basis
                                (spherical->cartesian sphere-coord))))

          ;; Step2: find the look at point
          (setf c-look 
                (quat-rotate-vec
                 (axis-rad->quat c-right (* +theta-scale+ (y input-vec)))
                 dir))

          ;; Step3: find the up vector
                                        ;#+disabled
          (setf (up c) (norm4 (cross c-right c-look)))
          
                                        ;#+disabled
          (setf (dir c) c-look)
          (setf (pos c) ideal-pos)

          (setf (dir target) (quat-rotate-vec 
                              (axis-rad->quat 
                               t-up 
                               (* +phi-scale+ (x input-vec)))
                              (dir target))))




        ;; hackity hacketter hack
        #+disabled
        (let ((theta-quat (axis-rad->quat 
                           c-right (* +theta-scale+ (y input-vec))))
              (phi-quat (axis-rad->quat
                         t-up (* +phi-scale+ (x input-vec)))))

          (unless (is-jumping target)
            (setf (new-up c) (norm4 (cross (cross dir t-up) dir))))

          (setf (up c) (quat-rotate-vec theta-quat up))
          (setf (dir target)
                (quat-rotate-vec phi-quat t-dir))
          (setf (dir c) (norm4 (cross  (cross t-dir up))))

          ;#+disabled
          (setf (pos c) (vec4+ look-at (vec-scale4 
                                        dir 
                                        (- (elt ideal-coord 2)))))

       
         
          #+disabled
          (setf (velocity c) (vec-scale4 
                              (norm4 (cross (up c) (cross t-dir (up c))))
                              (- (elt ideal-coord 2))))
          #+disabled
          (setf (pos c) look-at)
 
          ;(setf (dir c) (quat-rotate-vec dir))
          #+disabled
          (setf (pos c) (vec4+ look-at 
                               (vec-scale4 dir (- (elt ideal-coord 2))))))
        
        #+disabled
         (setf (dir target)
                (quat-rotate-vec
                 (axis-rad->quat t-up (* +phi-scale+ (x input-vec)))
                 (dir t-pos)))))))

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
  (with-slots ((c-up up) (c-dir dir) target minor-mode) c
    (with-slots ((t-up up) (t-dir dir)) target
      (let (x-axis y-axis)
        (ecase minor-mode
          (:free
           (setf x-axis (norm4 (cross c-dir t-up))
                 y-axis (cross t-up x-axis)))
          (:strafe
           (setf x-axis (cross t-dir t-up)
                 y-axis t-dir)))

        (vec4+ (vec-scale4 x-axis (x input-vec))
               (vec-scale4 y-axis (y input-vec)))))))

(defmethod move-player ((c camera) input-vec)
  (with-slots (target (c-up up) (x2 dir) (c-pos pos)) c
    (with-slots ((t-up up) (x1 dir) (t-pos pos) client) target
      (generate-move-vector c input-vec))))

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