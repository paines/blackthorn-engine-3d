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

;; This var is for applying external forces like gravity/wind
(defvar *external-force* +zero-vec+)

;;;
;;; particles
;;;


;; define a particle as: 
;;        #(energy fade-rate pos.x pos.y pos.z old.x old.y old.z
;;          vel.x vel.y vel.z color.r color.g color.b color.a size)
;; total length: 16 floats
(defun create-particle (particles index
                        position velocity
                        &key 
                        (energy 1.0)
                        (fade-rate 0.25)
                        (color +white+)
                        (size 0.1))
  (setf (col particles index) 
        (vector energy                  ; 0
                fade-rate               ; 1
                (x position)            ; 2
                (y position)            ; 3
                (z position)            ; 4
                (x position)            ; 5
                (y position)            ; 6
                (z position)            ; 7
                (x velocity)            ; 8
                (y velocity)            ; 9
                (z velocity)            ; 10
                (r color)               ; 11
                (g color)               ; 12
                (b color)               ; 13
                (a color)               ; 14
                size)))                 ; 15

(defun create-particle-array (max-particles)
  (make-array (list max-particles 16) :element-type 'float))

;; ENERGY
(defun p-energy (particle)
  (aref (car particle) (cdr particle) 0))
(defun (setf p-energy) (new-energy particle)
  (setf (aref (car particle) (cdr particle) 0) new-energy))

;; FADE
(defun p-fade (particle)
  (aref (car particle) (cdr particle) 1))
(defun (setf p-fade) (new-fade particle)
  (setf (aref (car particle) (cdr particle) 1) new-fade))

;; POSITION
(defun p-pos (particle)
 
  (make-point3 (aref (car particle) (cdr particle) 2)
               (aref (car particle) (cdr particle) 3)
               (aref (car particle) (cdr particle) 4)))
(defun (setf p-pos) (pos particle)
  (setf (aref (car particle) (cdr particle) 2) (x pos)
        (aref (car particle) (cdr particle) 3) (y pos)
        (aref (car particle) (cdr particle) 4) (z pos)))

;; OLD POSITION
(defun p-old-pos (particle)
  (make-point3 (aref (car particle) (cdr particle) 5)
               (aref (car particle) (cdr particle) 6)
               (aref (car particle) (cdr particle) 7)))
(defun (setf p-old-pos) (pos particle)
  (setf (aref (car particle) (cdr particle) 5) (x pos)
        (aref (car particle) (cdr particle) 6) (y pos)
        (aref (car particle) (cdr particle) 7) (z pos)))

;; VELOCITY
(defun p-vel (particle)
  (make-vec3 (aref (car particle) (cdr particle) 8)
             (aref (car particle) (cdr particle) 9)
             (aref (car particle) (cdr particle) 10)))
(defun (setf p-vel) (vel particle)
  (setf (aref (car particle) (cdr particle) 8) (x vel)
        (aref (car particle) (cdr particle) 9) (y vel)
        (aref (car particle) (cdr particle) 10) (z vel)))

;; COLOR
(defun p-color (particle)
  (make-vector4 (aref (car particle) (cdr particle) 11)
                (aref (car particle) (cdr particle) 12)
                (aref (car particle) (cdr particle) 13)
                (aref (car particle) (cdr particle) 14)))
(defun (setf p-color) (color particle)
  (setf (aref (car particle) (cdr particle) 11) (r color)
        (aref (car particle) (cdr particle) 12) (g color)
        (aref (car particle) (cdr particle) 13) (b color)
        (aref (car particle) (cdr particle) 14) (a color)))

;; SIZE
(defun p-size (particle)
  (aref (car particle) (cdr particle) 15))
(defun (setf p-size) (new-size particle)
  (setf (aref (car particle) (cdr particle) 15) new-size))

;;;
;;; Particle use functions
;;;

(defun is-alive (particle)
  (> (p-energy particle) 0.0))

(defun update-particle (particle dt)
  (setf (p-energy particle) 
        (- (p-energy particle) (* (p-fade particle) dt)))
  (when (is-alive particle)
    (setf (p-old-pos particle) (p-pos particle)
          (p-pos particle) 
          (vec3+ (p-pos particle)
                 (vec-scale3 (p-vel particle) dt))
          (p-vel particle) (vec3+ (p-vel particle) 
                                  (vec-scale3 *external-force* dt)))))


(defclass point-emitter ()
  ((pos
    :initarg :pos)
   (dir
    :initarg :dir)
   (up
    :initarg :up)
   (speed
    :initarg :speed)
   (speed-fuzzy
    :initarg :speed-fuzzy)
   (angle
    :initarg :angle
    :documentation "The angle phi around the direction that particles 
                    can be emitted in. a value of pi is a hemisphere
                    2*pi would be a sphere")))

(defclass particle-system ()
  ((emitter
    :accessor emitter
    :initarg :emitter)
   (spawn-rate
    :accessor spawn-rate
    :initarg :spawn-rate
    :documentation "The rate at which particles are to be emitted.
                    If set to 0, system will max-particles once and then
                    finish")
   (shader
    :accessor shader
    :initarg :shader
    :initform 0)
   (texture
    :accessor texture
    :initarg :texture)
   (blend-mode
    :accessor blend-mode
    :initarg :blend-mode
    :initform '(:src-alpha :one-minus-source-alpha))
   (type
    :accessor ps-type
    :initarg :type
    :initform :default)
   (particles
    :accessor particles
    :initarg :particles
    :documentation "the array of particles belonging to the system")
   (max-particles
    :initarg :max-particles)
   (num-alive
    :accessor num-alive
    :initform 0)))


;;;
;;; Emitters
;;;


(defmethod gen-initial-pos ((this point-emitter) time)
  (slot-value this 'pos))

(defmethod gen-initial-vel ((this point-emitter) time)
  (with-slots (dir angle speed speed-fuzzy) this
    ;; Generate random coordinates
    (let* ((angle/2 (* 0.5 angle))
           (phi (random (* 2.0 pi)))
           (theta (random (float angle/2)))
           (u dir)
           (v (get-perpendicular dir))
           (w (cross3 u v))
           (z (cos theta))
           (x (* (sin theta) (cos phi)))
           (y (* (sin theta) (sin phi))))
      (vec-scale3 (vec3+ (vec3+ (vec-scale3 u z)
                                (vec-scale3 v y))
                         (vec-scale3 w x))
                  (+ speed (- 1.0 (random (* (float speed-fuzzy))))))
      #+disabled
      (vec-scale3 dir
                  (+ speed (- 1.0 (random (* (float speed-fuzzy)))))))))



;;;
;;; Particle-System Methods and Functions
;;;


(defun create-particle-system (emitter spawn-rate max-particles)
  (make-instance 'particle-system
                 :emitter emitter
                 :spawn-rate spawn-rate
                 :max-particles max-particles
                 :particles (create-particle-array max-particles)))

(let ((spawn-num 1))
  (defmethod update-ps ((this particle-system) dt)
    ;; Loop over each particle and update it's position
    (let ((*external-force* (vec-scale4 +y-axis+ -1.0)))
      (with-slots (particles spawn-rate max-particles num-alive emitter) this   
        (incf spawn-num (* dt spawn-rate))
        (iter (with emitted = 0)
              (with max-emit = 
                    (if (zerop spawn-rate)
                        max-particles
                        (floor spawn-num)))
              (with alive-cnt = 0)
              (for index below max-particles)
              (for particle = (cons particles index))
              (while (or (< emitted max-emit)
                         (< alive-cnt num-alive)))
              (if (is-alive particle)
                  (progn 
                    (update-particle particle dt)
                    (incf alive-cnt))
                  ;; If dead check if we should revive it
                  (when (< emitted max-emit)
                    (format t "emitting particle~%")
                    (create-particle particles index
                                     (gen-initial-pos emitter dt)
                                     (gen-initial-vel emitter dt))
                    (incf emitted)))

              ;; Finally, update the number of alive particles
              (finally
               (format t "max emit: ~a~%" max-emit)
               (decf spawn-num emitted)
               (setf num-alive (+ alive-cnt emitted))))))))

;; TODO: add textures/quads, not just points
(defmethod render-ps ((this particle-system))
  (with-slots (particles max-particles num-alive) this
    (gl:with-primitives :points
      (format t "Rendering ~a particles ~%" num-alive)
      (iter (for index below max-particles)
            (for particle = (cons particles index))
            (count (is-alive particle) into alive-cnt)
            (while (< alive-cnt num-alive))

            ;; do particle rendering here
            (when (is-alive particle)
              (let ((position (p-pos particle))
                    (color (p-color particle)))
                (gl:color (r color) (g color) (b color))
                (gl:vertex (x position) (y position) (z position))))))))



(defclass particle-manager ()
  ((systems-list
    :accessor system-list)))
;;;
;;; Manager Methods 'n Functions
;;;

(defmethod update ((this particle-manager) dt))

(defmethod render ((this particle-manager)))

(defmethod init ((this particle-manager)))

(defmethod add-system ((this particle-manager) type init-parms))

(defmethod remove-system ((this particle-manager) (obj particle-system)))