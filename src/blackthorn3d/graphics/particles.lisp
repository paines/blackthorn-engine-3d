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
(defvar *particle-tex* nil)

;;;
;;; particles
;;;


;; define a particle as: 
;;        #(energy fade-rate pos.x pos.y pos.z old.x old.y old.z
;;          vel.x vel.y vel.z color.r color.g color.b color.a size)
;; total length: 16 floats
(defun create-particle (particles index
                        position velocity
                        energy fade-rate
                        color size)
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

(defun update-particle (particle force-fn dt)
  (setf (p-energy particle) 
        (- (p-energy particle) (* (p-fade particle) dt)))
  (when (is-alive particle)
    (setf (p-old-pos particle) (p-pos particle)
          (p-pos particle) 
          (vec3+ (p-pos particle)
                 (vec-scale3 (p-vel particle) dt))
          (p-vel particle) (vec3+ (p-vel particle) 
                                  (vec-scale3 
                                   (funcall force-fn
                                           (p-vel particle)
                                            dt) 
                                   dt)))))


;;;
;;; Classes
;;;

(defclass point-emitter ()
  ((pos
    :initarg :pos)
   (dir
    :initarg :dir)
   (up
    :initarg :up)
   (speed
    :initarg :speed)
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
   (lifetime
    :accessor lifetime
    :initarg :lifetime
    :initform 1.0
    :documentation "The time in seconds that the particles live
                    If given as a pair (start . end) a random value
                    is generated between start and end") 
   (size
    :initarg :size
    :documentation "size of particle.  same rule applies as lifetime")
   (color
    :accessor color
    :initarg :color
    :initform +white+
    :documentation "color of each particle. Would like to expand the
                    functionality to allow more interesting possibilities
                    but for now if you wanna change it do it at the 
                    particle system level")
   (force-fn
    :initarg :force-fn
    :initform #'(lambda (vel dt) (vec-neg4 +y-axis+)))
   (shader
    :accessor shader
    :initarg :shader
    :initform 0)
   (texture
    :accessor texture
    :initarg :texture
    :initform 0)
   (type
    :accessor ps-type
    :initarg :type
    :initform :default)
   (particles
    :accessor particles
    :initarg :particles
    :documentation "the array of particles belonging to the system")
   (max-particles
    :initarg :max-particles
    :reader  max-particles)
   (particle-stream
    :initarg :particle-stream
    :initform nil)
   (num-alive
    :accessor num-alive
    :initform 0)))


;;;
;;; Emitters
;;;


(defmethod gen-initial-pos ((this point-emitter) time)
  (slot-value this 'pos))

(defmethod gen-initial-vel ((this point-emitter) time)
  (with-slots (dir angle speed) this
    ;; Generate random coordinates
    (let* ((angle/2 (* 0.5 angle))
           (phi (random (* 2.0 pi)))
           (theta (random (float angle/2)))
           (u dir)
           (v (get-perpendicular dir))
           (w (cross3 u v))
           (z (cos theta))
           (x (* (sin theta) (cos phi)))
           (y (* (sin theta) (sin phi)))
           (speed (if (consp speed) 
                      (+ (car speed) (random (- (cdr speed) (car speed))))
                      speed)))
      (vec-scale3 (vec3+ (vec3+ (vec-scale3 u z)
                                (vec-scale3 v y))
                         (vec-scale3 w x))
                  speed))))


;;;
;;; Particle-System Methods and Functions
;;;

(defun create-particle-system (emitter spawn-rate max-particles
                               &key 
                               (color +white+)
                               (lifetime 2.0)
                               (size 0.1)
                               (force-fn #'(lambda (vel dt)
                                             (vec-neg4 +y-axis+))))
  (make-instance 'particle-system
                 :emitter emitter
                 :spawn-rate spawn-rate
                 :lifetime lifetime
                 :size size
                 :color color
                 :max-particles max-particles
                 :particles (create-particle-array max-particles)
                 :force-fn force-fn
                 :particle-stream
                 (create-billboard-stream max-particles)))

(defmethod gen-particle ((this particle-system) index dt)
  (with-slots (particles size lifetime emitter color) this
    (let ((pos (gen-initial-pos emitter dt))
          (vel (gen-initial-vel emitter dt))
          (fade (/ 1.0 (if (consp lifetime)
                           (+ (car lifetime) 
                              (random (- (cdr lifetime) (car lifetime))))
                           lifetime)))
          (size (if (consp size)
                    (+ (car size) 
                       (random (- (cdr size) (car size))))
                    size)))
      (create-particle particles index
                       pos vel
                       1.0 fade
                       color size))))

(let ((spawn-num 1))
  (defmethod update-ps ((this particle-system) dt)
    ;; Loop over each particle and update it's position
   ; (let ((*external-force* (funcall (slot-value this 'force-fn) dt))))
    (with-slots (particles 
                 spawn-rate 
                 max-particles 
                 num-alive 
                 force-fn) this

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
                  (update-particle particle force-fn dt)
                  (incf alive-cnt))
                ;; If dead check if we should revive it
                (when (< emitted max-emit)
                  (gen-particle this index dt)
                  (incf emitted)))

            ;; Finally, update the number of alive particles
            (finally
             (decf spawn-num emitted)
             (setf num-alive (+ alive-cnt emitted))
          #+disabled   (format t "num alive: ~a~%" num-alive))))))

;; TODO: add textures/quads, not just points
(defmethod render-ps ((this particle-system))
  (with-slots (particles max-particles num-alive texture 
                         particle-stream) this
    (render-particles particles max-particles
                      num-alive *particle-tex*))

  #+disabled
  (with-slots (particles max-particles num-alive particle-stream) this
                                        ;(gl:with-primitives :points)
    (iter (for index below max-particles)
          (for particle = (cons particles index))
          (count (is-alive particle) into alive-cnt)
          (while (< alive-cnt num-alive))

          ;; do particle rendering here
          (when (is-alive particle)
            (let ((position (p-pos particle))
                  (color (p-color particle)))
              (gl:color (r color) (g color) (b color) (* (p-energy particle)
                                                         (a color)))
                                        ;#+disabled
              (draw-billboard-quad position 0.1 0.1 *particle-tex*
                                   :screen)
              #+disabled
              (gl:vertex (x position) (y position) (z position)))))))





(defvar *system-list* nil)

(defun update-particle-systems (dt)
  (iter (for system in *system-list*)
        (update-ps system dt)))
(defun render-particle-systems (dt)
  (iter (for system in *system-list*)
        (render-ps system)))

(defun particles-init ())

(defun add-system (ps)
   (push ps *system-list*))

(defun remove-system (ps)
  (setf *system-list* (delete ps *system-list*)))