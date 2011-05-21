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
;;; particles
;;;

;; states: :alive :dead 
;; define a particle as: 
;;        #(energy fade-rate pos.x pos.y pos.z old.x old.y old.z
;;          vel.x vel.y vel.z color.r color.g color.b color.a size)
;; total length: 16 floats

#+disabled
(defun create-particle (particles index 
                        position velocity
                        &key (energy 1.0)
                        (fade-rate 1.0)
                        (color +white+)
                        (size 0.1))
  (labels ((set-array-row (array row values)
             (iter (for i from 0)
                   (for v in values)
                   (setf (aref array row i) v))))
    (setf (col particles index) 
          (vector energy                ; 0
                  fade-rate             ; 1
                  (x position)          ; 2
                  (y position)          ; 3
                  (z position)          ; 4
                  (x position)          ; 5
                  (y position)          ; 6
                  (z position)          ; 7
                  (x velocity)          ; 8
                  (y velocity)          ; 9
                  (z velocity)          ; 10
                  (r color)             ; 11
                  (g color)             ; 12
                  (b color)             ; 13
                  (a color)             ; 14
                  size))))              ; 15

(defun particle-energy (particles index)
  (aref particles index 0))

(defun (setf particle-energy) (new-energy particles index)
  (setf (aref particles index 0) new-energy))

(defun particle-pos (particles index)
  (make-point3 (aref particles index 2)
               (aref particles index 3)
               (aref particles index 4)))
(defun (setf particle-pos) (pos particles index)
  (setf (aref particles index 2) (x pos)
        (aref particles index 3) (y pos)
        (aref particles index 4) (z pos)))

(defun particle-old-pos (particles index)
  (make-point3 (aref particles index 5)
               (aref particles index 6)
               (aref particles index 7)))
(defun (setf particle-old-pos) (pos particles index)
  (setf (aref particles index 5) (x pos)
        (aref particles index 6) (y pos)
        (aref particles index 7) (z pos)))

(defun particle-vel (particles index)
  (make-vec3 (aref particles index 8)
             (aref particles index 9)
             (aref particles index 10)))
(defun (setf particle-vel) (vel particles index)
  (setf (aref particles index 8) (x pos)
        (aref particles index 9) (y pos)
        (aref particles index 10) (z pos)))

(defun particle-color (particles index)
  (make-point3 (aref particles index 11)
               (aref particles index 12)
               (aref particles index 13)
               (aref particles index 14)))
(defun (setf particle-color) (color particles index)
  (setf (aref particles index 11) (r color)
        (aref particles index 12) (g color)
        (aref particles index 13) (b color)
        (aref particles index 14) (a color)))


(defclass particle ()
  ((state
    :initform :alive)
   (energy
    :initarg :energy
    :initform 1.0)
   (fade-rate
    :initarg :fade-rate
    :initform 0.1)                      ; default to one second life
   (position
    :initarg :pos)
   (old-position
    :initform nil)
   (velocity
    :initarg :vel)
   (color
    :initarg :color
    :initform +white+)
   (size
    :initarg :size
    :initform 0.1)))

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
                    2*pi would be a circle")))
 
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
;;; particle systems?
;;;
(defun create-particle-system (emitter spawn-rate max-particles)
  (make-instance 'particle-system
                 :emitter emitter
                 :spawn-rate spawn-rate
                 :max-particles max-particles
                 :particles (make-array max-particles :fill-pointer 0)))

(defmethod create-particle ((this particle-system) time)
  (with-slots (emitter) this
    (let ((initial-pos (gen-initial-pos emitter time))
          (initial-vel (gen-initial-vel emitter time)))
      (make-instance 'particle
                     :pos initial-pos
                     :vel initial-vel))))

;;;
;;; Emitters
;;;

(defmethod gen-initial-pos ((this point-emitter) time)
  (slot-value this 'pos))

(defmethod gen-initial-vel ((this point-emitter) time)
  (with-slots (dir speed speed-fuzzy) this
    (vec-scale3 dir (+ speed (- 1.0 (random (* (float speed-fuzzy))))))))



;;;
;;; Particle Methods and Functions
;;;

(defmethod update-ps ((this particle-system) dt)
  ;; Loop over each particle and update it's position
  (with-slots (particles spawn-rate max-particles) this    
    (iter (with emitted = 0)
          (with max-emit = (if (zerop spawn-rate)
                               max-particles
                               (ceiling (* spawn-rate dt))))
          (for particle in-vector particles)
          (with-slots (state energy fade-rate
                             position old-position velocity) particle
            (case state
              (:alive
               (setf energy (- energy (* dt fade-rate)))
               (if (< energy 0.0) 
                   (setf state :dead)
                   (setf old-position position
                         position (vec3+ position 
                                         (vec-scale3 velocity dt)))))
              (:dead
               ;; If the particle is dead, check if we need to rebirth it
               (when (< emitted max-emit)
                 (setf particle (create-particle this dt))
                 (incf emitted)))))

          ;; Finally, if we still need to emit particles, and there is
          ;; room in the array, we need to make more
          (finally
           (let ((delta-p (- max-emit emitted))
                 (delta-m (- max-particles (length particles))))
             (format t "-----------~%max-emit: ~a emit-d: ~a~%length-d: ~a~%"
                     max-emit delta-p delta-m)
             (when (and (plusp delta-p)
                        (plusp delta-m))
               (iter (for i below (min delta-p delta-m))
                     (vector-push (create-particle this dt)
                                  particles))))))))

;; TODO: add textures/quads, not just points
(defmethod render-ps ((this particle-system))
  (with-slots (particles) this
    (gl:with-primitives :points
      (format t "Rendining ~a particles ~%" (length particles))
      (iter (for particle in-vector particles)
            ;; do particle rendering here
            (with-slots (state position color size) particle
              (when (eql state :alive)
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

