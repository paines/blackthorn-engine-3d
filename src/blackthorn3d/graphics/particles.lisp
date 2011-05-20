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

(defclass particle ()
  ((state
    :initform :alive)
   (energy
    :initarg :energy
    :initform 1.0)
   (fade-rate
    :initarg :fade-rate
    :initform 1.0)                      ; default to one second life
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


;;;
;;; Particle Methods and Functions
;;;

(defmethod create-particle ((this particle-system) time)
  (with-slots (emitter) this
    (let ((initial-pos (gen-initial-pos emitter time))
          (initial-vel (gen-initial-vel emitter time)))
      (make-instance 'particle
                     :pos initial-pos
                     :vel initial-vel))))

(defmethod update-ps ((this particle-system) dt)
  ;; Loop over each particle and update it's position
  (with-slots (particles spawn-rate max-particles) this    
    (iter (with emitted = 0)
          (with max-emit = (if (zerop spawn-rate)
                               max-particles
                               (* spawn-rate dt)))
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
             (when (and (plusp delta-p)
                        (plusp delta-m))
               (iter (for i below (min delta-p delta-m))
                     (vector-push (create-particle this dt)
                                  particles))))))))

;; TODO: add textures/quads, not just points
(defmethod render-ps ((this particle-system))
  (gl:with-primitives :points
    (iter (for particle in-vector particles)
          ;; do particle rendering here
          (with-slots (state pos color size) particle
            (when (eql state :alive)
              (gl:color (r color) (g color) (b color))
              (gl:vertex (x pos) (y pos) (z pos)))))))