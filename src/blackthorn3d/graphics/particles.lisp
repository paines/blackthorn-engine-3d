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
;; total length: 12 floats
;; CAN HAZ MACRO!?!
(defun create-particle (particles index
                        position velocity
                        energy fade-rate
                        color)
  (setf (col particles index) 
        (vector energy                  ; 0
                fade-rate               ; 1
                (x position)            ; 2
                (y position)            ; 3
                (z position)            ; 4
            ;    (x position)            ; 5
            ;    (y position)            ; 6
            ;    (z position)            ; 7
                (x velocity)            ; 8 -> 5
                (y velocity)            ; 9 -> 6
                (z velocity)            ; 10 -> 7
                (r color)               ; 11 -> 8
                (g color)               ; 12 -> 9
                (b color)               ; 13 -> 10
                (a color)               ; 14 -> 11
                ;size                   ; 15 -> 12
                )))                 

(defun create-particle-array (max-particles)
  (make-array (list max-particles 12) :element-type 'float))

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
#+disabled
(defun p-old-pos (particle)
  (make-point3 (aref (car particle) (cdr particle) 5)
               (aref (car particle) (cdr particle) 6)
               (aref (car particle) (cdr particle) 7)))
#+disabled
(defun (setf p-old-pos) (pos particle)
  (setf (aref (car particle) (cdr particle) 5) (x pos)
        (aref (car particle) (cdr particle) 6) (y pos)
        (aref (car particle) (cdr particle) 7) (z pos)))

;; VELOCITY
(defun p-vel (particle)
  (make-vec3 (aref (car particle) (cdr particle) 5)
             (aref (car particle) (cdr particle) 6)
             (aref (car particle) (cdr particle) 7)))
(defun (setf p-vel) (vel particle)
  (setf (aref (car particle) (cdr particle) 5) (x vel)
        (aref (car particle) (cdr particle) 6) (y vel)
        (aref (car particle) (cdr particle) 7) (z vel)))

;; COLOR
(defun p-color (particle)
  (make-vector4 (aref (car particle) (cdr particle) 8)
                (aref (car particle) (cdr particle) 9)
                (aref (car particle) (cdr particle) 10)
                (aref (car particle) (cdr particle) 11)))
(defun (setf p-color) (color particle)
  (setf (aref (car particle) (cdr particle) 8) (r color)
        (aref (car particle) (cdr particle) 9) (g color)
        (aref (car particle) (cdr particle) 10) (b color)
        (aref (car particle) (cdr particle) 11) (a color)))

;; SIZE
#+disabled
(defun p-size (particle)
  (aref (car particle) (cdr particle) 12))
#+disabled
 (defun (setf p-size) (new-size particle)
   (setf (aref (car particle) (cdr particle) 12) new-size))

 ;;;
 ;;; Particle use functions
 ;;;

 (defun is-alive (particle)
   (> (p-energy particle) 0.0))

 (defun update-particle (particle force-fn dt)
   (setf (p-energy particle) 
         (- (p-energy particle) (* (p-fade particle) dt)))
   (when (is-alive particle)
     (setf (p-pos particle) 
           (vec3+ (p-pos particle)
                  (vec-scale3 (p-vel particle) dt))
           (p-vel particle) 
           (vec3+ (p-vel particle) 
                  (vec-scale3 
                   (funcall force-fn (p-vel particle) dt) 
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
    :initform 0.1
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
   #+disabled ;; i think we can use the same shader...for everything
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
    :initform :default
    :documentation "types: :default
                           :explosion
                           :sparks")
   (mode
    :accessor mode
    :initarg :mode
    :initform '(:loop)
    :documentation " (:loop) never stops
                     (:once) plays until num-alive = 0. note that this could
                             go forever.
                     (:repeat n) playes n times. same applies as once
                     (:time sec) plays for sec seconds
                     (:stop) stops generation
                     (:kill) kills all particles")
   (particles
    :accessor particles
    :initarg :particles
    :documentation "the array of particles belonging to the system")
   (max-particles
    :initarg :max-particles
    :reader  max-particles)
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

#+disabled
(defmacro make-force-fn ((&rest statics) (&rest dynamics))
  (with-gensysm (total-static)))

;; what is this i don't even
(defmacro make-force-fn (gravity gravity-coeff drag-coeff 
                         &rest others)
  (with-gensyms (total)
    (let ((grav-term `(vec-scale3 ,gravity ,gravity-coeff))
          (drag-term `(vec-scale3 (vec-neg3 vel) ,drag-coeff)))
      `(let ((,total (reduce #'vec3+ (cons ,grav-term ,others)
                             :initial-value #(0.0 0.0 0.0))))
         #'(lambda (vel dt)
             (vec3+ ,total ,drag-term))))))

(defun create-particle-system (emitter spawn-rate max-particles
                               &key 
                               (color +white+)
                               (lifetime 2.0)
                               (size 0.1)
                               (mode '(:loop))
                               (force-fn #'(lambda (vel dt)
                                             (vec-neg4 +y-axis+))))
  (make-instance 'particle-system
                 :emitter emitter
                 :spawn-rate spawn-rate
                 :lifetime lifetime
                 :size size
                 :color color
                 :mode mode
                 :max-particles max-particles
                 :particles (create-particle-array max-particles)
                 :force-fn force-fn))


(defun create-explosion-ps (emitter max-particles
                            &key
                            (color +white+)
                            (texture *particle-tex*)
                            (lifetime 1.0)
                            (size 0.1)
                            (gravity +zero-vec+)
                            (grav-coeff 1.0)
                            drag-coeff)
  (make-instance 'particle-system
                 :type :explosion
                 :emitter emitter
                 :spawn-rate 0
                 :lifetime lifetime
                 :size size
                 :color color
                 :texture texture
                 :mode '(:loop 4)
                 :max-particles max-particles
                 :particles (create-particle-array max-particles)
                 :force-fn
                 (make-force-fn gravity grav-coeff 
                                (if drag-coeff
                                  drag-coeff
                                  (/ 1 (if (consp lifetime)
                                             (cdr lifetime)
                                             lifetime))))))

(defun create-spark-ps (emitter max-particles
                        &key 
                        (color +white+)
                        (texture *particle-tex*)
                        (lifetime 0.1)
                        (size 0.1)
                        (gravity +zero-vec+)
                        (grav-coeff 1.0)
                        drag-coeff)
  (make-instance 'particle-system
                 :type :sparks
                 :emitter emitter
                 :color color
                 :texture texture
                 :lifetime lifetime
                 :max-particles max-particles
                 :spawn-rate (ceiling
                              (/ max-particles 
                                 (if (consp lifetime)
                                     (car lifetime)
                                     lifetime)))
                 :particles (create-particle-array max-particles)
                 :mode '(:loop)
                 :force-fn
                 (make-force-fn gravity grav-coeff 
                                (if drag-coeff
                                  drag-coeff
                                  (/ 1 (if (consp lifetime)
                                             (cdr lifetime)
                                             lifetime))))))

(defmethod gen-particle ((this particle-system) index dt)
  (with-slots (particles size lifetime emitter color) this
    (let ((pos (gen-initial-pos emitter dt))
          (vel (gen-initial-vel emitter dt))
          (fade (/ 1.0 (if (consp lifetime)
                           (+ (car lifetime) 
                              (random (- (cdr lifetime) (car lifetime))))
                           lifetime))))
      (create-particle particles index
                       pos vel
                       1.0 fade
                       color))))

(let ((spawn-num 1))
  (defmethod update-ps ((this particle-system) dt)
    ;; Loop over each particle and update it's position
   ; (let ((*external-force* (funcall (slot-value this 'force-fn) dt))))
    (with-slots (particles 
                 spawn-rate 
                 max-particles 
                 num-alive 
                 lifetime
                 force-fn
                 mode type) this

      (unless (eql (car mode) :kill)
        (incf spawn-num (* dt spawn-rate))
        (iter (with emitted = 0)
              (with max-emit = 
                    (ecase type
                      (:explosion (if (<= num-alive 0) max-particles 0))
                      ((:default :sparks) (floor spawn-num))))
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
                  (when (and (not (eql (car mode) :stop))
                             (< emitted max-emit))
                    (gen-particle this index dt)
                    (incf emitted)))

              ;; Finally, update the number of alive particles      
              (finally
               (decf spawn-num emitted)
               (setf num-alive (+ alive-cnt emitted)))))

      (case (car mode)
        (:time (decf (second mode) dt)
               (when (< (second mode) 0) (setf mode '(:stop))))
        (:repeat
         (when (<= num-alive 0)
           (decf (second mode))
           (when (<= (second mode) 0) (setf mode '(:stop)))))
        (:once
         (when (<= num-alive 0) (setf mode '(:stop))))
        (:stop (when (<= num-alive 0) (setf mode '(:kill))))))))

;; TODO: add textures/quads, not just points
(defmethod render-ps ((this particle-system))
  (with-slots (particles max-particles num-alive texture size type) this
    (render-particles particles max-particles
                      num-alive *particle-tex* size
                      (case type
                        ((:explosion :sparks) :velocity)
                        (otherwise :screen)))))





(defvar *system-list* nil)


(defun remove-systems (lst systems)
  (if lst
    (remove-systems (cdr lst) (delete (car lst) systems))
    systems))

(defun update-particle-systems (dt)
  (iter (for system in *system-list*)
        (update-ps system dt)
        (when (eql (car (mode system)) :kill)
          (collect system into remove-lst))
        (finally
         (setf *system-list* 
               (remove-systems remove-lst *systems-list*)))))

(defun render-particle-systems (dt)
  (iter (for system in *system-list*)
        (render-ps system)))

(defun particles-init ())

(defun add-system (ps)
   (push ps *system-list*))

(defun remove-system (ps)
  (setf *system-list* (delete ps *system-list*)))
