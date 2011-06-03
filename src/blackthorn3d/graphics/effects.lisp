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
;;; Effects
;;; - mostly for managing particles

;; to be part of an 'effect' it needs a 'draw-object' method
;; as well as an 'update'
(defclass composite-effect ()
  ((objects
    :accessor objects
    :initarg :objects
    :initform ())))

(defun new-composite-effect (&rest gfx-list)
  (format t "effects: ~a~% gfx-list~%" gfx-list)
  (make-instance 'composite-effect
                 :objects gfx-list))

(defmethod client-update ((this composite-effect) dt)
  (with-slots (objects) this
    (setf objects (iter (for obj in objects)
                        (appending (client-update obj dt))))
    (if objects
      (list this)
      nil)))

(defmethod render-effect ((this composite-effect))
  (iter (for obj in (objects this))
        (render-effect obj)))

(defvar *effects-list* ())
(defun add-effect (effect)
  (push effect *effects-list*))

(defun remove-effect (effect)
  (setf *effects-list* (delete effect *effects-list*)))

(defun update-effects (dt)
  (setf *effects-list*
        (iter (for effect in *effects-list*)
              (appending (client-update effect dt)))))

(defun render-effects ()
  (iter (for effect in *effects-list*)
        (render-effect effect)))

;;
;; to define effects...
;; we need a mapping of ids to effects and how to play/or create them
;; so they need a 'handle' function that takes &rest args and does stuff
;; For now lets consider creating new effects every time.

;; 

(defvar *ghost-effect-color* +purple+)
(defvar *human-effect-color* +aqua+)
(defvar *ghost-beam-tex* nil)
(defvar *human-beam-tex* nil)
(defvar *explosion-smoke-tex* nil)
(defvar *laser-beam-size* #(0.1 0.4))
(defvar *laser-duration* 0.25)


(defclass gfx ()
  ((xform
    :initarg :xform
    :initform (make-identity-matrix))
   (state
    :initarg :state
    :initform :alive)
   (life
    :initarg :life)))

(defclass flare (gfx)
  ((pos
    :initarg :pos)
   (color
    :initarg :color)
   (texture
    :initarg :texture
    :initform *particle-tex*)
   (size
    :initarg :start-size)
   (growth
    :initarg :growth
    :initform (vector 1.0 1.0))))

(defclass beam (gfx)
  ((start
    :initarg :start)
   (beam
    :initarg :beam)
   (color
    :initarg :color)
   (texture
    :initarg :texture)
   (size
    :initarg :size)))


(defmethod client-update ((this gfx) dt)
  (with-slots (life state) this
    (decf life dt)
    (if (< life 0)
        (progn (setf state :dead) nil)
        (list this))))

(defmethod client-update :before ((this flare) dt)
  (with-slots (size growth life state) this
    (setf (svref size 0) (+ (x size) (* dt (x growth))))
    (setf (svref size 1) (+ (y size) (* dt (y growth))))))

(defmethod render-effect ((this flare))
  (with-slots (pos texture color size) this
    (draw-billboard-quad pos (x size) (y size) 
                         texture color)))

;; these are ugly 
#+disabled
(defmethod move-effect ((this gfx) xform)
  (setf (xform this) xform))



(defmethod render-effect ((this beam))
  (with-slots (xform start beam color texture size state) this
    (when (eql state :alive)
      (draw-beam start (vec4+ start beam) color texture size))))

(defun make-laser-flare (pos color)
  (make-instance 'flare
                 :pos pos
                 :color (map 'vector #'(lambda (x) (* x 3.0)) color)
                 :start-size (vector 0.1 0.1)
                 :growth #(1.0 1.0)
                 :life *laser-duration*))

(defun make-laser-spark-emitter (pos dir)
  (make-instance 'point-emitter
                 :pos pos
                 :dir dir
                 :up (get-perpendicular dir)
                 :angle pi
                 :speed 8))

(defun make-laser-pulse-emitter (pos dir beam)
  (make-instance 'line-emitter
                 :pos pos
                 :dir dir
                 :up (get-perpendicular dir)
                 :angle 0.01
                 :speed '(0.3 . 2.2)
                 :beam beam))

(defun make-laser-sparks (pos dir color)
  (create-spark-ps
   (make-laser-spark-emitter pos dir)
   50
   :size #(0.15 0.3)
   :lifetime 0.1
   :color color
   :mode (list :time *laser-duration*)
   :drag-coeff 8.5))

(defun make-laser-pulse (pos dir color)
  (create-explosion-ps
   (make-laser-pulse-emitter pos (norm4 dir) (vec4+ pos dir))
   100
   :size #(0.1 0.1)
   :lifetime '(1.0 . 1.5)
   :color color
   :drag-coeff 1.0))

(defun make-laser-beam (start beam color texture)
  (make-instance 'beam 
                 :start start
                 :beam beam
                 :life *laser-duration*
                 :color color
                 :texture texture
                 :size *laser-beam-size*))

;; Create teh effect instance for a human laser
;; and add it to the effects list
(defun make-laser (start dir color texture)
  (let ((norm-dir (norm4 dir)))
    (new-composite-effect
     ;; the laser beam!
     (make-laser-beam start dir color texture)
     ;; particle end sparks
     (make-laser-sparks (vec4+ start dir) (vec-neg4 norm-dir) color)
     ;; particle beam pulse
    ;; (make-laser-pulse start dir color)
     ;; flare
     (make-laser-flare start color)
     )))

(defun add-human-laser (start dir)
  (let ((beam-texture *human-beam-tex*)
        (color *human-effect-color*))
    (add-effect (make-laser start dir color beam-texture))))

(defun add-ghost-laser (start dir)
  (let ((beam-texture *ghost-beam-tex*)
        (color *ghost-effect-color*))
    (add-effect (make-laser start dir color beam-texture))))



(defun make-radial-emitter (pos speed)
  (make-instance 'point-emitter
                 :pos pos
                 :dir +y-axis+
                 :up +z-axis+
                 :angle (* 2 pi)
                 :speed speed))

(defun make-explosion-sparks (pos)
  (create-explosion-ps
   (make-radial-emitter pos '(10.0 . 14.0))
   25
   :size #(0.08 1.5)
   :lifetime '(0.3 . 0.4)
   :color +orange+
   :drag-coeff 7.0))

(defun make-explosion-smoke (pos)
  (create-explosion-ps
   (make-radial-emitter pos '(3.0 . 3.3))
   20
   :size #(0.6 1.0)
   :lifetime '(1.0 . 1.2)
   :color +orange+
   :texture *explosion-smoke-tex*
   :drag-coeff 3.5))

(defun make-explosion-flare (pos)
  (make-instance 'flare
                 :pos pos
                 :color +white+
                 :start-size (vector 0.0 0.0)
                 :growth #(4.0 4.0)
                 :life 0.3))

(defun add-an-explosion (pos)
  (add-effect
   (new-composite-effect
    (make-explosion-sparks pos)
    (make-explosion-smoke pos)
    (make-explosion-flare pos))))