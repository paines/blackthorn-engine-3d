;;;; Blackthorn -- Lisp Game Engine
;;;;
;;;; Copyright (c) 2011, Chris McFarland <askgeek@gmail.com>
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

(defvar *velocity-threshold-squared* 0.02)
(defvar *gravity-accel* 0.02)

(defun move-component (thing x y z)
  (setf (pos thing) (vec4+ (pos thing) (make-vec3 x y z))))
  
(defun move-vec (thing vec)
  (setf (pos thing) (vec4+ (pos thing) vec)))
  
(defun chase (self who speed)
  (let ((direction (norm4 (vec4- (pos who) (pos self)))))
      (setf (dir self) direction)
      (setf (pos self) (vec4+ (pos self) (vec-scale4 direction speed)))))
   
; crappy old hacked up way of doing it   
#+disabled
(defun standard-physics-step (self)
  (let ((movement-vec (vec4+ (vec-scale4 (vec-neg4 +y-axis+) 0.02) 
                             (velocity self))))

    (setf (velocity self) movement-vec)

    #+disabled
    (setf movement-vec (collide-with-world 
                        test-sphere
                        (velocity self)
                        (blt3d-res:get-model :companion-cube)))
    #+disabled
    (move-vec self movement-vec)
    ))
    
(defvar *hackity-hack__lookup-sector* nil)
(defvar *hackity-hack__collide-sector* (lambda (&rest whatever) (declare (ignore whatever)) nil))

; this will be the real one eventually
;#+disabled
(defun standard-physics-step (self)
  (let ((movement-vector (update-movement self 1)))
    (setf (velocity self) movement-vector)
    ;(setf (pos self) (vector-sum (list (pos self) (velocity self))))
    
    (let ((t-sector (funcall *hackity-hack__lookup-sector* (current-sector self))))
      (move-vec self
        (funcall *hackity-hack__collide-sector* self t-sector)))
    ))

(defun jump (p)
  (declare (ignore p))
  ;(setf (velocity p) (vec4+ (velocity p) (make-vec3 0.0 5.0 0.0))))
  )
  
(defun update-movement (an-entity dt)
  (vector-sum (mapcar #'(lambda (m) (funcall m an-entity dt)) 
                   (movers an-entity))))

(defun gravity-mover (an-entity dt)
  (vec-scale4 (vec-neg4 (up an-entity)) (* dt *gravity-accel*)))

;; need to correct for orientation, most likely
(defun make-camera-relative-player-mover (client camera)
  (lambda (an-entity dt)
    (let* ((input-vec (vector (s-input-move-x client) 
                             (s-input-move-y client)))
          (move-vec (vec-scale4 (move-player camera input-vec) (* 0.4 dt))))
      move-vec)))
    
(defun make-stupid-jump-mover (client)
  (lambda (an-entity dt)
    (if (> (s-input-jump client) 0)
      (vec-scale4 (vec-neg4 (up an-entity)) (* -2.0 dt *gravity-accel*))
      +zero-vec+
      )))
      
(defun its-sector (an-entity)
  (funcall *hackity-hack__lookup-sector* (current-sector an-entity)))
      
(defun standing-on-jumpable-p (an-entity)
  (standing-on-p an-entity (its-sector an-entity)))
      
(defun make-smarter-jump-mover (client)
  (lambda (an-entity dt)
    (if (and (> (s-input-jump client) 0)
             (standing-on-jumpable-p an-entity))
      (vec-scale4 (vec-neg4 (up an-entity)) (* -100.0 dt *gravity-accel*))
      +zero-vec+
      )))