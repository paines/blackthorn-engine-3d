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

(defvar *gravity-accel* 0.002)

(defun move-component (thing x y z)
  (setf (pos thing) (vec4+ (pos thing) (make-vec3 x y z))))
  
(defun move-vec (thing vec)
  (setf (pos thing) (vec4+ (pos thing) vec)))
  
(defun add-velocity (an-entity v)
  (setf (velocity an-entity) (vector-sum (list (velocity an-entity) v))))
  
(defun set-velocity (an-entity v)
  (setf (velocity an-entity) v))
  
(defun move-and-set-velocity (an-entity v)
  (set-velocity an-entity v)
  (move-vec an-entity v))
  
(defun chase (self who speed)
  (let ((direction (norm4 (vec4- (pos who) (pos self)))))
      (setf (dir self) direction)
      (setf (pos self) (vec4+ (pos self) (vec-scale4 direction speed)))))
    
(defvar *hackity-hack__lookup-sector* nil)
(defvar *hackity-hack__collide-sector* nil)

(defun collide-displace (an-entity vector sector)
  (let ((old-velocity (velocity an-entity))
        (result nil))
    (set-velocity an-entity vector)
    (setf result (funcall *hackity-hack__collide-sector* an-entity sector))
    (setf (velocity an-entity) old-velocity)
    result))


    
(defun standard-physics-step (self)
  (when (or (null *hackity-hack__lookup-sector*)
            (null *hackity-hack__collide-sector*))
    (return-from standard-physics-step))
    
  (let* ((t-sector 
          (funcall *hackity-hack__lookup-sector* (current-sector self)))
         ;; we can keep this
         (displace-vector (displace-step self 1)))
    
    (destructuring-bind (move-to new-up)
        (funcall *hackity-hack__collide-sector* 
                 self displace-vector t-sector)
  
      (move-vec self move-to)

      ;#+disabled
      (unless (standing-on-p self t-sector)
        #+disabled
        (setf (new-up self) 
              (norm4 move-to))
              
       ; #+disabled
        (destructuring-bind (move-to new-up2)
            (funcall *hackity-hack__collide-sector*
                     self 
                     (vec4+ (vec-neg4 displace-vector) 
                            (vec-scale4 (up self) (- (mag displace-vector))))
                     t-sector)
                     
          (move-vec self move-to)
          (when new-up2 (setf (new-up self) new-up2))))

      (destructuring-bind (new-vel new-up2)
          (funcall *hackity-hack__collide-sector*
                   self (velocity self) t-sector 1)
        
        (when (vec-eql new-vel +zero-vec+)
          (setf (is-jumping self) nil))
                   
        (move-and-set-velocity self new-vel)

        ;; We need to interpolate to new-up. I think we should store the
        ;; new up in the entity and use dt to interpolate to it
        (aif (or new-up2 new-up)
             (setf (new-up self) 
                   it))))))

  
(defun force-step (an-entity dt)
  (vector-sum (mapcar #'(lambda (m) (funcall m an-entity dt)) 
                   (forces an-entity))))
                   
(defun displace-step (an-entity dt)
  (vector-sum (mapcar #'(lambda (m) (funcall m an-entity dt)) 
                   (displacers an-entity)))) 

(defun make-gravity-mover () (lambda (an-entity dt)
  (vec-scale4 (vec-neg4 (up an-entity)) (* dt *gravity-accel*))))

;; need to correct for orientation, most likely
(defun make-camera-relative-player-mover (client camera)
  (lambda (an-entity dt)
    (let* ((input-vec (vector (s-input-move-x client) 
                             (s-input-move-y client)))
          (move-vec (vec-scale4 (move-player camera input-vec) (* 0.4 dt))))
      
      (if (is-jumping an-entity)
        +zero-vec+
        move-vec))))
    
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
      (progn 
        (setf (is-jumping an-entity) t)
        (vec-scale4 (vec-neg4 (up an-entity)) (* -25.0 dt *gravity-accel*)))
      +zero-vec+
      )))
      
(defun make-ghost-mover  (client)
  (lambda (self dt)
    (with-slots (up) self
      (let ((val +zero-vec+))
        (when (> (s-input-fly-up client) 0)
            (setf val (vec4+ val (vec-scale4 up (* dt 1.0)))))
          
        (when (> (s-input-fly-down client) 0)
          (setf val (vec4+ val (vec-neg4 (vec-scale4 up (* dt 1.0))))))
          
        val))))
      
(defun do-nothing-mover (self dt) +zero-vec+)
