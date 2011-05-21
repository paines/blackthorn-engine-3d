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

(defun move-component (thing x y z)
  (setf (pos thing) (vec4+ (pos thing) (make-vec3 x y z))))
  
(defun move-vec (thing vec)
  (setf (pos thing) (vec4+ (pos thing) vec)))
  
(defun chase (self who speed)
  (let ((direction (norm4 (vec4- (pos who) (pos self)))))
      (setf (dir self) direction)
      (setf (pos self) (vec4+ (pos self) (vec-scale4 direction speed)))))
      
(defun standard-physics-step (self)
  (let ((movement-vec (vec4+ (vec-scale4 (vec-neg4 +y-axis+) 0.02) (velocity self))))
   ; (format t "VEL: ~a~%" (velocity self))
  ;  (format t "pre-move: ~a ; " movement-vec)
    (setf (velocity self) movement-vec)
    (setf movement-vec (collide-with-world self (blt3d-res:get-model :companion-cube)))
   ; (format t "post-move: ~a~%" movement-vec)
    (move-vec self movement-vec)
    ))
    