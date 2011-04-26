;;;; Blackthorn -- Lisp Game Engine
;;;;
;;;; Copyright (c) 2011, Chris McFarland
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

(in-package :blackthorn3d-input)
    
(defclass input-system ()
    ((kind 
        :accessor input-kind
        :initarg :kind
        :documentation "Either :keyboard or :xbox"))
    (:documentation "Holds the state of the input system."))
        
; TODO: Customizable up/down/left/right keys
        
(defgeneric input-move-x (system)
   (:documentation 
     "Handles x-axis input from either xbox for keyboard. Result is a number in 
      the range [-1.0, 1.0]."))
      
(defgeneric input-move-y (system)
   (:documentation 
     "Handles y-axis input from either xbox for keyboard. Result is a number in 
      the range [-1.0, 1.0]."))
        
(defmethod input-move-x ((system input-system))
    (with-slots (kind) system
        (case kind
            (:keyboard 
              (+ (if (sdl:get-key-state :sdl-key-right) 1.0  0)
                 (if (sdl:get-key-state :sdl-key-left) -1.0  0)))
            #+windows
            (:xbox (/ (xbox360_get_lx 0) 65535))
            (otherwise 0))))

(defmethod input-move-y ((system input-system))
    (with-slots (kind) system
        (case kind
            (:keyboard
               (+ (if (sdl:get-key-state :sdl-key-up) 1.0 0)
                  (if (sdl:get-key-state :sdl-key-down) -1.0 0)))
            #+windows
            (:xbox (/ (xbox360_get_ly 0) 65535))
            (otherwise 0))))

(defgeneric set-controller (system type)
    (:documentation 
      "Sets the input system to use either :xbox or :keyboard controls."))
            
(defmethod set-controller ((system input-system) type)
    (setf (input-kind system) type))
