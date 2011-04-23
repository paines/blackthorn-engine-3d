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



(defvar *main-cam* nil)
(defvar *main-cam-quat* nil)
(defvar *frustum* nil)

(defparameter cube-mesh nil)

(defun init ()
  (setf cube-mesh (car (load-dae #p"res/models/room_01.dae")))
  (setf *main-cam* (make-instance 'camera 
                           :position (make-point3 0.0 0.0 5.0)
                           :direction (norm4 (vec4- (make-vec3 0.0 0.0 0.0)
                                                    (make-vec3 0.0 0.0 5.0)))
                           :mode :third-person ))
  (setf *main-cam-quat* (axis-rad->quat (make-vec3 0.0 1.0 0.0) (/ pi 100)))
  (setf *frustum* (make-frstm 1.0 1000.0 8/6 (/ pi 2))))


(defun prepare-scene ()
  (gl:viewport 0 0 800 600)

  (gl:enable :texture-2d)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-color 0 0 0 0)
  (gl:enable :depth-test)
  (gl:depth-func :lequal)

  (load-frstm *frustum*)
  (gl:load-identity)

  (gl:light :light0 :position '(3.0 3.0 0.0 1.0))
  (gl:light :light0 :diffuse (make-vec3 1.0 1.0 1.0))
  (gl:enable :lighting)
  (gl:enable :light0))


(defun render-frame ()
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:load-matrix (camera-inverse *main-cam*))
  (gl:color 1.0 .75 0.0)

  (draw-object cube-mesh)

  (gl:flush)
  (sdl:update-display))
