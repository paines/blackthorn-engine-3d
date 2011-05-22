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

(in-package :blackthorn3d-renderer)


(defvar *main-viewport* nil)
(defvar *test-skele* nil)
(defvar *test-ps* nil)

(defparameter vao-cube nil)
(defparameter shader nil)
(defparameter animated nil)


(defun init ()
  "Called to initialize the graphics subsystem"
  (format t "Initializing Rendering Subsystem~%")
  (setf %gl:*gl-get-proc-address* #'sdl:sdl-gl-get-proc-address)
 
  (setf *main-light* (make-instance 'light
                                    :position (make-point3 0.0 10.0 0.0)))

  (setf *main-viewport* (create-viewport '(800 600) 0.1 1000))
  (setf *frustum* (make-frstm 1.0 1000.0 8/6 (/ pi 2))))


(defun set-camera (cam)
  (setf *main-cam* cam))

(defvar *collide-mat* nil)

(defun prepare-scene ()
  "Called after sdl is initialized, before first frame is drawn
   or when changing the 'scene' settings"
  ;; Display our version #s
  (format t "GL Version: ~a.~a~%GLSL Version: ~a.~a~%"
          (gl:major-version) (gl:minor-version)
          (gl:glsl-major-version) (gl:glsl-minor-version))
  

  (init-gfx)


 ; (gl:viewport 0 0 800 600)

  (gl:enable :texture-2d)
  (gl:enable :blend :sample-alpha-to-coverage)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-color 0 0 0 0)
  (gl:enable :depth-test)
  (gl:depth-func :lequal)

  (format t "### LOADING CONTROLLER MODEL ###~%")
  ;#+disabled
  (let ((scientist-model 
         ;#+disabled
         (blt3d-imp:dae-geometry 
          (blt3d-imp:load-dae #p "res/models/cylinder-test-2.dae"))))
    
    (setf *test-skele* (load-obj->models scientist-model))
    (apply-transform *test-skele* (make-scale #(0.05 0.05 0.05)))
    ;#+disabled
    (apply-transform *test-skele* 
                     (make-inv-ortho-basis (make-point3 1.0 0.0 0.0)
                                           (make-point3 0.0 0.0 1.0)
                                           (make-point3 0.0 1.0 0.0))))

  (set-viewport *main-viewport*)
 ; (load-frstm *frustum*)
  (gl:load-identity)

 ; (gl:enable :lighting)
  (gl:enable :light0)
  (gl:enable :rescale-normal)

  (setf *test-ps* (create-particle-system 
                   (make-instance 'point-emitter
                                  :pos (make-point3 0.0 -0.5 0.0)
                                  :dir +y-axis+
                                  :up +y-axis+
                                  :angle (/ pi 2)
                                  :speed '(1.0 . 2.5))
                   100
                   1000
                   :lifetime 4
                   :force-fn
                   #'(lambda (vel dt)
                       (vec-neg3 (vec3+ vel +y-axis+)))))

  (setf *collide-mat* (make-blt-material :ambient #(0.5 0.0 0.0)
                                         :diffuse #(1.0 0.0 0.0))))

(defun update-graphics (entities time)
  (when *test-ps*
    (update-ps *test-ps* time))

  (when animated
    (update-model animated time))

  #+disabled
  (when *test-skele*
    (update-model *test-skele* time))

  (iter (for e in entities)
        (with-slots (shape) e
          (when shape
            (update-model shape time)))))

(defun render-frame (entities level)
  (gl:depth-mask t)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
 
  ;; Create PVS from entities and level
  (let ((PVS (find-pvs entities level))))

  (when *main-cam*
    (gl:load-matrix (look-dir-matrix (pos *main-cam*)
                                     (dir *main-cam*)
                                     (up  *main-cam*)))
    (update-billboarder (pos *main-cam*)
                        (dir *main-cam*)
                        (up *main-cam*)
                        +y-axis+))

  (init-light *main-light* :light0)

  (gl:color-material :front :diffuse)
  (gl:enable :color-material)
  ;(enable-shader shader)

  ;; draw axes
  #+disabled
  (gl:with-primitive :lines
    (gl:color 0.0 1.0 1.0)
    (gl:vertex 0.0 0.0 0.0)
    (gl:vertex 5.0 0.0 0.0)
    (gl:vertex 0.0 0.0 0.0)
    (gl:vertex 0.0 2.5 0.0)
    (gl:vertex 0.0 0.0 0.0)
    (gl:vertex 0.0 0.0 1.25))

  (when animated
    (draw-object animated))

  ;#+disabled
  (when level
    (gl:with-pushed-matrix
        ;; (use-material plane-mat)
        ;;(draw-plane 20)
        ;;(gl:scale .05 .05 .05)
       
      #+disabled
      (gl:mult-matrix (make-inv-ortho-basis (make-point3 1.0 0.0 0.0)
                                            (make-point3 0.0 0.0 1.0)
                                            (make-point3 0.0 1.0 0.0)))
      (draw-object level)))

  #+disabled
  (when *test-skele*
    (gl:with-pushed-matrix
        ;(gl:scale 0.03 0.03 0.03)
      (draw-object *test-skele*)))

  (dolist (e entities)
    (when (and (shape e) (not (eql e *main-cam*)))
      (with-slots (pos dir up shape) e
        (let ((z-axis (cross dir up)))
          (gl:with-pushed-matrix
            (gl:translate (x pos) (y pos) (z pos))
            (gl:mult-matrix (make-inv-ortho-basis dir up z-axis))
            (draw-object shape))))))

  ;; DO PARTICLES YEAH!
  ;#+disabled
  (gl:use-program 0)
  (gl:depth-mask nil)
  (gl:blend-func :src-alpha :one)
  (when *test-ps*
    (render-ps *test-ps*))

  (gl:flush)
  (sdl:update-display))