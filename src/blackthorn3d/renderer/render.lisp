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
(defvar *test-ui* nil)

(defparameter vao-cube nil)
(defparameter shader nil)
(defparameter animated nil)


(defun init ()
  "Called to initialize the graphics subsystem"
  (format t "Initializing Rendering Subsystem~%")
  (setf %gl:*gl-get-proc-address* #'sdl:sdl-gl-get-proc-address)
 
  (setf *main-light* (make-instance 'light
                                    :position (make-point3 0.0 10.0 0.0)))

  (setf *main-viewport* (create-viewport '(960 720) 0.1 200))
;  (setf *frustum* (make-frstm 1.0 1000.0 8/6 (/ pi 2)))
  )


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

  (gl:enable :texture-2d)
  (gl:enable :blend :sample-alpha-to-coverage)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-color 0 0 0 0)
  (gl:enable :depth-test)
  (gl:depth-func :lequal)

  (format t "### LOADING CONTROLLER MODEL ###~%")

;  #+disabled
  (let ((scientist-model 
         (blt3d-imp:dae-geometry 
          (blt3d-imp:load-dae 
          ; #p "res/models/KatanaSpiderMaterialAnimated.dae"
           #p "res/models/Player2Rigged.dae"
           ))))
    
    (setf *test-skele* (load-obj->models scientist-model))
    (format t "SKELE: ~a~%" *test-skele*)
    (format t "~2T skele nodes: ~a~%" (mesh-nodes *test-skele*))

    (apply-transform *test-skele* (make-scale #(0.008 0.008 0.008)))
 
    (apply-transform *test-skele* 
                     (make-inv-ortho-basis (make-point3 1.0 0.0 0.0)
                                           (make-point3 0.0 0.0 1.0)
                                           (make-point3 0.0 1.0 0.0))))

  (set-viewport *main-viewport*)
  (gl:matrix-mode :modelview)
  (gl:load-identity)

 ; (gl:enable :lighting)
  (gl:enable :light0)
  (gl:enable :rescale-normal)

  (setf *test-ps* (create-particle-system 
                   (make-instance 'point-emitter
                                  :pos (make-point3 0.0 -0.5 0.0)
                                  :dir +y-axis+
                                  :up +y-axis+
                                  :angle (/ pi 1.5)
                                  :speed '(.7 . 4.5))
                   1000
                   8000
                   :lifetime 4
                   :force-fn
                   #'(lambda (vel dt)
                       (vec-neg3 (vec3+ vel +y-axis+)))))

  (add-ui-element
   (setf *test-ui* (make-instance 'ui-gauge
                                  :offset '(0 0)
                                  :size '(.05 .25)
                                  :orientation :up
                                  :max-value (max-particles *test-ps*)
                                  :current-value 0)))

  (setf *collide-mat* (make-blt-material :ambient #(0.5 0.0 0.0)
                                         :diffuse #(1.0 0.0 0.0))))





(defun update-graphics (entities level time)
  (when *test-ps*
    (update-ui-element *test-ui* (num-alive *test-ps*))
    (update-ps *test-ps* time))

  (when animated
    (update-model animated time))

  #+disabled
  (when *test-skele*
    (update-model *test-skele* time))

 ; #+disabled
  (when level
    (update-model level time)) 

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

  #+disabled
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

  ;#+disabled
  (when *test-skele*
    (gl:with-pushed-matrix
        ;(gl:scale 0.03 0.03 0.03)
      (draw-object *test-skele*)))

  ;#+disabled
  (dolist (e entities)
    (when (and (shape e) (not (eql e *main-cam*)))
      (with-slots (pos dir up shape) e
        (let ((z-axis (cross dir up)))
          (gl:with-pushed-matrix
            (gl:translate (x pos) (y pos) (z pos))
            (gl:mult-matrix (make-inv-ortho-basis dir up z-axis))
            (draw-object shape))))))

  ;; DO PARTICLES YEAH!
  (gl:blend-func :src-alpha :one)  
  #+disabled
  (when *test-ps*
    (render-ps *test-ps*))

  ;; Lastly render the ui
  (render-ui)

  (gl:flush)
  (sdl:update-display))