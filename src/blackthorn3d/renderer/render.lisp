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
(defvar *test-fbo* nil)
(defvar *render-tex* nil)
(defvar *test-tex* nil)
(defvar *depth-tex* nil)
(defvar *laser-ps* nil)

(defvar home-sector nil)

(defparameter vao-cube nil)
(defparameter shader nil)
(defparameter animated nil)


(defun init ()
  "Called to initialize the graphics subsystem"
  (format t "Initializing Rendering Subsystem~%")
  (init-gfx)

  (setf *main-viewport* (create-viewport '(960 720) 0.2 200))

  (format t "### LOADING CONTROLLER MODEL ###~%")
  ;#+disabled
  (let ((scientist-model 
         (blt3d-imp:dae-geometry 
          (blt3d-imp:load-dae 
        ;   #p "res/models/PlayerAnimationTest.dae"
           #p "res/models/KatanaSpiderMaterialAnimated.dae"
         ;  #p "res/models/player-3.dae"
         ;  #p "res/models/FireBeastAnimated.dae"
           )))
        (epic-sword
         (blt3d-imp:dae-geometry
          (blt3d-imp:load-dae
           #p "res/models/SwordTextured.dae"))))
    
    (setf *test-skele* (load-obj->models scientist-model))
    (setf *test-sword* (load-obj->models epic-sword))
   ; (apply-transform *test-sword* +3dsmax-convert+)
    (apply-transform *test-sword* (make-inv-ortho-basis 
                                   (make-vec3 0.0 0.0 1.0)
                                   (make-vec3 1.0 0.0 0.0)
                                   (make-vec3 0.0 1.0 0.0)))
    (apply-transform *test-sword* (make-scale #(0.02 0.02 0.02)))
    (apply-transform *test-sword* (make-translate #(1.0 0.0 0.0 1.0)))

   ; #+disabled
    (attach-node-to-model (car (mesh-nodes *test-sword*))
                          "Bip001_Head" *test-skele*)

    (apply-transform *test-skele* (make-scale #(0.2 0.2 0.2)))
 ;   (apply-transform *test-skele* (make-scale #(0.008 0.008 0.008)))
    (apply-transform *test-skele*
                     (make-translate #(0.0 -2.0 0.0 0.0))))

  (setf *main-light* (make-light 'light
                      :position (make-point3 0.0 2.0 0.0)))

  ;#+disabled
  (setf *laser-ps*
        (create-particle-system
         (make-instance 'point-emitter
                        :pos +origin+
                        :dir +x-axis+
                        :up +y-axis+
                        :angle 0.1
                        :speed '(0.5 . 2.5))
         10
         300
         :lifetime '(5.0 . 10.0)
         :color #(0.0 0.5 2.0 1.0)
         :force-fn #'(lambda (x y) +zero-vec+)))

  ;#+disabled
  (setf *test-ps* 
        #+disabled
        (create-spark-ps
         (make-instance 'point-emitter
                        :pos (make-point3 0.0 -1.7 0.0)
                        :dir +y-axis+
                        :up +y-axis+
                        :angle (* 2 pi);(/ pi 6)
                        :speed 8)
         100
         :size #(0.05 0.3)
         :lifetime 0.2
         :color +orange+
         :drag-coeff 8.5)

      ; #+disabled
        (create-explosion-ps 
         (make-instance 'point-emitter
                        :pos (make-point3 0.0 -1.7 0.0)
                        :dir +y-axis+
                        :up +y-axis+
                        :angle (* 2 pi);(/ pi 6)
                        :speed '(15.0 . 24.0 ))
         300
         :lifetime '(1.3 . 2.4)
         :color +orange+
         :gravity +zero-vec+;(vec-neg4 +y-axis+)
         :grav-coeff 0.0
         :drag-coeff 8.0)

        #+disabled
        (create-particle-system 
                   (make-instance 'point-emitter
                                  :pos (make-point3 0.0 -1.7 0.0)
                                  :dir +y-axis+
                                  :up +y-axis+
                                  :angle (/ pi 6)
                                  :speed '(.05 . .13 ))
                   5
                   80
                   :lifetime '(5.0 . 7.0)
                   :color #(0.0 .3 1.0 2.0)
                   :force-fn
                   #'(lambda (vel dt)
                       +zero-vec+
                       ;(vec-neg3 (vec3+ vel +y-axis+))
                       )))

  #+disabled
  (add-ui-element
   (setf *test-ui* (make-instance 'ui-gauge
                                  :offset '(0 0)
                                  :size '(.05 .25)
                                  :orientation :up
                                  :max-value (max-particles *test-ps*)
                                  :current-value 0)))

  (format t "laser tex: ~a~%" *laser-tex*)
  #+disabled
  (when (>= *gl-version* 3.0)
    (init-deferred-renderer)))


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
  

  
  (gl:enable :texture-2d)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-color 0 0 0 0)
  (gl:enable :depth-test :cull-face)
  (gl:depth-func :lequal)


 
  (set-viewport *main-viewport*)
  (gl:matrix-mode :modelview)
  (gl:load-identity)

  (gl:enable :lighting)
  (gl:enable :light0)
  (gl:enable :rescale-normal)

  )



;;;
;;; Render updates
;;;

(defun update-graphics (entities time)
  (when *main-cam*
    (setf home-sector (lookup-sector (current-sector *main-cam*)))  
    (setf *cam-view-matrix* (look-dir-matrix (pos *main-cam*)
                                             (dir *main-cam*)
                                             (up  *main-cam*)))
    (setf *cam-inv-view-matrix* (rt-inverse *cam-view-matrix*))
    (update-billboarder (pos *main-cam*)
                        (dir *main-cam*)
                        (up *main-cam*)
                        +y-axis+))
  
  (update-planes (view-frustum *main-viewport*)
                 *cam-view-matrix*)

  (when *test-ps*
    #+disabled
    (update-ui-element *test-ui* (num-alive *test-ps*))
    (update-ps *test-ps* time))

  (when *laser-ps*
    (update-ps *laser-ps* time))
  
  #+disabled
  (when animated
    (update-model animated time))

  ;#+disabled
  (when *test-skele*
    (update-model *test-skele* time))

  (iter (for e in entities)
        (with-slots (shape) e
          (when shape
            (update-model shape time)))))



;;;
;;; Render frame
;;;


#+disabled
(defun render-frame (entities)
  (gl:enable :depth-test :lighting)
  (gl:depth-mask t)
  (gl:depth-func :lequal)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:cull-face :back)
  
  (use-light *main-light* :light0)

  
  
; (set-viewport *main-viewport*)
 ;#+disabled
  (when *main-cam*
    (let ((depth-buffer (get-attachment 
                          (view-fbo (light-viewport *main-light*))
                          :depth-attachment-ext)))


       (enable-shader *depth-shader*)
        (gl:uniformi (gl:get-uniform-location (program *depth-shader*) "tex")
                   3)

        ;(unbind-framebuffer)
        (gl:depth-mask t)
        (gl:blend-func :src-alpha :one-minus-src-alpha)
        (gl:viewport 0 0 960 720)
        (gl:clear :color-buffer-bit :depth-buffer-bit)
        (enable-shader *depth-shader*)
       ; (disable-shader)
        (gl:disable :lighting)
        (gl:matrix-mode :projection)
        (gl:load-identity)
        (gl:ortho 0 1 0 1 -10 10)
        (gl:matrix-mode :modelview)
        (gl:load-identity)
        (gl:active-texture :texture3)
        (gl:enable :texture-2d)
        (gl:bind-texture :texture-2d depth-buffer)
  ;      (gl:generate-mipmap-ext :texture-2d)
        (gl:color 1 1 1)
        (draw-screen-quad)
        (gl:bind-texture :texture-2d 0)))
  (gl:flush)
  (sdl:update-display))


