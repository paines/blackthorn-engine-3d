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

(defparameter vao-cube nil)
(defparameter shader nil)
(defparameter animated nil)


(defun init ()
  "Called to initialize the graphics subsystem"
  (format t "Initializing Rendering Subsystem~%")
  (init-gfx)

 
  (setf *main-viewport* (create-viewport '(960 720) 0.2 200))

  (format t "### LOADING CONTROLLER MODEL ###~%")
  (let ((scientist-model 
         (blt3d-imp:dae-geometry 
          (blt3d-imp:load-dae 
          ; #p "res/models/KatanaSpiderMaterialAnimated.dae"
           #p "res/models/player-3.dae")))
        (epic-sword
         (blt3d-imp:dae-geometry
          (blt3d-imp:load-dae
           #p "res/models/SwordTextured.dae"))))
    
    (setf *test-skele* (load-obj->models scientist-model))
    (setf *test-sword* (load-obj->models epic-sword))
    (apply-transform *test-sword* +3dsmax-convert+)
    (apply-transform *test-sword* (make-translate #(2.0 0.0 -22.0 1.0)))

    (attach-node-to-model (car (mesh-nodes *test-sword*))
                          "Bip003_R_Hand" *test-skele*)

    (apply-transform *test-skele* (make-scale #(0.008 0.008 0.008)))
    (apply-transform *test-skele*
                     (make-translate #(0.0 -2.0 0.0 0.0))))

  (setf *main-light* (make-light 'light
                      :position (make-point3 0.0 5.0 0.0)))

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
         :size 0.01
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
                        :speed '(8.0 . 16.0 ))
         300
         :lifetime 2.0
         :gravity (vec-neg4 +y-axis+)
         :grav-coeff 3.5
         :drag-coeff 4.0)

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

  (add-ui-element
   (setf *test-ui* (make-instance 'ui-gauge
                                  :offset '(0 0)
                                  :size '(.05 .25)
                                  :orientation :up
                                  :max-value (max-particles *test-ps*)
                                  :current-value 0)))

  (setf *test-fbo* (make-framebuffer))
  (setf *render-tex* 
        (create-texture 960 720 :rgba
                        :format :rgba))
  (setf *depth-tex*
        (create-texture 960 720 :depth-component
                        :min-filter :nearest
                        :mag-filter :nearest
                        :wrap-s :clamp
                        :wrap-t :clamp
                        :format :depth-component))

  (attach-texture *test-fbo* :color-attachment0-ext
                  *render-tex*)
  (attach-texture *test-fbo* :depth-attachment-ext
                  *depth-tex*)

  (with-framebuffer *test-fbo*
    (format t "Framebuffer Status: ~a~%"
            (gl::enum= 
             (gl:check-framebuffer-status-ext :framebuffer-ext) 
             :framebuffer-complete-ext))))


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
    (setf *cam-view-matrix* (look-dir-matrix (pos *main-cam*)
                                             (dir *main-cam*)
                                             (up  *main-cam*))))
  
  (update-planes (view-frustum *main-viewport*)
                 *cam-view-matrix*)


  (when *test-ps*
    (update-ui-element *test-ui* (num-alive *test-ps*))
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



;;;
;;; Render frame
;;;
(defvar home-sector nil)
(defun render-frame (entities)

  (gl:enable :depth-test :lighting)
  (gl:depth-mask t)
  (gl:depth-func :lequal)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:cull-face :back)

  ;; Test framebuffer
  
; (with-framebuffer *test-fbo*)
;  (bind-framebuffer *test-fbo*)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:enable :depth-test :lighting)
  (gl:depth-mask t)
  (gl:depth-func :lequal)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:cull-face :back)

  ;; Create PVS from entities and level
;  (let ((PVS (find-pvs entities level))))

  (set-viewport *main-viewport*)

  (when *main-cam*
    (setf home-sector (lookup-sector (current-sector *main-cam*)))
;    (setf home-sector (current-sector *main-cam*))
    (gl:matrix-mode :modelview)
    (gl:load-matrix (look-dir-matrix (pos *main-cam*)
                                     (dir *main-cam*)
                                     (up  *main-cam*)))

    (update-billboarder (pos *main-cam*)
                        (dir *main-cam*)
                        (up *main-cam*)
                        +y-axis+))

  (use-light *main-light* :light0)

  (gl:color-material :front :diffuse)
  (gl:enable :color-material)

    ;#+disabled    
  (when home-sector
    (gl:with-pushed-matrix
        (draw-object home-sector)))

    ;#+disabled    
  (when *test-skele*
    (gl:with-pushed-matrix
        (draw-object *test-skele*)))

    ;#+disabled    
  (dolist (e entities)
    (when (and (shape e) (not (eql e *main-cam*)))
      (draw-object e)))

  ;; DO PARTICLES YEAH!
  (gl:blend-func :src-alpha :one)  
  (gl:depth-mask nil)
  ;;  #+disabled
   (when *test-ps*
    (render-ps *test-ps*))

  ;; now render the texture
  #+disabled
  (progn
    (unbind-framebuffer)
    (gl:depth-mask t)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    (disable-shader)
    (gl:disable :lighting)

    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:ortho 0 1 0 1 -10 10)
    (gl:matrix-mode :modelview)
    (gl:load-identity)
    (gl:enable :texture-2d)
    (gl:bind-texture :texture-2d *render-tex*)
    (gl:generate-mipmap-ext :texture-2d)
    (gl:color 1 1 1)
    (draw-screen-quad)
    (gl:bind-texture :texture-2d 0))

  ;; Lastly render the ui
  (render-ui)

  (gl:flush)
  (sdl:update-display))