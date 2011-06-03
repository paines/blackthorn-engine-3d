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
(defvar *beast* nil)
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

  (make-main-viewport (list *screen-width* *screen-height*))  


  (setf *main-light* (make-light 'light
                                 :position (make-point3 0.0 2.0 0.0)))
  
  #+disabled
  (add-ui-element
   (setf *test-ui* (make-instance 'ui-gauge
                                  :offset '(0 0)
                                  :size '(.05 .25)
                                  :orientation :up
                                  :max-value (max-particles *test-ps*)
                                  :current-value 0)))

  (add-ui-element
   (make-instance 'ui-bitmap
                  :texture *crosshair-tex*
                  :offset '(0.5 0.5)
                  :size '(0.025 0.025)))

  #+disabled
  (when (>= *gl-version* 3.0)
    (init-deferred-renderer))
  

  (gl:enable :blend :rescale-normal)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-color 0 0 0 0)
  (gl:enable :depth-test :cull-face)
  (gl:depth-func :lequal))

(defun set-viewport-size (width height)
  (setf *screen-width* width
        *screen-height* height)
  (make-main-viewport (list width height)))

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
  

                                        ; #+disabed
 
  (gl:enable :texture-2d)
  
  (set-viewport *main-viewport*)
  (gl:matrix-mode :modelview)
  (gl:load-identity)

  (gl:enable :lighting)
  (gl:enable :light0)
  (gl:enable :rescale-normal))

(defun make-main-viewport (size)
  (setf *main-viewport* (create-viewport size 0.2 200)))

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
    (client-update *test-ps* time))

  (when *laser-ps*
    (client-update *laser-ps* time))
  
                                        
  (when *test-skele*
    (update-model *test-skele* time))
  (when *beast*
  ;  (format t "beasts' animations ~a~%" (animations *beast*))
    (update-model *beast* time))

  (update-effects time)

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


