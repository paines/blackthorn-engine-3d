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

(defvar pre-z-fbo nil)
(defvar light-pass-fbo nil)
(defvar dr-test-fbo nil)

(defvar *pre-z-shader* nil)
(defvar *light-pass-shader* nil)
(defvar *forward-shader* nil)

(defvar *depth-buffer* nil)
(defvar *normal-buffer* nil)
(defvar *diffuse-buffer* nil)
(defvar *spec-buffer* nil)

(defvar *render-list* nil)


;;;
;;; Initialization
;;;

(defun create-pre-z-fbo ()
  (let ((framebuffer (make-framebuffer)))
    (setf *depth-buffer* 
          (create-texture *screen-width* *screen-height*
                          :depth-component
                          :min-filter :nearest
                          :mag-filter :nearest
                          :wrap-s :clamp-to-edge
                          :wrap-t :clamp-to-edge
                          :format :depth-component))
    (setf *normal-buffer*
          (create-texture *screen-width* *screen-height*
                          :rgba16f
                          :min-filter :nearest
                          :mag-filter :nearest
                          :wrap-s :clamp
                          :wrap-t :clamp
                          :format :rgba
                          :type :float))
    (gl:bind-framebuffer-ext :framebuffer-ext (fbo framebuffer))

    ;; Attach the depth buffer
    (gl:framebuffer-texture-2d-ext 
     :framebuffer-ext :depth-attachment-ext 
     :texture-2d *depth-buffer* 0)
    (setf (depth-attachment framebuffer) *depth-buffer*)

    ;; Attach the normal buffer
    (gl:framebuffer-texture-2d-ext
     :framebuffer-ext :color-attachment0-ext 
     :texture-2d *normal-buffer* 0)
    (push (list :color-attachment0-ext *normal-buffer*) 
          (attachments framebuffer))

    (when (not (check-framebuffer-status))
      (format t "#### PRE-Z FBO ERROR!~%"))

    (gl:bind-framebuffer-ext :framebuffer-ext 0)
    (setf pre-z-fbo framebuffer)))



(defun create-light-pass-fbo ()
  (let ((framebuffer (make-framebuffer)))
    (setf *diffuse-buffer*
          (create-texture *screen-width* *screen-height*
                          :rgba
                          :min-filter :nearest
                          :mag-filter :nearest
                          :format :rgba))

    (gl:bind-framebuffer-ext :framebuffer-ext (fbo framebuffer))

    ;; Attach the diffuse buffer
    (gl:framebuffer-texture-2d-ext 
     :framebuffer-ext :color-attachment0-ext 
     :texture-2d *diffuse-buffer* 0)
    (push (list :color-attachment0-ext *diffuse-buffer*) 
          (attachments framebuffer))

    (when (not (check-framebuffer-status))
      (format t "#### LIGHT-PASS FBO ERROR!~%"))
    
    (gl:bind-framebuffer-ext :framebuffer-ext 0)
    (setf light-pass-fbo framebuffer)))



(defun init-deferred-renderer ()
  ;; Load shaders
  (format t "~%Loading pre-z-shader: #####################~%")
  (setf *pre-z-shader*
        (make-shader (blt3d-res:file-contents
                      (blt3d-res:resolve-resource 
                       #p "res/shaders/DR/pre-z-pass.vert"))
                     (blt3d-res:file-contents
                      (blt3d-res:resolve-resource
                       #p "res/shaders/DR/pre-z-pass.frag"))))

  (format t "~%Loading light-pass-shader: ################~%")
  (setf *light-pass-shader*
        (make-shader (blt3d-res:file-contents
                      (blt3d-res:resolve-resource 
                       #p "res/shaders/DR/light-pass-point.vert"))
                     (blt3d-res:file-contents
                      (blt3d-res:resolve-resource
                       #p "res/shaders/DR/light-pass-point.frag"))
                     :uniforms '("depthTex" "normalTex"
                                 "znear" "zfar")))

  (format t "~%Loading forward-pass-shader: ##############~%")
  (setf *forward-pass-shader*
        (make-shader (blt3d-res:file-contents
                      (blt3d-res:resolve-resource 
                       #p "res/shaders/DR/forward-pass.vert"))
                     (blt3d-res:file-contents
                      (blt3d-res:resolve-resource
                       #p "res/shaders/DR/forward-pass.frag"))
                     :uniforms '("normals")))

  ;; Init FBOs
  (create-pre-z-fbo)
  (create-light-pass-fbo))

;;;
;;; Rendering functions.  
;;;    These all assume that the viewport is correctly
;;;    setup.
;;;

(defun do-z-pass ()
  (gl:bind-framebuffer-ext :framebuffer-ext (fbo pre-z-fbo))
  (gl:enable :depth-test)
  (gl:depth-mask t)
  (gl:depth-func :lequal)

  (gl:disable :blend)

  (gl:clear :color-buffer-bit  :depth-buffer-bit)
   
  (let ((*disable-shading* t))
    (enable-shader *pre-z-shader*)
    
    ;; Draw objects
    (iter (for obj in *render-list*)
          (draw-object obj)))

  (gl:bind-framebuffer-ext :framebuffer-ext 0))



(defun do-light-pass (lights)
  (gl:bind-framebuffer-ext :framebuffer-ext (fbo light-pass-fbo))
  (gl:disable :depth-test)
  (gl:depth-mask nil)

  (gl:clear :color-buffer-bit)

  (gl:enable :blend)
  (gl:blend-func :src-alpha :one)

  (enable-shader *light-pass-shader*)

  (gl:uniformi (gl:get-uniform-location 
                (program *light-pass-shader*)
                "depthTex") 0)
  (gl:uniformi (gl:get-uniform-location 
                (program *light-pass-shader*)
                "normalTex") 1)
  (gl:uniformf (gl:get-uniform-location 
                (program *light-pass-shader*)
                "znear")
               0.2)
  (gl:uniformf (gl:get-uniform-location 
                (program *light-pass-shader*)
                "zfar")
               200.0)
  
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d *depth-buffer*)

  (gl:active-texture :texture1)
  (gl:bind-texture :texture-2d *normal-buffer*)

  ;#+disabled
  (progn
    (gl:matrix-mode :projection)
    (gl:load-identity)
    (gl:ortho 0 1 0 1 -10 10))
  (gl:matrix-mode :modelview)
  (gl:load-identity)

  (iter (for light in lights)
        (use-light light :light0)
        (let* ((z-near (frstm-near (view-frustum *main-viewport*)))
               (world-width (* z-near
                               (tan (* 0.5 (view-fov *main-viewport*)))))
               (world-height (/ world-width (view-ratio *main-viewport*))))
          (setf world-width .2)
          (setf world-height .2)
    
          (draw-screen-quad)
          #+disabled
          (gl:with-primitives :quads
            (gl:vertex (- world-width) (- world-height) (- z-near))
            (gl:vertex    world-width  (- world-height) (- z-near))
            (gl:vertex    world-width     world-height  (- z-near))
            (gl:vertex (- world-width)    world-height  (- z-near)))))

  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d 0)
  
  (gl:active-texture :texture1)
  (gl:bind-texture :texture-2d 0)

  (gl:bind-framebuffer-ext :framebuffer-ext 0))


(defun do-forward-pass ()
  ;; Change this to use the previous z-buffer!
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (gl:enable :depth-test :blend)
  (gl:depth-mask t)
  (gl:depth-func :lequal)
  (gl:blend-func :src-alpha :one-minus-src-alpha)

  (enable-shader *forward-pass-shader*)
  
  (gl:uniformi (gl:get-uniform-location
                (program *forward-pass-shader*)
                "normals")
               1)
  (gl:active-texture :texture1)
  (gl:bind-texture :texture-2d *normal-buffer*)

  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d 0)

  (let ((mesh-shader *forward-pass-shader*))
    (iter (for obj in *render-list*)
          (draw-object obj))))





(defun dr-render-frame (entities)
  (gl:cull-face :back)
  (gl:clear-color 0.0 0.0 0.0 0.0)
  (gl:clear-depth 1.0)

  (set-viewport *main-viewport*)
  
  (when *main-cam*
    (gl:matrix-mode :modelview)
    (with-slots (position direction) *main-light*
      (gl:load-identity)
      (gl:load-matrix *cam-view-matrix*)))

  (setf *render-list* 
        (remove-if 
         #'null
         (cons home-sector
               (remove-if-not 
                #'(lambda (x) 
                    (and (shape x) (not (eql *main-cam* x))))
                entities))))

  (do-z-pass)
  (do-light-pass (list *main-light*))
  ;(do-forward-pass)

  ;; draw the buffers
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (disable-shader)
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 2 0 2 -10 10)
  (gl:matrix-mode :modelview)
  (gl:load-identity)

  (gl:color 1 1 1)
 ; (enable-shader *depth-shader*)
 ; (gl:uniformi (gl:get-uniform-location (program *depth-shader*) "tex") 0)
  (gl:enable :texture-2d)
  (gl:disable :lighting)
  (gl:active-texture :texture0)
  (gl:bind-texture :texture-2d *depth-buffer*)
  (draw-screen-quad)
  
 ;(disable-shader)
 ;(gl:enable :texture-2d)
  (gl:translate 1 0 0)
  (gl:bind-texture :texture-2d *normal-buffer*)
  (draw-screen-quad)
  
  (gl:translate 0 1 0)
  (gl:bind-texture :texture-2d *diffuse-buffer*)
  (draw-screen-quad)

  (gl:flush)
  (sdl:update-display))