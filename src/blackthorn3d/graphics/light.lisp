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

(defvar *shadow-map-width* 2048)
(defvar *shadow-map-height* 2048)
(defvar *shadow-texture-unit* '(:texture5 . 5))
(defvar *shadow-depth-tex* nil)


(defun light-init ()
  (setf *standard-shadow*
        (make-shader (blt3d-res:file-contents
                      (blt3d-res:resolve-resource 
                       #p "res/shaders/shadow-map.vert"))
                     (blt3d-res:file-contents
                      (blt3d-res:resolve-resource
                       #p "res/shaders/shadow-map.frag"))))

  (setf *skinned-shadow*
        (make-shader (blt3d-res:file-contents
                      (blt3d-res:resolve-resource 
                       #p "res/shaders/skin-shadow-map.vert"))
                     (blt3d-res:file-contents
                      (blt3d-res:resolve-resource
                       #p "res/shaders/shadow-map.frag"))
                     :attributes '("jointIndices" "jointWeights")
                     :uniforms '("jointMats"))))


(defclass light ()
  ((position
    :accessor light-pos
    :initarg :position
    :initform +origin+)
   (direction
    :accessor light-dir
    :initarg :direction
    :initform (vec-neg4 +y-axis+))
   (diffuse
    :accessor light-diff
    :initarg :diffuse
    :initform +white+)
   (ambient
    :accessor light-amb
    :initarg :ambient
    :initform +black+)
   (attenuation
    :accessor light-att
    :initarg :attenuation
    :initform 0)
   (light-viewport
    :accessor light-viewport
    :initarg :viewport
    :initform
    (create-viewport
     (list *shadow-map-width* *shadow-map-height*)
     0.1 100
     :aspect-ratio 1.0
     :framebuffer (create-shadow-framebuffer
                   *shadow-map-width* *shadow-map-height*)))
   (texture-matrix
    :accessor light-texmat
    :initarg :texture-matrix)))


(defclass spot-light (light)
  ((spot-expt
    :accessor light-spot-expt
    :initarg :exponent
    :initform 1.0)
   (spot-cutoff
    :accessor light-spot-cutoff
    :initarg :cutoff
    :initform 45)))


(defun make-light (type &rest keys)
  (apply
   #'make-instance type keys))

(defmethod use-light ((this light) gl-light)
  (gl:light gl-light :position (light-pos this))
  (gl:light gl-light :diffuse (light-diff this))
  (gl:light gl-light :ambient (light-amb this))
  (gl:light gl-light :linear-attenuation 0.5))

(defmethod use-light :after ((this spot-light) gl-light)
  (gl:light gl-light :spot-direction (light-dir this))
  (gl:light gl-light :spot-cutoff (light-spot-cutoff this))
  (gl:light gl-light :spot-exponent (light-spot-ext this)))


(defvar *bias-matrix* 
  (make-matrix4x4
   '((0.5 0.0 0.0 0.0)
     (0.0 0.5 0.0 0.0)
     (0.0 0.0 0.5 0.0)
     (0.5 0.5 0.5 1.0))))

(defun set-texture-matrix (light)
  (with-slots (texture-matrix) light
    (gl:with-pushed-attrib (:transform-bit)
      (gl:matrix-mode :texture)
      (gl:active-texture :texture5)
      (gl:load-matrix texture-matrix)
      (gl:active-texture :texture0))))

(defmethod shadow-pass ((this light) objects)
  (with-slots (light-viewport position direction texture-matrix) this
    (let* ((light-view-matrix 
            (look-dir-matrix position direction +x-axis+))
           (light-proj-matrix
            (proj-matrix (view-frustum light-viewport))))

      ;; first load the viewport (and by extension the framebuffer)
      (set-viewport light-viewport)

      ;; Clear the sturf
      (gl:clear :color-buffer-bit :depth-buffer-bit)

      (gl:matrix-mode :modelview)

      ;; only render back faces
    ;  (gl:cull-face :front)
    ;  (gl:front-face :cw)
      
      ;; Load the light matrix
      (gl:load-matrix light-view-matrix)
      
      ;; now we can has draw?
      ;#+disabled
      (let ((mesh-shader *standard-shadow*)
            (skin-shader *skinned-shadow*))
        (iter (for obj in objects)
              (draw-object obj)))

       ;; generate mipmaps
 ;     (gl:generate-mipmap-ext :texture-2d)
      
      (unbind-framebuffer)
      (gl:bind-framebuffer-ext :framebuffer-ext 0)

      ;; Setup texture matrix
      ;; The texture matrix is the product:
      ;; BIAS * PROJ * VIEW * cam-inverse
      ;#+disabled
      (progn
        (setf texture-matrix
             (matrix-multiply-m
              *bias-matrix*
              (matrix-multiply-m
               light-proj-matrix
               (matrix-multiply-m
                light-view-matrix
                *cam-inv-view-matrix*))))
 
        (disable-shader))

      #+disabled
      (progn
        (gl:active-texture (car *shadow-texture-unit*))

        (gl:enable :texture-2d)
        (gl:matrix-mode :texture)
        (gl:load-identity)
        (gl:active-texture :texture0)
        (gl:load-identity)
        (gl:load-matrix *bias-matrix*)
        (gl:mult-matrix light-proj-matrix)
        (gl:mult-matrix light-view-matrix)
        (gl:mult-matrix *cam-inv-view-matrix*)
        (gl:matrix-mode :modelview))

      ;; Bind the depth buffer
      ;#+disabled
      (let ((depth-texture (get-attachment (view-fbo light-viewport)
                                           :depth-attachment-ext)))
        ;;  (gl:active-texture :texture5)
        ;;  (gl:enable :texture-2d)
        ;;  (gl:bind-texture :texture-2d depth-texture)
        ;(gl:bind-texture :texture-2d depth-texture)
        (setf *shadow-depth-tex* depth-texture))

;      (gl:active-texture :texture0)


      ;; display the depth buffer
      #+disabled
      (let ((depth-buffer (get-attachment (view-fbo light-viewport)
                                          ;:color-attachment0-ext
                                          :depth-attachment-ext
                                          )))
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
        (gl:bind-texture :texture-2d 0)))))