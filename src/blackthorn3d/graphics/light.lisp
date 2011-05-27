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

(defvar *shadow-shader* nil)
(defvar *shadow-map-width* 2048)
(defvar *shadow-map-height* 2048)


(defun light-init ()
  (setf *shadow-shader* 
        (make-shader (blt3d-res:file-contents
                      (blt3d-res:resolve-resource 
                       #p "res/shaders/shadow-map.vert"))
                     (blt3d-res:file-contents
                      (blt3d-res:resolve-resource
                       #p "res/shaders/shadow-map.frag")))))


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
    :initarg :viewport)
   (texture-matrix
    :accessor light-texmat
    :initarg :texture-matrix)))

(defun make-light (&key position 
                   (diffuse +white+) 
                   (ambient +black+))
  (make-instance 'light
                 :position position
                 :diffuse diffuse
                 :ambient ambient
                 :viewport
                 (create-viewport
                  (list *shadow-map-width* *shadow-map-height*)
                  0.2 200
                  :aspect-ratio 1.0
                  :framebuffer (create-shadow-framebuffer
                                *shadow-map-width* *shadow-map-height*))))

(defmethod use-light ((this light) gl-light)
  (gl:light gl-light :position (light-pos this))
  (gl:light gl-light :diffuse (light-diff this))
  (gl:light gl-light :ambient (light-amb this))
  (gl:light gl-light :linear-attenuation 0.5))


(defvar *bias-matrix* 
  (make-matrix4x4
   '((0.5 0.0 0.0 0.0)
     (0.0 0.5 0.0 0.0)
     (0.0 0.0 0.5 0.0)
     (0.5 0.5 0.5 1.0))))

;; The texture matrix is the product:
;; BIAS * PROJ * MODELVIEW
(defmethod set-texture-matrix ((this light))
  (with-slots (texture-matrix framebuffer) this
    (gl:with-pushed-attrib (:transform-bit)
      (gl:matrix-mode :texture)
      (gl:load-matrix *bias-matrix*)
      )))

#+disabled
(defmethod shadow-pass ((this light) objects)
  (with-slots (light-viewport position direction) this
    (let* ((light-view-matrix 
            (matrix-multiple-m (make-translate 
                                (- (x position))
                                (- (y position))
                                (- (z position)))
                               (quat->matrix
                                (quat-rotate-to-vec
                                 direction
                                 (vec-neg4 +z-axis+)))))
           (light-inv-view-matrix
            (rt-inverse light-view-matrix))))

    ;; first load the viewport (and by extension the framebuffer)
    (set-viewport light-viewport)

    ;; Then bind the shader
    (enable-shader *shadow-shader*)
    
    ;; Clear the sturf
    (gl:clear :color-buffer-bit :depth-buffer-bit)

    ;; Load the light matrix
    (gl:load-matrix light-view-matrix)
   
    ;; only render back faces
  ;  (gl:cull-face :front)
    
    ;; now we can has draw?
    (let ((*use-shadow-shader* t))
      (iter (for obj in objects)
            (draw-object obj)))

    ;; generate mipmaps
    (gl:generate-mipmap-ext :texture-2d)

    ;; Setup texture matrix
    
    ;; display the depth buffer
    (let ((depth-texture (get-attachment 
                          (view-fbo light-viewport) 
                          :depth-attachment-ext)))
      (format t "")
      (unbind-framebuffer)
      (disable-shader)
      (gl:viewport 0 0 960 720)
      (gl:clear :color-buffer-bit :depth-buffer-bit)
      (gl:matrix-mode :projection)
      (gl:load-identity)
      (gl:ortho 0 1 0 1 -10 10)
      (gl:matrix-mode :modelview)
      (gl:load-identity)
      (gl:color 1 1 1)
      (gl:active-texture :texture0)
      (use-texture depth-texture)
      (gl:enable :texture-2d)
      (draw-screen-quad))))