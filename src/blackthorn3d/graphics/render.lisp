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
(defvar *frustum* nil)
(defvar *main-light* nil)

(defparameter vao-cube nil)
(defparameter shader nil)

(defun init ()
  "Called to initialize the graphics subsystem"
  (setf %gl:*gl-get-proc-address* #'sdl:sdl-gl-get-proc-address)
 
  (setf *main-light* (make-instance 'light
                                    :position (make-point3 0.0 10.0 0.0)))
  (setf *frustum* (make-frstm 1.0 1000.0 8/6 (/ pi 2))))


(defun set-camera (cam)
  (setf *main-cam* cam))

(defun prepare-scene ()
  "Called after sdl is initialized, before first frame is drawn
   or when changing the 'scene' settings"
  (gl:viewport 0 0 800 600)

  (gl:enable :texture-2d)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:clear-color 0 0 0 0)
  (gl:enable :depth-test)
  (gl:depth-func :lequal)

  (setf cube-tex (image->texture2d 
                  (load-image #p"res/images/test-tex.png")))
  (setf cube-mat (make-instance 'material
                                :ambient #(.5 .38 0.0 1.0)
                                :diffuse #(1.0 .75 0.0 1.0)
                                :tex cube-tex))

  (load-frstm *frustum*)
  (gl:load-identity)

  ;(gl:light :light0 :position '(20.0 20.0 20.0 1.0))
  ;(gl:light :light0 :diffuse (make-vec3 1.0 1.0 1.0))
  (gl:enable :lighting)
  (gl:enable :light0)
  (gl:enable :rescale-normal)

  #+disabled
  (setf shader (make-shader (blt3d-res:file-contents
                             (blt3d-res:resolve-resource 
                              #p "res/shaders/FinalProjShader.vert"))
                            (blt3d-res:file-contents
                             (blt3d-res:resolve-resource
                              #p "res/shaders/FinalProjShader.frag"))))
  
  ;(make-vao-cube)
  )


(defun render-frame (entities)
  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (when *main-cam*
    (gl:load-matrix (look-dir-matrix (pos *main-cam*)
                                     (dir *main-cam*)
                                     (up  *main-cam*)))
    ())
  ;(gl:light :light0 :position '(6.0 6.0 6.0 1.0))   
  (init-light *main-light* :light0)

  (gl:color-material :front :diffuse)
  (gl:enable :color-material)
  (gl:use-program 0)
 
  (dolist (e entities)
    (when (shape e)
      (with-slots (pos dir up shape) e
        (let ((z-axis (cross dir up)))
          (gl:color 1.0 0.1 0.1)
          (draw-plane 20)
          (gl:color 0.0 1.0 1.0)
          (gl:with-pushed-matrix
            (gl:translate (x pos) (y pos) (z pos))
            (gl:scale .1 .1 .1)
            (gl:mult-matrix (make-inv-ortho-basis dir up z-axis))
            (draw-object shape))))))

  (gl:flush)
  (sdl:update-display))