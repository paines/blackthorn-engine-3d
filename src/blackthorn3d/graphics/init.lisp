;;; Blackthorn -- Lisp Game Engine
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

(defvar *gl-version*)
(defvar *glsl-version*)

;; Tells draw methods to use their shadow vertex shader
(defvar *use-shadow-shader* nil)
(defvar *cam-view-matrix* (make-identity-matrix))
(defvar *default-texture* nil)
(defvar *tex-loc* nil)


(defvar *standard-texture* nil)
(defvar *standard-color* nil)
(defvar *skinned-texture* nil)
(defvar *skinned-color* nil)

(defun init-gfx ()
  (format t "Initializing Graphics subsystem~%")
  (setf %gl:*gl-get-proc-address* #'sdl:sdl-gl-get-proc-address)
  (setf *gl-version* (gl:major-version))
  (setf *glsl-version* (gl:minor-version))

  (setf *particle-tex*
        (image->texture2d (load-image #p"res/images/round-particle1.png")))

  (setf *default-texture*
        (image->texture2d (load-image #p "res/images/MetalAircraft2.jpg")))

  ;; Init Skin Shader(s)
  (setf skin-shader
        (make-shader (blt3d-res:file-contents
                      (blt3d-res:resolve-resource 
                       #p "res/shaders/skin-shader.vert"))
                     (blt3d-res:file-contents
                      (blt3d-res:resolve-resource
                       #p "res/shaders/texture-std.frag"))))

   (setf joint-indices-loc 
        (gl:get-attrib-location skin-shader "jointIndices")
        
        joint-weights-loc
        (gl:get-attrib-location skin-shader "jointWeights")
        
        joint-mats-loc
        (gl:get-uniform-location skin-shader "jointMats"))

  (setf *standard-tex*
        (make-shader (blt3d-res:file-contents
                      (blt3d-res:resolve-resource 
                       #p "res/shaders/texture-std.vert"))
                     (blt3d-res:file-contents
                      (blt3d-res:resolve-resource
                       #p "res/shaders/texture-std.frag"))))

  (setf mesh-shader *standard-tex*)

  (setf *tex-loc*
        (gl:get-uniform-location *standard-tex* "tex"))

  (light-init)
  (billboard-init))