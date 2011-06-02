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

(in-package :cl-user)

(defpackage :blackthorn3d-graphics
  (:nicknames :blt3d-gfx)
  (:use :iter :cl :blt3d-utils :alexandria
        :blt3d-math :blt3d-phy :blt3d-ent :blt3d-ani :blt3d-sec)
  (:import-from :blt3d-ent :entity)
  (:export

   ;; init.lisp
   :init-gfx
   :*gl-version*
   :*glsl-version*
   :*cam-view-matrix*
   :*cam-inv-view-matrix*
   :*depth-shader*
   :*standard-tex*
   :*screen-height*
   :*screen-width*
   :*disable-shading*
   :*laser-tex*

   ;; draw.lisp
   :draw-triangle
   :draw-cube
   :make-cube
   :draw-vert-array
   :make-vao-cube
   :draw-vao-cube
   :gfx-init
   :gfx-draw
   :draw-object
   :draw-screen-quad

   ;; frustum.lisp
   :viewport
   :create-viewport
   :set-viewport
   :view-frustum
   :frustum
   :frstm-near
   :frstm-far
   :make-frstm
   :load-frstm
   :cull-sphere
   :update-planes
   :view-fbo
   :view-fov
   :view-ratio


   ;; texture.lisp
   
   :+white+
   :+black+
   :+red+
   :+green+
   :+blue+
   :+orange+
   :+aqua+ 
   :load-image
   :image->texture2d
   :create-texture
   :use-texture

   ;; framebuffer.lisp
   :framebuffer
   :fbo
   :make-framebuffer
   :attach-texture
   :attachments
   :get-attachment
   :depth-attachment
   :bind-framebuffer
   :unbind-framebuffer
   :with-framebuffer
   :check-framebuffer-status

   ;; mesh.lisp
   :mesh
   :mesh-elements
   
   ;; model.lisp
   :model-shape
   :load-obj->models
   :controller
   :update-model

   ;; light.lisp
   :light
   :light-viewport
   :light-texmat
   :use-light
   :make-light
   :position
   :direction
   :set-texture-matrix
   :shadow-pass
   :*shadow-depth-tex*
   :*shadow-texture-unit*

   ;; shader.lisp
   :*shader*
   :program
   :make-shader
   :enable-shader
   :disable-shader
   :get-uniform-loc

   ;; particles.lisp
   :particle-system
   :num-alive
   :max-particles
   :client-update
   :render-ps
   :create-particle-system
   :create-explosion-ps
   :create-spark-ps
   :point-emitter  
   
   ;; billboards.lisp
   :update-billboarder
   :draw-billboard-quad

   ;; ui.lisp
   :ui-element
   :ui-gauge
   :update-ui-element
   :draw-ui-element
   :render-ui
   :add-ui-element
   ))
        
        
        