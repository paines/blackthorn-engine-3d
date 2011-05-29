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

(defvar *normal-buffer* nil)
(defvar *diffuse-buffer* nil)
(defvar *spec-buffer* nil)

(defun setup-z-pass (screensize)
  ;; Gen the framebuffer object
  (let ((framebuffer (car (gl:gen-framebuffers-ext 1)))
        (normal-texture (car (gl:gen-textures 1))))
    
    ;; setup framebuffer
    (gl:bind-framebuffer-ext :framebuffer-ext framebuffer)
    
    ;; Setup the texture
    (gl:bind-texture :texture-2d normal-texture)
    (gl:tex-parameter :texture-2d :texture-min-filter :linear)
    (gl:tex-parameter :texture-2d :texture-mag-filter :linear)
    (gl:tex-image-2d :texture-2d 0 :rgb8 
                     (first screensize) (second screensize) 0
                     :unsigned-byte (cffi:null-pointer))
    (gl:bind-texture :texture-2d 0)

    ;; attach the texture to the framebuffer
    (gl:framebuffer-texture-2d-ext :framebuffer-ext
                                   :color-attachment0-ext
                                   :texture-2d
                                   texture
                                   0)
  )