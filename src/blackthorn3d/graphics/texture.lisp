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

;;;
;;; All things textures
;;;


(defun gen-texture ()
  (car (gl:gen-textures 1)))

;; Returns an sdl:surface of the image
(defun load-image (filename)
  (sdl-image:load-image (blt3d-res:resolve-resource filename)))

(defun image->texture2d (image &key
                         (min-filter :linear) 
                         (mag-filter :linear)
                         (wrap-s :repeat)
                         (wrap-t :repeat))  
  (sdl:with-pixel (pix (sdl:fp image))
    (let ((tex-id (gen-texture))
          (image-fmt (ecase (sdl:pixel-bpp pix)
                       (3 :rgb)
                       (4 :rgba)))
          (width (sdl:width image))
          (height (sdl:height image)))

      ;; make sure the image is valid format
      (assert (and (= (sdl:pixel-pitch pix)
                      (* (sdl:width image) (sdl:pixel-bpp pix)))
                   (zerop (rem (sdl:pixel-pitch pix) 4))))
      (gl:bind-texture :texture-2d tex-id)
      (gl:tex-parameter :texture-2d :texture-min-filter min-filter)
      (gl:tex-parameter :texture-2d :texture-mag-filter mag-filter)
      (gl:tex-image-2d :texture-2d 0 :rgba 
                       width height 0 
                       image-fmt :unsigned-byte 
                       (sdl:pixel-data pix))
      (gl:bind-texture :texture-2d 0)
      tex-id)))