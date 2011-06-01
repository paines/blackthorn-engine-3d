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



(defun render-frame (entities)
  (gl:color-material :front :diffuse)
  (gl:enable :color-material)
  
  (if (>= *gl-version* 3.0)
      (old-render-frame entities)
;      (dr-render-frame entities)
      (old-render-frame entities)))


(defun old-render-frame (entities)

  (gl:enable :depth-test :lighting)
  (gl:depth-mask t)
  (gl:depth-func :lequal)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:cull-face :back)

  ;; Create PVS from entities and level
  ;; (let ((PVS (find-pvs entities level))))
  
  ;; create shadow map
  #+disabled
  (when (and *main-cam* (> *gl-version* 3.0))
    (shadow-pass *main-light* 
                 (cons home-sector 
                   ;(cons *test-skele*)
                       (remove-if-not
                        #'(lambda (e)
                            (and (shape e) 
                                 (not (eql e *main-cam*)))) 
                        entities))))   

  (gl:clear :color-buffer-bit :depth-buffer-bit)
  (set-viewport *main-viewport*)

  (when *main-cam*
    (progn
      (gl:matrix-mode :modelview)
      (with-slots (position direction) *main-light*
        (gl:load-matrix *cam-view-matrix*))))

  (use-light *main-light* :light0)

  (gl:active-texture :texture0)
 ; (gl:color-material :front :diffuse)
  (gl:enable :texture-2d)
  (gl:color 1 1 1 1)

 
  #+disabled
  (when *shadow-depth-tex*
    (enable-shader *standard-tex*)
    (gl:uniformi (gl:get-uniform-location (program *standard-tex*) "shadow")
                   3)
    (set-texture-matrix *main-light*)
    (gl:active-texture :texture3)
    (gl:bind-texture :texture-2d *shadow-depth-tex*)
    (gl:active-texture :texture0))

  ;#+disabled    
  (when home-sector
    (gl:with-pushed-matrix
        (draw-object home-sector)))

  ;#+disabled    
  (when *test-skele*
    (gl:with-pushed-matrix
        (draw-object *test-skele*)))
  (draw-object *beast*)
  #+disabled
  (when *beast*
    (gl:with-pushed-matrix
        (draw-object *beast*)))

  ;#+disabled    sssssssssss
  (dolist (e entities)
    (when (and (shape e) (not (eql e *main-cam*)))
      (draw-object e)))

  ;; DO PARTICLES YEAH!
  (gl:blend-func :src-alpha :one)  
  (gl:depth-mask nil)

  (draw-object *test-ps*)
  (draw-object *laser-ps*)

  ;; test lazor
 ; #+disabled
  (blt3d-gfx::draw-beam +origin+ (make-point3 15.0 0.0 0.0)
                       #(0.0 0.4 1.0 1.0) 
                       *laser-tex* #(0.5 0.5))

  ;;(disable-shader)
  ;; Lastly render the ui
  ;; (render-ui)

  (gl:flush)
  (sdl:update-display))