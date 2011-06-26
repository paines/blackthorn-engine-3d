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
;;; billboards, woot
;;;

;; The current eye position and related data
(defvar *bill-screen-normal* (vec-neg4 +z-axis+))
(defvar *bill-eye-pos* +origin+)
(defvar *bill-eye-up* +y-axis+)
(defvar *bill-eye-right* +x-axis+)
(defvar *bill-world-up* +y-axis+)

(defvar *billboard-shader* nil)
(defvar *billboard-world-shader* nil)
(defvar *billboard-vel-shader* nil)
(defvar *right-loc* nil)
(defvar *up-loc* nil)
(defvar *size1-loc* nil)
(defvar *size2-loc* nil)


(defun billboard-init ()

  (format t "Loading billboard-shader:~%")
  (setf *billboard-shader*
        (make-shader (blt3d-res:file-contents
                      (blt3d-res:resolve-resource
                       #p "res/shaders/billboard-shader.vert"))
                     (blt3d-res:file-contents
                      (blt3d-res:resolve-resource
                       #p "res/shaders/billboard-shader.frag"))
                     :uniforms '("size")))

  (format t "Loading billboard-world-shader:~%")
  (setf *billboard-world-shader*
        (make-shader (blt3d-res:file-contents
                      (blt3d-res:resolve-resource
                       #p "res/shaders/billboard-world-shader.vert"))
                     (blt3d-res:file-contents
                      (blt3d-res:resolve-resource
                       #p "res/shaders/billboard-shader.frag"))
                     :uniforms '("size" "right" "up")))

  (format t "Loading billboard-vel-shader:~%")
  (setf *billboard-vel-shader*
        (make-shader (blt3d-res:file-contents
                      (blt3d-res:resolve-resource
                       #p "res/shaders/billboard-vel-shader.vert"))
                     (blt3d-res:file-contents
                      (blt3d-res:resolve-resource
                       #p "res/shaders/billboard-shader.frag"))
                     :uniforms '("size")
                     :attributes '("velocity"))))

(defun update-billboarder (eye-pos eye-dir eye-up world-up)
  (setf *bill-eye-pos* eye-pos
        *bill-eye-right* (norm4 (cross eye-dir eye-up))
        *bill-eye-up* (cross *bill-eye-right*  eye-dir)
        *bill-screen-normal* eye-dir
        *bill-world-up* world-up))

(defun setup-shader (align axis size)
  (case align
    (:screen (enable-shader *billboard-shader*)
             (set-uniform "size" size))

    (:world
     (enable-shader *billboard-world-shader*)
     (let* ((surface-normal (vec-neg4 *bill-screen-normal*))
            (surface-right *bill-eye-right*)
            (surface-up (cross surface-normal surface-right)))
       (set-uniform "right"
                    (to-vec4 surface-right))
       (set-uniform "up"
                    (to-vec4 surface-up)))
     (set-uniform  "size" size))

    (:axis
     (enable-shader *billboard-world-shader*)
     (let* ((surface-up axis)
            (surface-right (norm4 (cross *bill-screen-normal* axis))))
       (set-uniform "right"
                    (to-vec4 surface-right))
       (set-uniform  "up"
                    (to-vec4 surface-up)))
     (set-uniform "size" size))

    (:velocity
     (enable-shader *billboard-vel-shader*)
     (set-uniform "size" size))))

(defun draw-billboard-quad (pos size-x size-y texture color
                            &optional (align :screen) axis)
  (setup-shader align axis (vector size-x size-y 0.0))
  (use-texture texture)
  (gl:color (r color) (g color) (b color) (a color))
  (gl:with-primitives :quads

    (gl:tex-coord 0.0 0.0)
    (gl:vertex (x pos) (y pos) (z pos))

    (gl:tex-coord 1.0 0.0)
    (gl:vertex (x pos) (y pos) (z pos))

    (gl:tex-coord 1.0 1.0)
    (gl:vertex (x pos) (y pos) (z pos))

    (gl:tex-coord 0.0 1.0)
    (gl:vertex (x pos) (y pos) (z pos))))

(defun draw-particles (particles count num-alive)
  (gl:with-primitive :quads
    (iter (for i below count)
          (for particle = (cons particles i))
          (count (is-alive particle) into alive-cnt)
          (while (< alive-cnt num-alive))
          (when (is-alive particle)
            (let ((pos (p-pos particle))
                  (color (p-color particle)))

              (gl:color (r color) (g color) (b color)
                        (* (p-energy particle) (a color)))

              (gl:tex-coord 0.0 0.0)
              (gl:vertex (x pos) (y pos) (z pos))
              (gl:tex-coord 1.0 0.0)
              (gl:vertex (x pos) (y pos) (z pos))
              (gl:tex-coord 1.0 1.0)
              (gl:vertex (x pos) (y pos) (z pos))
              (gl:tex-coord 0.0 1.0)
              (gl:vertex (x pos) (y pos) (z pos)))))))

(defun draw-v-particles (particles count num-alive)
  (let ((vel-place (get-attribute-loc *billboard-vel-shader* "velocity")))
    (gl:disable :rescale-normal)
    (gl:with-primitive :quads
      (iter (for i below count)
            (for particle = (cons particles i))
            (count (is-alive particle) into alive-cnt)
            (while (< alive-cnt num-alive))
            (when (is-alive particle)
              (let ((pos (p-pos particle))
                    (color (p-color particle))
                    (vel (p-vel particle)))

                (gl:color (r color) (g color) (b color)
                          (* (p-energy particle) (a color)))

                (gl:tex-coord 0.0 0.0)
                (gl:normal (x vel) (y vel) (z vel))
                (gl:vertex (x pos) (y pos) (z pos))
;                (gl:vertex-attrib vel-place (x vel) (y vel) (z vel))


                (gl:tex-coord 1.0 0.0)
                (gl:normal (x vel) (y vel) (z vel))
                (gl:vertex (x pos) (y pos) (z pos))
             ;   (gl:vertex-attrib vel-place (x vel) (y vel) (z vel))

                (gl:tex-coord 1.0 1.0)
                (gl:normal (x vel) (y vel) (z vel))
                (gl:vertex (x pos) (y pos) (z pos))
              ;  (gl:vertex-attrib vel-place (x vel) (y vel) (z vel))

                (gl:tex-coord 0.0 1.0)
                (gl:normal (x vel) (y vel) (z vel))
                (gl:vertex (x pos) (y pos) (z pos))
               ; (gl:vertex-attrib vel-place (x vel) (y vel) (z vel))
                ))))
    (gl:enable :rescale-normal)))

(defun render-particles (particles count num-alive texture size
                         &optional (align :screen) axis)

  (setup-shader align axis size)

  (use-texture texture)
  (gl:with-pushed-attrib (:depth-buffer-bit)
  ;  (gl:enable-client-state :vertex-array)
 ;   (gl:enable-client-state :texture-coord-array)

    (case align
      (:velocity (draw-v-particles particles count num-alive))
      (otherwise (draw-particles particles count num-alive))))

  (use-texture 0)
  (disable-shader))


;;;
;;; LAZORS!!   - probably don't belong here, but wth
;;;

(defun draw-beam (start end color texture size)
  (let* ((line (vec4- end start))
         (tex-len (svref size 1))
         (beam-len (mag line))
         (axis (norm4 line))
         (step-axis (vec-scale3 axis tex-len))
         (size (if (/= 3 (length size))
                   (vector (x size) (y size) 0.0))))

    (setup-shader :axis axis size)
    (use-texture texture)
    (gl:color (r color) (g color) (b color) (a color))

    (gl:with-primitives :quads
      (iter (for i below (ceiling (/ beam-len tex-len)))
            (for pos first start then (vec3+ pos step-axis))

            (gl:tex-coord 0.0 0.0)
            (gl:vertex (x pos) (y pos) (z pos))

            (gl:tex-coord 1.0 0.0)
            (gl:vertex (x pos) (y pos) (z pos))

            (gl:tex-coord 1.0 1.0)
            (gl:vertex (x pos) (y pos) (z pos))

            (gl:tex-coord 0.0 1.0)
            (gl:vertex (x pos) (y pos) (z pos))))))
