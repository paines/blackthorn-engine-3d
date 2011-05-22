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
(defvar *bill-world-up* +y-axis+)


(defun update-billboarder (eye-pos eye-dir eye-up world-up)
  (setf *bill-eye-pos* eye-pos
        *bill-eye-up* (cross (norm4 (cross eye-up eye-dir)) eye-dir)
        *bill-screen-normal* eye-dir
        *bill-world-up* world-up))

(defun draw-billboard-quad (pos size-x size-y texture alignment
                            &rest rest)
  (use-texture texture)
  (let (surface-normal
        surface-up
        surface-right
        (s1 (* size-x 0.5))
        (s2 (* size-y 0.5)))
    (case alignment
      (:screen
       (setf surface-normal (vec-neg4 *bill-screen-normal*)
             surface-up     *bill-eye-up*
             surface-right  (cross surface-normal surface-up)))
      (:world
       (setf surface-normal (vec-neg4 *bill-screen-normal*)
             surface-right  (norm4 (cross surface-normal *bill-world-up*))
             surface-up     (cross surface-right surface-normal)))
      (:axis
       (destructuring-bind (axis &rest dc) rest
         (setf surface-up axis
               surface-right (norm4 (cross *bill-screen-normal* surface-up))
               surface-normal (cross surface-up surface-normal)))))
    (gl:with-pushed-matrix
      (gl:translate (x pos) (y pos) (z pos))
      (gl:mult-matrix (make-inv-ortho-basis surface-right 
                                            surface-up 
                                            surface-normal))
    
      (gl:with-primitives :quads

        (gl:tex-coord 0.0 0.0)
        (gl:vertex (- s1) (- s2) 0.0)
        
        (gl:tex-coord 1.0 0.0)
        (gl:vertex s1 (- s2) 0.0)
        
        (gl:tex-coord 1.0 1.0)
        (gl:vertex s1 s2 0.0)

        (gl:tex-coord 0.0 1.0)
        (gl:vertex (- s1) s2 0.0)))))


(gl:define-gl-array-format blt-billboard
  (gl:vertex :type :float :components (vx vy vz))
  (gl:tex-coord :type :float :components (u v)))

(defvar *billboard-shader* nil)

(defun create-billboard-stream (count align &rest rest)
  "align is :screen :world or :axis. If axis, axis should be in rest"
  (let ((gl-array (gl:alloc-gl-array 'blt-billboard count)))
    gl-array))

(defun write-to-bb-stream (stream index vertex uv)
  (setf (glaref stream index 'vx) (x vertex)
        (glaref stream index 'vy) (y vertex)
        (glaref stream index 'vz) (z vertex)
        
        (glaref stream index 'u)  (x uv)
        (glaref stream index 'v)  (y uv)))

(defun flush-billboard-stream (stream)
  (gl:with-pushed-attrib (:depth-buffer-bit)
    ;; Turn off depth write /hack for transparency
    (gl:depth-mask nil)

    (gl:enable-client-state :vertex-array)
    (gl:enable-client-state :texture-coord-array)

    (use-shader *billboard-shader*)

    (gl:bind-gl-vertex-array stream)
   ; (gl:draw-arrays :quads 0 )
    ))