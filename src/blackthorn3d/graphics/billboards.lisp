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

(defvar *billboard-shader* nil)
(defvar *right-loc* nil)
(defvar *up-loc* nil)
(defvar *size-loc* nil)


(defun billboard-init ()
   (setf *billboard-shader*
        (make-shader (blt3d-res:file-contents
                      (blt3d-res:resolve-resource 
                       #p "res/shaders/billboard-shader.vert"))
                     (blt3d-res:file-contents
                      (blt3d-res:resolve-resource
                       #p "res/shaders/billboard-shader.frag"))))
   #+disabled
   (progn
     (setf *right-loc*
           (gl:get-uniform-location
            *billboard-shader*
            "right"))
     (setf *up-loc*
           (gl:get-uniform-location
            *billboard-shader*
            "up"))
     (setf *size-loc*
           (gl:get-uniform-location
            *billboard-shader*
            "size"))))

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

(defun draw-billboard-points (points texture alignment &rest rest)
  (use-texture texture)
  (let (surface-normal
        surface-up
        surface-right)
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
    
      (let ((p1 (first points))
            (p2 (second points))
            (p3 (third points))
            (p4 (fourth points)))
        (gl:with-primitives :quads

          (gl:tex-coord 0.0 0.0)
          (gl:vertex (- s1) (- s2) 0.0)
          
          (gl:tex-coord 1.0 0.0)
          (gl:vertex s1 (- s2) 0.0)
          
          (gl:tex-coord 1.0 1.0)
          (gl:vertex s1 s2 0.0)

          (gl:tex-coord 0.0 1.0)
          (gl:vertex (- s1) s2 0.0))))))


(gl:define-gl-array-format blt-billboard
  (gl:vertex :type :float :components (vx vy vz))
  (gl:tex-coord :type :float :components (u v)))

(defun create-billboard-stream (count)
  (gl:alloc-gl-array 'blt-billboard count))

(defun write-to-bb-stream (stream index vertex uv)
  (setf (gl:glaref stream index 'vx)  (x vertex)
        (gl:glaref stream index 'vy)  (y vertex)
        (gl:glaref stream index 'vz)  (z vertex)
        
        (gl:glaref stream index 'u)   (x uv)
        (gl:glaref stream index 'v)   (y uv)))


(defun write-billboard (stream index position size align)
  (let ((order (list '(0.0 . 0.0)
                     '(1.0 . 0.0)
                     '(1.0 . 1.0)
                     '(0.0 . 1.0))))
    (iter (with start-index = (* 4 index))
          (with pos = (make-point3 (coerce (x position) 'single-float)
                                   (coerce (y position) 'single-float)
                                   (coerce (z position) 'single-float)))
          (for (u . v) in order)
          (for i from start-index)
          (write-to-bb-stream
           stream i 
           pos (vector u v)))))

#+disabled
(defun write-billboard (stream index position size align)
  (let (surface-normal surface-up surface-right basis)
    (case align
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

    (let ((start-index (* 4 index))
          (order (list '(0.0 . 0.0)
                       '(1.0 . 0.0)
                       '(1.0 . 1.0)
                       '(0.0 . 1.0))))
      (iter (with pos = (make-point3 (coerce (x position) 'single-float)
                                     (coerce (y position) 'single-float)
                                     (coerce (z position) 'single-float)))
            (for (u . v) in order)
            (for xs = (* (- u 0.5) size))
            (for ys = (* (- v 0.5) size))
            (for i from start-index)
            (write-to-bb-stream
             stream i
             (vec4+ pos (vec4+
                              (vec-scale4 surface-right xs)
                              (vec-scale4 surface-up ys)))
             (vector u v))))))

#+disabled
(defun flush-billboard-stream (stream count align)
  (gl:with-pushed-attrib (:depth-buffer-bit)
    ;; Turn off depth write /hack for transparency
    
    (gl:depth-mask nil)

    (gl:enable-client-state :vertex-array)
    (gl:enable-client-state :texture-coord-array)

    (format t "Drawing ~a billboards~%~3Tlength of stream: ~a~%"
            count (gl::gl-array-size stream))

    (enable-shader *billboard-shader*)

    (gl:bind-gl-vertex-array stream)
    (gl::draw-arrays :quads 0 (min count (gl::gl-array-size stream)))
    (disable-shader)
    (gl:bind-gl-vertex-array dummy-array)))

(defun render-particles (particles count num-alive texture)
  (enable-shader *billboard-shader*)
  (gl:enable :texture-2d)
  (gl:bind-texture :texture-2d texture)
  (gl:with-pushed-attrib (:depth-buffer-bit)    
    (gl:depth-mask nil)
    (gl:enable-client-state :vertex-array)
    (gl:enable-client-state :texture-coord-array)
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
             ;   (gl:color 1.0 0.7 0.0 1.0)

                (gl:tex-coord 0.0 0.0)
                (gl:vertex (x pos) (y pos) (z pos))
                
                (gl:tex-coord 1.0 0.0)
                (gl:vertex (x pos) (y pos) (z pos))

                (gl:tex-coord 1.0 1.0)
                (gl:vertex (x pos) (y pos) (z pos))

                (gl:tex-coord 0.0 1.0)
                (gl:vertex (x pos) (y pos) (z pos)))))))
  (use-texture 0)
  (disable-shader))