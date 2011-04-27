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

(defun draw-cube (&key (color #(1.0 1.0 1.0)))
  (gl:color (r color) (g color) (b color))
  (gl:with-primitive :quads

    ;; front face
    (gl:vertex -1.0 1.0 1.0)
    (gl:vertex -1.0 -1.0 1.0)
    (gl:vertex 1.0 -1.0 1.0)
    (gl:vertex 1.0 1.0 1.0)
        
    ;; top face
    (gl:vertex -1.0 1.0 -1.0)
    (gl:vertex -1.0 1.0 1.0)
    (gl:vertex 1.0 1.0 1.0)
    (gl:vertex 1.0 1.0 -1.0)
        
    ;; right face
    (gl:vertex 1.0 1.0 1.0)
    (gl:vertex 1.0 -1.0 1.0)
    (gl:vertex 1.0 -1.0 -1.0)
    (gl:vertex 1.0 1.0 -1.0)
        
    ;; back face
    (gl:vertex 1.0 1.0 -1.0)
    (gl:vertex 1.0 -1.0 -1.0)
    (gl:vertex -1.0 -1.0 -1.0)
    (gl:vertex -1.0 1.0 -1.0)
        
    ;; left face
    (gl:vertex -1.0 1.0 -1.0)
    (gl:vertex -1.0 -1.0 -1.0)
    (gl:vertex -1.0 -1.0 1.0)
    (gl:vertex -1.0 1.0 1.0)
        
    ;; bottom face
    (gl:vertex -1.0 -1.0 -1.0)
    (gl:vertex 1.0 -1.0 -1.0)
    (gl:vertex 1.0 -1.0 1.0)
    (gl:vertex -1.0 -1.0 1.0)))

(defun draw-triangle (&key (color #(1.0 1.0 1.0)))
  (gl:color (r color) (g color) (b color))
  (gl:with-primitive :triangles
    (gl:vertex 0.0 1.0 0.0)
    (gl:vertex -1.0 0.0 0.0)
    (gl:vertex 1.0 0.0 0.0)))
    
(gl:define-gl-array-format position
  (gl:vertex :type :float :components (x y z)))


 (defun set-vec-in-glarray (a i v)
  (setf (gl:glaref a i 'x) (x v))
  (setf (gl:glaref a i 'y) (y v))
  (setf (gl:glaref a i 'z) (z v)))
      
(defun set-quad-indices (a i v)
  (iter (for j from (* i 4) below (+ (* i 4) 4))
        (for k below 4)
    (setf (gl:glaref a j) (nth k v))))

(defun make-cube ()
  (let ((vert-arr (gl:alloc-gl-array 'position 8))
        (ind-arr  (gl:alloc-gl-array :unsigned-short 24)))
        
    ;; Vertex Array
    (set-vec-in-glarray vert-arr 0 #(-1.0 -1.0 -1.0))
    (set-vec-in-glarray vert-arr 1 #(-1.0 -1.0  1.0))
    (set-vec-in-glarray vert-arr 2 #(-1.0  1.0 -1.0))
    (set-vec-in-glarray vert-arr 3 #(-1.0  1.0  1.0))
    (set-vec-in-glarray vert-arr 4 #( 1.0 -1.0 -1.0))
    (set-vec-in-glarray vert-arr 5 #( 1.0 -1.0  1.0))
    (set-vec-in-glarray vert-arr 6 #( 1.0  1.0 -1.0))
    (set-vec-in-glarray vert-arr 7 #( 1.0  1.0  1.0))
    
    ;; Index array
    (set-quad-indices ind-arr 0 '(4 5 1 0))
    (set-quad-indices ind-arr 1 '(2 3 7 6))
    (set-quad-indices ind-arr 2 '(1 5 7 3))
    (set-quad-indices ind-arr 3 '(7 5 4 6))
    (set-quad-indices ind-arr 5 '(2 0 1 3))
    (set-quad-indices ind-arr 4 '(6 4 0 2))
    (list vert-arr ind-arr)))

(defun draw-vert-array (vert-arr ind-arr)
  (gl:enable-client-state :vertex-array)
  (gl:bind-gl-vertex-array vert-arr)
  (gl:draw-elements :quads ind-arr)
  (gl:flush))

(defun make-vao-cube ()
  (destructuring-bind (v-arr i-arr) (make-cube)
    (let ((vao (gl:gen-vertex-array))
          (vbo (car (gl:gen-buffers 1)))
          (ibo (car (gl:gen-buffers 1))))  
      (gl:bind-vertex-array vao)
      (gl:bind-buffer :array-buffer vbo)
      (gl:buffer-data :array-buffer :static-draw v-arr)
      (gl:enable-client-state :vertex-array)
      (gl:vertex-attrib-pointer 0 3 :float nil 0 0)

      (gl:bind-buffer :element-array-buffer ibo)
      (gl:buffer-data :element-array-buffer :static-draw i-arr)
      (gl:bind-vertex-array 0)
      vao)))

(defun draw-vao-cube (vao)
  (gl:bind-vertex-array vao)
;  (gl:bind-buffer :array-buffer vbo)
 ; (gl:enable-client-state :vertex-array)
  ;(%gl:vertex-pointer 3 :float 0 0)
;  (gl:bind-buffer :element-array-buffer ibo)
  (%gl:draw-elements :quads 24 :unsigned-short (cffi:null-pointer))
  ;(gl:bind-vertex-array 0)
  (gl:flush))

(defmethod draw-object ((m mesh))
  (with-slots (vert-data index-data) m
    (gl:enable-client-state :vertex-array)
    (gl:enable-client-state :normal-array)
    (gl:enable-client-state :texture-coord-array)
    (gl:bind-gl-vertex-array vert-data)
    (gl:draw-elements :triangles index-data)
    (gl:flush)))

(defun draw-sphere (pos r &optional (color #(1.0 1.0 1.0)) (segs 8))
  (gl:with-pushed-matrix
    (gl:scale r r r)
    (gl:translate (x pos) (y pos) (z pos))
    (gl:with-primitives :quads
      (gl:color (r color) (g color) (b color))
      (let ((step (/ pi segs)))
        (iter (for theta below pi by step)
              (iter (for phi below (* 2.0 pi) by step)
                    (gl:vertex (* (sin theta) (cos phi)) 
                               (* (sin theta) (sin phi)) 
                               (cos theta))
                    (gl:vertex (* (sin (+ theta step)) (cos phi))
                               (* (sin (+ theta step)) (sin phi))
                               (cos (+ theta step)))
                    (gl:vertex (* (sin (+ theta step)) (cos (+ phi step)))
                               (* (sin (+ theta step)) (sin (+ phi step)))
                               (cos (+ theta step)))
                    (gl:vertex (* (sin theta) (cos (+ phi step))) 
                               (* (sin theta) (sin (+ phi step))) 
                               (cos theta))))))))