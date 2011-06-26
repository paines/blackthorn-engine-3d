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
;;; VAO-MESH: a mesh object that uses a vertex-array-object to
;;; store it's data and state
;;; The tricky part about these is going to be syncing the
;;;

(defclass vao-mesh ()
  ((vao
    :accessor vao
    :initarg :vao
    :initform nil)
   (indices
    :accessor indices
    :initarg :indices
    :initform nil
    :documentation "Because we can't use a vertex buffer")))

(defparameter *vertex-program* nil)
(defparameter *fragment-program* nil)


(defvar vbo nil)
(defvar ibo nil)
(defvar vao nil)
(defvar verts nil)
(defvar indices nil)
(defvar program nil)

(defun arr->gl-arr (arr type)
  (let ((gl-arr (gl:alloc-gl-array type (length arr))))
    (iter (for i below (length arr))
          (setf (gl:glaref gl-arr i) (aref arr i)))
    gl-arr))

(defun make-vao-cube ()
  (setf *vertex-program*
"#version 330

in vec3 in_Position;

uniform mat4 projectionMatrix;
uniform mat4 modelViewMatrix;

void main()
{
  gl_Position = projectionMatrix * modelViewMatrix * vec4(in_Position, 1.0);
}
")
  (setf *fragment-program*
"#version 330

layout (location = 0) out vec4 out_Color;

void main()
{
  out_Color = vec4(1.0, 1.0, 1.0, 1.0);
}
" )

  (setf vbo (car (gl:gen-buffers 1)))
  (setf ibo (car (gl:gen-buffers 1)))
  (setf vao (gl:gen-vertex-array))
  (setf verts (arr->gl-arr
               #(-1.0 -1.0 -1.0       ; 0
                 -1.0 -1.0  1.0       ; 1
                 -1.0  1.0 -1.0       ; 2
                 -1.0  1.0  1.0       ; 3
                 1.0 -1.0 -1.0        ; 4
                 1.0 -1.0  1.0        ; 5
                 1.0  1.0 -1.0        ; 6
                 1.0  1.0  1.0)       ; 7
               :float))
  (setf indices (arr->gl-arr
                 #(4 5 1 0
                   2 3 7 6
                   1 5 7 3
                   7 5 4 6
                   6 4 0 2
                   2 0 1 3)
                 :unsigned-short))

  ;; First we need to create the shader program
  (let ((vs (gl:create-shader :vertex-shader))
        (fs (gl:create-shader :fragment-shader)))

    (print *vertex-program*)
    (print *fragment-program*)
    (setf program (gl:create-program))
    (gl:shader-source vs *vertex-program*)
    (gl:compile-shader vs)
    (gl:shader-source fs *fragment-program*)
    (gl:compile-shader fs)

    (print (gl:get-shader-info-log vs))
    (print (gl:get-shader-info-log fs))

    (gl:attach-shader program vs)
    (gl:attach-shader program fs)

    (gl:link-program program)
    (gl:bind-attrib-location program 0 "in_Position")
    (gl:use-program program))


  ;; Then set up vertex buffer
  (gl:bind-buffer :array-buffer vbo)
  (gl:buffer-data :array-buffer :static-draw verts)
  (gl:bind-buffer :array-buffer 0)

  ;; Now the element buffer
  (gl:bind-buffer :element-array-buffer ibo)
  (gl:buffer-data :element-array-buffer :static-draw indices)
  (gl:bind-buffer :element-array-buffer 0)

  ;; Now we setup the vertex array object
  (gl:bind-vertex-array vao)
  (gl:bind-buffer :array-buffer vbo)
  (gl:enable-vertex-attrib-array 0)
  (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))

  ;; This doesn't seem to work...it's fine if you pass in a
  ;; gl-array to draw-elements, using a vertex buffer is not
  ;;(gl:bind-buffer :element-array-buffer ibo)

  (gl:bind-vertex-array 0)
  (gl:bind-buffer :array-buffer 0)
  (gl:bind-buffer :element-array-buffer 0))

(defun draw-vao ()
  (gl:use-program program)
  (let ((proj-mat (gl:get-float :projection-matrix)))
    (gl:uniform-matrix
     (gl:get-uniform-location program "projectionMatrix")
     4
     (vector proj-mat)
     nil))
 #+disabled
  (gl:uniform-matrix
   (gl:get-uniform-location program "projectionMatrix")
   4
   (vector (frustum-projection-matrix *frustum*))
   nil)

  (let ((mv-mat (gl:get-float :modelview-matrix)))
    (gl:uniform-matrix
     (gl:get-uniform-location program "modelViewMatrix")
     4
     (vector mv-mat)
     nil))
 #+disbled
  (gl:uniform-matrix
   (gl:get-uniform-location program "modelViewMatrix")
   4
   (vector (camera-inverse *main-cam*))
   nil)

  (gl:bind-vertex-array vao)
  (gl:draw-elements :quads indices);(gl:make-null-gl-array :unsigned-short)
  (gl:bind-vertex-array 0)
  )

(defun draw-vbo ()
  (gl:use-program program)
  (gl:bind-buffer :array-buffer vbo)
  (gl:vertex-attrib-pointer 0 3 :float nil 0 (cffi:null-pointer))
  (gl:enable-vertex-attrib-array 0)

  ;(gl:bind-buffer :element-array-buffer ibo)

  (gl:draw-elements :quads indices);(gl:make-null-gl-array :unsigned-short)

  (gl:bind-buffer :array-buffer 0)
  (gl:bind-buffer :element-array-buffer 0))
