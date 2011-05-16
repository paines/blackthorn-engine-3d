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
;;; Please be a shader...
;;;


;; Global shader (should always be the one that's bound
(defvar *shader* nil)

(defun make-shader (vert frag) 
  "@arg[vert]{string of vert shader}
   @arg[frag]{string of frag shader}
   @return{the shader program}"
  (let ((vs (gl:create-shader :vertex-shader))
        (fs (gl:create-shader :fragment-shader))
        (shader (gl:create-program)))
                         
    (gl:shader-source vs vert)
    (gl:compile-shader vs)
    (gl:shader-source fs frag)
    (gl:compile-shader fs)

    (print (gl:get-shader-info-log vs))
    (print (gl:get-shader-info-log fs))

    (gl:attach-shader shader vs)
    (gl:attach-shader shader fs)
    (gl:link-program shader)
    shader))

(defun enable-shader (shader) 
  (setf *shader* shader)
  (gl:use-program shader))

(defun disable-shader () 
  (setf *shader* nil)
  (gl:use-program 0))