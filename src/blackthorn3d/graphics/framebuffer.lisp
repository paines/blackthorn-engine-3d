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
;;; framebuffer class to make managing ogl's framebuffer easier
;;;

(defclass framebuffer ()
  ((fbo
    :reader fbo
    :initform (first (gl:gen-framebuffers-ext 1)))
   (depth-attachment
    :accessor depth-attachment
    :initarg :depth-attachment)
   (attachments
    :accessor attachments
    :initarg :attachments
    :initform ())))

(defun make-framebuffer (&optional (add-depth-renderbuffer nil))
  (let ((fb (make-instance 'framebuffer)))
    (when add-depth-renderbuffer
      (let ((depth (first (gl:gen-renderbuffers-ext 1))))
        (bind-framebuffer fb)
        (gl:bind-renderbuffer-ext :renderbuffer-ext depth)
        (gl:renderbuffer-storage-ext 
         :renderbuffer-ext :depth-component32 
         960 720)

        (gl:framebuffer-renderbuffer-ext :framebuffer-ext
                                         :depth-attachment-ext
                                         :renderbuffer-ext
                                         depth)
        (unbind-framebuffer)))
    fb))

(defun bind-framebuffer (fb)
  (gl:bind-framebuffer-ext :framebuffer-ext (fbo fb)))

(defun unbind-framebuffer ()
  (gl:bind-framebuffer-ext :framebuffer-ext 0))

(defmacro with-framebuffer (fb &body body)
  `(progn
     (bind-framebuffer ,fb)
     ,@body
     (unbind-framebuffer)))

(defun attach-texture (fbo attach-point texture)
  (with-framebuffer fbo
    (gl:framebuffer-texture-2d-ext
     :framebuffer-ext attach-point 
     :texture-2d texture 0))
  (case attach-point
    (:depth-attachment-ext
     (setf (depth-attachment fbo) texture))
    (otherwise
     (push (list attach-point texture) (attachments fbo)))))

(defun get-attachment (fbo attach-point)
  (case attach-point
    (:depth-attachment-ext
     (depth-attachment fbo))
    (otherwise
     (second (find attach-point (attachments fbo) :key #'car)))))

(defun create-shadow-framebuffer (width height)
  (let ((framebuffer (make-framebuffer))
        (depth-texture 
         (create-texture width height :depth-component
                         :min-filter :nearest
                         :mag-filter :nearest
                         :wrap-s :clamp
                         :wrap-t :clamp
                         :format :depth-component))
        (color-texture
         (create-texture width height :rgb16f
                         :wrap-s :clamp
                         :wrap-t :clamp
                         :format :rgb
                         :type :float)))

    (attach-texture framebuffer :depth-attachment-ext depth-texture)
    (attach-texture framebuffer :color-attachment0-ext color-texture)
    (with-framebuffer framebuffer
      (format t "Framebuffer Status: ~a~%"
              (gl::enum= 
               (gl:check-framebuffer-status-ext :framebuffer-ext) 
               :framebuffer-complete-ext)))
    (gl:bind-framebuffer-ext :framebuffer-ext 0)
    framebuffer))