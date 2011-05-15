;;;; Blackthorn -- Lisp Game Engine
;;;;
;;;; Copyright (c) 2011, Elliott Slaughter <elliottslaughter@gmail.com>
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

(in-package :blackthorn3d-sound)

(defun init ()
  (sdl-mixer:open-audio))

(defun init-p ()
  (let ((open (sdl-mixer:audio-opened-p)))
    (and open (> open 0))))

(defun exit ()
  ;;(sdl-mixer:halt-music)
  (sdl-mixer:close-audio))

(defclass music ()
  ((src
    :initarg :src)
   (sound
    :accessor sound
    :initarg :sound)))

(defgeneric load-sound (type src))

(defmethod load-sound ((type (eql :music)) src)
  (when (init-p)
    (let ((sound (sdl-mixer:load-music (resolve-resource src))))
      (make-instance 'music :src src :sound sound))))

(defgeneric play-sound (sound &key))

(defmethod play-sound ((music music) &key loop)
  (when (init-p)
    (sdl-mixer:play-music (sound music) :loop loop)))

(defgeneric stop-sound (type &key))

(defmethod stop-sound ((type (eql :music)) &key)
  (sdl-mixer:halt-music))
