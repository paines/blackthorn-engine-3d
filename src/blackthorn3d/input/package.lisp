;;;; Blackthorn -- Lisp Game Engine
;;;;
;;;; Copyright (c) 2011, Chris McFarland
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

(in-package :cl-user)

(defpackage :blackthorn3d-input
  (:nicknames :blt3d-input)
  (:use :cl)
  (:export

   ;; input-control.lisp
   :input-system
   :input-move-x
   :input-move-y
   :input-view-x
   :input-view-y
   :input-jump
   :set-controller
   :input-kind
   :input-camera-mode
   :input-attack
   :input-fly-up
   :input-fly-down
   :input-use
   :input-xbox-y
   :input-alt-attack

   ;; server-control.lisp
   :new-server-controller
   :remove-server-controller
   :s-input-update
   :s-input-move-x
   :s-input-move-y
   :s-input-view-x
   :s-input-view-y
   :s-input-jump
   :s-input-camera-mode
   :s-input-attack
   :s-input-fly-up
   :s-input-fly-down
   :s-input-use
   :s-input-xbox-y
   :s-input-alt-attack

   ;; xbox360.lisp
   :xbox360-vibrate
   :xbox360_poll
   :xbox360_get_a
   :xbox360_get_b
   :xbox360_get_x
   :xbox360_get_y
   :xbox360_get_lx
   :xbox360_get_ly
   :xbox360_get_rx
   :xbox360_get_ry
   :xbox360_get_ltrig
   :xbox360_get_rtrig

   ))
