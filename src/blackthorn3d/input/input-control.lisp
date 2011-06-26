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

(in-package :blackthorn3d-input)

(defclass input-system ()
    ((kind
        :accessor input-kind
        :initarg :kind
        :documentation "Either :keyboard or :xbox"))
    (:documentation "Holds the state of the input system."))


;; This should make it a bit easier to change keys...
(defvar *walk-left-key*  :sdl-key-a)
(defvar *walk-right-key* :sdl-key-d)
(defvar *walk-up-key*    :sdl-key-w)
(defvar *walk-down-key*  :sdl-key-s)
(defvar *jump-key*       :sdl-key-space)

(defvar *view-left-key*  :sdl-key-left)
(defvar *view-right-key* :sdl-key-right)
(defvar *view-up-key*    :sdl-key-up)
(defvar *view-down-key*  :sdl-key-down)

(defvar *camera-mode-key* :sdl-key-z)
(defvar *attack-key* :sdl-key-x)

(defvar *fly-up-key* :sdl-key-e)
(defvar *fly-down-key* :sdl-key-q)

(defvar *use-key* :sdl-key-j)
(defvar *xbox-y-key* :sdl-key-k)
(defvar *alt-attack-key* :sdl-key-l)

(defvar *deadzone-range-lx* 0.2)
(defvar *deadzone-range-ly* 0.2)
(defvar *deadzone-range-rx* 0.2)
(defvar *deadzone-range-ry* 0.2)

(defun xbox-thumb (fn controller deadzone)
  (let* ((query  (funcall fn controller))
         (scaled (/ query 32770)))
    (if (< (abs scaled) deadzone)
      0.0
      scaled)))

(defgeneric input-move-x (system)
   (:documentation
     "Handles x-axis input from either xbox for keyboard. Result is a number in
      the range [-1.0, 1.0]."))

(defgeneric input-move-y (system)
   (:documentation
     "Handles y-axis input from either xbox for keyboard. Result is a number in
      the range [-1.0, 1.0]."))

(defmethod input-move-x ((system input-system))
    (with-slots (kind) system
        (case kind
            (:keyboard
              (+ (if (sdl:get-key-state *walk-right-key*) 1.0  0)
                 (if (sdl:get-key-state *walk-left-key*) -1.0  0)))
            #+windows
            (:xbox (xbox-thumb #'xbox360_get_lx 0 *deadzone-range-lx*))
            (otherwise 0))))

(defmethod input-move-y ((system input-system))
    (with-slots (kind) system
        (case kind
            (:keyboard
               (+ (if (sdl:get-key-state *walk-up-key*) 1.0 0)
                  (if (sdl:get-key-state *walk-down-key*) -1.0 0)))
            #+windows
            (:xbox (xbox-thumb #'xbox360_get_ly 0 *deadzone-range-ly*))
            (otherwise 0))))

(defgeneric set-controller (system type)
    (:documentation
      "Sets the input system to use either :xbox or :keyboard controls."))

(defmethod set-controller ((system input-system) type)
    (setf (input-kind system) type))

(defgeneric input-view-x (system)
    (:documentation
      "Handles x-axis of view stick on xbox controller, or corresponding keys
       from moving view horizontally on keyboard."))

(defmethod input-view-x ((system input-system))
    (with-slots (kind) system
        (case kind
            (:keyboard
                (+ (if (sdl:get-key-state *view-left-key*) -1.0 0)
                   (if (sdl:get-key-state *view-right-key*)  1.0 0)))
            #+windows
            (:xbox  (xbox-thumb #'xbox360_get_rx 0 *deadzone-range-rx*))
            (otherwise 0))))

(defgeneric input-view-y (system)
    (:documentation
      "Handles y-axis of view stick on xbox controller, or corresponding keys
       from moving view vertically on keyboard."))

(defmethod input-view-y ((system input-system))
    (with-slots (kind) system
        (case kind
            (:keyboard
                (+ (if (sdl:get-key-state *view-up-key*) 1.0 0)
                   (if (sdl:get-key-state *view-down-key*) -1.0 0)))
            #+windows
            (:xbox (xbox-thumb #'xbox360_get_ry 0 *deadzone-range-ry*))
            (otherwise 0))))

(defmethod input-jump ((system input-system))
  (with-slots (kind) system
    (case kind
      (:keyboard
        (if (sdl:get-key-state *jump-key*) 1.0 0.0))
      #+windows
      (:xbox
        (if (eql (xbox360_get_a 0) 1) 1.0 0.0))
      (otherwise 0.0))))

(defmethod input-camera-mode ((system input-system))
  (with-slots (kind) system
    (case kind
      (:keyboard
        (if (sdl:get-key-state *camera-mode-key*) 1.0 0.0))
      #+windows
      (:xbox
        (xbox360_get_ltrig 0))
      (otherwise 0.0))))

(defmethod input-attack ((system input-system))
  (with-slots (kind) system
    (case kind
      (:keyboard
        (if (sdl:get-key-state *attack-key*) 1.0 0.0))
      #+windows
      (:xbox
        (xbox360_get_rtrig 0))
      (otherwise 0.0))))

(defmethod input-fly-up ((system input-system))
  (with-slots (kind) system
    (case kind
      (:keyboard
        (if (sdl:get-key-state *fly-up-key*) 1.0 0.0))
      #+windows
      (:xbox
        (xbox360_get_rbump 0))
      (otherwise 0.0))))

(defmethod input-fly-down ((system input-system))
  (with-slots (kind) system
    (case kind
      (:keyboard
        (if (sdl:get-key-state *fly-down-key*) 1.0 0.0))
      #+windows
      (:xbox
        (xbox360_get_lbump 0))
      (otherwise 0.0))))

(defmethod input-use ((system input-system))
  (with-slots (kind) system
    (case kind
      (:keyboard
        (if (sdl:get-key-state *use-key*) 1.0 0.0))
      #+windows
      (:xbox
        (if (eql (xbox360_get_x 0) 1) 1.0 0.0))
      (otherwise 0.0))))

(defmethod input-xbox-y ((system input-system))
  (with-slots (kind) system
    (case kind
      (:keyboard
        (if (sdl:get-key-state *xbox-y-key*) 1.0 0.0))
      #+windows
      (:xbox
        (if (eql (xbox360_get_y 0) 1) 1.0 0.0))
      (otherwise 0.0))))

(defmethod input-alt-attack ((system input-system))
  (with-slots (kind) system
    (case kind
      (:keyboard
        (if (sdl:get-key-state *alt-attack-key*) 1.0 0.0))
      #+windows
      (:xbox
        (if (eql (xbox360_get_b 0) 1) 1.0 0.0))
      (otherwise 0.0))))


(defun s-input-update (  src
  move-x-amt    move-y-amt
  view-x-amt    view-y-amt
  jmp-amt
  cam-amt      attack-amt
  fly-up-amt fly-down-amt
  use-amt xbox-y-amt alt-attack-amt)

    (when (getf *client-controllers* src)
              (setf (move-x (getf *client-controllers* src)) move-x-amt)
              (setf (move-y (getf *client-controllers* src)) move-y-amt)
              (setf (view-x (getf *client-controllers* src)) view-x-amt)
              (setf (view-y (getf *client-controllers* src)) view-y-amt)
              (setf (jump (getf *client-controllers* src)) jmp-amt)

              (setf (attacking (getf *client-controllers* src)) attack-amt)
              (setf (camera-mode-button (getf *client-controllers* src)) cam-amt)

              (setf (fly-up (getf *client-controllers* src)) fly-up-amt)
              (setf (fly-down (getf *client-controllers* src)) fly-down-amt)

              (setf (use (getf *client-controllers* src)) use-amt)
              (setf (xbox-y (getf *client-controllers* src)) xbox-y-amt)
              (setf (alt-attack (getf *client-controllers* src)) alt-attack-amt)
              ))
