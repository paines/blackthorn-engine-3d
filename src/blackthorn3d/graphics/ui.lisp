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
;;; GUI elements and such
;;;
(defun draw-rect (offset size color)
  (destructuring-bind ((x0 y0) (dx dy)) (list offset size)
    (gl:color (r color) (g color) (b color) (a color))
    (gl:tex-coord 0.0 0.0) (gl:vertex x0 y0)
    (gl:tex-coord 1.0 0.0) (gl:vertex (+ x0 dx) y0)
    (gl:tex-coord 1.0 1.0) (gl:vertex (+ x0 dx) (+ y0 dy))
    (gl:tex-coord 0.0 1.0) (gl:vertex x0 (+ y0 dy))))

(defclass bg ()
  ((outline
    :initarg :outline
    :initform nil)
   (color
    :initarg :color
    :initform +white+)
   (texture
    :initarg :texture
    :initform 0)))

(defun make-bg (&key (outline nil) (color +white+) (texture 0))
  (make-instance 'bg
                 :outline outline
                 :color color
                 :texture texture))

(defmethod draw-bg ((this bg))
  (with-slots (outline color texture) this
    (when outline
      (gl:line-width outline)
      )))

(defclass ui-element ()
  ((offset
    :accessor ui-offset
    :initarg :offset
    :documentation "the offset of the bottom-left corner of ui-element")
   (size
    :accessor ui-size
    :initarg :size
    :documentation "a list of (x y) that specifies the width and height
                    of the element")
   (alignment
    :accessor ui-alignment
    :initarg :alignment
    :initform '(:none :none nil)
    :documentation "The alignment of an element tells the renderer where to 
                    offset the element from. :none means the offset is in
                    screen-coords.  :bottom, :top, :right :left mean the 
                    offset is from the bottom, top, right or left of the anchor
                    the second element in the alignment is the anchor.
                    It can be: 
                      nil - the screen
                      ui-element - an element to specify relative to")))


(defclass ui-gauge (ui-element)
  ((orientation
    :accessor gauge-orientation
    :initarg :orientation
    :initform :up
    :documentation "the direction of increasing values for the gauge.
                    May be :up :down :left or :right")
   (base-texture
    :accessor gauge-base-texture
    :initarg :base-texture
    :documentation "base texture for the gauge is what is the static
                    layer on top of which the gauge renders")
   (gauge-texture
    :accessor gauge-gauge-texture
    :initarg :gauge-texture
    :documentation "gauge texture is rendered with the ")
   (max-value
    :accessor gauge-max-value
    :initarg :max-value)
   (current-value
    :accessor gauge-current-value
    :initarg :current-value)))

#+disabled
(defmethod get-screen-offset ((this ui-element))
  (with-slots (offset alignment) this
    (destructuring-bind (h-align v-align object) alignment
      (let ((origin
             (if (null object)
                 '(0.0 0.0)
                 (ui-offset object)))
            (size
             (if (null object)
                 (list *screen-x* *screen-y*)
                 (ui-size object))))
        (let ((base
               (list
                (case h-align
                  ((:none :left)
                   (first origin))
                  (:right
                   (+ (first origin) (first size))))
                (case v-align
                  ((:none :bottom)
                   (second origin))
                  (:top
                   (+ (second origin) (second size)))))))
          (list
           (+ (first base) (first offset))
           (+ (second base) (second offset))))))))


(defmethod update-ui-element ((gauge ui-gauge) update)
  (setf (gauge-current-value gauge) update))

(defmethod draw-ui-element ((gauge ui-gauge))
  ;; the gauge is drawn thusly:
  ;; draw the base texture as a quad the size of the element
  ;; then draw the gauge as a quad of size relative to 
  ;; the value
  (with-slots (offset size base-texture gauge-texture
                      orientation max-value current-value) 
      gauge
    (let ((g-offset
           (case orientation
             ((:up :right)
              offset)
             ((:down :left)
              (list (+ (first offset) (first size))
                    (+ (second offset) (second size))))))
          (g-size
           (let ((scale (/ current-value max-value)))
             (case orientation
               (:up
                (list (first size) (* scale (second size))))
               (:right
                (list (* scale (first size)) (second size)))
               (:down
                (list (first-size) (- (* scale (second size)))))
               (:left
                (list (- (* scale (first size))) (second size)))))))
      (gl:with-primitives :quads
        (draw-rect offset size  +white+)
        (draw-rect g-offset g-size +orange+)))))




(defvar ui-elements nil)

(defun add-ui-element (ui-element)
  (push ui-element ui-elements))

(defun remove-ui-element (ui-element)
  (setf ui-elements (delete ui-element ui-elements)))

(defun render-ui ()
  (gl:with-pushed-attrib (:color-buffer-bit 
                          :depth-buffer-bit
                          :transform-bit)

    (gl:disable :depth-test :lighting)
    (gl:depth-func :always)
    (gl:blend-func :src-alpha :one-minus-src-alpha)
    (disable-shader)
    (use-texture 0)

    (gl:matrix-mode :projection)
    (gl:with-pushed-matrix 
      (gl:load-identity)
      (gl:ortho 0 1 0 1 -10 10)
        
      (gl:matrix-mode :modelview)
      (gl:load-identity)

      (iter (for element in ui-elements)
            (draw-ui-element element))

      (gl:matrix-mode :projection))))