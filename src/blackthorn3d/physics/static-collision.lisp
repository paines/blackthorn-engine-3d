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

(in-package :blackthorn3d-physics)

;;;
;;; Collection of tools for static collision detection
;;; This should be put somewhere else...
;;;


(defun make-rect (shape)
  (destructuring-bind (lows-v highs-v) (shape-bounds shape)
    (rectangles:make-rectangle
     :lows (list (x lows-v) (y lows-v) (z lows-v))
     :highs (list (x highs-v) (y highs-v) (z highs-v)))))

(defun build-r-tree (triangles)
  "take a vector of triangles and return an r-tree around them"
  (let ((r-tree 
         (spatial-trees:make-spatial-tree :r 
                                          :rectfun #'make-rect)))
    (iter (for tri in-vector triangles)
          (spatial-trees:insert tri r-tree))
    r-tree))

;; For now, we'll just return t or nil
;; probably will want the hit location ... 
(defun sphere-rtree-intersection (sphere rtree)
  ;; get the list of potentially intersecting triangles
  (iter (for tri in (spatial-trees:search sphere rtree))
        (for result = (sphere-triangle-intersection sphere tri))
        (until result)
        (finally (return result))))