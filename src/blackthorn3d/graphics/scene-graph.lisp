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
;;; Scene graph/tree
;;;
;;;  a simple implementation of a list-based tree with some utility
;;;  functions. 
;;; Structure of a node:
;;;  (WORLD-MATRIX OBJECT LEFT-CHILD RIGHT-CHILD)
;;;  WORLD-MATRIX - the matrix used to transform local coordinates
;;;                 at this node to world coordinates
;;;  OBJECT - a place to store some other data, a model, or joint data
;;;  LEFT-CHILD - either another node or nil
;;;  RIGHT-CHILD - either another node or ni
;;;

(defclass node ()
  ((xform
    :accessor node-xform
    :initarg :xform)
   (obj
    :accessor node-obj
    :initarg :obj)
   (children
    :accessor node-children
    :initarg :children
    :initform nil)))

(defun scene-dfs (node fn &key (order :pre))
  (when node
    (case order
      (:pre 
       (funcall fn node) 
       (iter (for c in (node-children node)) (scene-dfs c fn :order order)))
      (:post 
       (iter (for c in (node-children node)) (scene-dfs c fn :order order))
       (funcall fn node)))))

(defun scene-bfs (node fn)
  (when node
    (iter (for c in (node-children node))
          (funcall fn c))
    (iter (for c in (node-children node))
          (scene-bfs c fn))))