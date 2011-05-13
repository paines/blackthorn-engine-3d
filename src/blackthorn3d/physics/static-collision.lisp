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
        (collect (sphere-triangle-intersection sphere tri) into results)
        (finally 
         (return (iter (with min-d = `(1.0e+INF nil))
                       (for hit in results)
                       (unless (null hit)
                         (when (< (car hit) (car min-d))
                           (setf min-d hit)))
                       (finally (return min-d)))))))

(defun initialize-cube (level-model)
  "Take a model file contains level information 
  (a 30x30x30 cube) and create a thinger out of it.
  r-tree...thinger...replace the blt-meshes with r-trees"
  (labels ((helper (node)
             (setf (mesh node)
                   (build-r-tree (build-triangle-array (mesh node))))
             (iter (for n in (child-nodes node))
                   (helper n)))))
  (iter (for node in (mesh-nodes level-model))
        (helper node))
  level-model)

(defun min-collide (hits)
  "Return the hit with the smallest distance from the collider"
  (when hits
    (iter (with min-d = '(1.0e+INF nil))
          (for hit in hits)
          (when (< (car hit) (car min-d))
            (setf min-d hit))
          (finally (return min-d)))))

;(defgeneric collide-test (bv bv))
(defmethod collide-test ((bv bounding-shape) (model blt-model))
  "returns a point p if bv intersects with the geometry of model
   nil otherwise"
  (min-collide
   (iter (for node in (mesh-nodes model))
         (collect (collide-test bv node)))))

(defmethod collide-test ((bv bounding-shape) (node model-node))
  "recursively tests bv against this node and its children.  Note
   that, since currently the bounding volume of a node is not 
   guaranteed to enclose its children, the children must always be
   tested"
  ;; We need to translate, and transform, the collidee to the 
  ;; node coordinates.  We do this instead of translating the 
  ;; node b/c it's a lot easier to translate a bounding sphere than
  ;; an r-tree.
  (let ((xformed-bv (transform-bounding-volume bv (transform node))))
    (min-collide
     (cons (aif (collide-test xformed-bv (node-bounding-shape node))
                ;; if we intersect the bounding-shape, check the mesh
                (collide-test xformed-bv (mesh node)))
           (iter (for child in (child-nodes node))
                 (collect (collide-test xformed-bv child)))))))

(defmethod collide-test ((bv bounding-shape) (mesh blt-mesh))
  t)

(defmethod collide-test ((sphere bounding-sphere) 
                         (r-tree spatial-trees-protocol:spatial-tree))
  (iter (for tri in (spatial-trees:search sphere rtree))
        (collect (sphere-triangle-intersection sphere tri) into results)
        (finally 
         (return (iter (with min-d = `(1.0e+INF nil))
                       (for hit in results)
                       (unless (null hit)
                         (when (< (car hit) (car min-d))
                           (setf min-d hit)))
                       (finally (return min-d)))))))