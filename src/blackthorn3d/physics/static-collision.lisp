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
         (return (iter (with min-d = (list most-positive-single-float))
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
             (when (slot-exists-p node 'mesh)
               (setf (mesh node)
                     (build-r-tree (build-triangle-array (mesh node)))))
             (iter (for n in (child-nodes node))
                   (helper n))))
    (iter (for node in (mesh-nodes level-model))
          (helper node))
    level-model))

(defun min-collide (hits)
  "Return the hit with the smallest distance from the collider"
  (iter (with min-d = nil)
        (for hit in hits)
        (when (and hit (or (null min-d) (< (car hit) (car min-d))))
          (setf min-d hit))
        (finally (return min-d))))

;; intersect two spheres with velocities
(defmethod swept-sphere-collide ((sph-a bounding-sphere) va
                                 (sph-b bounding-sphere) vb)
  ;; Use a swept-sphere-point intersection test
  ;; by combining the radii in one sphere and and
  ;; subtract the velocities
  (let* ((sph-c (make-instance 'bounding-sphere
                               :rad (+ (rad sph-a)
                                       (rad sph-b))
                               :pos (pos sph-a)))
         (va-vb (vec3- va vb)))
  ;  (format t "~%new velocity: ~a~%" va-vb)
    (if-let (it (sphere-point-intersection sph-c va-vb (pos sph-b) 1.0))
            ;; return time of intersection and the point
            ;; NOTE: if we don't need the point, or want to calculate
            ;; it later, we can just return the time...
            (list
             it
             (let ((end-a (vec3+ (pos sph-a) (vec-scale3 va it)))
                   (end-b (vec3+ (pos sph-b) (vec-scale3 vb it))))

               (vec3->point
                (vec3+ end-a
                       (vec-scale3
                        (vec3- end-b end-a)
                        (/ (rad sph-a)
                           (+ (rad sph-a) (rad sph-b))))))))))
  #+disabled
  (let* ((ab (vec3- (pos sph-b) (pos sph-a)))
         (vb-va (vec3- vb va))
         (rab (+ (rad sph-a) (rad sph-b)))
         (q-a (dot vb-va vb-va))
         (q-b (* 2 (dot vb-va ab)))
         (q-c (- (dot ab ab) (sq rab))))
    ;; Check if overlapping at x=0
    (if (<= (dot ab ab) (sq rab))
        0
        (let ((r1 (quadratic q-a q-b q-c)))
          (if (null r1) (format t "null r1!!%"))
          (when (and r1 (>= r1 0) (<= r1 1.0))
              r1)))))

;;;
;;; Static Geometry Collision testing
;;;

;(defgeneric collide-test (bv bv))
;;;
;;; Collide
;;;  when testing entities (bounding-spheres) against static geometry
;;;  or, geometry that we put in r-trees (which has already been updated),
;;;  We transform the bounding sphere into the coordinate systems
;;;
;;;

(defvar +max-collision-depth+ 5)
(defvar *current-collision-depth* 0)
(defvar +min-collide-dist+ 1.0e-2)


(defun copy-sphere (sphere)
  (make-instance 'bounding-sphere
                 :rad (rad sphere)
                 :pos (pos sphere)))

(defvar +collision-eps+ 1.0e-3)

;; World = blt-model, for now
(defmethod collide-with-world ((sphere bounding-sphere) velocity
                               (world blt-model)
                               &optional (depth +max-collision-depth+))
  "Updates the entity obj after performing world-collision"
  (when (null depth)
    (setf depth +max-collision-depth+))

  (let ((test-sph (copy-sphere sphere))
        (test-vel velocity)
        up)

    (setf (svref test-vel 3) 0.0)

    ;; Debug version: only do one iteration
    #+disable
    (let ((hit (min-collide
                (iter (for node in (mesh-nodes world))
                      (collect (collide-with-world-node
                                test-sph test-vel node))))))
      (if hit
          (slide-sphere test-sph velocity hit)))

    ;; Loop to find the displacement vector
    (iter (with test-vel = velocity)
          (setf (svref test-vel 3) 0.0)
          (for i below depth)
          (for hit = (min-collide
                      (iter (for node in (mesh-nodes world))
                            (collect (collide-with-world-node
                                      test-sph test-vel node)))))
          (for end first (vec4+ (pos test-sph) test-vel)
               then (vec4+ end test-vel))
          (until (null hit))
          (when hit
            ;; get new origin and velocity
            (destructuring-bind (new-pos new-vel new-up)
                (slide-sphere test-sph test-vel hit)
              (setf (pos test-sph) new-pos
                    test-vel new-vel
                    end new-pos
                    up new-up)))

          ;; At the end return the displacement from original sphere
          ;; to new one
          (finally (return
                     (list (vec4- end (pos sphere)) up))))))

;; x0 stays the same, but the position needs to be moved
(defun transform-hit (transform hit)
  (when hit
    (list (car hit)
          (matrix-multiply-v transform (vec3->point (second hit))))))

(defmethod collide-with-world-node ((sphere bounding-sphere)
                                    velocity
                                    (node node))
  (let* ((inv-mat (rt-inverse (transform node)))
         (xformed-bv
          (transform-bounding-volume sphere inv-mat))
         (xformed-vel
          (matrix-multiply-v inv-mat velocity)))

    (transform-hit (transform node)
     (min-collide
      #+disabled
      (append
       (if-let (it (swept-sphere-collide
                    sphere velocity
                    (node-bounding-volume node) +zero-vec+))
               ;; if we intersect the bounding-shape, check the mesh
               (progn
                 (list (collide-test xformed-bv xformed-vel (mesh node))))))

       ;; Recursively collide with the children

      (iter (for child in (child-nodes node))
            (collect
             (collide-with-world-node xformed-bv xformed-vel child)))))))

(defmethod collide-with-world-node ((sphere bounding-sphere)
                                    velocity
                                    (node model-node))
  (let* ((inv-mat (rt-inverse (transform node)))
         (xformed-bv
          (transform-bounding-volume sphere inv-mat))
         (xformed-vel
          (matrix-multiply-v inv-mat velocity)))


    (transform-hit (transform node)
     (min-collide
      (append
      ; #+disabled
       (when (slot-exists-p node 'mesh)
         (list (collide-test xformed-bv
                             xformed-vel
                             (mesh node))))

       ;; Todo- fix swept-sphere-collide
       #+disabled
       (if-let
        (it
         ;;#+disabled
         (swept-sphere-collide
          sphere velocity
          (node-bounding-volume node) +zero-vec+)
         #+disabled
         (swept-sphere-collide
          xformed-bv xformed-vel
          (node-bounding-volume node) +zero-vec+))
        ;; if we intersect the bounding-shape, check the mesh
        (progn
          ;;(format t "Collided with node-sphere!~%")
          (list (collide-test xformed-bv xformed-vel (mesh node)))))

       ;; Recursively collide with the children
       (iter (for child in (child-nodes node))
             (collect
              (collide-with-world-node xformed-bv xformed-vel child))))))))


;; performs collision testing of an entity against an rtree of triangles
;; returns the results of moving-sphere-triangle-intersection
(defmethod collide-test ((sphere bounding-sphere) velocity
                         (r-tree spatial-trees-protocol:spatial-tree))

  (let ((results
         (spatial-trees:search (swept-sphere->aabb sphere velocity)
                               r-tree)))

    (iter (with min-hit = nil)
          (for tri in results)
          (for hit = (moving-sphere-triangle-intersection
                      sphere tri velocity))
          (when (and hit (or (null min-hit)
                             (< (car hit) (car min-hit))))
            (setf min-hit hit))
          (finally (return min-hit)))))

;; Do nothing for meshes
(defmethod collide-test ((sphere bounding-sphere) velocity
                         (mesh blt-mesh))
  t)




;; Ignoring these for now...dunno if we want them
#+disabled
(defmethod collide-test ((bv bounding-shape) velocity (model blt-model))
  "returns a point p if bv intersects with the geometry of model
   nil otherwise"
  (min-collide
   (iter (for node in (mesh-nodes model))
         (collect (collide-test bv velocity node)))))

#+disabled
(defmethod collide-test ((bv bounding-shape) velocity (node model-node))
  "recursively tests bv against this node and its children.  Note
   that, since currently the bounding volume of a node is not
   guaranteed to enclose its children, the children must always be
   tested"
  ;; We need to translate, and transform, the collidee to the
  ;; node coordinates.  We do this instead of translating the
  ;; node b/c it's a lot easier to translate a bounding sphere than
  ;; an r-tree.
  (let* ((inv-mat (rt-inverse (transform node)))
         (xformed-bv (transform-bounding-volume bv inv-mat))
        (xformed-vel (matrix-multiply-v inv-mat velocity)))
    (min-collide
     (cons (if-let (it (collide-test xformed-bv (node-bounding-shape node)))
                   ;; if we intersect the bounding-shape, check the mesh
                   (collide-test xformed-bv (mesh node)))
           (iter (for child in (child-nodes node))
                 (collect (collide-test xformed-bv xformed-vel child)))))))
