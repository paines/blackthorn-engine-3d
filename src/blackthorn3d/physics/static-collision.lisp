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
  (format t "BUILD-R-TREE: num tri: ~a~%" (length triangles))
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
             (setf (mesh node)
                   (build-r-tree (build-triangle-array (mesh node))))
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
              r1))))

  ;; turn sph1 into a line and add the radius to 2
  #+disabled
  (let* ((point (pos sph1))
         (sph3 (make-instance 'bounding-sphere 
                              :rad (+ (rad sph1) (rad sph2))
                              :pos (pos sph2)))
         ;; don't norm so we can test [0 1]
         (l (vec3- v1 v2))
         (sq-l (sq-mag l))
         (one/sq-l (/ 1 sq-l))
         ;; make the line at the origin
         (c (vec3- (pos sph3) point))
         (B (dot l c))
         (det (- (sq B) 
                 (* sq-l (+ (dot c c) 
                            (sq (rad sph3)))))))
    (if (minusp det)
        nil
        (let ((r (min (* (+ B (sqrt det)) one/sq-l) 
                      (* (- B (/ (sqrt det))) one/sq-l))))
          (if (and (>= r 0.0) (<= r 1.0))
              (list r (vec4+ point (vec-scale3 l r))))))))

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
(defmethod collide-with-world ((obj entity-server) (world blt-model))
  "Updates the entity obj after performing world-collision"
  (with-slots ((sphere bounding-volume) velocity) obj
    (let ((test-sph (copy-sphere sphere))
          (test-vel velocity))

      (setf (pos test-sph) (pos obj)
            #+disabled (vec4+ (pos test-sph) (pos obj)))
      (setf (svref test-vel 3) 0.0)

      (when (and (/= 0.0 (x velocity))
                 (/= 0.0 (y velocity))
                 (/= 0.0 (z velocity)))
       ; (format t "~%## VELOCITY: ~a~%~3T~a~%" velocity test-vel)
        )

     ; (format t "TEST SPHERE AT: ~a~%" (pos test-sph))

      ;; Debug version: only do one iteration
      (let ((hit (min-collide
                  (iter (for node in (mesh-nodes world))
                        (collect (collide-with-world-node 
                                  test-sph test-vel node))))))
        (if hit
            (let* ((x0 (car hit))
                   (hit-pt (second hit))
                   (new-bp (pos test-sph))
                   (dest (vec4+ new-bp velocity))
                   (closest-dist (mag (vec-scale4 velocity x0))))

             ; (format t "top level hit: ~a~%" hit)
              
              ;; only move the base point if we aren't already
              ;; very close to the hit
              (if (>= closest-dist 0.0001)
                  (setf new-bp 
                        (vec4+ new-bp 
                               (set-length4 velocity 
                                            (- closest-dist 0.0001)))
                        hit-pt (vec4- hit-pt (set-length4 velocity
                                                          0.0001))))

              ;; make sliding plane
              (let* ((plane-normal (norm4 (vec4- new-bp hit-pt)))
                     (plane-origin hit-pt)
                     (sliding-plane (point-normal->plane plane-origin
                                                         plane-normal))
                     (new-dest (vec4-
                                dest
                                (vec-scale4
                                 plane-normal
                                 (plane-dist sliding-plane dest))))
                     (new-vel (vec4- new-dest new-bp)))
                ;; return the new velocity as the vector from start
                ;; to new-dest + new-vel
               ; (setf (pos obj) new-bp)
                (vec-scale4 velocity (car hit))
               ; (vec4- (vec4+ new-dest new-vel) (pos test-sph))
                ))

            #+disabled
            (progn
              (format t "top level hit: ~a~%" hit)
              ;; don't allow the player to move into geometry
              (destructuring-bind (new-pos new-vel)
                  (slide-sphere test-sph test-vel hit)

                (vec3->vec (vec3- (vec3+ new-pos new-vel) (pos test-sph)))))

            #+disabled
            (progn 
              (when (> (car hit) 0.0) (format t "hit!!   ~a~%" hit))
              (let* ((ret-vel 
                      (vec-scale4 test-vel (car hit))
                       #+disabled(vec-scale4 (norm4 test-vel) 
                                             +min-collide-dist+)))

                (format t "new vector: ~a~%" ret-vel)
                ret-vel))
            (progn; (setf (pos obj) (vec4+ (pos obj) velocity))
                   test-vel)))

      ;; Loop to find the displacement vector
      #+disabled
      (iter (with test-vel = velocity)
            (for i below +max-collision-depth+)
            ;(until (< (sq-mag test-vel) +min-collide-dist+))
            (for hit = (min-collide
                        (iter (for node in (mesh-nodes world))
                              (collect (collide-with-world-node 
                                        test-sph test-vel node)))))
            (until (null hit))
            (when hit
              (format t "top level hit = ~a~%" hit)
              ;; get new origin and velocity
              (destructuring-bind (new-pos new-vel)
                  (slide-sphere test-sph test-vel hit)
                (setf (pos test-sph) new-pos
                      test-vel new-vel)))

            ;; At the end return the displacement from original sphere
            ;; to new one
            (finally (return  
                       (vec4- (vec4+ (pos test-sph) test-vel) 
                              (vec4+ (pos sphere) (pos obj)))))))))

;; x0 stays the same, but the position needs to be moved
(defun transform-hit (transform hit)
  (when hit
    (list (car hit)
          (matrix-multiply-v transform (vec3->point (second hit))))))

(defmethod collide-with-world-node ((sphere bounding-sphere)
                                    velocity
                                    (node model-node))
  (let* ((inv-mat (rt-inverse (transform node)))
         (xformed-bv 
          (transform-bounding-volume sphere inv-mat))
         (xformed-vel 
          (matrix-multiply-v inv-mat velocity)))

    ;(format t "## sphere-center: ~a~%" (pos xformed-bv))
    ;; don't forget to re-transform on way back up
    ;(format t "node transform ~a~%" (transform node))
    (transform-hit (transform node)
     (min-collide
      (append
       (list (collide-test xformed-bv
                           xformed-vel
                           (mesh node)))

       ;; Todo- fix swept-sphere-collide
       #+disabled
       (aif (swept-sphere-collide xformed-bv xformed-vel
                                  (node-bounding-volume node) +zero-vec+)
            ;; if we intersect the bounding-shape, check the mesh
            (progn
              ;(format t "WE HIT A SPHERE! @ node ~a~%" (id node))
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
     (cons (aif (collide-test xformed-bv (node-bounding-shape node))
                ;; if we intersect the bounding-shape, check the mesh
                (collide-test xformed-bv (mesh node)))
           (iter (for child in (child-nodes node))
                 (collect (collide-test xformed-bv xformed-vel child)))))))