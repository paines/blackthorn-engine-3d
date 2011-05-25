;;;; Blackthorn -- Lisp Game Engine
;;;;
;;;; Copyright (c) 2007-2011, Elliott Slaughter <elliottslaughter@gmail.com>
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

(defpackage :blackthorn3d-physics
  (:nicknames :blt3d-phy)
  (:use :iter :cl :alexandria :userial :blt3d-utils :blt3d-math :blt3d-ent)
  (:import-from :blt3d-ani :apply-transform)
  (:export

   ;; blt-mesh.lisp
   :blt-mesh
   :make-blt-mesh
   :vertex-streams
   :elements
   :mesh-bounding-volume

   :blt-skin
   :make-blt-skin
   :bind-skeleton
   :bind-shape-matrix

   :node
   :model-node
   :make-model-node
   :make-scene-node
   :transform
   :mesh
   :material-array
   :node-bounding-volume
   :child-nodes

   :vertex-stream
   :vs-semantic
   :vs-ref
   :get-stream
   
   :element
   :make-element
   :element-material
   :element-indices
   :element-count


   ;; blt-model.lisp
   :id
   :blt-model
   :mesh-nodes
   :animations
   :expand-bounding-spheres
   :apply-transform

   :blt-material
   :make-blt-material
   :ambient
   :diffuse
   :specular
   :shininess
   :textures

   :interleave
   :make-triangle

   ;; shapes.lisp
   :bounding-shape
   :bounding-sphere

   ;; aa-bounding-box
   :aa-bounding-box

   :move-bounding-volume
   :move-bounding-volume-set
   :find-bounding-points
   :make-bounding-box
   :make-bounding-sphere
   :make-bounding-volume

   ;; intersect.lisp
   :point-line-sq-distance
   :sphere-triangle-intersection
   :sphere-edge-intersection
   :sphere-point-intersection
   :make-ray
   :ray-sphere-intersection
   :make-plane
   :plane-dist

   ;; collision.lisp
   :collide-p
   
   ;; motion.lisp
   :move
   :chase 
   :jump
   :standard-physics-step

   ;; skeleton.lisp
   :joint-id
   :joint-matrix
   :child-joints
   :make-joint
   :make-skeleton
   :update-skeleton
   :root-joint
   ;:get-skeleton
   :get-joint-matrices

   ;; static-collision.lisp (needs refactoring?)
   :initialize-cube
   :swept-sphere-collide
   :point-in-triangle-p
   :collide-with-world

   ;; octree-lisp
   :octree
   :octree-node
   :children
   :center
   :width
   :make-octree
   :octree-insert
   :octree-delete
   :octree-query

   ;; camera.lisp
   :camera-matrix
   :camera-inverse
   :camera
   :target
   :mode
   :update-camera
   :move-player
   :minor-mode
   ))