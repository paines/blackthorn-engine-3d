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
  (:use :iter :cl :alexandria :userial :blt3d-utils :blt3d-math)
  (:export

   ;; blt-model.lisp
   :id
   :blt-model
   :mesh-nodes
   :animations

   :blt-mesh
   :make-blt-mesh
   :vertex-streams
   :elements
   :mesh-bounding-volume

   :model-node
   :make-model-node
   :transform
   :mesh
   :material-array
   :node-bounding-volume
   :child-nodes

   :vertex-stream
   :vs-ref
   :get-stream
   
   :element
   :make-element
   :element-material
   :element-indices
   :element-count

   :blt-material
   :make-blt-material
   :ambient
   :diffuse
   :specular
   :shininess
   :textures

   :interleave

   ;; shapes.lisp

   ;; aa-bounding-box
   :aa-bounding-box

   :move-bounding-volume
   :move-bounding-volume-set
   :find-bounding-points
   :make-bounding-box
   :make-bounding-sphere
   :make-bounding-volume

   ;; collision.lisp
   :collide-p
   ))