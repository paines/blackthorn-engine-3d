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

(in-package :blackthorn3d-import)

;;;
;;; The Intermediate Representation
;;; and processing tools
;;;


;; This is our intermediate representation, basically just a 
;; representation/organization of all the data we loaded from
;; the dae file
;;
;; In it's current state, it only holds dae-mesh objects
;; (ie, no scene data).
(defclass load-object ()
  ((meshes
    :accessor lo-meshes
    :initarg :meshes)))

(defclass vertex-stream ()
  ((semantic
    :accessor vs-semantic
    :initarg :semantic)
   (stream
    :accessor vs-stream
    :initarg :stream)
   (stride
    :accessor vs-stride
    :initarg :stride)))

(defun vs-ref (vs index)
  (with-slots (stream) vs
    (aref stream index)))

;; for now i'm going to assume no one is being dumb and putting
;; wrong things in streams
(defun (setf vs-ref) (vec vs index)
  (with-slots (stream stride) vs
    (setf (aref stream index) vec)
    #+disabled
    (iter (for elt in-vector vec)
          (for i below stride)
          (setf (svref stream (+ i (* index stride))) elt))))

(defclass elem ()
  ((indices
    :accessor elem-indices
    :initarg :indices)
   (count
    :accessor elem-count
    :initarg :count)
   (material
    :accessor elem-material
    :initarg :material)
   (unifiedp 
    :accessor elem-unifiedp
    :initarg :unifiedp
    :initform nil)))

(defclass blt-material ()
  ((ambient
    :initarg :ambient
    :initform #(0.0 0.0 0.0 1.0))
   (diffuse
    :initarg :diffuse
    :initform #(1.0 1.0 1.0 1.0))
   (specular
    :initarg :specular
    :initform #(0.0 0.0 0.0 1.0))
   (shininess
    :initarg :shininess
    :initform 0.0)
   (textures
    :initarg :textures
    :initform nil)))


;; This is going to be our generic mesh object. It's what we load 
;; other formats into, and is converted by the graphics subsystem into
;; appropriate objects for rendering.  
;; It will need to have vertex and index information, as well as skinning,and
;; whatever else
;; I think I want to store the vertexes unified, if not interleaved.
;; Unfortunately this makes life harder for the dae-geometry section.
(defclass blt-mesh ()
  ((id
    :accessor id
    :initarg :id)
   (vertex-streams
    :accessor vertex-streams
    :initarg :vertex-streams
    :documentation "A list of vertex-stream objects")
   (controller
    :accessor controller
    :initarg :controller
    :documentation "Rigged models need a control element to attach
                   the vertices to bone data.  See dae-controller.lisp
                   for more details")
   (elements
    :accessor elements
    :initarg :elements
    :documentation "A list of elem objects")
   (bounding-volume
    :accessor bounding-volume
    :initarg :bounding-volume
    :documentation "For narrow-phase collision detection
                   to start we can use something coarse like a sphere
                   but eventually might be something shiny, like bsp,
                   or heirarchy of bvs")))


(defun make-blt-mesh (&key id vertex-streams elements transform)
  (make-instance 'blt-mesh
                 :id id
                 :vertex-streams vertex-streams
                 :elements elements))

(defmethod get-stream (stream (this blt-mesh))
  (aif (find stream (vertex-streams this) :key #'vs-semantic)
       (vs-stream it)))



(defclass mesh-instance ()
  ((transform
    :accessor transform
    :initarg :transform)
   (material-array
    :accessor material-array
    :initarg :material-array)
   (mesh
    :accessor mesh
    :initarg :mesh)
   (bounding-volume
    :accessor bounding-volume
    :initarg :bounding-volume)))

(defun make-mesh-instance (&key transform material-array mesh)
  (make-instance 'mesh-instance
                 :transform transform
                 :material-array material-array
                 :mesh mesh
                 :bounding-volume (blt3d-phy:make-bounding-volume
                                   (get-stream :vertex mesh))))

#+disabled
(defmethod finalize ((this blt-mesh) &key (xform-bv t))
  (with-slots (vertex-streams elements transform bounding-volume) this
    (setf bounding-volume (transform-bounding-volume 
                           (make-bounding-volume 
                            (vs-stream
                             (find :vertex vertex-streams :key #'vs-semantic)))
                           transform))))

;;;
;;; Model loading-specific code.
;;;


;; Note that this assumes that all the semantics in order exist in 
;; vertex-streams. The behavior is currently incorrect if this isn't true
;; It is fine to have extra semantics in vertex-streams, they will
;; be dropped
(defun order-streams (vertex-streams order)
  "@arg[order]{A list of form (SEMANTIC SEMANTIC ... ) specifiying
               a desired order for the streams}"
  (iter (for vs in vertex-streams)
        (for o in order)
        (format t "order semantic: ~a   VS semantic: ~a~%" o (vs-semantic vs)))
  (iter (for o in order)
        (collect (find o vertex-streams :key #'(lambda (vs) (vs-semantic vs))))))

;; combines unified vertex-streams into one large 2-d array
;; If you want a specific order, re-order/prune the data before
;; calling this function on it.
;; Returns (2d-array indexing-fn)
;; indexing-fn is a function that returns a list
;; containing (SEMANTIC VECTOR) elements for each stream that
;; was interleaved into the array
(defun interleave (vertex-streams)
  (let ((size 0)
        (depth 0))
    (iter (for vs in vertex-streams)
          (minimizing (length (vs-stream vs))  into s)
          (sum (vs-stride vs) into d)
          (finally (setf size s) (setf depth d)))
    (let ((interleaved (make-array (list size depth)))
          (index 0))
      ;(format t "size: ~a depth: ~a~%" size depth)
      ;; For each vertex
      (iter (for i below size)
            (iter (for vs in vertex-streams)
                  (iter (for elt in-vector (vs-ref vs i))
                        (setf (row-major-aref interleaved index)
                              (float elt))
                        (incf index))))
      (values
       interleaved
       #'(lambda (index) 
           (iter (for vs in vertex-streams)
                 (collect 
                     (list (vs-semantic vs) (vs-ref vs index)))))))))

;;;
;;; Triangle access
;;;

;; I will represent a triangle using 3 vertices and a face normal
;; This will be represented by an array #(v0 v1 v2 n)
;;

(defun make-triangle (v0 v1 v2)
  (let ((normal (cross (vec4- v1 v0)
                       (vec4- v2 v0))))
    (vector v0 v1 v2 normal)))

;; Returns the triangle at index from the blt-mesh
;; If the mesh has multiple elements, the indexes are treated
;; as incrementing accross elements. The first element has 
;; indices [0 elem1.count) the next one has [elem1.count elem2.count)
;; etc.

(defun tri-in-elt (elem vs index)
  (let ((i (* 3 index)))
    (make-triangle
     (v0 (vs-ref vertices i))
     (v1 (vs-ref vertices (+ i 1)))
     (v2 (vs-ref vertices (+ i 2))))))

(defmethod triangle ((this blt-mesh) index)
  (with-slots (elements vertex-streams) this
    ;; First find the element we're in
    (multiple-value-bind (element start-index)
        (iter (for elt in elements)
              (for s first 0 then (+ s (elem-count elt)))
              (for last-s previous s initially 0)
              (finding elt such-that (> s index))
              (finally (return (values elt (- index last-s))))))
    (let ((vertices (find :vertex vertex-streams :key #'vs-semantic)))
      ;; MUST have vertex positions!
      (unless (null vertices)
        (tri-in-elt element vertices start-index)))))

(defmethod build-triangle-array ((this blt-mesh))
  (with-slots (elements vertex-streams) this
    (let ((vertices (find :vertex vertex-streams :key #'vs-semantic))
          (triangles (make-array (/ (iter (for elt in elements)
                                          (sum (elem-count elt))) 3)))
          (index 0))
      (iter (for element in elements)
            (iter (for i below (elem-count element))
                  (setf (svref triangles index) 
                        (tri-in-elt element vertices i)))))))


;;;
;;; Bounding Volume stuff herr
;;;

