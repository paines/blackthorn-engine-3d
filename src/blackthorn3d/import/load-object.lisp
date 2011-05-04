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
  (with-slots (stream stride) vs
    (subseq stream (* index stride) (+ (* index stride) stride))))

(defun (setf vs-ref) (vec vs index)
  (with-slots (stream stride) vs
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
   (transform
    :accessor transform
    :initarg :transform
    :initform nil
    :documentation "Base object->world transform")
   (elements
    :accessor elements
    :initarg :elements
    :documentation "A list of elem objects")
   (bounding-volume
    :accessor bounding-volume
    :initarg :bounding-volume
    :documentation "For kicks, but I think we're going to need this")))




;;;
;;; Model loading-specific code.
;;;

;; Note that this assumes that all the semantics in order exist in 
;; vertex-streams. The behavior is currently incorrect if this isn't true
;; It is fine to have extra semantics in vertex-streams, they will
;; be dropped
(defun organize-streams (vertex-streams order)
  "@arg[order]{A list of form (SEMANTIC SEMANTIC ... ) specifiying
               a desired order for the streams}"
  (iter (for vs in vertex-streams)
        (for o in order)
        (format t "order semantic: ~a   VS semantic: ~a~%" o (vs-semantic vs)))
  (iter (for o in order)
        (collect (find o vertex-streams :key #'(lambda (vs) (vs-semantic vs))))))

;; combines vertex-streams into one large 2-d array
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
          (minimizing (/ (length (vs-stream vs))
                         (vs-stride vs)) into s)
          (sum (vs-stride vs) into d)
          (finally (setf size s) (setf depth d)))
    (let ((interleaved (make-array (list size depth)))
          (index 0))
      (format t "size: ~a depth: ~a~%" size depth)
      ;; For each vertex
      (iter (for i below size)
            (iter (for vs in vertex-streams)
                  (iter (for elt in-vector (vs-ref vs i))
                        (setf (row-major-aref interleaved index)
                              (float elt))
                        (incf index))))
      (format t "~a~%" interleaved)
      (values
       interleaved
       #'(lambda (index) 
           (iter (for vs in vertex-streams)
                 (collect 
                     (list (vs-semantic vs) (vs-ref vs index)))))))))