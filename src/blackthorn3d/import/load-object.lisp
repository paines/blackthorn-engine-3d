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

(defclass source ()
  ((id 
    :accessor src-id
    :initarg :id)
   (array 
    :accessor src-array
    :initarg :array)
   (stride 
    :initarg :stride
    :initform 1)
   (components 
    :accessor src-components
    :initarg :components)))

(defun src-accessor (src index)
  (with-slots (stride array) src
    (subseq array (* index stride) (+ (* index stride) stride))))

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

(defclass load-mesh ()
  ((id
    :accessor id
    :initarg :id)
   (sources
    :accessor sources
    :initarg :sources
    :documentation "A list of sources, which are lists of 
                   (SEMANTIC DATA)")
   (controller
    :accessor controller
    :initarg :controller
    :documentation "Rigged models need a control element to attach
                   the vertices to bone data.  See dae-controller.lisp
                   for more details")
   (transform
    :accessor transform
    :initarg :transform
    :initform nil)
   (elements
    :accessor elements
    :initarg :elements
    :documentation "A list of elems")))

;; Returns a list of (fn . array) where calling fn with an index modifies array
;; fn is designed to take in index to a source and add the corresponding value
;; to array. 
(defun get-source-functions (sources)
  (iter (for src in sources)
        (collect 
         (let* ((source (second src))
                (attrib-len (/ (length (src-array source)) 
                               (length (src-components source))))
                (attrib-vec (make-array attrib-len 
                                        :fill-pointer 0
                                        :adjustable t)))
           (cons #'(lambda (index)
                     (vector-push-extend 
                      (src-accessor source index)
                      attrib-vec))
                 attrib-vec)))))

(defun make-src-arrays (sources)
  (iter (for input in sources)
        (let* ((source (second input))
               (attrib-len (/ (length (src-array source))
                              (length (src-components source)))))
          (collect (make-array attrib-len 
                               :fill-pointer 0
                               :adjustable t)))))


;;;
;;; Model loading-specific code.
;;;

;; Takes the elements of a load mesh and returns 
;; the unified arrays (in the format (SEMANTIC COMPONENTS ARRAY)
;; the array is an array of arrays
(defmethod unify-indices ((this load-mesh))
  (with-slots (sources controller elements) this
    (let ((n-inputs (length sources))
          (src-fns (get-source-functions sources))
          #+disabled(unified-arrs (make-src-arrays sources))
          (vertex-ht (make-hash-table :test #'equalp)))
      (list 
       (iter 
        (for elt in elements)
        (let* ((indices (elem-indices elt))
               (n-verts (/ (length indices) n-inputs))
               (curr-index 0)
               (new-indices (make-array n-verts :fill-pointer 0)))
          (iter (for i below (length indices) by n-inputs)
                (let ((vertex (subseq indices i (+ i n-inputs))))
                  (aif (gethash vertex vertex-ht)
                       (vector-push it new-indices)
                       (progn
                         (setf (gethash vertex vertex-ht)
                               curr-index)
                         (vector-push curr-index new-indices)
                         (iter (for f in src-fns)
                               (for i in-vector vertex)
                               (funcall (car f) i))
                         (incf curr-index)))))
          (collect (make-instance
                    'elem
                    :indices new-indices
                    :material (elem-material elt)))))
       (iter (for (semantic source) in sources)
             (for fn in src-fns)
             (collect 
              (list semantic 
                    (src-components source) 
                    (cdr fn))))))))

;; combines arrays into one large 2-d array
;; input ARAYS are of format (SEMANTIC COMPONENTS ARRAY)
;; ORDER is a list of format ((SEMANTIC . SIZE) (SEMANTIC . SIZE) ... )
;; that defines the order in which to interleave and how many
;; elements for each one
;; If there is a semantic in ORDER that is not found
;; in ARRAYS then it will be set to 0.0
(defun interleave (arrays order)
  (let* ((size (iter (for (sem comp array) in arrays)
                     (minimizing (length array))))
         (depth (iter (for (sem . sz) in order)
                      (sum sz)))
         (interleaved (make-array (list size depth)))
         (index 0))
    ;; For each vertex
    (iter (for j below size)
          (iter (for (semantic . el-sz) in order)
                (aif (find semantic arrays :key #'car :test #'equal)
                     (let ((i 0))
                       (iter (for elt in-vector (aref (third it) j))
                             (setf (row-major-aref interleaved index) elt)
                             (incf i) (incf index))
                       (iter (for k from i below el-sz)
                             (setf (row-major-aref interleaved index) 0.0)
                             (incf index)))
                     (iter (for k below el-sz)
                           (setf (row-major-aref interleaved index) 0.0)
                           (incf index)))))
    interleaved))