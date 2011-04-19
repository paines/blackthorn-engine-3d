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

(defvar +library_geometries+ "library_geometries")
(defvar +geometry-block+ "geometry")
(defvar +mesh-block+ "mesh")
(defvar +source-block+ "source")
(defvar +accessor+ "accessor")
(defvar +vertices+ "vertices")
(defvar +triangles+ "triangles")

(defparameter *file* nil)
(defparameter *source-ht* nil)

(defun xml-listp (lst)
  (and (consp lst)
       (consp (car lst))))

(defun children (xml-lst)
  (cddr xml-lst))

;; Returns first xml child of xml-lst
(defun first-child (xml-lst)
  (find-if #'consp (children xml-lst)))

(defun attributes (xml-lst)
  (second xml-lst))

(defun tag-name (xml-lst)
  (caar xml-lst))

(defun find-tag (tag lst) 
  (if (consp lst)
      (let ((xml-lst (find-if #'consp lst)))
        (if (string-equal (tag-name xml-lst) tag)
            xml-lst
            (or (find-tag tag (children xml-lst))
                (find-tag tag (cdr lst)))))
      nil))

(defun find-tag-in-children (tag xml-lst)
  (let ((children (remove-if-not #'consp (children xml-lst))))
    (find tag children
          :test #'string-equal
          :key #'car)))

(defun get-attribute (attrib attrib-lst)
  (aif (member attrib attrib-lst :test #'string-equal :key #'car)
       (second (car it))
       nil))
  
(defun string->sv (str)
  (with-input-from-string (s str)
    (apply #'vector
           (iter (for val = (read s nil :eof ))
                 (until (eql val :eof))
                 (collect val)))))

(defun make-accessor (accessor-lst)
  (let ((stride (parse-integer (get-attribute "stride" 
                                              (attributes accessor-lst)))))
    #'(lambda (array index)
        (apply #'vector 
               (iter (for i below stride)
                     (collect (svref array (+ i (* index stride)))))))))

(defun make-components (accessor-lst)
  (mapcar #'(lambda (child) 
              (get-attribute "name" (attributes child)))
          (remove-if-not #'consp (children accessor-lst))))

(defclass dae-source ()
  ((id 
    :accessor src-id
    :initarg :id)
   (array 
    :accessor src-array
    :initarg :array)
   (accessor 
    :accessor src-accessor
    :initarg :accessor
    :documentation "function taking an array and index")
   (components 
    :accessor src-components
    :initarg :components)))

(defun make-source (src-lst)
  (let ((accessor-lst (find-tag +accessor+ (children src-lst))))
    (make-instance 'dae-source 
                   :id (get-attribute "id" (attributes src-lst))
                   :array (string->sv (car (children (first-child src-lst))))
                   :accessor (make-accessor accessor-lst)
                   :components (make-components accessor-lst))))

(defun hash-sources (xml-lsts)
  (let ((src-lst (mapcar #'make-source
                         (remove-if-not #'(lambda (x)
                                            (and (consp x)
                                                 (string-equal (tag-name x) "source")))
                                        xml-lsts))))
    (iter (for src in src-lst)
          (setf (gethash (src-id src) *source-ht*) src))))

(defun set-vertices (vert-lst)
  (setf (gethash (get-attribute "id" (attributes vert-lst)) *source-ht*)
        (gethash (subseq (get-attribute "source" (attributes (first-child vert-lst))) 1) 
                 *source-ht*)))

(defun input->source (str)
  (gethash (subseq str 1) *source-ht*))

(defun build-input-lst (prim-chld-lst)
  (iter (for xml in prim-chld-lst)
        (if (and (consp xml) (string-equal "input" (tag-name xml)))
            (let ((attribs (attributes xml)))
              (collect (list (get-attribute "semantic" attribs)
                             (input->source (get-attribute "source" attribs))))))))


;; Returns a list of (fn . array) where calling fn with an index modifies array
;; fn is designed to take in index to a source and add the corresponding value(s) 
;; to array. 
(defun get-source-functions (inputs)
  (iter (for i in inputs)
        (collect 
         (let* ((source (second i))
                (attrib-len (/ (length (src-array source)) 
                               (length (src-components source))))
                (attrib-vec (make-array attrib-len :fill-pointer 0
                                        :adjustable t)))
           (cons #'(lambda (index)
                     (let ((src-array-vec (funcall (src-accessor source) 
                                                   (src-array source) 
                                                   index)))
                       #+disabled(iter (for elt in-vector src-array-vec)
                             (vector-push elt attrib-vec))
                       (vector-push-extend src-array-vec attrib-vec)))
                 attrib-vec)))))

;; this function is going to need some serious refactoring ... >_<
(defun process-indices (tri-lst)
  (let* ((input-lst (build-input-lst (children tri-lst)))
         (num-inputs (length input-lst))
         ;; The list of indices
         (prim-arr (string->sv (third (find-tag "p" (children tri-lst)))))
         ;; Hash table of previously-seen vertex arrangements
         (vertex-ht (make-hash-table :test #'equalp))
         (num-tri (parse-integer (get-attribute "count" (attributes tri-lst))))
         (num-vert (* num-tri 3))
         (fns (get-source-functions input-lst))
         ;; Index Array
         (curr-index 0)
         (indices (make-array num-vert :fill-pointer 0)))
    ;; For each index section: If it's already been seen before get that index
    ;; otherwise create a new index and set the arrays
   (iter (for i below (length prim-arr) by num-inputs)
          (let ((vertex (subseq prim-arr i (+ i num-inputs))))
            (aif (gethash vertex vertex-ht)
                (vector-push it indices)
                (progn
                  (setf (gethash vertex vertex-ht) curr-index)
                  (vector-push curr-index indices)
                  (iter (for f in fns)
                        (for i in-vector vertex)
                        (funcall (car f) i))
                  (incf curr-index)))))
    ;; we return a list of each attribute array with it's semantic
   (cons indices (mapcar #'(lambda (input fn)
                             (cons (car input) (cdr fn)))
                         input-lst fns))))

;; combines arrays into one large 2-d array
(defun interleave (arrays)
  (let* ((size (length (cdar arrays)))
         (depth (iter (for (s . a) in arrays)
                      (sum (length (aref a 0)))))
         (interleaved (make-array `(,size ,depth)))
         (index 0))
    (iter (for j below size)
          (iter (for (semantic . a) in arrays)
                (iter (for elt in-vector (aref a j))
                      (setf (row-major-aref interleaved index) elt)
                      (incf index))))
    interleaved))

(defun blt-mesh-array->gl-array (array)
  (let* ((count (array-dimension array 0))
         (vertex-size (array-dimension array 1))
         (gl-array (gl:alloc-gl-array 'blt-vnt-mesh count)))
    (iter (for i below count)
          (iter (for j below vertex-size)
                (for c in *blt-mesh-components*)
                (setf (gl:glaref gl-array i c) (aref array i j))))
    gl-array))

(defun indices->gl-array (indices)
  (let* ((count (length indices))
         (gl-array (gl:alloc-gl-array :unsigned-short count)))
    (iter (for i below count)
          (setf (gl:glaref gl-array i) (aref indices i)))
    gl-array))

;; constructs a mesh object from an xml-list mesh tag
(defun build-mesh (xml-lst)
  (let ((children (children xml-lst)))
    (setf *source-ht* (make-hash-table :test #'equal))
    (hash-sources children)
    (set-vertices (find-tag +vertices+ children))
    (destructuring-bind (indices &rest arrays) 
        (process-indices (find-tag +triangles+ children))
      ;(cons indices (interleave arrays))
      ;#+disabled
      (make-instance 'mesh 
                     :vert-data (blt-mesh-array->gl-array 
                                 (interleave arrays))
                     :indices (indices->gl-array indices)
                     :array-format 'blt-mesh
                     :primitive 'triangles))))

(defun load-dae (filename)
  (let ((dae-file (cxml:parse-file (blt3d-res:resolve-resource filename) 
                                   (cxml-xmls:make-xmls-builder))))
    #+disabled(mapcar #'build-mesh 
            (remove-if-not #'(lambda (x)
                               (and (consp x) 
                                    (string-equal "mesh" (tag-name x))))
                           (children dae-file))) 
    (build-mesh (find-tag "mesh" (children dae-file)))))