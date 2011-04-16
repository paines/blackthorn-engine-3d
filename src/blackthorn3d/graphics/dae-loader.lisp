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
  (setf (gethash (get-attribute "id" (attributes vert-lst)) *sources-ht*)
        (gethash (get-attribute "source" (attributes (first-child vert-lst))) 
                 *sources-ht*)))

(defun input->source (str)
  (gethash (string-left-trim '(#\#) str) *source-ht*))

(defun build-input-lst (prim-chld-lst)
  (iter (for xml in prim-chld-lst)
        (if (and (consp xml) (string-equal "input" (tag-name xml)))
            (let ((attribs (attributes xml)))
              (collect (list (get-attribute "semantic" attribs)
                             (input->source (get-attribute "source" attribs))))))))


(gl:define-gl-array-format blt-mesh
  (gl:vertex :type :float :components (px py pz))
  (gl:normal :type :float :components (nx ny nz))
  (gl:tex-coord :type :float :components (u v)))


;; Step 1: check if indices are already in vert-ht
;; if step 1 = true, get the index
;; else
;; Step 2: increment curr-index and add to vert-ht hashed with indices
;; Step 3: get the vertex values from positions, normals, texcoords, and add to arrays
;; 
(defun process-index (index-lst inputs vert-ht)
  ())

(defun get-vertex-attrib (semantic input-lst primative)
  (iter (for i below (length primative))
        (for input in input-lst)
        (when (eql (intern (car input)) semantic)
            (let ((source (second input)))
              (return-from get-vertex-attribute 
                (apply (src-accessor source) (src-array source) (svref primative i)))))))

;; this function is going to need some serious refactoring ... >_<
(defun process-indices (tri-lst)
  (let* ((input-lst (build-input-lst (children tri-lst)))
         ;; The list of indices
         (prim-arr (string->sv (third (find-tag "p" (children tri-lst)))))
         ;; Hash table of previously-seen vertex arrangements
         (vertex-ht (make-hash-table :test #'equalp))
         (ind-len (length input-lst))
         (num-tri (parse-integer (get-attribute "count" (attributes tri-lst))))
         (num-vert (* num-tri 3))
         (curr-index 0)
         ;; Array indexes (into primatives)
         (pos-index (position "VERTEX" input-lst :key #'car))
         (norm-index (position "NORMAL" input-lst :key #'car))
         (tex-index (position "TEXCOORD" input-lst :key #'car))
         ;; Arrays (conservatively sized for now) to hold data (will be re-arranged later)
         (vertices  (make-array num-vert :fill-pointer 0 :adjustable t))
         (normals   (make-array num-vert :fill-pointer 0 :adjustable t))
         (texcoords (make-array num-vert :fill-pointer 0 :adjustable t))
         (indices   (make-array num-vert :fill-pointer 0 :adjustable t)))
    (iter (for i below (length prim-arr) by ind-len)
          (let ((triangle (subseq prim-arr (+ (* ind-len i)) ind-len)))
            (aif (gethash triangle vert-ht)
                 (vector-push-extend indices it)
                 (progn
                   (setf (gethash triangle vert-ht) curr-index)
                   (vector-push-extend vertices  ())
                   (vector-push-extend normals   (get-vertex-attrib 'normal input-lst triangle))
                   (vector-push-extend texcoords (get-vertex-attrib 'texcoord input-lst triangle))
                   (vector-push-extend curr-index)
                   (incf curr-index)))))))

;; constructs a mesh object from an xml-list mesh tag
(defun build-mesh (xml-lst)
  (setf *source-ht* (make-hash-table))
  (hash-sources (children xml-lst))
  (set-vertices (find-tag +vertices+ xml-lst))
  (process-indices (find-tag +triangles+ xml-lst)))

(defun load-dae (filename)
  (let ((dae-file (cxml:parse-file filename (cxml-xmls:make-xmls-builder))))
    (build-mesh (find-tag "mesh" (list dae-file)))))