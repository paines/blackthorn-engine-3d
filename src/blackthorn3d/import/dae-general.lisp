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
;;; Functions, data structures, and constants
;;; common to many collada elements
;;;

(defvar +geometry-library+ "library_geometries")
(defvar +material-library+ "library_materials")
(defvar +image-library+    "library_images")
(defvar +effect-library+   "library_effects")
(defvar +scene-library+    "library_visual_scenes")
(defvar +light-library+    "library_lights")
(defvar +animation-library+ "library_animations")

;;;
;;; Collada helper objects
;;;


;;;
;;; Collada helper functions
;;;

;; Source related functions

(defun make-accessor (accessor-lst array)
  "Returns a function that takes an index and returns a vector containing
   the data for that index"
  (let ((stride (parse-integer 
                 (get-attribute "stride" (attributes accessor-lst)))))
    #'(lambda (index)
        (subseq array (* index stride) (+ (* index stride) stride)))))

(defun make-components (accessor-lst)
  (mapcar #'(lambda (child) 
              (get-attribute "name" (attributes child)))
          (children-with-tag "param" accessor-lst)))

(defun make-source (src-lst)
  (let ((accessor-lst (find-tag +accessor+ (children src-lst)))
        (array (string->sv (car (children (first-child src-lst))))))
    (make-instance 'source 
                   :id (get-attribute "id" (attributes src-lst))
                   :array array
                   :stride (parse-integer
                            (get-attribute "stride" (attributes accessor-lst)))
                 ;  :accessor (make-accessor accessor-lst array)
                   :components (make-components accessor-lst))))

(defun hash-sources (xml-lsts)
  (let ((src-lsts (children-with-tag "source" xml-lsts))
        (src-table (make-id-table)))
    (iter (for src-lst in src-lsts)
          (let ((src (make-source src-lst)))
            (setf (gethash (src-id src) src-table) src)))
    src-table))

;; Input related 
;; (can be used for primitive blocks or any other block
;;  that uses the input-source model)

(defun input->source (str source-table)
  (gethash (subseq str 1) source-table))

(defun build-input-lst (prim-lst sources)
  (iter (for input in (children-with-tag "input" prim-lst))
        (let ((attribs (attributes input)))
          (collect (list (intern (get-attribute "semantic" attribs) "KEYWORD")
                         (input->source (get-attribute "source" attribs)
                                        sources))))))

;; Other...

;; Helper function to construct a 4x4 matrix (should probably be
;; extended to support arbitrary sized matrices
;; note that collada gives us row-major matrices
(defun matrix-tag->matrix (xml-lst)
  (when (equal "matrix" (tag-name xml-lst))
    (transpose (reshape (string->sv (third xml-lst)) '(4 4)))))