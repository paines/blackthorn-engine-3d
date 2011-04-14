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
  (let ((stride (parse-integer (get-attribute "stride" (attributes accessor-lst)))))
    #'(lambda (array index)
        (apply #'vector 
               (iter (for i below stride)
                     (collect (svref array (+ i (* index stride)))))))))

(defun make-components (accessor-lst)
  (mapcar #'(lambda (child) 
              (get-attribute "name" (attributes child)))
          (remove-if-not #'consp (children accessor-lst))))

(defclass dae-source ()
  ((id :accessor src-id
       :initarg :id)
   (array :accessor src-array
          :initarg :array)
   (accessor :accessor src-accessor
             :initarg :accessor)
   (components :accessor src-components
               :initarg :components)))

(defun make-source (src-lst)
  (let ((accessor-lst (find-tag +accessor+ (children src-lst))))
    (make-instance 'dae-source 
                   :id (get-attribute "id" (attributes src-lst))
                   :array (string->sv (car (children (first-child src-lst))))
                   :accessor (make-accessor accessor-lst)
                   :components (make-components accessor-lst))))

;; constructs a mesh object from an xml-list mesh tag
(defun build-mesh (xml-lst)
  (let ((sources-lst (mapcar #'make-source 
                             (remove-if-not #'(lambda (x)
                                                (and (consp x)
                                                     (string-equal (caar x) "source")))
                                            (children xml-lst)))))
    sources-lst))

(defun load-dae (filename)
  (let ((dae-file (cxml:parse-file filename (cxml-xmls:make-xmls-builder))))
    (build-mesh (find-tag "mesh" (list dae-file)))))