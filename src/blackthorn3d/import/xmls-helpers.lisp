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

(defun make-id-table ()
  (make-hash-table :test #'equal))

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
  (when (consp xml-lst) (caar xml-lst)))

(defun find-tag (tag lst) 
  (if (consp lst)
      (let ((xml-lst (find-if #'consp lst)))
        (if (string-equal (tag-name xml-lst) tag)
            xml-lst
            (or (find-tag tag (children xml-lst))
                (find-tag tag (cdr lst)))))
      nil))

(defun find-tag-in-children (tag xml-lst)
  "Searches the children of xml-list for the first element with tag <tag>"
  (let ((children (remove-if-not #'consp (children xml-lst))))
    (iter (for elt in children)
          (when (string-equal tag (tag-name elt)) (return elt)))))

(defun children-with-tag (tag xml-lst)
  "Return a list of children that have tag-name tag"
  (iter (for xml in (children xml-lst))
        (when (and (consp xml) (string-equal tag (tag-name xml)))
            (collect xml))))

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

(defun uri-indirect (uri)
  (and (simple-string-p uri) (subseq uri 1)))

(defun get-url (xml-lst)
  (uri-indirect (get-attribute "url" (attributes xml-lst))))