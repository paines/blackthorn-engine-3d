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

(defconstant +library_geometries+ "library_geometries")
(defconstant +geometry-block+ "geometry")
(defconstant +mesh-block+ "mesh")
(defconstant +source-block+ "source")
(defconstant +accessor+ "accessor")
(defconstant +vertices+ "vertices")
(defconstant +triangles+ "triangles")

(defparameter *file* nil)

(defun string->sv (str)
  (with-input-from-string s str 
    (apply #'vector 
           (iter (for val = (read s nil :eof ))
                 (until (eql val :eof))
                 (collect val)))))

(defun find-tag (tag) 
  (klacks:find-element *file* tag))

(defun current-tag ()
  (klacks:current-lname *file*))

(defun find-end-tag (tag)
  (while (not (eql (klacks:peek-next *file* :end-element))
              (and (string-equal (current-tag) tag))))
  (klacks:current-lname *file*))

(defun get-attribute (attrib-name)
  (klacks:get-attribute *file* attrib-name))

(defun next-tag ()
  (while (not (eql (klacks:peek-next *file*) :start-element)))
  (klacks:current-lname *file*))

;;;
;;; Format:
;;; <blah...material stuff>
;;; <source>...</source>
;;; ...
;;; <source>...</source>
;;; <vertices><input source="src"/></vertices>
;;; <primitive-type>
;;;   <input source=...>
;;;   <input source=...>
;;; <p> ...[indices]...</p>
;;; </primitive-type>
;;;

(defun make-accessor (array)
  (find-tag +accessor+)
  (let ((stride (get-attribute "stride")))
    (#'lambda (index) 
      (aref array (* index stride)))))

(defun build-components ()
  (next-element)
  (iter (while (string-equal (next-tag) "param"))
        (collect (read-from-string (get-attribute "name")))))

;; returns a list containing the id of the source and
;; a vector containing the values in the source, an
;; accessor function (for use later) and the names of components
(defun consume-source (source-id)
  (next-element)
  (let* ((data (string->sv (klacks:peek-next *file*)))
         (accessor-fn (make-accessor data))
         (components (build-components)))
    (find-end-tag +source_block+)
    (list source-id data accessor-fn components)))

;; returns a p-list of the sources in the .DAE file for the current
;; mesh
(defun build-sources ()
  (if (string-equal (next-element) +source-block+)
    (cons (consume-source (get-attribute "id")) (build-sources))
    nil))

;; consturcts a mesh object from a .DAE file between 
;; <mesh></mesh> tags.
(defun build-mesh (name)
  (find-tag +mesh-block+)
  (let ((sources-lst (build-sources)))
    sources-lst))

;; Load a dae file into a list of meshes
(defun load-dae (filename)
  (let ((*file* (cxml:make-source filename))
        (meshes nil))
    (when (find-tag +library-geometries+)
      (iter (while (find-tag +geometry-block+))
            (cons (build-mesh (get-attribute "name'")) meshes)))))