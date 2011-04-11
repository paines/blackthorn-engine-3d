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

(defparameter *file* nil)

(defun string->sv (str)
  (with-input-from-string s str 
    (apply #'vector 
           (iter (for val = (read s nil :eof ))
                 (unitl (eql val :eof))
                 (collect val)))))

(defun find-tag (tag) 
  (klacks:find-element *file* tag))

(defun get-attribute (attrib-name)
  (klacks:get-attribute *file* attrib-name))

(defun next-element ()
  (while (not (eql (klacks:peek-next *file*) :start-element))))

;; returns a dotted list containing the id of the source and
;; a vector containing the values in the source
(defun consume-source ())

;; returns a p-list of the sources in the .DAE file for the current
;; mesh
(defun build-sources ()
  (next-element)
  (if (string-equal (current-element) +source-block+)
    (cons (consume-source) (build-sources))
    nil))

;; consturcts a mesh object from a .DAE file between 
;; <mesh></mesh> tags.
(defun build-mesh (name)
  (find-tag +mesh-block+)
  (let ((sources-lst ()))
    (next-element)
    (when (equal (current-element) +source+)
      )))

;; Load a dae file into a list of meshes
(defun load-dae (filename)
  (let ((*file* (cxml:make-source filename))
        (meshes nil))
    (when (find-tag +library-geometries+)
      (iter (while (find-tag +geometry-block+))
            (cons (build-mesh (get-attribute "name'")) meshes)))))