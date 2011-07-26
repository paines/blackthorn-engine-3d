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
;;; For dealing with the <libraries_geometry> section
;;;

;;;
;;; Constants (tag names)
;;;

(defvar +geometry-block+ "geometry")
(defvar +mesh-block+     "mesh")
(defvar +source-block+   "source")

(defvar +accessor+  "accessor")
(defvar +vertices+  "vertices")
(defvar +triangles+ "triangles")

(defun set-vertices (vert-lst source-table)
  (setf (gethash (get-attribute "id" (attributes vert-lst)) source-table)
        (gethash (uri-indirect
                  (get-attribute "source" (attributes (first-child vert-lst))))
                 source-table)))

;; constructs a load-mesh object from an xml-list geometry tag
;; Returns a load-mesh object
(defun build-blt-mesh-data (geometry-lst)
  "From a single geometry tag, collects mesh data. That is, read in
   all the vertice data (position, normal, tex-coord) store it for future
   reference
   @arg[geometry-lst]{xml-list with tag 'geometry' and correct children}
   @return{A tuple of (id elements inputs)}"
  (let* ((id (get-attribute "id" (attributes geometry-lst)))
         (mesh-lst (find-tag-in-children "mesh" geometry-lst))
         (children (children mesh-lst))
         (source-table (hash-sources mesh-lst)))


    ;; for some reason collada uses "vertices" as an alias for "position"
    (set-vertices (find-tag +vertices+ children) source-table)
    (list
     id
     ;; collect mesh elements
     (iter (for tri-lst in (children-with-tag "triangles" mesh-lst))
           (for mat-index upfrom 0)
           (collect
            (make-instance
             'element
             :indices (string->sv (third (find-tag "p" (children tri-lst))))
             :material
             (cons mat-index
                   (get-attribute "material" (attributes tri-lst)))
             :count (parse-integer
                     (get-attribute "count" (attributes tri-lst))))))
     ;; build input list
     (build-input-lst (find-tag-in-children "triangles" mesh-lst)
                      source-table))))



;; Build a hash table of mesh ids and meshes
(defun process-geometry (geom-library)
  "parses the geometry items in the dae file, building a hash table of the
   meshes (hashed by id)
   @arg[geom-library]{the xml-list of the geometry library}"
  (dae-debug "~%Processing geometry~%")
  (let ((*dbg-level* (1+ *dbg-level*))
        (mesh-table (make-id-table)))
    (iter (for geom-xml in (children-with-tag +geometry-block+ geom-library))
          (let ((new-mesh (build-blt-mesh-data geom-xml)))
            (setf (gethash (car new-mesh) mesh-table) new-mesh)
            (counting t into total-models)
            (finally
             (dae-debug "Loaded ~a meshes:~%" total-models)
             (let ((*dbg-level* (1+ *dbg-level*)))
               (iter (for (mesh-id mesh-lst) in-hashtable mesh-table)
                     (dae-debug "id: ~a~%" mesh-id))))))
    mesh-table))



;;                  |
;; deprecated, yo   |
;;                  v




;; this function is going to need some serious refactoring ... >_<
;; ... yup, it will...keeping it for now for reference/stealing
#+disabled
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
            (if-let (it (gethash vertex vertex-ht))
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
