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
;;; Collada Tag names
;;;

(defvar +instance-geometry+ "instance_geometry")



;; Responsible for taking the table in the tables
;; and compiling it to a dae-object
(defun compile-dae-data (&key geometry scenes materials)
  (make-instance 
   'load-object
   :meshes 
   (iter (for (node (xform mesh-id mats)) in-hashtable scenes)
         (let* ((mesh (gethash mesh-id geometry))
                (mat-array (make-array (length (elements mesh)))))
           ;; Build the material-array
           (iter (for elt in (elements mesh))
                 (with-slots ((mat-id material)) elt
                   (setf (aref mat-array (car mat-id)) 
                         (aif (find (cdr mat-id) 
                                    mats :test #'equal :key #'car)
                              (gethash (second it) materials)
                              nil))
                   ;(setf mat-id (car mat-id))
                   ))
           (collect
               (make-instance 'mesh-instance
                              :transform xform
                              :material-array mat-array
                              :mesh mesh)))
         ;; T0D0: stuff
         )))



(defvar *geometry-table* nil)
(defvar *scene-table* nil)
(defvar *material-table* nil)

;; Returns an intermediate representation of the dae file
(defun load-dae (filename)
  "Loads the objects from a dae file"
  (let ((dae-file (cxml:parse-file filename
                   #+disabled(blt3d-res:resolve-resource filename) 
                                   (cxml-xmls:make-xmls-builder))))
    
    (let ((*material-table* 
           (process-materials
            (find-tag-in-children +material-library+ dae-file)
            (find-tag-in-children +image-library+ dae-file)
            (find-tag-in-children +effect-library+ dae-file)))
          (*geometry-table* 
           (process-geometry 
            (find-tag-in-children +geometry-library+ dae-file)))
          (*scene-table*    
           (process-scene 
            (find-tag-in-children +scene-library+ dae-file))))
      ;; Combine all the tables into a list of model-shape objects
      (compile-dae-data :geometry *geometry-table*
                        :scenes   *scene-table*
                        ;; TODO: poor materials aren't ready yet
                        :materials *material-table*
                        ))))

(defun build-models (obj-lst)
  "Converts IR from load-dae into game objects
   @return{this may change.  a list of objects for the game}"
  (iter (for obj in obj-lst)
        ;; TEMP obj is a load-mesh
        (let ((mesh (build-mesh obj))))        
        (collect (make-instance 'model-shape
                                :mesh (gethash geom-id geometry)
                                :matrix transform
                                ))))