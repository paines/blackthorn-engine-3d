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

(defvar +instance-material+ "instance_material")
(defvar +bind-material+ "bind_material")

(defun create-xform (node-tag)
  
  (reduce 
   #'matrix-multiply-m
   (remove-if 
    #'null
    (iter (for tag in (tag-children node-tag))
          (collect
              (cond
                ((equal "matrix" (tag-name tag)) 
                 (matrix-tag->matrix tag))
                
                ((equal "translate" (tag-name tag)) 
                 (make-translate (string->sv (third tag))))
                
                ((equal "scale" (tag-name tag))
                 (make-scale (string->sv (third tag))))
                
                ((equal "rotate" (tag-name tag))
                 (let ((r-vec (string->sv (third tag))))
                   ;(make-z-rot (* (/ pi 180.0) (w r-vec)))
                   ;#+disabled
                   (format t "~%rotation values: ~a~%" r-vec)
                   (quat->matrix (axis-rad->quat
                                  (make-vec3 (x r-vec)
                                             (y r-vec)
                                             (z r-vec))
                                  ;(w r-vec)
                                  (* (w r-vec) (/ pi 180.0))
                                  ))))))))
   :initial-value (make-identity-matrix))
#+disabled
  (let ((xform (make-identity-matrix)))
    (iter (for tag in (tag-children node-tag))
          (when (member (tag-name tag) '("translate" "matrix") :test #'equal)
            (setf xform
                  (matrix-multiply-m 
                   xform
                   (cond
                     ((equal "matrix" (tag-name tag)) 
                      (matrix-tag->matrix tag))
                     ((equal "translate" (tag-name tag)) 
                      (make-translate (string->sv (third tag))))
                     ((equal "scale" (tag-name tag))
                      (make-scale val))
                     #+disabled
                     ((equal "rotate" (tag-name tag))
                      (let ((r-vec (string->sv (third-tag))))
                        (quat->matrix (axis-rad->quat
                                       (make-vec3 (x r-vec)
                                                  (y r-vec)
                                                  (z r-vec))
                                       (* (/ pi 180.0) (w r-vec)))))))))))
    xform))

(defun map-materials (bind-tag)
  (iter 
    (for mat in (children-with-tag +instance-material+ (first-child bind-tag)))
    (collect (list (get-attribute "symbol" (attributes mat))
                   (uri-indirect (get-attribute "target" (attributes mat)))))))

;; Build a table of scene nodes.  This is assuming a flat graph, which
;; so far is all that max has given me.  SO it should be fine, until
;; we start looking at character animation.  Then...who knows.
(defun process-scene (scene-library)
  (format t "   Loading Scene ...~%")
  (let ((scene (first-child scene-library))
        (scene-table (make-id-table)))
    (iter (for node in (children-with-tag "node" scene))
          (let ((node-id (get-attribute "id" (attributes node)))
                (transform (create-xform node))
                (geometry-tag (find-tag-in-children +instance-geometry+ 
                                                   node)))
            (when geometry-tag
              (let ((geom-id (get-url geometry-tag))
                    (material-map 
                     (map-materials (find-tag-in-children +bind-material+ 
                                                          geometry-tag))))
                (print transform)
                (setf (gethash node-id scene-table) 
                      (list transform geom-id material-map))))))
    scene-table))