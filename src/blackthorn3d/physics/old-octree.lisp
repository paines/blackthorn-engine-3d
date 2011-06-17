;;;; Blackthorn -- Lisp Game Engine
;;;;
;;;; Copyright (c) 2011, Michael Matthews <iismichaels@gmail.com>
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

(in-package :blackthorn3d-physics)


;;; @Michael: this can all be rewritten, i was mostly playing around
;;; with it, but it was headed for a strict octree. 

;;;
;;; Spatial trees.
;;;  all spatial trees require that the primitives being used
;;;  be able to provide the following values:
;;;    Min/Max for each axis or Extents
;;;    Centroid
;;;

;;;
;;; Octree
;;;

;; For dynamic collision detection, and potentially more??
;; Octree is contructed as cube

(defclass octree ()
  ((threshold)
   (max-depth)))


;; classification:
;; 9: current cell, ie, isn't contained in a child
;; 1: (- - -)
;; 2: (+ - -)
;; 3: (- + -)
;; 4: (+ + -)
;; ... etc
(defvar +current-cell+ 9)
(defun classify-pt (pt cell-center)
  (iter (for i below 3)
        (when (> (svref pt i) (svref cell-center i))
          (sum (ash 1 i)))))

(defun classify-obj (obj cell-center)
  (let* ((centroid ())
         (min-pt ())
         (max-pt ())
         (min-pt-classification (classify-pt min-pt)))
    ;; Test if the object spans multiple boxes. 
    ;;  in this case we return 0 
    ;;  otherwise return the cell it was in
    (if (= min-pt-classification (classify-pt max-pt))
         min-pt-classification
         +current-cell+)))

(defmacro grid-center-offset (id bounds)
  (with-gensyms (center radius)
    (let ((arr 
           #(#(-0.5 -0.5 -0.5)
             #( 0.5 -0.5 -0.5)
             #(-0.5  0.5 -0.5)
             #( 0.5  0.5 -0.5)
             #(-0.5 -0.5  0.5)
             #( 0.5 -0.5  0.5)
             #(-0.5  0.5  0.5)
             #( 0.5  0.5  0.5))))
      `(destructuring-bind (,center . ,radius) ,bounds
         (vec3+ ,center (vec-scale3 (aref ,array ,id) ,radius))))))))))

;; structure of an octree node is:
;; (objs bounds children)
;; where children is: #(c0 c1 c2 c3 c4 c5 c6 c7)
(defun build-octree (objs bounds current-depth 
                     threshold maximum-depth growth-k)
  (labels ((select-by-child (obj-lst child-no)
             (iter (for obj in obj-lst)
                   (when (= child-no (car obj))
                     (collect (cdr obj) result-type 'vector)))))
    (let ((n-objs (length objs)))

      ;; Test if we should end recursion here. If so, then make a leaf
      ;; Otherwise we split the objects up into 9 sequences
      (if (or (<= n-objs threshold) 
              (>= current-depth maximum-depth))
          (make-octree-leaf objs)

          (progn          
            (let ((classified-objs 
                   (iter (for object in objs)
                         (collect 
                             (cons (classify-obj object (car bounds)) 
                                   object)))))
              (list 
               ;; Gather the objects (if any) that belong in this cell
               (select-by-child classified-objs +current-cell+)

               ;; Set the bounds
               bounds

               ;; iterate over each child and call build-octree recursively
               (iter (for child-id below 8)
                     ;; gather objects
                     (build-octree
                      ;; Gather objects
                      (select-by-child classified-objs child-id)
                      ;; Find bounds
                      (cons (grid-center-offset i bounds)
                            (* 0.5 (cdr bound)))
                      (1+ current-depth)
                      threshold maximum-depth growth-k)))))))))
