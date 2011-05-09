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
;;; Loads controller, or bone, data from a collada file
;;;

(defvar +controller+ "controller")
(defvar +skin+ "skin")
(defvar +bind-shape-mat+ "bind_shape_matrix")
(defvar +joints+ "joints")
(defvar +vertex-weights+ "skin")


(defun build-index-weight-inputs (weights-tag sources)
  "Constructs the inputs (semantic source) that are fed to unify-indices
   (later on).  The inputs are arrays of indices and weights. Each
   vertex is allowed to have up to 4 indices and 4 weights. If it uses
   fewer, 0s will be stored"
  (dae-debug "building skin weights~%")
  (let* ((*dbg-level* (1+ *dbg-level*))
         (n-verts (get-attribute "count" (attributes weights-tag)))
         (weights (input-by-semantic :weight (build-input-lst weights-tag)))
         (counts (string->sv (third 
                              (find-tag-in-children 
                               "vcount" weights-tag))))
         (v-indices (string->sv (third 
                                 (find-tag-in-children 
                                  "v" weights-tag)))))

    (dae-debug "Is N-VERTS == (length COUNTS)? ~a~%"
               (= n-verts (length counts)))

    (iter (with index-array = (make-array n-verts))
          (with weight-array = (make-array n-verts))
          (with ii = -1) (with wi = -1)
          (for vi below n-verts)
          (for count in-vector counts)
          (for v-index first 0 then (+ v-index count))
                    
          ;; Build the index and weight streams
          (iter (for i from v-index to (+ v-index (min 4 count)))
                (setf (svref index-array (incf ii)) 
                      (svref v-indices (* 2 i)))

                (setf (svref weight-array (incf wi))
                      (svref weights (svref v-indices (1+ (* 2 i))))))
          (finally 
           (return 
             (list
              (list :joint-index
                    (make-instance 'source
                                   :array index-array
                                   :stride 4
                                   :components '(i0 i1 i2 i3)))
              (list :joint-weight
                    (make-instance 'source
                                   :array weight-array
                                   :stride 4
                                   :components '(w0 w1 w2 w3))))))))))


(defun process-controller (controller-library)
  (dae-debug "processing skin controllers~%")
  (let ((*dbg-level* (1+ *dbg-level*)))
    (iter (for controller in (children-with-tag +controller+ 
                                                controller-library))
          (dae-debug "processing controller: ~a~%" 
                     (get-attribute "id" (attributes controller)))
          (let* ((*dbg-level* (1+ *dbg-level*))
                 (skin (find-tag-in-children +skin+ controller))
                 (sources (hash-sources skin))
                 (bind-pose (matrix-tag->matrix 
                             (find-tag-in-children +bind-shape-mat+ skin) 
                             +bind-shape-mat+))
                 (joint-lst (build-input-lst 
                             (find-tag-in-children +joints+ skin)
                             sources))
                 )
            ;; DEBUG
            
            ;; TODO:- do something with the return of the next 2 statements
            ;; Build joint array
            (dae-debug "building joint array~%")
            (let ((joint-names (src-array 
                                (input-by-semantic :joint joint-lst)))
                  (ibm-array (src-array 
                              (input-by-semantic :inv-bind-matrix joint-list))))
              (iter (for joint-name in-vector joint-names)
                    (for ibm in-vector ibm-array)
                    (collect (make-joint joint-name ibm))))

            ;; Build vertex stream with for indexes and weights
            (build-index-weight-streams
             (find-tag-in-children +vertex-weights+ skin) sources)))))