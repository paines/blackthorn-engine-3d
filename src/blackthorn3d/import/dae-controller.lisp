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
(defvar +vertex-weights+ "vertex_weights")


(defun build-index-weight-inputs (weights-tag sources)
  "Constructs the inputs (semantic source) that are fed to unify-indices
   (later on).  The inputs are arrays of indices and weights. Each
   vertex is allowed to have up to 4 indices and 4 weights. If it uses
   fewer, 0s will be stored"
  (dae-debug "building skin weights~%")
  (let* ((*dbg-level* (1+ *dbg-level*))
         (n-verts (parse-integer
                   (get-attribute "count" (attributes weights-tag))))
         (weights (src-array
                   (input-by-semantic :weight 
                                      (build-input-lst weights-tag 
                                                       sources))))
         (counts (string->sv (third 
                              (find-tag-in-children 
                               "vcount" weights-tag))))
         (v-indices (string->sv (third 
                                 (find-tag-in-children 
                                  "v" weights-tag))))
         (index-array (make-array (* 4 n-verts)))
         (weight-array (make-array (* 4 n-verts)))
         (ii -1) (wi -1))

    (dae-debug "Is N-VERTS == (length COUNTS)? ~a~%"
               (= n-verts (length counts)))

    ;; The v-indices array is considered an array of stride 2
    ;; so, indices have to be multiplied by 2
    (labels ((set-thingy (start-index count)
           (iter (for i below count)
                 (for v-ind-inc = (* 2 (+ i start-index)))
                 ;; we have to collect them FIRST
                 ;; then make sure they're NORMALIZED (in case we drop some)
                 (collect 
                  (cons (svref v-indices v-ind-inc)
                        (svref weights (svref v-indices (1+ v-ind-inc))))
                  into pairs-lst)

                 (finally
                  ;; select the 4 biggest weights, pad with 0s, and 
                  ;; normalize
                  
                  ;;quick check to see sum
                  (when (zerop (iter (for (index . weight) in pairs-lst)
                                     (sum weight)))
                    (format t "ZERO-TOTAL!: pairs: ~a~%" pairs-lst))

                  (when (> count 4)
                    (format t "WE HAVE ~a WEIGHTS!~%~2Tpairs: ~a~%"
                            count pairs-lst))

                  ;; combine same-joint weights
                  #+disabled
                  (setf 
                   pairs-lst
                   (iter (with result = (make-hash-table))
                         (for (index . weight) in pairs-lst)
                         (setf (gethash index result)
                               (+ (gethash index result 0)
                                  weight))
                         (finally
                          (return (iter (for (key value) in-hashtable result)
                                        (collect (cons key value)))))))

                  #+disabled
                  (when (> count 4)
                    (format t "~2Tcombined: ~a~%"
                            pairs-lst)
                    (setf pairs-lst ()))

                 ; (setf pairs-lst (sort pairs-lst #'> :key #'cdr))
                  (progn
                    (iter (for i below (- 4 (length pairs-lst)))
                          (push '(0 . 0.0) pairs-lst))
                  
                    
                    ;; normalize
                    ;#+disabled
                    (let ((total 
                           (iter (for (index . weight) in pairs-lst)
                                 (for i below 4)
                                 (sum weight))))
                      (when (zerop total))
                      (setf total 1.0)

                      (iter (with norm = (/ 1.0 total))
                            (for i below 4)
                            (for (index . weight) in pairs-lst)
                            (setf (svref index-array (incf ii))
                                  index)
                            (setf (svref weight-array (incf wi))
                                  (* norm weight)))))))))

      (iter (for vi below n-verts)
            (for v-index first 0 then (+ v-index count))
            (for count in-vector counts)
            (when (< vi 20)
              (format t "V-INDEX: ~a~%" v-index))
            ;; Build the index and weight streams
            (set-thingy v-index count)
            (finally (format t "Was vi = nverts?!? ~a: ~a~%" 
                            vi  (= vi n-verts))))
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
                            :components '(w0 w1 w2 w3)))))))


(defun process-controllers (controller-library)
  "Returns a hash table of controllers; each controller is
   a list of the joint array and the vertex weights ( a source )
   for the specific controller"
  (dae-debug "Processing skin controllers~%")
  (let ((*dbg-level* (1+ *dbg-level*))
        (controller-table (make-id-table)))
    (iter (for controller in (children-with-tag +controller+ 
                                                controller-library))
          (dae-debug "processing controller: ~a~%" 
                     (get-attribute "id" (attributes controller)))
          (let* ((*dbg-level* (1+ *dbg-level*))

                 (skin (find-tag-in-children +skin+ controller))
                 (sources (hash-sources skin))
                 (bind-pose (matrix-tag->matrix 
                             (find-tag-in-children +bind-shape-mat+ skin) 
                             :tag +bind-shape-mat+))
                 (joint-list (build-input-lst 
                              (find-tag-in-children +joints+ skin)
                              sources)))
            
            ;; DEBUG
            ;(print joint-list)
            
            ;; TODO:- do something with the return of the next 2 statements

            ;; Build joint array
            ;;  we can only make the joints...we don't know the skeleton
            ;;  until the process-scene stage, where we'll have to assume
            ;;  the joint data isn't malformed
            (dae-debug "building joint array~%")
            (setf (gethash (get-attribute "id" (attributes controller))
                           controller-table)
                  (list
                   ;; Geometry id link
                   (get-uri "source" (attributes skin))
                   ;; Bind pose matrix
                   bind-pose
                   ;; Joint array
                   (let ((joint-names (src-array 
                                       (input-by-semantic :joint joint-list)))
                         (ibm-source 
                          (input-by-semantic :inv_bind_matrix 
                                             joint-list)))
                     (iter (for joint-name in-vector joint-names)
                           (for i upfrom 0)
                           (for ibm = (transpose
                                       (reshape
                                        (src-accessor ibm-source i)
                                        '(4 4))))
                           (collect (make-joint joint-name ibm)
                                    result-type 'vector)))

                   ;; The vertex sources
                   ;; Build inputs with for indexes and weights
                   ;; these get fed to unify-indices with the other inputs
                   (build-index-weight-inputs
                    (find-tag-in-children +vertex-weights+ skin) sources)))))
    controller-table))