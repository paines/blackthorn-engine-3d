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

(defvar *geometry-table* nil)
(defvar *scene-table* nil)
(defvar *material-table* nil)
(defvar *animation-table* nil)
(defvar *controller-table* nil)
(defvar *portal-list* nil)

;;;
;;; Collada load functions
;;;

(defmacro make-set-fn (place)
  `#'(lambda (val) (setf ,place val 
                         #+disabled(transpose (reshape val '(4 4))))))

;; this'll be fun....
;; gets a function that will modify a slot/location based on an id...
;; Note: for starters will only support one level.
;; nodes is a list of scene nodes (

;; Simplified to a hashtable lookup
(defun get-location-fn (loc nodes)
  (labels ((node-finder (node-id nodes)
             (iter (for node in nodes)
                   (if (equal node-id (id node)) (leave node)
                       (node-finder node-id (child-nodes node)))))

           (split-loc (str start)
             (let ((p1 (position #\/ str :start start)))
               (if p1
                   (cons (subseq str start p1)
                         (split-loc str (1+ p1)))
                   (if (< start (length str))
                       (cons (subseq str start) nil)
                       nil)))))
   ; (format t "~%SPLIT_LOC: ~A~%" (split-loc loc 0))
    (aif (gethash (car (split-loc loc 0)) nodes)
         it
         (progn 
           (format t "WARNING: found no mapping for ~a~%" 
                   (car (split-loc loc 0)))
           #'(lambda (val))))
   ; (format t "~2Tlocation: ~a~%" loc)
    #+disabled
    (destructuring-bind (node-id location &rest dc) (split-loc loc 0)
      (let ((node (node-finder node-id nodes)))
        (format t "~4TBinding ~a~%" node-id )
        ;; Set up the function, and stuff
        #'(lambda (val) (setf (slot-value node 'transform) 
                              (transpose (reshape val '(4 4)))))))))

(defun build-material-array (elements materials)
  "takes a list of elements and materials and returns an array
   matching blt-material objects to the index of the element"
  (iter (for elt in elements)
        (let ((mat-id (element-material elt)))
          (collect (aif (find (cdr mat-id) 
                              materials :test #'equal :key #'car)
                        (gethash (second it) *material-table*)
                        nil) result-type 'vector))))

(defun geometry-type (id)
  (if (char= #\_ (char id 0))
      :portal
      :mesh))

(defun compile-portal (data)
  (destructuring-bind (name geom-id) data
    (let* ((mesh-lst (gethash geom-id *geometry-table*))
           (vertex-source (input-by-semantic 
                           :vertex
                           (third mesh-lst)))
           (vertices
            (iter (for i below (/ (length (src-array vertex-source))
                                  (src-stride vertex-source)))
                  (collect (src-accessor vertex-source i)
                           result-type 'vector))))
      (list name (make-bounding-box vertices)))))

(defun compile-geometry (data)
  (destructuring-bind (mesh-id materials) data
    ;; Check on mesh-id.  portals need to be separated out
    
    (let* ((mesh (mesh-list->blt-mesh (gethash mesh-id *geometry-table*)))
           (mat-array (build-material-array (elements mesh) materials)))
      (list mesh mat-array))))

;; For now, lets assume the skeleton data is well formed
;; that is, all the nodes are in joint-arr
(defun compile-skeleton (joint-arr root-node)
  (format t "root node: ~a~%" root-node)
  (labels ((skele-builder (root-node)
             (let* ((joint-name (car (node-extra root-node)))
                    (joint-obj (find joint-name ;(read-from-string joint-name)
                                     joint-arr
                                     :key #'joint-id)))
              ; (format t "joint-name: ~a  object~a~%" joint-name joint-obj)
               ;; set initial local matrix
               (setf (joint-matrix joint-obj) (node-xform root-node))

               ;; Add a mapping to *xform-mappings* so that animations
               ;; can find us!
               (setf (gethash (node-id root-node) *xform-mappings*)
                     (make-set-fn (joint-matrix joint-obj)))

               ;; Recursive step: set up the children!
               (setf (child-joints joint-obj)
                     (iter (for ch in (node-children root-node))
                           (collect (skele-builder ch))))
               joint-obj)))

    (make-skeleton (skele-builder root-node)
                   joint-arr)))

(defun compile-controller (data)
  (dae-debug "compiling a controller~%")
  (labels ((find-root-node (nodes sid) 
             (when nodes
               (iter (for n in nodes)
                     (with-slots (id children) n
                       (if (equal id sid)
                           (progn (format t "found node ~a~%" n)
                                  (return-from find-root-node n))
                           (aif (find-root-node children sid)
                               (return-from find-root-node it))))))))

    (destructuring-bind (controller-id root-node materials) data
      ;; First get the controller and then the geometry
      (destructuring-bind (geom-id bind-pose joint-arr skin-inputs)
          (gethash controller-id *controller-table*)
        (destructuring-bind (ignore-id elements inputs)
            (gethash geom-id *geometry-table*)

         ; (dae-debug "root-node: ~a~%" root-node)
          ;; Construct the mesh objects and skeleton
          (format t "Vertex Indices & Weights:~%")
          (let ((indices (input-by-semantic :joint-index skin-inputs))
                (weights (input-by-semantic :joint-weight skin-inputs)))
            (format t "Indices: ~a~%" (iter (for i below 60)
                                            (collect (src-accessor indices i))))
            (format t "Weights: ~a~%" (iter (for i below 60)
                                            (collect (src-accessor weights i)))))
          (format t "looking for node: ~a~%" root-node)
          (let* ((mesh (mesh-list->blt-mesh 
                        #+disabled(list geom-id elements inputs)
                        (list geom-id 
                              (duplicate-indices elements 0 2)
                              (append inputs skin-inputs))))
                 (found-node (find-root-node *scene-table*
                                             root-node)))

            (format t "found-node: ~a~%" found-node)
            (let ((skeleton (compile-skeleton joint-arr
                                              found-node)))

              (dae-debug "Vertices:~%")
              (iter (for elt in-vector (subseq (get-stream :vertex mesh) 0 10))
                    (dae-debug "~a  ~%" elt))
              
              (apply-transform mesh bind-pose)

              (list
               (make-blt-skin :mesh mesh
                              :skeleton skeleton
                              :bind-matrix bind-pose)
               (build-material-array (elements mesh) materials)))))))))

(defvar *xform-mappings* nil)

(defun compile-node (node geometry-table material-table)
  ;; Convert mesh-lst into a blt-mesh
  (with-slots (id type xform extra children) node
    ;; recurse on children
    (let* ((node-children 
            (iter (for child-node in children)
                  (collect (compile-node child-node
                                         geometry-table
                                         material-table))))
           (new-node
            (progn   
              (dae-debug "node ~a is of type ~a~%" id type)
              (case type
                (:portal
                 (destructuring-bind (name bounding-volume)
                     (compile-portal extra)
                   (let* ((pos (matrix-multiply-v xform +origin+))
                          (dir (to-vec4 (norm4 pos))))
                     (format t "adding portal ~a~%" name)
                     (push (list name pos dir bounding-volume) *portal-list*)
                     ;; don't make a node for the portal??
                     nil)))
                (:geometry 
                 (destructuring-bind (mesh materials) 
                     (compile-geometry extra)
                   (make-model-node :id id
                                    :transform xform
                                    :material-array materials
                                    :mesh mesh
                                    :child-nodes node-children)))
                (:controller 
                 (destructuring-bind (skin materials) 
                     (compile-controller extra)
                   (make-model-node :id id
                                    :transform xform
                                    :material-array materials
                                    :mesh skin
                                    :child-nodes node-children)))
                (:parent
                 (if (find-if-not #'null node-children)
                     (progn
                       (format t "parent ~a is saved!~%" id)
                       (make-scene-node :id id
                                        :transform xform
                                        :child-nodes node-children))
                     (format t "parent ~a is killed!~%" id)))
                ;; anything else, we don't really care about much
                (otherwise nil)))))

   ;   (format t "NODE ~a's transform: ~a~%" id xform)

      ;; Add an entry in *xform-mappings* for the animation pass
      (when new-node
        (setf (gethash id *xform-mappings*)
              (make-set-fn (transform new-node))
              #+disabled
              #'(lambda (val) (setf (transform new-node) val))))

      ;; return the node
      new-node)))

;; Responsible for taking the table in the tables
;; and compiling it to a dae-object
(defun compile-dae-data (&key geometry scenes materials animations)
  (let* ((*xform-mappings* (make-id-table))
         (meshes 
          (remove-if 
           #'null 
           (iter (for node in scenes)
                 (collect (compile-node node geometry materials)))))
         (anims     
          ;; Need to update the animation clips with the proper target fn
          (when animations
            (iter (for (anim-id clip) in-hashtable animations)
                  (format t "~5Tclip: ~a~%" anim-id)
                  (iter (for ch in (channel-lst clip))
                        (setf (slot-value ch 'ch-target) 
                              (get-location-fn (slot-value ch 'ch-target) 
                                               *xform-mappings*)))
                  (collect clip)))))
    
    (iter (for node in meshes)
          (format t "Node ~a:  transform: ~a  bv: ~a~%"
                  (id node) (transform node) (node-bounding-volume node)))
 
    (format t "Nodes: ~a~%" meshes)

    (format t "~%~%ANIM-MAPPINGS:~%")
    (iter (for (key value) in-hashtable *xform-mappings*)
          (format t "key: ~a~%" key))

    (when anims
      (format t "~%~%ANIMATIONS:~%")
      (iter (for clip in anims)
            (format t "clip: ~a~%" clip)))

    (make-instance 
     'blt-model
     :nodes meshes
     :animations (when anims (make-anim-controller anims)))))



;; Returns an intermediate representation of the dae file
(defun load-dae (filename)
  "Loads the objects from a dae file"
  (dae-debug "~%LOADING DAE FILE ~a~%" filename)
  (let ((dae-file (cxml:parse-file filename
                   #+disabled(blt3d-res:resolve-resource filename) 
                                   (cxml-xmls:make-xmls-builder))))
    
    (let ((*portal-list* ())
          (*material-table* 
           (process-materials
            (find-tag-in-children +material-library+ dae-file)
            (find-tag-in-children +image-library+ dae-file)
            (find-tag-in-children +effect-library+ dae-file)))
          (*geometry-table* 
           (process-geometry 
            (find-tag-in-children +geometry-library+ dae-file)))
          (*controller-table*
           (process-controllers
            (find-tag-in-children +controller-library+ dae-file)))
          (*scene-table*    
           (process-scene 
            (find-tag-in-children +scene-library+ dae-file)))
          (*animation-table*
           (process-animations
            (find-tag-in-children +animation-library+ dae-file))))
      ;; Combine all the tables into a list of model-shape objects
      (make-instance 'loaded-dae
                     :geometry
                     (apply-transform
                      (compile-dae-data :geometry *geometry-table*
                                        :scenes   *scene-table*
                                        :materials *material-table*
                                        ;; TODO: implement animations
                                        :animations *animation-table*)
                      (make-inv-ortho-basis (make-point3 -1.0 0.0 0.0)
                                            (make-point3 0.0 0.0 1.0)
                                            (make-point3 0.0 1.0 0.0)))
                     :portals
                     *portal-list*))))