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

;;;
;;; Collada load functions
;;;

;; this'll be fun....
;; gets a function that will modify a slot/location based on an id...
;; Note: for starters will only support one level.
;; nodes is a list of scene nodes (
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
    (format t "    location: ~a~%" loc)
    (destructuring-bind (node-id location &rest dc) (split-loc loc 0)
      (let ((node (node-finder node-id nodes)))
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

(defun compile-geometry (data)
  (destructuring-bind (mesh-id materials) data
    (let* ((mesh (mesh-list->blt-mesh (gethash mesh-id *geometry-table*)))
           (mat-array (build-material-array (elements mesh) materials)))
      (list mesh mat-array))))

;; For now, lets assume the skeleton data is well formed
;; that is, all the nodes are in joint arr
(defun compile-skeleton (joint-arr root-node)
  (labels ((skele-builder (root-node)
             (let* ((joint-name (car (node-extra root-node)))
                    (joint-obj (find joint-name ;(read-from-string joint-name)
                                     joint-arr
                                     :key #'joint-id)))
               ;(format t "joint-name: ~a~%" joint-name)
               ;; set initial local matrix
               (setf (joint-matrix joint-obj) (node-xform root-node))
               ;; Recursive step: set up the children!
               (setf (child-joints joint-obj)
                     (iter (for ch in (node-children root-node))
                           (collect (compile-skeleton joint-arr ch))))
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
                           (return-from find-root-node n)
                           (find-root-node children sid)))))))

    (destructuring-bind (controller-id root-node materials) data
      ;; First get the controller and then the geometry
      (destructuring-bind (geom-id bind-pose joint-arr skin-inputs)
          (gethash controller-id *controller-table*)
        (destructuring-bind (ignore-id elements inputs)
            (gethash geom-id *geometry-table*)

          (dae-debug "root-node: ~a~%" root-node)
          ;; Construct the mesh objects and skeleton
          (let ((mesh (mesh-list->blt-mesh 
                       (list geom-id 
                             (duplicate-indices elements 0 2)
                             (append inputs skin-inputs))))
                (skeleton (compile-skeleton joint-arr
                                            (find-root-node *scene-table*
                                                            root-node))))
            
            (list 
             (make-blt-skin :mesh mesh
                            :skeleton skeleton
                            :bind-matrix bind-pose)
             (build-material-array (elements mesh) materials))))))))

(defun compile-node (node geometry-table material-table)
  ;; Convert mesh-lst into a blt-mesh
  ;; TODO:- hack in skinning data!
  (with-slots (id type xform extra children) node
    (dae-debug "node ~a is of type ~a~%" id type)
    ;; recurse on children
    (let ((new-node 
           (case type
             (:geometry 
              (destructuring-bind (mesh materials) 
                  (compile-geometry extra)
                (make-model-node :id id
                                 :transform xform
                                 :material-array materials
                                 :mesh mesh)))
             (:controller 
              (destructuring-bind (skin materials) 
                  (compile-controller extra)
                (make-model-node :id id
                                 :transform xform
                                 :material-array materials
                                 :mesh skin)))
             ;; anything else, we don't really care about much
             (otherwise nil))))

      ;; recurse on children
      (when new-node
        (setf (child-nodes new-node)
              (iter (for child-node in children)
                    (collect (compile-node child-node
                                           geometry-table 
                                           material-table)))))
      ;; return the node
      new-node)))

;; Responsible for taking the table in the tables
;; and compiling it to a dae-object
(defun compile-dae-data (&key geometry scenes materials animations)
  (let* ((meshes 
          (remove-if 
           #'null 
           (iter (for node in scenes)
                 (collect (compile-node node geometry materials))

                 #+disabled
                 (let* ((mesh (mesh-list->blt-mesh 
                               (gethash mesh-id geometry)))
                        (mat-array (make-array (length (elements mesh)))))
                   ;; Build the material-array (mat-id: (index . material-id))
                   (iter (for elt in (elements mesh))
                         (let ((mat-id (element-material elt)))
                           (setf (aref mat-array (car mat-id)) 
                                 (aif (find (cdr mat-id)
                                            mats :test #'equal :key #'car)
                                      (gethash (second it) *material-table*)
                                      nil))
                                        ;(setf mat-id (car mat-id))
                           ))
                   (collect (make-model-node :id node
                                             :transform xform
                                             :material-array mat-array
                                             :mesh mesh)))
                 ;; T0D0: stuff
                 )))
         (anims     
          ;; Need to update the animation clips with the proper target fn
          (when animations
            (iter (for (anim-id clip) in-hashtable animations)
                  (iter (for ch in (channel-lst clip))
                        (setf (slot-value ch 'target) 
                              (get-location-fn (slot-value ch 'target) 
                                               meshes)))
                  (collect clip)))))

    (make-instance 
     'blt-model
     :nodes meshes
     :animations (make-animation-controller anims))))



;; Returns an intermediate representation of the dae file
(defun load-dae (filename)
  "Loads the objects from a dae file"
  (dae-debug "~%LOADING DAE FILE ~a~%" filename)
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
      (compile-dae-data :geometry *geometry-table*
                        :scenes   *scene-table*
                        :materials *material-table*
                        ;; TODO: implement animations
                        :animations *animation-table*
                        ))))