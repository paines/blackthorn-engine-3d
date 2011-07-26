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
                   (quat->matrix (axis-rad->quat
                                  (make-vec3 (x r-vec)
                                             (y r-vec)
                                             (z r-vec))
                                  (* (w r-vec) (/ pi 180.0))))))))))
   :initial-value (make-identity-matrix)))

(defun map-materials (bind-tag)
  (iter (for mat in (children-with-tag +instance-material+
                                       (first-child bind-tag)))
    (collect (list (get-attribute "symbol" (attributes mat))
                   (get-uri "target" (attributes mat))))))



(defclass scene-node ()
  ((id
    :accessor node-id
    :initarg :id)
   (type
    :accessor node-type
    :initarg :type)
   (xform
    :accessor node-xform
    :initarg :xform)
   (extra
    :accessor node-extra
    :initarg :extra)
   (children
    :accessor node-children
    :initarg :children)))

(defun make-node (id type transform extra children)
  (make-instance 'scene-node
                 :id id
                 :type type
                 :xform transform
                 :extra extra
                 :children children))

(defvar *transform* nil)
(defun jointp (node)
  (equal "JOINT" (get-attribute "type" (attributes node))))
(defun portalp (node)
  (let ((name (get-attribute "name" (attributes node))))
    (string-equal "_portal"
                  (subseq name
                          0 (min (length name) 7)))))
(defun platformp (node)
  (let ((name (get-attribute "name" (attributes node))))
    (string-equal "_platform"
                  (subseq name
                          0 (min (length name) 9)))))


(defun classify-node (node)
  (cond
    ((portalp node) :portal)
    ((platformp node) :platform)
    ((jointp node) :joint)
    ((find-tag-in-children +instance-geometry+ node) :geometry)
    ((find-tag-in-children +instance-controller+ node) :controller)
    (t :unknown)))

(defun process-geometry-node (node-tag)
  (let* ((node-id (get-attribute "id" (attributes node-tag)))
         (geometry-tag (find-tag-in-children +instance-geometry+ node-tag))
         (geom-id (get-url geometry-tag))
         (material-map
          (map-materials (find-tag-in-children +bind-material+
                                               geometry-tag))))

    (dae-debug "loading geometry node: ~a with mesh ~a~%"
               node-id geom-id)

    (make-node node-id :geometry *transform* (list geom-id material-map)
               (iter (for node in (children-with-tag "node" node-tag))
                     (if-let (it (process-node node))
                             (collect it))))))

(defun process-platform-node (node-tag)
  (let* ((node-id (get-attribute "id" (attributes node-tag)))
         (geometry-tag (find-tag-in-children +instance-geometry+ node-tag))
         (geom-id (get-url geometry-tag))
         (material-map
          (map-materials (find-tag-in-children +bind-material+
                                               geometry-tag))))

    (dae-debug "loading geometry node: ~a with mesh ~a~%"
               node-id geom-id)

    (make-node node-id :platform *transform* (list geom-id material-map)
               (iter (for node in (children-with-tag "node" node-tag))
                     (if-let (it (process-node node))
                             (collect it))))))
(defun process-portal-node (node-tag)
  (let* ((node-id (get-attribute "id" (attributes node-tag)))
         (portal-name (portal-name (get-attribute "name" (attributes node-tag))))
         (geometry-tag (find-tag-in-children +instance-geometry+ node-tag))
         (geom-id (get-url geometry-tag)))

    (dae-debug "loading portal node: ~a name: ~a~%"
               node-id portal-name)

    (make-node node-id :portal *transform* (list portal-name geom-id)
               nil
               ;; it is an error for a portal node to have children.
               #+disabled
               (iter (for node in (children-with-tag "node" node-tag))
                     (if-let (it (process-node node))
                             (collect it))))))

(defun process-joint-node (node-tag)
  (let ((node-id (get-attribute "id" (attributes node-tag)))
        (joint-id (read-from-string
                   (get-attribute "sid" (attributes node-tag))))
        (joint-name
          (get-attribute "name" (attributes node-tag))))
    (dae-debug "loading joint node: ~a with joint ~a~%" node-id joint-id)

    (make-node node-id :joint *transform* (list joint-id joint-name)
               (iter (for node in (children-with-tag "node" node-tag))
                     (if-let (it (process-node node))
                             (collect it))))))

(let ((default-count -1))
  (defun get-default-name (prefix)
    (format nil "~a-~a" prefix (incf default-count))))

(defun process-controller-node (node-tag)
  (let* ((node-id (or (get-attribute "id" (attributes node-tag))
                      (get-default-name "node")))
         (controller-tag (find-tag-in-children +instance-controller+ node-tag))
         (controller-id (get-url controller-tag))
         (skeleton (uri-indirect
                    (third (find-tag-in-children "skeleton" controller-tag))))
         (material-map
          (map-materials (find-tag-in-children +bind-material+
                                               controller-tag))))

    (dae-debug "loading controller node: ~a with skeleton ~a~%"
               node-id skeleton)

    (make-node node-id :controller *transform*
               (list controller-id skeleton material-map)
               (iter (for node in (children-with-tag "node" node-tag))
                     (if-let (it (process-node node))
                             (collect it))))))

;; Returns tree of nodes

(defun process-node (node-tag)
  "node format: (node-id transform geometry-id material-mapping children)"
  (let ((*dbg-level* (1+ *dbg-level*))
        (node-id (get-attribute "id" (attributes node-tag)))
        (*transform* (create-xform node-tag)))

    ;; (dae-debug "loading node: ~a~%" node-id)

    ;; Two cases (that we handle atm):
    ;; 1) node has an instance_geometry tag.  We'll assume there are
    ;;    no cases where the geometry is buried in the tree <_<
    ;; 2) node is a joint.  In this case we need to build the tree of joints
    ;;    with the initial pose matrices. joint nodes should look like:
    ;;    (node-id transform joint-name children)
    (case (classify-node node-tag)
      (:portal
       (process-portal-node node-tag))
      (:joint
       (process-joint-node node-tag))
      (:geometry
       (process-geometry-node node-tag))
      (:controller
       (process-controller-node node-tag))
      (otherwise
       ;; We just make a node...these will be ignored, i think, when
       ;; putting everything together
       (let* ((children (iter (for node in (children-with-tag "node" node-tag))
                              (if-let (it (process-node node)) (collect it))))
              (type (if (find-if #'(lambda (x)
                                     (not (eql :unknown (node-type x))))
                                 children)
                        :parent
                        :unknown)))
         (format t "making node ~a with type ~a~%" node-id type)
         (make-node node-id type  *transform* '()
                    children))))))


(defun prune-tree (node-tree test)
  (with-slots (id type xform extra children) node-tree
    (let ((new-children
           (apply #'append
                  (iter (for c in children)
                        (collect (prune-tree c test))))))
      (if (funcall test node-tree)
          ;; If it doesn't hold info we care about, remove it from the
          ;; tree, bringing all child nodes up to the next level
          ;; also, apply the transforms to the children
          (iter (for c in new-children)
                (setf (node-xform c)
                      (matrix-multiply-m
                       xform (node-xform c)))
                (finally (dae-debug "pruning node ~a  " id)
                         (dae-debug "children: ~a~%" new-children)
                         (return new-children)))
          ;; otherwise we set the children and pass on up, as a list
          ;; so new-children will be constructed correctly
          (progn
            (setf children new-children)
            (list node-tree))))))

(defun prune-nodes (top-nodes
                    &key (test #'(lambda (x)
                                   (eql (node-type x) :unknown))))
  (apply #'append
         (iter (for tree in top-nodes)
               (collect (prune-tree tree test)))))

;; Build a table of scene nodes.  This is assuming a flat graph, which
;; so far is all that max has given me.  SO it should be fine, until
;; we start looking at character animation.  Then...who knows.
(defun process-scene (scene-library)
  (dae-debug "~%Processing scene~%")
  (let ((scene (first-child scene-library)))
    (prune-nodes
     (remove-if #'null
                (iter (for node in (children-with-tag "node" scene))
                      (collect (process-node node)))))))
