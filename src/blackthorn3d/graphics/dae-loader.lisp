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

(defvar +geometry-library+ "library_geometries")
(defvar +material-library+ "library_materials")
(defvar +image-library+    "library_images")
(defvar +effect-library+   "library_effects")
(defvar +scene-library+    "library_visual_scenes")
(defvar +light-library+    "library_lights")

(defvar +geometry-block+ "geometry")
(defvar +mesh-block+     "mesh")
(defvar +source-block+   "source")

(defvar +instance-geometry+ "instance_geometry")

(defvar +accessor+  "accessor")
(defvar +vertices+  "vertices")
(defvar +triangles+ "triangles")

(defparameter *file* nil)
(defparameter *source-ht* nil)

(defun make-accessor (accessor-lst)
  (let ((stride (parse-integer 
                 (get-attribute "stride" (attributes accessor-lst)))))
    #'(lambda (array index)
        (apply #'vector 
               (iter (for i below stride)
                     (collect (svref array (+ i (* index stride)))))))))

(defun make-components (accessor-lst)
  (mapcar #'(lambda (child) 
              (get-attribute "name" (attributes child)))
          (remove-if-not #'consp (children accessor-lst))))

(defclass dae-source ()
  ((id 
    :accessor src-id
    :initarg :id)
   (array 
    :accessor src-array
    :initarg :array)
   (accessor 
    :accessor src-accessor
    :initarg :accessor
    :documentation "function taking an array and index")
   (components 
    :accessor src-components
    :initarg :components)))

(defun make-source (src-lst)
  (let ((accessor-lst (find-tag +accessor+ (children src-lst))))
    (make-instance 'dae-source 
                   :id (get-attribute "id" (attributes src-lst))
                   :array (string->sv (car (children (first-child src-lst))))
                   :accessor (make-accessor accessor-lst)
                   :components (make-components accessor-lst))))

(defun hash-sources (xml-lsts)
  (let ((src-lst (mapcar #'make-source
                         (remove-if-not #'(lambda (x)
                                            (and (consp x)
                                                 (string-equal (tag-name x) "source")))
                                        xml-lsts))))
    (iter (for src in src-lst)
          (setf (gethash (src-id src) *source-ht*) src))))

(defun set-vertices (vert-lst)
  (setf (gethash (get-attribute "id" (attributes vert-lst)) *source-ht*)
        (gethash (subseq (get-attribute "source" (attributes (first-child vert-lst))) 1) 
                 *source-ht*)))

(defun input->source (str)
  (gethash (subseq str 1) *source-ht*))

(defun build-input-lst (prim-chld-lst)
  (iter (for xml in prim-chld-lst)
        (if (and (consp xml) (string-equal "input" (tag-name xml)))
            (let ((attribs (attributes xml)))
              (collect (list (get-attribute "semantic" attribs)
                             (input->source (get-attribute "source" attribs))))))))


;; Returns a list of (fn . array) where calling fn with an index modifies array
;; fn is designed to take in index to a source and add the corresponding value(s) 
;; to array. 
(defun get-source-functions (inputs)
  (iter (for i in inputs)
        (collect 
         (let* ((source (second i))
                (attrib-len (/ (length (src-array source)) 
                               (length (src-components source))))
                (attrib-vec (make-array attrib-len :fill-pointer 0
                                        :adjustable t)))
           (cons #'(lambda (index)
                     (let ((src-array-vec (funcall (src-accessor source) 
                                                   (src-array source) 
                                                   index)))
                       #+disabled(iter (for elt in-vector src-array-vec)
                             (vector-push elt attrib-vec))
                       (vector-push-extend src-array-vec attrib-vec)))
                 attrib-vec)))))

;; this function is going to need some serious refactoring ... >_<
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
            (aif (gethash vertex vertex-ht)
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

;; combines arrays into one large 2-d array
(defun interleave (arrays)
  (let* ((size (length (cdar arrays)))
         (depth (iter (for (s . a) in arrays)
                      (sum (length (aref a 0)))))
         (interleaved (make-array `(,size ,depth)))
         (index 0))
    (iter (for j below size)
          (iter (for (semantic . a) in arrays)
                (iter (for elt in-vector (aref a j))
                      (setf (row-major-aref interleaved index) elt)
                      (incf index))))
    interleaved))

(defun blt-mesh-array->gl-array (array)
  (let* ((count (array-dimension array 0))
         (vertex-size (array-dimension array 1))
         (gl-array (gl:alloc-gl-array 'blt-vnt-mesh count)))
    (iter (for i below count)
          (iter (for j below vertex-size)
                (for c in *blt-mesh-components*)
                (setf (gl:glaref gl-array i c) (aref array i j))))
    gl-array))

(defun indices->gl-array (indices)
  (let* ((count (length indices))
         (gl-array (gl:alloc-gl-array :unsigned-short count)))
    (iter (for i below count)
          (setf (gl:glaref gl-array i) (aref indices i)))
    gl-array))

;; constructs a mesh object from an xml-list geometry tag
;; Builds the *source-ht* table
;; Returns a mesh object
(defun build-mesh (geometry-lst)
  "From a single geometry tag, builds a mesh object. That is, read in
   all the vertice data (position, normal, tex-coord) and feed it to 
   opengl as needed
   @arg[geometry-lst]{xml-list with tag 'geometry' and correct children}
   @return{A mesh object with vertex and index data}"
  (let ((id (get-attribute "id" (attributes geometry-lst)))
        (children (children (find-tag-in-children "mesh" geometry-lst))))
    (setf *source-ht* (make-hash-table :test #'equal))
    (hash-sources children)
    (set-vertices (find-tag +vertices+ children))
    (destructuring-bind (indices &rest arrays) 
        (process-indices (find-tag +triangles+ children))
                                        ;(cons indices (interleave arrays))
                                        ;#+disabled
      (make-instance 'mesh 
                     :id (intern id)
                     :vert-data (blt-mesh-array->gl-array 
                                 (interleave arrays))
                     :indices (indices->gl-array indices)
                     :array-format 'blt-mesh
                     :primitive 'triangles))))

;; Helper function to construct a 4x4 matrix (should probably be
;; extended to support arbitrary sized matrices
(defun matrix-tag->matrix (xml-lst)
  (reshape (string->sv (third xml-lst)) '(4 4)))

;; Build a hash table of mesh ids and meshes 
(defun process-geometry (geom-library)
  "parses the geometry items in the dae file, building a hash table of the 
   meshes (hashed by id)
   @arg[geom-library]{the xml-list of the geometry library}"
  (let ((mesh-table (make-hash-table)))
    (iter (for geom-xml in (children-with-tag +geometry-block+ geom-library))
          (let ((new-mesh (build-mesh geom-xml)))
            (setf (gethash (mesh-id new-mesh) mesh-table) new-mesh)))
    mesh-table))

;; Build a table of scene nodes.  This is assuming a flat graph, which
;; so far is all that max has given me.  SO it should be fine, until
;; we start looking at character animation.  Then...who knows.
(defun process-scene (scene-library)
  (let ((scene (first-child scene-library))
        (scene-table (make-hash-table)))
    (iter (for node in (children-with-tag "node" scene))
          (let ((node-id (intern (get-attribute "id" (attributes node))))
                (transform (matrix-tag->matrix (first-child node)))
                (geometry-id (get-url 
                                (find-tag-in-children +instance-geometry+ 
                                                      node))))
            (setf (gethash node-id scene-table) (cons geometry-id transform))))
    scene-table))

(defun effect-xmls->material (effect)
  (make-instance 
   'material
   :ambient (string->sv (third (find-tag "ambient" (children effect))))
   :diffuse (string->sv (third (find-tag "diffuse" (children effect))))
   :specular (string->sv (third (find-tag "specular" (children effect))))
   :specularity (string->sv (third (find-tag "shininess" (children effect))))
   :texture (image->texture (load-image
                             (gethash
                              (get-attribute "texture"
                                             (find-tag "texture" 
                                                       (children effect)))
                              images-ht)))))

;; Build a hash table of materials (hashed by id)
(defun process-materials (mat-library image-library effect-library)
  (let ((images-ht (make-hashtable :test #'equal))
        (effects-ht (make-hashtable :test #'equal))
        (materials-ht (make-hashtable :test #'equal)))

    ;; construct image table
    (iter (for image in (children image-library))
          (let ((image-id (get-attribute "id" (attributes image))))
            (setf (gethash image-id images-ht) (third (first-child image)))))

    ;; construct effects table
    (iter (for effect in (children effect-library))
          (setf (gethash (get-attribute "id" (attributes effect))
                         effects-ht)
                (effect-xmls->material effect)))
    
    ;; Finally the materials
    (iter (for material in (children mat-library))
          (setf (gethash )))))

(defun build-models (&key geometry scenes lights materials)
  "This is called last by load-dae to take the data parsed from
   the dae file and construct the objects to pass to the game,
   or store in level files, or wherever
   @return{this may change.  a list of objects for the game}"
  (iter (for (node-id (geom-id transform)) in-hashtable scenes)
        (collect (make-instance 'model-shape
                                :mesh (gethash geom-id geometry)
                                :matrix transform))))

(defun load-dae (filename)
  "Loads the objects from a dae file"
  (let ((dae-file (cxml:parse-file filename
                   #+disabled(blt3d-res:resolve-resource filename) 
                                   (cxml-xmls:make-xmls-builder))))
    (let ((geometry-table (process-geometry 
                           (find-tag-in-children +geometry-library+ dae-file)))
          (scene-table    (process-scene 
                           (find-tag-in-children +scene-library+ dae-file)))
          (material-table (process-materials
                           (find-tag-in-children +material-library+ dae-file)
                           (find-tag-in-children +image-library+ dae-file)
                           (find-tag-in-children +effect-library+ dae-file))))
      ;; Combine all the tables into a list of model-shape objects
      (build-models :geometry geometry-table
                    :scenes   scene-table))))