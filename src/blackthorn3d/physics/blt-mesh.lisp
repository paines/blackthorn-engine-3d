;;; of the Software, and to permit persons to whom the Software is
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

;; This is going to be our generic mesh object. It's what we load 
;; other formats into, and is converted by the graphics subsystem into
;; appropriate objects for rendering.  
;; It will need to have vertex and index information, as well as skinning,and
;; whatever else
;; I think I want to store the vertexes unified, if not interleaved.
;; Unfortunately this makes life harder for the dae-geometry section.
(defclass blt-mesh ()
  ((id
    :accessor id
    :initarg :id)
   (vertex-streams
    :accessor vertex-streams
    :initarg :vertex-streams
    :documentation "A list of vertex-stream objects")
   (controller
    :accessor controller
    :initarg :controller
    :documentation "Rigged models need a control element to attach
                   the vertices to bone data.  See dae-controller.lisp
                   for more details")
   (elements
    :accessor elements
    :initarg :elements
    :documentation "A list of elem objects")
   (bounding-volume
    :accessor mesh-bounding-volume
    :initarg :bounding-volume
    :documentation "For narrow-phase collision detection
                   to start we can use something coarse like a sphere
                   but eventually might be something shiny, like bsp,
                   or hierarchy of bvs")))


(defun make-blt-mesh (&key id vertex-streams elements)
  (make-instance 'blt-mesh
                 :id id
                 :vertex-streams vertex-streams
                 :elements elements
                 :bounding-volume
                 (make-bounding-volume
                  (vs-get-stream :vertex vertex-streams))))

(defmethod make-bounding-volume ((this blt-mesh))
  (make-bounding-sphere (get-stream :vertex this)))

(defclass blt-skin (blt-mesh)
  ((bind-skeleton
    :accessor bind-skeleton
    :initarg :bind-skeleton)
   (bind-shape-matrix
    :accessor bind-shape-matrix
    :initarg :bind-shape-matrix
    :initform (make-identity-matrix))))

(defun make-blt-skin (&key mesh skeleton bind-matrix)
  (change-class mesh 'blt-skin)
  ;(format t "after change, mesh-vertex-streams: ~a~%" (vertex-streams mesh))
  (with-slots (bind-skeleton bind-shape-matrix) mesh
    (setf bind-skeleton skeleton
          bind-shape-matrix bind-matrix))
  mesh)

;; makes a vertex array of the bind pose...
(defmethod bind-pose->vert-array ((this blt-skin))
  (labels ((get-vert-attrib (semantic data)
             (second (find semantic data :key #'car))))

    (with-slots (vertex-streams elements bind-skeleton)
        this

      (update-skeleton bind-skeleton)

      (let ((joint-mats (get-joint-matrices bind-skeleton))
            (vertices (vs-get-stream :vertex vertex-streams))
            (j-indices (vs-get-stream :joint-index vertex-streams))
            (j-weights (vs-get-stream :joint-weight vertex-streams)))
        (iter (for vertex in-vector vertices)
              (for j-i in-vector j-indices)
              (for j-w in-vector j-weights)
              (collect
               (iter (for i below 4)
                     (for pos initially +origin+ 
                          then (vec4+ 
                                pos
                                (vec-scale4
                                 (matrix-multiply-v
                                  (aref joint-mats (aref j-i i))
                                  (vec3->point vertex))
                                 (aref j-w i))))
                     (finally 
                      (return pos))) result-type 'vector))))))


(defmethod make-bounding-volume ((this blt-skin))
  (make-bounding-sphere
   (bind-pose->vert-array this)))

(defun vs-get-stream (stream vs-lst)
  (aif (find stream vs-lst :key #'vs-semantic)
       (vs-stream it)))

(defmethod get-stream (stream (this blt-mesh))
  (vs-get-stream stream (vertex-streams this)))


(defmethod apply-transform ((this blt-mesh) xform)
  "Apply a transformation matrix to a blt-mesh at the vertex and normal level.
   PLEASE NO SCALING!!"
  (labels ((apply-v-helper (vertex)
             (let ((xformed-v (matrix-multiply-v xform (vec3->point vertex))))
               (iter (for i below 3)
                     (setf (svref vertex i) (svref xformed-v i)))))
           (apply-n-helper (normal)
             (let ((xformed-v (norm4
                               (matrix-multiply-v xform (vec3->point normal)))))
               (iter (for i below 3)
                     (setf (svref normal i) (svref xformed-v i))))))
    (let ((vertices (get-stream :vertex this))
          (normals (get-stream :normal this)))
      (format t "transforming vertices~%")
      (iter (for v in-vector vertices)
            (for n in-vector normals)
            (apply-v-helper v)
        ;    (apply-n-helper n)
            ))))



(defclass node ()
  ((id
    :accessor id
    :initarg :id)
   (transform
    :accessor transform
    :initarg :transform)
   (bounding-volume
    :accessor node-bounding-volume
    :initarg :bounding-volume
    :documentation "bounding volume encompassing all children??")
   (child-nodes
    :accessor child-nodes
    :initarg :child-nodes
    :initform nil)))

(defclass model-node (node)
  ((material-array
    :accessor material-array
    :initarg :material-array)
   (mesh
    :accessor mesh
    :initarg :mesh)))

(defun make-scene-node (&key id transform child-nodes)
  (let ((children (remove-if #'null child-nodes)))
    (format t "~3TMaking scene node ~a with children ~a~%"
            id child-nodes)
    (make-instance 'node
                   :id id
                   :transform transform
                   :child-nodes children
                   :bounding-volume
                   (when children
                     (transform-bounding-volume
                      (combine-bounding-spheres
                       (iter (for c in children)
                             (collect (node-bounding-volume c))))
                      transform)))))

(defun make-model-node (&key id transform material-array mesh child-nodes)
  (let ((bv (transform-bounding-volume
             (make-bounding-volume mesh)
             transform)))
    (make-instance 'model-node
                   :id id
                   :transform transform
                   :material-array material-array
                   :mesh mesh
                   :child-nodes child-nodes
                   :bounding-volume bv)))

(defmethod copy-node ((node model-node))
  (with-slots (id transform material-array mesh child-nodes bounding-volume) 
      node
    (make-instance 'model-node
     :id id
     :transform transform
     :material-array material-array
     :mesh mesh
     :child-nodes (iter (for child in  child-nodes)
                        (collect (copy-node child)))
     :bounding-volume bounding-volume)))

(defmethod copy-node ((node node))
  (with-slots (id transform child-nodes bounding-volume)
      node
    (make-instance 'node
                   :id id
                   :transform transform
                   :child-nodes
                   (iter (for child in child-nodes)
                         (collect (copy-node child)))
                   :bounding-volume bounding-volume)))


(defmethod find-node ((obj string) (node node))
  (format t "find-node visiting ~a~%" (id node))
  (if (equal obj (id node))
      (progn (format t "Found it!~%") node)
      (iter (for child in (child-nodes node))
            (for result =
                 (find-node obj child))
            (until result)
            (finally (return result)))))

(defmethod attach-node ((obj node) (node node))
  (format t "Attaching ~a to ~a~%" (id obj) (id node))
  (setf (node-bounding-volume node)
        (combine-bounding-spheres
         (list (node-bounding-volume node)
               (node-bounding-volume obj))))
  (push obj (child-nodes node)))