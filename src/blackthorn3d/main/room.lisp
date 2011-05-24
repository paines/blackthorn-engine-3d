(defclass room (entity-server)
  ((contents
    :accessor contents
    :initform (make-hash-table))))
    
(defun clone-table (table)
  (let ((clone (make-hash-table)))
    (maphash #'(lambda (k v) (setf (gethash k clone) v)) table)
    clone))
    
(defun foreach-in-room (a-room func)
  (let ((new-table (clone-table (contents a-room))))
    (maphash #'(lambda (k v) 
                 (declare (ignore k))
                 (func v))  
             new-table)))
    
(defun add-to-room (an-entity a-room)
  (setf (gethash (oid an-entity)) an-entity)
  (setf (current-room an-entity) a-room))
  
(defun remove-from-room (an-entity)
  (let ((the-room (current-room an-entity)))
    (remhash (oid an-entity) the-room)
    (setf (current-room an-entity) nil)))
    
(defmethod update ((a-room room))
  (foreach-in-room a-room #'(lambda (an-entity) (update an-entity))))
