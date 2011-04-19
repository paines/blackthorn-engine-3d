;;;; Blackthorn -- Lisp Game Engine
;;;;
;;;; Copyright (c) 2011, Elliott Slaughter <elliottslaughter@gmail.com>
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

(in-package :blackthorn3d-entity)

(defclass entity ()
  ((pos
    :accessor pos
    :initarg :pos)
   (dir
    :accessor dir
    :initarg :dir)
   (veloc
    :accessor veloc
    :initarg :veloc)))

;; Hoping that this abomination will be unnecessary in a future version
;; of userial....
(defmacro make-init-slot-serializer (type (&rest factory)
                                     (&rest init) (&rest fields))
  (labels ((get-types-and-slots (fields &optional types slots)
	     (cond
	       ((null fields) (values (nreverse types) (nreverse slots)))
	       ((null (rest fields))
		  (error "Expected same number of TYPEs as SLOTs"))
	       (t (get-types-and-slots (rest (rest fields))
				       (cons (first fields) types)
				       (cons (second fields) slots))))))
    (multiple-value-bind (init-types init-slots) (get-types-and-slots init)
      (multiple-value-bind (types slots) (get-types-and-slots fields)
        (let ((obj-sym (gensym "OBJ-")))
          `(progn
             (defmethod serialize ((type (eql ,type)) value
                                   &key (buffer *buffer*))
               (with-slots ,(append init-slots slots) value
                 ,@(mapcar #'(lambda (type slot)
                               `(serialize ,type ,slot :buffer buffer))
                           (append init-types types)
                           (append init-slots slots)))
               buffer)
             (defmethod unserialize ((type (eql ,type))
                                     &key (buffer *buffer*))
               (let ((,obj-sym
                      ,(append
                        factory
                        (mapcar #'(lambda (type)
                                    `(unserialize ,type
                                                  :buffer buffer))
                                init-types))))
                 (with-slots ,slots ,obj-sym
                   ,@(mapcar #'(lambda (type slot)
                                 `(setf ,slot (unserialize ,type
                                                           :buffer buffer)))
                             types slots))
                 (values ,obj-sym buffer)))))))))

(make-init-slot-serializer :entity-create
                           (make-entity)
                           (:oid oid)
                           (:vec3 pos
                            :vec3 dir
                            :vec3 veloc))

(make-init-slot-serializer :entity-update-fields
                           (lookup-entity)
                           (:oid oid)
                           (:vec3 pos
                            :vec3 dir
                            :vec3 veloc))
