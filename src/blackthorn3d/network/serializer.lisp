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

(in-package :blackthorn3d-network)

(defmacro make-vec-serializer (vec-type elt-type count)
  (with-gensyms (type value component)
    (let ((buffer (make-symbol (symbol-name 'buffer))))
      `(progn
         (defmethod serialize ((,type (eql ,vec-type)) ,value
                               &key (,buffer *buffer*))
           (assert (= (length ,value) ,count))
           (iter (for ,component in-vector ,value)
                 (serialize ,elt-type ,component :buffer ,buffer)))
         (defmethod unserialize ((type (eql ,vec-type))
                                 &key (,buffer *buffer*))
           (iter (repeat ,count)
                 (collect (unserialize ,elt-type :buffer ,buffer)
                          result-type 'vector)))))))

(defmacro make-list-serializer (list-type elt-type
                                &optional (count-type :uint32))
  (with-gensyms (type value item count)
    (let ((buffer (make-symbol (symbol-name 'buffer))))
      `(progn
         (defmethod serialize ((,type (eql ,list-type)) ,value
                               &key (,buffer *buffer*))
           (serialize ,count-type (length ,value) :buffer ,buffer)
           (iter (for ,item in ,value)
                 (serialize ,elt-type ,item :buffer ,buffer)))
         (defmethod unserialize ((,type (eql ,list-type))
                                 &key (,buffer *buffer*))
           (let ((,count (unserialize ,count-type :buffer ,buffer)))
             (iter (repeat ,count)
                   (collect (unserialize ,elt-type :buffer ,buffer)))))))))

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
                               `(serialize ,type ,slot
                                                   :buffer buffer))
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

(defmethod serialize ((type (eql :single-float)) value &key (buffer *buffer*))
  (serialize :float32 (coerce value 'single-float) :buffer buffer))

(defmethod unserialize ((type (eql :single-float)) &key (buffer *buffer*))
  (unserialize :float32 :buffer buffer))

(defmethod serialize ((type (eql :symbol)) value &key (buffer *buffer*))
  (serialize :string (symbol-name value) :buffer buffer)
  (serialize :string (package-name (symbol-package value)) :buffer buffer))

(defmethod unserialize ((type (eql :symbol)) &key (buffer *buffer*))
  (let ((symbol-name (unserialize :string :buffer buffer))
        (package-name (unserialize :string :buffer buffer)))
    (intern symbol-name package-name)))

