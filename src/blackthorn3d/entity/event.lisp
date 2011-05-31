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

(make-list-serializer :entity-create-list :entity-create)
(make-list-serializer :entity-update-list :entity-update)
(make-list-serializer :entity-remove-list :entity-remove)

(defmessage :event-entity-create (:entity-create-list))
(defmessage :event-entity-update (:entity-update-list))
(defmessage :event-entity-remove (:entity-remove-list))

(defgeneric make-event (type &key))

(defmethod make-event ((type (eql :entity-create)) &key include-all)
  (let ((entities
         (if include-all
             (iter (for (nil entity) in-hashtable *global-oid-table*)
                   (collect entity))
             *recently-created-server-entities*)))
    (make-message-list :event-entity-create entities)))

(defmethod make-event ((type (eql :entity-update)) &key)
  (let ((entities
         (iter (for (nil entity) in-hashtable *global-oid-table*)
               (with-slots (modified) entity
                 (when modified (collect entity))))))
    (make-message-list :event-entity-update entities)))

(defmethod make-event ((type (eql :entity-remove)) &key)
  (let ((entities *recently-removed-server-entities*))
    (make-message-list :event-entity-remove entities)))

;; TODO: MOVE ELSEWHERE!!! (E.g. input package, hint hint.)
(defclass input-event ()
  ((input-type
    :accessor input-type
    :initarg :input-type)
   (input-amount
    :accessor input-amount
    :initarg :input-amount)))

(make-enum-serializer :input-type (:move-x :move-y :view-x :view-y :jmp))

(make-slot-serializer (:input input-event
                       (make-instance 'input-event))
                      :input-type input-type
                      :float32 input-amount)

(make-list-serializer :input-list :input)

(defmessage :event-input (:input-list))

(defmethod make-event ((type (eql :input)) &key move-x move-y view-x view-y jmp)
  (make-message-list
   :event-input
   (list (make-instance 'input-event
                        :input-type :move-x
                        :input-amount move-x)
         (make-instance 'input-event
                        :input-type :move-y
                        :input-amount move-y)
         (make-instance 'input-event
                        :input-type :view-x
                        :input-amount view-x)
         (make-instance 'input-event
                        :input-type :view-y
                        :input-amount view-y)
         (make-instance 'input-event
                        :input-type :jmp
                        :input-amount jmp))))

(defmessage :event-camera (:entity-oid))

(defmethod make-event ((type (eql :camera)) &key camera)
  (make-message-list :event-camera camera))
