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

(defmessage :event-entity-create nil (:entity-create-list))
(defmessage :event-entity-update nil (:entity-update-list))
(defmessage :event-entity-remove nil (:entity-remove-list))

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

(defmessage :event-input send-input
  (:single-float ; move-x
   :single-float ; move-y
   :single-float ; view-x
   :single-float ; view-y
   :single-float ; jmp
   :single-float ; left trigger
   :single-float ; right trigger
   :single-float ; left bumper
   :single-float ; right bumper
   :single-float ; use (xbox x button)
   :single-float ; xbox y button
   :single-float ; alt attack (xbox b button)
   ))

(defmessage :event-camera send-camera
  (:entity-oid))

;; TODO: Move to game-specific location.
(defmessage :play-explosion send-play-explosion
  (:symbol ; name
   :vec4   ; pos
   ))

(defmessage :play-laser send-play-laser
  (:symbol ; name
   :vec4   ; start-pos
   :vec4   ; vector to draw laser on
   ))

(defmessage :play-animation send-play-animation
  (:entity-oid ; entity
   :symbol     ; name
   :symbol     ; mode
   ))
