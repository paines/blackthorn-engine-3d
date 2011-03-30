;;;; Blackthorn -- Lisp Game Engine
;;;;
;;;; Copyright (c) 2007-2011, Elliott Slaughter <elliottslaughter@gmail.com>
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

(in-package :cl-user)

;;;
;;; Internal Interface:
;;; Anything exported here but not exported in the public interface is
;;; intended for internal use only.
;;;

(defpackage :blackthorn3d-utils
  (:nicknames :blt3d-utils)
  (:use :cl :alexandria :iter)
  (:export

   ;; utils.lisp
   :aif
   :acond
   :it

   ;; resources.lisp
   :add-resource-path
   :resolve-resource

   ))

;;;
;;; Public Interface:
;;; The generic functions and classes listed form the interface to Blackthorn.
;;;

(defpackage :blackthorn3d
  (:nicknames :blt3d)
  (:use :cl :blt3d-utils)
  (:export

   ;; utils.lisp
   :aif
   :acond
   :it

   ;; resources.lisp
   :add-resource-path
   :resolve-resource

   ))

;;;
;;; User Package:
;;;

(defpackage :blackthorn3d-user
  (:nicknames :blt3d-user)
  (:use :cl :iter :blt3d)
  #+allegro (:import-from :cl-user :exit)
  (:export

   ;; main.lisp
   :main

   ))

#-allegro
(eval-when (:compile-toplevel :load-toplevel)
  (setf (symbol-function 'blt3d-user::exit) #'quit))
