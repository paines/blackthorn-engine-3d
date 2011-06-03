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

(defpackage :blackthorn3d-sector
  (:nicknames :blt3d-sec)
  (:use :iter :cl :alexandria :userial :blt3d-utils :blt3d-math :blt3d-ent
        :blt3d-phy)
  (:export
   
   ;; sector.lisp
   :sector
   :sector-id
   :geometry
   :portals

   :foreach-in-sector
   :add-to-sector
   :remove-from-sector
   :lookup-sector
   :new-sector
   :collide-sector
   :collide-sector-portals
   :kill-entity
   :update-sectors

   :add-sector-relative
   :link-sectors

   :get-transform-to-world

   :add-test-sectors
   :collect-sectors
   
   ;; portal.lisp
   :portal
   :portal-id
   :links-to-portal
   :links-to-sector
   :transform-portal
   :make-portal
   :link-portals
   ))