;;;; Blackthorn -- Lisp Game Engine
;;;;
;;;; Copyright (c) 2011 Chris McFarland <askgeek@gmail.com>,
;;;;               2011 Robert Gross <r.gross.3@gmail.com>
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

(in-package :blackthorn3d-main)


(defun make-sector (name level 
                    &optional 
                    (orientation (quat-identity)))
  (format t "portals from importer ~a~%" (blt3d-imp:dae-portals level))
  (new-sector name
              (blt3d-imp:dae-geometry level)
              :portals 
              (iter (for portal in (blt3d-imp:dae-portals level))
                    (collect 
                     (destructuring-bind (name pos dir bv) portal
                       (transform-portal
                        (transform-portal
                         (make-portal name pos dir bv)
                         (make-inv-ortho-basis 
                          (make-point3 -1.0 0.0 0.0)
                          (make-point3 0.0 0.0 1.0)
                          (make-point3 0.0 1.0 0.0)))
                        (make-translate #(0.0 -15.0 0.0 1.0))))))
              :orientation orientation))

 
;; the room players land in when they first connect
(defun make-start-sector (loaded-dae)
  (make-sector :start-sector loaded-dae))

