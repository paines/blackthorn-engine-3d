;;;; Blackthorn -- Lisp Game Engine
;;;;
;;;; Copyright (c) 2011 Chris McFarland <askgeek@gmail.com>
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

(defun make-sector (name level)
  (new-sector name
              (blt3d-imp:dae-geometry level)
              :portals (blt3d-imp:dae-portals level)))


;; the room players land in when they first connect
(defun make-start-sector (loaded-dae)
  (make-sector :start-sector loaded-dae))


;; server side only
(defun load-level ()
  (register-model-loader :dae 
                         #'(lambda (path) 
                             (blt3d-imp:dae-geometry
                              (blt3d-imp:load-dae path))))
  (register-model-loader :level
                         #'(lambda (path)
                             (let ((level (blt3d-imp:load-dae path)))
                               (setf (blt3d-imp:dae-geometry level)
                                     (blt3d-phy:initialize-cube
                                      (blt3d-imp:dae-geometry level)))
                               level)))
                            
  (load-models-n-stuff)
  (blt3d-phy:apply-transform (get-model :wedge) (make-scale #(0.01 0.01 0.01)))


  ;; load our test model
  (let ((level
         (blt3d-res:load-model 
          :companion-cube :level #p "res/models/DeadEndRoom.dae")))

      
    #+disable
    (blt3d-phy:apply-transform 
     level
     (make-inv-ortho-basis (make-point3 1.0 0.0 0.0)
                           (make-point3 0.0 0.0 1.0)
                           (make-point3 0.0 1.0 0.0)))
    (make-start-sector level)
    level))

