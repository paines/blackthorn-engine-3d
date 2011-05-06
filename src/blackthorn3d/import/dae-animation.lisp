;;;; Blackthorn -- Lisp Game Engine
;;;;
;;;; Copyright (c) 2011, Robert Gross <r.gross.3@gmail.com>
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

(in-package :blackthorn3d-import)

;;;
;;; Stuff for loading animations from COLLADA files
;;; 
;;; Current support is for baked matrices with linear interpolation
;;; only
;;;

;;;
;;; Constants
;;;

(defvar +animation+ "animation")
(defvar +sampler+ "sampler")
(defvar +channel+ "channel")

;;;
;;; Functions
;;;

(defun process-animations (animations-library)
  (let ((animation-table (make-id-table))
        (sampler-table (make-id-table)))    
    (iter (for animation in (children-with-tag +animation+ 
                                               animations-library))
          (let ((source-table (hash-sources animation))
                (channel-list ()))

            ;; Setup samplers
            (iter (for sampler in (children-with-tag +sampler+ animation))
                  (setf (gethash (get-attribute "id" (attributes sampler))
                                 sampler-table)
                        (build-inputs sampler source-table)))

            ;; Setup channels
            (iter (for channel in (children-with-tag +channel+ animation))
                  (let ((inputs 
                         (gethash (get-uri "source" (attributes channel)) 
                                  sampler-table)))
                    (collect
                     (make-channel
                      :times (input-by-semantic :input inputs)
                      :values (input-by-semantic :output inputs)
                      :target (get-uri "target" (attributes channel))))))

            ;; Setup the animation-clip
            (let ((end-time
                   (iter (for ch in channel-list)
                         (maximizing  (time-step (frames ch) 0))))
                  (anim-id (get-attribute "id" (attributes animation))))
              (setf (get-hash anim-id animation-table)
                    (make-instance 'animation-clip
                                   :id anim-id
                                   :channel-lst channel-list
                                   :t-start 0.0
                                   :t-end end-time)))))
    ;; Finally, return the table
    animation-table))