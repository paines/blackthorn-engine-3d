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


;; Build a hash table of materials (hashed by id)
(defun process-materials (mat-library image-library effect-library)
  (let ((images-ht (make-id-table))
        (effects-ht (make-id-table))
        (materials-ht (make-id-table)))

    ;; construct image table
    (iter (for image in (children image-library))
          (when (consp image)
            (let ((image-id (get-attribute "id" (attributes image))))
              (setf (gethash image-id images-ht) (third (first-child image))))))

    ;; construct effects table
    (labels ((mat-prop-finder (attrib e)
               (aif (find-tag attrib (children e))
                    (string->sv (third it))
                    nil)))
      (iter (for effect in (children effect-library))
            (when (consp effect)
              (setf (gethash (get-attribute "id" (attributes effect))
                             effects-ht)
                    (make-instance
                     'blt-material
                     :ambient     (mat-prop-finder "ambient" effect)
                     :diffuse     (mat-prop-finder "diffuse" effect)
                     :specular    (mat-prop-finder "specular" effect)
                     :shininess   (mat-prop-finder "shininess" effect)
                     :textures (aif (find-tag "texture" (children effect))
                                    (load-image (gethash
                                                 (get-attribute "texture" it) 
                                                 images-ht))
                                    nil))))))
    
    ;; Finally the materials
    (iter (for material in (children mat-library))
          (when (consp material)
            (setf (gethash (get-attribute "id" (attributes material))
                           materials-ht)
                  (gethash (get-url (first-child material))
                           effects-ht))))
    materials-ht))
