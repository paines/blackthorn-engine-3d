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

(defvar +image-path+ #p "res/images/")
(defvar *param-table* nil)
(defvar *epic-texture* nil)

(defun get-filename (dumb-string)
  (let ((p (position #\_ dumb-string :from-end t)))
    (concatenate 'string
                 (subseq dumb-string 0 p)
                 "."
                 (subseq dumb-string (1+ p)))))

(defun mat-prop-finder (attrib e)
  (aif (find-tag attrib (children e))
       (let ((value (first-child it)))
         (cond
           ((equal "color" (tag-name value))
            (string->sv (third value)))
           ((equal "float" (tag-name value))
            (float (read-from-string (third value))))
           ((equal "texture" (tag-name value))
            ;; We'll just load it to texture here, I think.
            ;; save me some trouble
            (merge-pathnames
             (pathname (get-filename
                        (gethash
                         (get-attribute "texture" (attributes value))
                         *param-table*)))
             +image-path+)

            #+disabled
            (let ((filename ))
              (setf *epic-texture*
                    (image->texture2d
                     (load-image (merge-pathnames
                                  (pathname filename)
                                  +image-path+))))))
           (t nil)))))

(defun load-param (param)
  (let ((child (first-child param)))
    (cond
      ((equal "surface" (tag-name child))
       ;; Texture, return the image name
       (format t "param val ~a~%" (third (first-child child)))
       (third (first-child child)))
      ((equal "sampler2D" (tag-name child))
       ;; 'sampler' for texture. just get the name and put it in
       (format  t "param val ~a~%" (gethash (third (first-child child)) *param-table*))
       (gethash (third (first-child child)) *param-table*)))))

(defun process-effect (effect-tag effect-table)
  ;; Find 'newparam's that contain texture data (this is annoyingly
  ;; similar to the way sources/inputs work)

  (let ((profile-elem (find-tag-in-children "profile_COMMON" effect-tag))
        (*param-table* (make-id-table)))

    (iter (for param in (children-with-tag "newparam" profile-elem))
          (format t "param sid: ~a~%" (get-attribute "sid" (attributes param)))
          (setf (gethash (get-attribute "sid" (attributes param))
                         *param-table*)
                (load-param param)))

    (setf (gethash (get-attribute "id" (attributes effect-tag))
                   effect-table)
          (make-blt-material
           :ambient     (mat-prop-finder "ambient" effect-tag)
           :diffuse     (mat-prop-finder "diffuse" effect-tag)
           :specular    (mat-prop-finder "specular" effect-tag)
           :shininess   (mat-prop-finder "shininess" effect-tag)
           :textures
           (aif (find-tag "texture" (children effect-tag))
                #+disabled(load-image (gethash
                                       (get-attribute "texture"
                                                      (attributes it))
                                       images-ht))
                nil)))))

;; Build a hash table of materials (hashed by id)
(defun process-materials (mat-library image-library effect-library)
  (let ((images-ht (make-id-table))
        (effects-ht (make-id-table))
        (materials-ht (make-id-table)))

    ;; construct image table
    (iter (for image in (children image-library))
          (when (consp image)
            (let ((image-id (get-attribute "id" (attributes image))))
              (setf (gethash image-id images-ht)
                    (third (first-child image))))))

    ;; construct effects table
    (iter (for effect in (children effect-library))
          (when (consp effect)
            (process-effect effect effects-ht)))

    ;; Finally the materials
    (iter (for material in (children mat-library))
          (when (consp material)
            (setf (gethash (get-attribute "id" (attributes material))
                           materials-ht)
                  (gethash (get-url (first-child material))
                           effects-ht))))

    materials-ht))
