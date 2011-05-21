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

(ql:quickload :alexandria)
(ql:quickload :trivial-features)
(ql:quickload :cl-fad)
(ql:quickload :cl-ppcre)
(ql:quickload :quicklisp-slime-helper)

(defun file-contents (filename)
  (with-open-file (filestream filename :direction :input)
    (let ((string (make-string (file-length filestream))))
      (read-sequence string filestream)
      string)))

(defun (setf file-contents) (string filename)
  (with-open-file (filestream filename :direction :output :if-exists :supersede)
    (write-sequence string filestream)))

(let* ((user-homedir
        #-windows (user-homedir-pathname)
        #+windows (merge-pathnames #p"AppData/Roaming/"
                                   (user-homedir-pathname)))
       (blackthorn3d-dir
	(fad:directory-exists-p
	 (merge-pathnames
	  #p"../../"
	  (make-pathname
	   :host (pathname-host #.(or *compile-file-truename*
				      *load-truename*))
	   :directory (pathname-directory #.(or *compile-file-truename*
						*load-truename*))))))
       (emacs-default-config
	(merge-pathnames #p"build/emacs/emacs.el" blackthorn3d-dir))
       (emacs-user-config (merge-pathnames #p".emacs" user-homedir)))
  (when (or (not (fad:file-exists-p emacs-user-config))
            (yes-or-no-p
             "You are about to override yor .emacs file. Continue?"))
    (format t "Copying ~s to ~s~%" emacs-default-config emacs-user-config)
    (fad:copy-file emacs-default-config emacs-user-config :overwrite t)
    (setf (file-contents emacs-user-config)
	  (ppcre:regex-replace-all "@BLACKTHORN_DIR@"
				   (file-contents emacs-user-config)
				   (namestring
				    (fad:pathname-as-file blackthorn3d-dir))))))

#+allegro (exit) #-allegro (quit)
