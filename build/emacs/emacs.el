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

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(show-paren-mode t))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

(setq inhibit-startup-message t)
(setq-default indent-tabs-mode nil)
(delete-selection-mode t)

;;;
;;; Lisp File Extensions
;;;

(setq auto-mode-alist (cons '("\\.lisp$" . lisp-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.lsp$" . lisp-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.cl$" . lisp-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.asd$" . lisp-mode) auto-mode-alist))

;;;
;;; Paredit
;;;

(add-to-list 'load-path "@BLACKTHORN_DIR@/build/emacs/")
(autoload 'paredit-mode "paredit"
  "Minor mode for pseudo-structurally editing Lisp code."
  t)
(add-hook 'lisp-mode-hook (lambda () (paredit-mode +1)))

;;;
;;; Slime
;;;

(load (expand-file-name "@BLACKTHORN_DIR@/build/quicklisp/slime-helper.el"))

(setq slime-lisp-implementations
      '((clozure ("@BLACKTHORN_DIR@/build/ccl/wx86cl" "--load" "@BLACKTHORN_DIR@/build/scripts/quicklisp-setup.lisp"))
        (sbcl ("sbcl" "--load" "@BLACKTHORN_DIR@/build/scripts/quicklisp-setup.lisp"))))

(eval-after-load "slime"
  '(progn
     (slime-setup '(slime-fancy slime-asdf slime-banner))
     (setq slime-complete-symbol*-fancy t)
     (setq slime-complete-symbol-function 'slime-fuzzy-complete-symbol)))

(defun load-game ()
  (slime-interactive-eval "(defparameter *driver-system* \"blank\")")
  (slime-load-file "@BLACKTHORN_DIR@/build/scripts/load.lisp"))

(defun clozure ()
  (interactive)
  (setq slime-connected-hook (delq 'load-game slime-connected-hook))
  (slime 'clozure))

(defun sbcl ()
  (interactive)
  (setq slime-connected-hook (delq 'load-game slime-connected-hook))
  (slime 'sbcl))

(defun clozure-game ()
  (interactive)
  (add-hook 'slime-connected-hook 'load-game)
  (slime 'clozure))

(defun sbcl-game ()
  (interactive)
  (add-hook 'slime-connected-hook 'load-game)
  (slime 'sbcl))

(global-set-key [f5] 'clozure)
(global-set-key [f6] 'sbcl)
(global-set-key [f7] 'clozure-game)
(global-set-key [f8] 'sbcl-game)
