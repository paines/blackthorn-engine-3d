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

(in-package :blackthorn3d-main)

;;;
;;; System paths
;;;

(defun setup-paths ()
  (add-resource-path *default-pathname-defaults*)
  (add-resource-path
   (merge-pathnames #p"../../" #.(or *compile-file-truename* *load-truename*)))
  #+darwin
  (add-resource-path
   (merge-pathnames #p"../Resources/" (command-line-executable))))

;;;
;;; Command-line option parsing
;;;

(defun command-line-executable ()
  "Returns the path to the executable being run."
  #+allegro (car (sys:command-line-arguments))
  #+clisp (aref (ext:argv) 0)
  #+clozure (car ccl:*command-line-argument-list*)
  #+ecl (car (ext:command-args))
  #+lispworks (car system:*line-arguments-list*)
  #+sbcl (car sb-ext:*posix-argv*)
  #-(or allegro clisp clozure ecl lispworks sbcl)
  (error "Don't know how to get command line args."))

(defparameter *cli-options*
  '((("server" #\s) :type string :optional t)
    (("connect" #\c) :type string :optional t)
    (("port" #\P) :type integer :initial-value 12345)))

(defun cli-get-mode ()
  (let* ((args (command-line-arguments:get-command-line-arguments))
         (opts (command-line-arguments:process-command-line-options
                        *cli-options*
                        (aif (position "--" args :test #'equal)
                             (nthcdr (1+ it) args)
                             args))))
    (append (or (aif (getf opts :server) (list :server it))
                (aif (getf opts :connect) (list :client it))
                (list :offline nil))
            (list (getf opts :port)))))

;;;
;;; Main Game Driver
;;;

(defparameter *input* (make-instance 'input-system :kind :keyboard))

(defun main (&key (exit-when-done t))
  "Main entry point for the game. Deals with initialization, finalization, and the main game loop."
  
  ;; switch to server mode, or else continue based on command line args
  (let ((modes (cli-get-mode)))
    (when (eql (first modes) :server)
      (apply #'server-main (rest modes))
      (if exit-when-done
          (exit)
          (return-from main)))
    (when (eql (first modes) :client)
      (apply #'client-main (rest modes))
      (if exit-when-done
          (exit)
          (return-from main))))

  ;; Initialization:
  (setup-paths)
  (load-dlls)
  (blt3d-rend:init)

  (setf mt19937:*random-state* (mt19937:make-random-state t))

  (sdl:with-init ()
    ;; TODO: temporary code, abstract this away
    (sdl:window 800 600 :bpp 32 :flags sdl:sdl-opengl
                :title-caption "Test" :icon-caption "Test")

    (blt3d-rend:prepare-scene)

    ;; Main loop:
    (let ((input-queue (make-instance 'containers:basic-queue))
          (box-entity (make-server-entity
                       'entity-server 
                       :pos (make-point3 0.0 0.0 0.0)
                       :dir (make-vec3 1.0 0.0 0.0) 
                       :up  (make-vec3 0.0 1.0 0.0)
                       :shape (make-instance
                               'blt3d-gfx:model-shape
                               :mesh
                               (car (blt3d-imp:load-dae
                                     #p"res/models/orange-box2.dae"))))))

      ;;(camera-orbit! cam 0.0 -0.2 5.0)
      (sdl:with-events ()
        (:quit-event ()
          t)
        (:key-down-event (:key k :mod m :mod-key m-k :unicode u)
          (when (sdl:key= k :sdl-key-return)
            (if (eql (input-kind *input*) :keyboard)
                (set-controller *input* :xbox)
                (set-controller *input* :keyboard))))
        (:key-up-event (:key k :mod m :mod-key m-k :unicode u))
        (:idle ()
          #+windows
          (xbox360_poll 0)

          ;; move camera based on keyboard/xbox controller
          #+disabled
          (let ((rot-amt  (* -1 (input-move-x *input*)))
                (step-amt (*  1 (input-move-y *input*))))

            (setf (blt3d-gfx:cam-dir blt3d-gfx:*main-cam*)
                  (quat-rotate-vec
                   (axis-rad->quat (make-vec3 0.0 1.0 0.0)
                                   (deg->rad (* 2.7 rot-amt)))
                   (blt3d-gfx:cam-dir blt3d-gfx:*main-cam*)))
            (setf (blt3d-gfx:cam-pos blt3d-gfx:*main-cam*) 
                  (vec4+ (blt3d-gfx:cam-pos blt3d-gfx:*main-cam*) 
                         (vec-scale4
                          (blt3d-gfx:cam-dir blt3d-gfx:*main-cam*)
                          step-amt))))

          (let ((x-amt (input-move-x *input*))
                (z-amt (* -1.0 (input-move-y *input*))))
            (setf (pos box-entity)
                  (vec4+ (pos box-entity)
                         (make-vec3 (float x-amt) 0.0 (float z-amt)))))

          (blt3d-rend:render-frame (list box-entity))

          #+disabled
          (let ((x (* 2 (abs (xbox360_get_lx 0))))
                (y (* 2 (abs (xbox360_get_ly 0))))) 
            (xbox360-vibrate 0 x y))

          ;; Rotate the camera around the target each frame
          #+disabled
          (camera-orbit! cam (/ pi 100) 0.0 5.0)

          #+blt-debug
          (let ((connection (or swank::*emacs-connection*
                                (swank::default-connection))))
            (when (and connection
                       (not (eql swank:*communication-style* :spawn)))
              (swank::handle-requests connection t)))))))

  ;; Finalization:
  #+windows (xbox360-vibrate 0 0 0)
  (when exit-when-done
    (exit)))
