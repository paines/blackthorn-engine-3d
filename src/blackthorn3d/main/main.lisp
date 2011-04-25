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
    (("port" #\P) :type integer :initial-value 12345)
    (("players") :type integer :initial-value 2)))

(defun cli-get-mode ()
  (let* ((args (command-line-arguments:get-command-line-arguments))
         (opts (command-line-arguments:process-command-line-options
                        *cli-options*
                        (aif (position "--" args :test #'equal)
                             (nthcdr (1+ it) args)))))
    (append (or (aif (getf opts :server) (list :server it))
                (aif (getf opts :connect) (list :client it))
                (list :normal nil))
            (list (getf opts :port)
                  (getf opts :players)))))

;;;
;;; Main Game Driver
;;;

(defparameter *input* (make-instance 'input-system :kind :keyboard))
    
(defun main-init-abort-handler ()
  (throw 'main-init nil))

(defun main-loop-abort-handler ()
  (throw 'main-loop nil))

(defun main-process-event (event)
  (send *game* event))

(defun main (&key (exit-when-done t))
  "Main entry point for the game. Deals with initialization, finalization, and the main game loop."
  
  ; switch to server mode, or else continue based on command line args
  (let ((modes (cli-get-mode)))
      (when (eq (first modes) :server)
        (server-main)
        (exit))
      (when (eq (first modes) :client)
        (client-main)
        (exit))
   )
  
  ;; Initialization:
  (setup-paths)
  (load-dlls)
  (blt3d-gfx:init)

  ;(unless *game* (error "No game specified.~%"))

  ;(apply #'net-init (cli-get-mode))

  (setf mt19937:*random-state* (mt19937:make-random-state t))

  (unwind-protect
  (catch 'main-init
    ;(net-game-connect #'main-init-abort-handler)

    (sdl:with-init ()
      ;(init-mixer)
      ;(game-init *game* :player (hostname) :players (hostnames))

      ; TODO: temporary code, abstract this away
      (sdl:window 800 600 :bpp 32 :flags sdl:sdl-opengl
                  :title-caption "Test" :icon-caption "Test")

      (blt3d-gfx:prepare-scene)
      
      ;; Main loop:
      (let ((input-queue (make-instance 'containers:basic-queue)))
        ;(camera-orbit! cam 0.0 -0.2 5.0)
        (catch 'main-loop
                                        ;(net-game-start #'main-loop-abort-handler)

          (sdl:with-events ()
            (:quit-event ()
                                        ;(net-game-quit)
                         t)
            (:key-down-event (:key k :mod m :mod-key m-k :unicode u)
                (when (sdl:key= k :sdl-key-return)
                  (if (eql (input-kind *input*) :keyboard)
                    (set-controller *input* :xbox)
                    (set-controller *input* :keyboard)))
                                        ;(containers:enqueue
                                        ; input-queue
                                        ; (make-instance 'key-event :host (hostname) :type :key-down :key k
                                        ;                :mod m :mod-key m-k :unicode u))
                             )
            (:key-up-event (:key k :mod m :mod-key m-k :unicode u)
                                        ;(containers:enqueue
                                        ; input-queue
                                        ; (make-instance 'key-event :host (hostname) :type :key-up :key k
                                        ;                :mod m :mod-key m-k :unicode u))
                           )
            (:idle ()
                   
                   (progn
                   #+windows
                     (xbox360_poll 0)
                                                     
                     ; move camera based on keyboard/xbox controller
                     (let ((rot-amt  (* -1 (input-move-x *input*)))
                           (step-amt (*  1 (input-move-y *input*))))
                         
                         (setf (blt3d-gfx:cam-dir blt3d-gfx:*main-cam*) (quat-rotate-vec
                            (axis-rad->quat (make-vec3 0.0 1.0 0.0) (deg->rad (* 2.7 rot-amt)))
                            (blt3d-gfx:cam-dir blt3d-gfx:*main-cam*)))
                         (setf (blt3d-gfx:cam-pos blt3d-gfx:*main-cam*) 
                               (vec4+ (blt3d-gfx:cam-pos blt3d-gfx:*main-cam*) 
                                      (vec-scale4 (blt3d-gfx:cam-dir blt3d-gfx:*main-cam*) step-amt)) )
                         )
                    
                    (blt3d-gfx:render-frame)
                    
                     #+disabled
                     (let ((x (* 2 (abs (xbox360_get_lx 0))))
                           (y (* 2 (abs (xbox360_get_ly 0))))) 
                       (xbox360-vibrate 0 x y)))
                   
                   ;; Rotate the camera around the target each frame
                   #+disabled
                   (camera-orbit! cam (/ pi 100) 0.0 5.0)

                   #+blt-debug
                   (let ((connection (or swank::*emacs-connection*
                                         (swank::default-connection))))
                     (when (and connection
                                (not (eql swank:*communication-style* :spawn)))
                       (swank::handle-requests connection t)))

                                        ;(net-game-update input-queue #'main-process-event
                                        ;                 #'main-loop-abort-handler)
                                        ;(game-update *game*)
                   )))))
    ;#-clozure ;; FIXME: This causes a crash on Clozure builds on Windows.
    ;(unload-graphics)
    ;(unload-mixer)
    )

  ;; Finalization:
  ;(net-exit)
  #+windows (xbox360-vibrate 0 0 0)
  )
  (when exit-when-done
    (exit)))
