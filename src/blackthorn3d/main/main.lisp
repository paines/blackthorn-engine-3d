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

; TODO: Move these to the proper package
(defun deg->rad (x)
    (* x (/ pi 180)))
    
(defclass input-system ()
    ((kind 
        :accessor input-kind
        :initarg :kind
        :documentation "Either :keyboard or :xbox")))
        
(defmethod input-move-x ((system input-system))
    (with-slots (kind) system
        (case kind
            (:keyboard 
              (+ (if (sdl:get-key-state :sdl-key-right) 1.0  0)
                 (if (sdl:get-key-state :sdl-key-left) -1.0  0)))
            #+windows
            (:xbox (/ (xbox360_get_lx 0) 65535))
            (otherwise 0))))

(defmethod input-move-y ((system input-system))
    (with-slots (kind) system
        (case kind
            (:keyboard
               (+ (if (sdl:get-key-state :sdl-key-up) 1.0 0)
                  (if (sdl:get-key-state :sdl-key-down) -1.0 0)))
            #+windows
            (:xbox (/ (xbox360_get_ly 0) 65535))
            (otherwise 0))))
        
(defparameter *input* (make-instance 'input-system :kind :keyboard))

(defmethod set-controller ((system input-system) type)
    (setf (input-kind system) type))

; end todo
    
(defun main-init-abort-handler ()
  (throw 'main-init nil))

(defun main-loop-abort-handler ()
  (throw 'main-loop nil))

(defun main-process-event (event)
  (send *game* event))

(defun main (&key (exit-when-done t))
  "Main entry point for the game. Deals with initialization, finalization, and the main game loop."
  ;; Initialization:
  (setup-paths)
  (load-dlls)

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
      (gl:viewport 0 0 800 600)

      (gl:enable :texture-2d)
      (gl:enable :blend)
      (gl:blend-func :src-alpha :one-minus-src-alpha)
      (gl:clear-color 0 0 0 0)
      (gl:enable :depth-test)
      (gl:depth-func :lequal)
      (gl:matrix-mode :projection)
      (gl:load-identity)
      (let ((fx 1.0) (fy (* 1.0 6/8)))
        (gl:frustum (- fx) fx (- fy) fy 1.0 100.0))
      (gl:matrix-mode :modelview)
      (gl:load-identity)
      (gl:light :light0 :position '(3.0 3.0 0.0 1.0))
      (gl:light :light0 :diffuse (make-vec3 1.0 1.0 1.0))
      ;(gl:enable :lighting)
      (gl:enable :light0)

      (defparameter cube (make-cube))
      (defparameter turn (make-y-rot (/ pi 100)))
      
      ;; Main loop:
      (let ((input-queue (make-instance 'containers:basic-queue))
            (cam (make-instance 'camera 
                                :position (make-point3 0.0 0.0 5.0)
                                :direction (norm4 (vec4- (make-vec3 0.0 0.0 0.0)
                                                         (make-vec3 0.0 0.0 5.0)))
                                :mode :third-person ))
            (cam-quat (axis-rad->quat (make-vec3 0.0 1.0 0.0) (/ pi 100))))
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
                                                     
                     ; (/ (xbox360_get_lx 0) 65535)
                                                     
                     ;(setf (cam-dir cam) (quat-rotate-vec cam-quat (cam-dir cam)))
                     (let ((rot-amt  (* -1 (input-move-x *input*)))
                           (step-amt (*  1 (input-move-y *input*))))
                         
                         (setf (cam-dir cam) (quat-rotate-vec
                            (axis-rad->quat (make-vec3 0.0 1.0 0.0) (deg->rad (* 2.7 rot-amt)))
                            (cam-dir cam)))
                         (setf (cam-pos cam) (vec4+ (cam-pos cam) (vec-scale4 (cam-dir cam) step-amt)) )
                         )
                    
                    
                    
                     #+disabled
                     (let ((x (* 2 (abs (xbox360_get_lx 0))))
                           (y (* 2 (abs (xbox360_get_ly 0))))) 
                       (xbox360-vibrate 0 x y)))
                   
                   ;; Rotate the camera around the target each frame
                   #+disabled
                   (camera-orbit! cam (/ pi 100) 0.0 5.0)

                   (gl:clear :color-buffer-bit :depth-buffer-bit)
                   (gl:load-matrix (camera-inverse cam))
                                        ;(gl:translate 1.0 0.0 -1.0)
                   (gl:color 1.0 .75 0.0)
                   (apply #'draw-vert-array cube)
                          
                                        ;(render *game* #c(0 0) 1d0 -1d0)
                   (gl:flush)
                   (sdl:update-display)

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
