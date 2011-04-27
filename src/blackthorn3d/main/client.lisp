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

(defvar *my-client-buffer*)

(defun handle-message-client (src b size)
  (let ((msg (read-string b)))
    (format t "Msg from ~a was ~a~%" src msg)))
        
(defvar counter 0)
        
(defun client-main ()
    (setup-paths)
    (load-dlls)
    (blt3d-gfx:init)
    
    (setf *my-client-buffer* (userial:make-buffer))
    (setf *random-state* (make-random-state t))
        
    ; TODO: Don't hard code connection information
    (handler-case 
        (socket-client-connect "127.0.0.1" 9001 :timeout 1.0)
      (usocket:connection-refused-error ()
        ; TODO: handle this more gracefully.
        (format t "Error: Connection refused.~%")
        (return-from client-main)))
        
    (setf mt19937:*random-state* (mt19937:make-random-state t))
        
    (unwind-protect
    (catch 'main-init
      (sdl:with-init ()
        (sdl:window 800 600 :bpp 32 :flags sdl:sdl-opengl
                  :title-caption "Test" :icon-caption "Test")
        (blt3d-gfx:prepare-scene)
        
        (let ((input-queue (make-instance 'containers:basic-queue)))
        (catch 'main-loop
          (sdl:with-events ()
            (:quit-event () t)
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
                ; this whole block is supposed to be run once per frame
              (progn
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
                
                (socket-receive-all *my-client-buffer* #'handle-message-client 
                    :timeout 0)
                (send-string :server (format nil "Msg #~a (rand: ~a)" counter (random 10)))
                (incf counter)
                    
            ))))))))
)
