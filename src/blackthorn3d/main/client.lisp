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

(defun use-model-on (model-symbol entity)
    (setf (shape entity) (get-model model-symbol)))

(defvar *should-quit* nil)

(defun handle-entity-create-client (src entities)
  (iter (for entity in entities)
        (use-model-on (shape-name entity) entity)
        #+disabled (setf (blt3d-ani:state 
                          (blt3d-rend:controller (shape entity))) :loop)))

(defun handle-entity-update-client (src entities)
  (iter (for entity in entities)
        (when (eql (blackthorn3d-entity::die-now entity) :yes)
          (format t  "Killed by monster!~%")
          (setf *should-quit* t))
        ;; this really shouldn't be done every step
        (use-model-on (shape-name entity) entity)))

(defun handle-entity-remove-client (src entity)
  ;; TODO: Do it.
  )

(defun handle-camera-client (src camera-event)
  (let ((camera (camera-event-camera camera-event)))
    (blt3d-rend:set-camera camera)))

(defun handle-message-client (src message)
  (ecase (message-type message)
    (:string
     (let ((str (read-string message)))
       (format t "Msg from ~a was ~a~%" src str)))
    ;; TODO: Register these handlers globally.
    (:event-entity-create
     (apply-message-handler #'handle-entity-create-client src message))
    (:event-entity-update
     (apply-message-handler #'handle-entity-update-client src message))
    (:event-entity-remove
     (apply-message-handler #'handle-entity-remove-client src message))
    (:force-disconnect
      (setf *should-quit* t))
    (:event-camera
     (apply-message-handler #'handle-camera-client src message))))

(defun finalize-client ()
  (socket-disconnect-all)
  (blt3d-snd:exit))

(defmacro with-finalize-client (() &body body)
  `(unwind-protect
        (progn ,@body)
     (finalize-client)))

(defvar *message-counter* 0)

(defvar *level* nil)

(defun client-main (host port)
  (setup-paths)
  (load-dlls)
  (blt3d-rend:init)

  (register-model-loader :dae #'(lambda (path) 
    (blt3d-gfx:load-obj->models (blt3d-imp:load-dae path))))
    
  (load-models-n-stuff)
  
  (setf *level* (blt3d-gfx:load-obj->models 
                 (blt3d-imp:load-dae #p"res/models/test-room.dae")))
  #+disabled
  (blt3d-phy:expand-bounding-spheres *level*)
  #+disabled
  (blt3d-phy:apply-transform 
   *level* 
   (make-inv-ortho-basis (make-point3 1.0 0.0 0.0)
                         (make-point3 0.0 0.0 1.0)
                         (make-point3 0.0 1.0 0.0)))
  #+disabled
  (blt3d-phy:apply-transform *level* (make-scale #(0.05 0.05 0.05)))

  (setf *random-state* (make-random-state t))

  (unless (socket-client-connect host port :timeout 1.0)
    (format t "Error: Failed to connect.~%")
    (return-from client-main))

  (setf mt19937:*random-state* (mt19937:make-random-state t))

  (with-finalize-client ()
    (sdl:with-init ()
      (sdl:window 800 600 :bpp 32 :flags sdl:sdl-opengl
                  :title-caption "Test" :icon-caption "Test")
      (blt3d-snd:init)
      (blt3d-rend:prepare-scene)

      (let ((music (blt3d-snd:load-sound :music #p"res/sound/music.mp3")))
        (when music
          (blt3d-snd:play-sound music :loop t)))

      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event (:key k :mod m :mod-key m-k :unicode u)
          (when (sdl:key= k :sdl-key-return)
            (if (eql (input-kind *input*) :keyboard)
                (set-controller *input* :xbox)
                (set-controller *input* :keyboard)))

          (when (sdl:key= k :sdl-key-escape)
            (return-from client-main)))
        (:key-up-event (:key k :mod m :mod-key m-k :unicode u))
        (:idle ()
          ;; this whole block runs once per frame

          ;; move camera based on keyboard/xbox controller
          #+windows
         (xbox360_poll 0)

         (let ((mx (float (input-move-x *input*)))
               (my (float (input-move-y *input*)))
               (vx (float (input-view-x *input*)))
               (vy (float (input-view-y *input*))))
           (message-send :server (make-event :input 
                                             :move-x (* 0.1 mx) 
                                             :move-y (* 0.1 my)
                                             :view-x (* 0.1 vx)
                                             :view-y (* 0.1 vy))))


         (blt3d-rend:update-graphics (list-entities) 1/60)

         (blt3d-rend:render-frame (list-entities) *level*)

         (iter (for (src message) in (message-receive-all :timeout 0))
               (handle-message-client src message))

         (when *should-quit*
            (return-from client-main))
            
         #+disabled
         (send-string
          :server
          (format nil "Msg #~a (rand: ~a)" *message-counter* (random 10)))
         #+disabled
         (incf *message-counter*))))))
