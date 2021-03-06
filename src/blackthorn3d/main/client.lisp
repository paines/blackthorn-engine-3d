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
        (format t "entity's sector: ~a~%" (current-sector entity))
        (use-model-on (shape-name entity) entity)
        #+disabled (setf (blt3d-ani:state
                          (blt3d-rend:controller (shape entity))) :loop)))

(defun handle-entity-update-client (src entities)
  (iter (for entity in entities)
        (when (eql (blackthorn3d-entity::die-now entity) :yes)
          (format t  "Killed by monster!~%")
          (setf *should-quit* t))
        ;; this really shouldn't be done every step
        ;;(setf (current-sector entity) (lookup-sector (current-sector entity)))
        #+disabled
        (use-model-on (shape-name entity) entity)))

(defun handle-play-explosion (src name pos)
  (format t "Explosion ~a goes boom at ~a~%" name pos)
  (add-an-explosion pos))

(defun handle-play-laser (src name start-pos dir)
  (case name
    (:human (add-human-laser start-pos dir))
    (:ghost (add-ghost-laser start-pos dir))))

(defun handle-play-animation (src entity name mode)
  (format t "Animation ~a goes boom on ~a with mode ~a~%" name entity mode)
  (play-model-animation (shape entity) name mode))

(defun handle-entity-remove-client (src entity)
  ;; TODO: Do it.
  )

(defun handle-camera-client (src camera)
  (blt3d-rend:set-camera camera))

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
     (apply-message-handler #'handle-camera-client src message))
    (:event-sound
     (apply-message-handler #'blt3d-snd:handler-sound src message))
    (:play-explosion
     (apply-message-handler #'handle-play-explosion src message))
    (:play-laser
     (apply-message-handler #'handle-play-laser src message))
    (:play-animation
     (apply-message-handler #'handle-play-animation src message))))

(defun finalize-client ()
  (socket-disconnect-all)
  (blt3d-snd:exit))

(defmacro with-finalize-client (() &body body)
  `(unwind-protect
        (progn ,@body)
     (finalize-client)))

(defvar *message-counter* 0)

(defvar *level* nil)

(defvar *windowed-width* 960)
(defvar *windowed-height* 720)
(defvar *window-fullscreen-p* nil)

(defun window-flags (&key fullscreen)
  (logior sdl:sdl-opengl (if fullscreen sdl:sdl-fullscreen 0)))

(defun set-window (width height &key fullscreen reset-viewport)
  (sdl:window width height :bpp 32
              :flags (window-flags :fullscreen fullscreen)
              :title-caption "Test" :icon-caption "Test")
  (when reset-viewport
    (blt3d-rend:init)
    (load-models-n-stuff)
    (blt3d-rend:set-viewport-size width height)))

(defun largest-video-dimensions (&key fullscreen)
  (let ((modes (sdl:list-modes (window-flags :fullscreen fullscreen))))
    (if (eql modes t)
        (sdl:video-dimensions)
        (first modes))))

(defun toggle-fullscreen ()
  (setf *window-fullscreen-p* (not *window-fullscreen-p*))
  (let* ((fullscreen-size (largest-video-dimensions :fullscreen t))
         (width (if *window-fullscreen-p*
                    (sdl:x fullscreen-size)
                    *windowed-width*))
         (height (if *window-fullscreen-p*
                     (sdl:y fullscreen-size)
                     *windowed-height*)))
    (set-window width height
                :fullscreen *window-fullscreen-p*
                :reset-viewport t)))

(defun load-sounds ()
  (blt3d-snd:make-sound :music-intro
                        :music #p"res/sound/game_music_menu.mp3")
  (blt3d-snd:make-sound :music-exploration
                        :music #p"res/sound/game_music_exploration.mp3")
  (blt3d-snd:make-sound :music-combat
                        :music #p"res/sound/game_music_exploration.mp3")
  (blt3d-snd:make-sound :boot :sample #p"res/sound/dialogue_boot.ogg")
  (blt3d-snd:make-sound :intro :sample #p"res/sound/dialogue_intro.wav")
  (blt3d-snd:make-sound :win :sample #p"res/sound/dialogue_win.wav")
  (blt3d-snd:make-sound :lose :sample #p"res/sound/dialogue_lose.wav")
  (blt3d-snd:make-sound :pebkac :sample #p"res/sound/dialogue_pebkac.wav")
  (blt3d-snd:make-sound :bullet :sample #p"res/sound/sample_bullet.wav")
  (blt3d-snd:make-sound :explosion
                        :sample #p"res/sound/sample_explosion.wav")
  (blt3d-snd:make-sound :laser :sample #p"res/sound/sample_laser.wav"))

(defun client-main (host port)
  (set-connection-side :client)

  (setup-paths)
  (load-dlls)

  (setf *random-state* (make-random-state t))

  (unless (socket-client-connect host port :timeout 1.0)
    (format t "Error: Failed to connect.~%")
    (return-from client-main))

  (setf mt19937:*random-state* (mt19937:make-random-state t))

  (with-finalize-client ()
    (sdl:with-init ()
      (set-window *windowed-width* *windowed-height*
                  :fullscreen *window-fullscreen-p*)

      (blt3d-snd:init)
      (blt3d-rend:init)
      (init-client)

      (blt3d-rend:prepare-scene)

      (format t "####################~%")

      (load-sounds)

      (setf (sdl:frame-rate) 60)

      (sdl:with-events ()
        (:quit-event () t)
        (:key-down-event (:key k :mod m :mod-key m-k :unicode u)
          (when (sdl:key= k :sdl-key-return)
            (if (eql (input-kind *input*) :keyboard)
                (set-controller *input* :xbox)
                (set-controller *input* :keyboard)))

          (when (sdl:key= k :sdl-key-f11)
            (toggle-fullscreen))

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
               (vy (float (input-view-y *input*)))
               (jmp (float (input-jump *input*))))

           (send-input
            :server
            (* 0.1 mx)
            (* 0.1 my)
            (* 0.1 vx)
            (* 0.1 vy)
            jmp
            (input-camera-mode *input*)
            (input-attack *input*)
            (input-fly-up *input*)
            (input-fly-down *input*)

            (input-use *input*)
            (input-xbox-y *input*)
            (input-alt-attack *input*)))

         (blt3d-rend:update-graphics (list-entities) 1/60)

         (blt3d-rend:render-frame (list-entities))

         (iter (for (src message) in (message-receive-all :timeout 0))
               (handle-message-client src message))
         (socket-flush-all)

         (when *should-quit*
            (return-from client-main))

         #+disabled
         (send-string
          :server
          (format nil "Msg #~a (rand: ~a)" *message-counter* (random 10)))
         #+disabled
         (incf *message-counter*))))))
