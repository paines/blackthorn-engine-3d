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


(defmethod update ((p player))
  (with-slots (client) p
    (when (> (s-input-jump client) 0)
      (format t "JUMP!~%")
      (jump p))))

(defun is-alive-p (thing)
  (oid-in-use-p (oid thing)))


(defmethod update ((c camera))
  (let* ((player (target c))
         (client (player-client player)))

    ;; camera should remove itself after the player disconnects
    (when (not (is-alive-p player))
      (kill-entity c)
      (return-from update))

    (let* ((input-vec (vector (s-input-move-x client) (s-input-move-y client)))
           (move-vec (vec-scale4 (move-player c input-vec) 0.4))
           (target (blt3d-phy::target c))
           (c-sector (lookup-sector (current-sector c)))
           (t-sector (lookup-sector (current-sector target))))

      ;(setf (velocity target) move-vec)

      (setf blt3d-phy::*hackity-hack__lookup-sector* #'lookup-sector)
      (setf blt3d-phy::*hackity-hack__collide-sector* #'collide-sector)
      (standard-physics-step target)

      ;; And we do the sector check here:
      (collide-sector-portals target t-sector)

      (when (and (eql (minor-mode c) :free)
                 (or (/= 0.0 (x input-vec)) (/= 0.0 (y input-vec))))
        (setf (dir target) (norm4 move-vec)))


      ;; Ray-test: Lets have the main character shoot out a ray
      ;; every step and see what it hits!
      #+disabled
      (aif (ray-cast (make-ray (pos target) (to-vec4 (dir target)))
                     t-sector)
           (format t "Ray hit something! dist: ~a~%" it))




      (update-camera c (/ 1.0 120.0) (vector (s-input-view-x client)
                                             (s-input-view-y client)))

      ;; Note: I use t-sector to avoid awkward behavior when the target
      ;; moves around a wall but the camera is in a different sector
      ;; not that this can't cause problems either..
      (let ((movement-vec (collide-sector 
                           c t-sector 1)))
        (blt3d-phy::move-camera c movement-vec))

      ;; do sector check for camera
      (collide-sector-portals c c-sector))))
      



(defun next-frame ()
  ;; (sleep) call moved to with-timer-loop
  )

(defvar *client-count* 0)

(defun check-for-clients ()
  (let ((client (socket-server-connect :timeout 0)))
    (when client
      (incf *client-count*)
      (format t "Client ~a joined! (Total: ~a)~%" client *client-count*))
    client))

(defun read-string (msg)
  (message-value msg))

(defun send-string (dst str)
  (message-send dst (make-message :string str)))

(defun send-all-entities (destination)
  (message-send destination (make-event :entity-create :include-all t)))

(defun handle-message-server (src message)
  (ecase (message-type message)
    (:string
     (let ((str (read-string message)))
       (format t "The message from ~a was ~a~%" src str)
       (send-string src (concatenate 'string "ACK: " str))))
    ;; TODO: Register these handlers globally.
    (:event-input
     (apply-message-handler #'s-input-update src message))))

(defvar *delay-disconnected-clients* nil)
       
(defun handle-disconnect (client)
  (remove-server-controller client)
  (push client *delay-disconnected-clients*)
  (decf *client-count*)
  (format t "Client ~a disconnected. (Total: ~a)~%" client *client-count*))
  
(defun new-camera (player-entity)
    (make-server-entity
        'camera
        :pos (make-point3 0.0 0.0 0.0)
        :dir (make-vec3 1.0 0.0 0.0)
        :up  (make-vec3 0.0 1.0 0.0)
        :ideal-coord (list 0.0 (/ pi 6) 4.0)
        :target player-entity
        :shape-name :cylinder
        :bv (make-instance 'bounding-sphere
                                        :rad 0.07
                                        :pos +origin+)
        :mode :third-person))

(defun finalize-server ()
  (socket-disconnect-all))

(defmacro with-finalize-server (() &body body)
  `(unwind-protect
        (progn ,@body)
     (finalize-server)))

(defun time-left-in-frame (fps last)
  "Returns the amount of time that remains in a frame with a given frame rate
   and the last time."
  (let* ((now (get-real-time))
         (elapsed (- now last))
         (target (max 0 (- (/ 1.0 fps) elapsed))))
    target))

(defmacro with-timer-loop ((fps) &body body)
  "Loops infinitely with a given frame rate. The parameter fps should be a
   special variable so that the user can rebind it at runtime to change
   the speed of the loop."
  (with-gensyms (last delta)
    `(iter
      (declare (special ,fps))
      (let ((,last (get-real-time)))
        (progn ,@body)
        (let ((,delta (time-left-in-frame ,fps ,last)))
          (sleep ,delta))))))

(defun check-for-new-clients ()
  (forget-server-entity-changes)
  (let ((new-client (check-for-clients)))
    (when new-client
      (new-server-controller new-client)
      (send-all-entities new-client)
      (let* ((the-new-player (new-player new-client))
             (camera (new-camera the-new-player)))
        
        (add-to-sector the-new-player :start-sector)
        (add-to-sector camera :start-sector)
        (push (make-camera-relative-player-mover new-client camera) 
              (displacers the-new-player))
        
        (message-send :broadcast (make-event :entity-create))
        (send-camera new-client camera)
        #+disabled
        (send-sound new-client :soundtrack t))))
  (forget-server-entity-changes))

(defun synchronize-clients ()
  (iter (for (src message) in (message-receive-all :timeout 0))
               (handle-message-server src message))
               
 (message-send :broadcast (make-event :entity-create))
 (message-send :broadcast (make-event :entity-update))
 (message-send :broadcast (make-event :entity-remove)))
     
(defun update-entities ()
  (iter (for thing in (list-entities))
        (update thing)))       
       
(defun combinations (input-list)
  (iter outer (for x on input-list) 
    (iter (for y in (rest x)) 
      (in outer (collect (list (first x) y))))))
      

;;;
;;; COLLISION STEP
;;;

(defmethod collide (a b)
  (declare (ignore a)
           (ignore b)))

(defmethod collide ((obj entity-server) (p portal))
  (when (crosses-portal-p obj p)
    ;; Update the sector of the entity
    (setf (current-sector obj) (sector-id (links-to-sector p)))))
      
(defun check-collisions ()
  (iter (for (e1 e2) in (combinations (list-entities)))
        (when (blackthorn3d-physics:collide-p e1 e2)
          (collide e1 e2))))





(defun remove-disconnected-clients ()
  (iter (for client in *delay-disconnected-clients*)
    (format t "Removing client: ~a~%" client)
    (remove-player client))
  (setf *delay-disconnected-clients* nil))

;; added by Robert
(defvar *level* nil)

(defvar *server-frame-rate*)
      
(defun server-main (host port)
  (declare (ignore host))
  
  (init-server)

  ;; Start the server, or print a message and quit if we can't use desired port
  (when (not (socket-server-start port))
    (format t "Unable to start the server~%")
    (return-from server-main))
  (socket-disconnect-callback #'handle-disconnect)
  (format t "Server running on port ~a.~%" port)

  
  
;  (setf *level* (load-level))
  ;(make-monster :start-sector (make-point3 20.0 0.0 0.0))

  (setf *server-frame-rate* 120)
  
  (with-finalize-server ()
    (with-timer-loop (*server-frame-rate*)
      (next-frame)
      (check-for-new-clients)
      (remove-disconnected-clients)
      (update-sectors)
      (check-collisions)
      (synchronize-clients))))
