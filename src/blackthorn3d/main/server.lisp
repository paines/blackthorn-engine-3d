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


(defmethod update :before ((self entity-server))
  ;; Perform quaternion interpolation 
  (with-slots (up dir new-up) self
    (when new-up
     ;; makeh the quat
      (when (< (+ 1.0 (dot up new-up) 0.0001))
            (setf (up self) (vec4+ up (vec-scale4 dir 0.001))))

      (let* ((up-quat (quat-rotate-to-vec up new-up))
             (rot-quat (quat-slerp +quat-identity+ up-quat 
                                   (if (is-jumping self) 5/120 10/120))))
        (setf (up self) 
              (norm4 (quat-rotate-vec
                      rot-quat up))
              (dir self)
              (norm4 (cross up (cross dir up))))))))


(defvar last-laser 0.0)
(defvar laser-delay 0.3)

(defun hit-thing-with-laser (ray excluding)
  (let ((min-dist MOST-POSITIVE-SINGLE-FLOAT)
        (thing nil)
        (stuff-to-check (set-difference (list-entities) excluding)))
    (iter (for entity in stuff-to-check)
          (let ((this-dist (ray-cast ray entity)))
            (format t "entity result ~a~%" this-dist)
            (when (and this-dist (< this-dist min-dist))
              (setf min-dist this-dist)
              (setf thing entity))))
    thing))

(defun run-into-something (me pos dir sector)
  (let* ((ray (make-ray pos dir))
         (sector-distance (ray-cast ray sector))
         (distances (mapcar #'(lambda (e) (ray-cast ray e)) (list-entities))))
    (aif (hit-thing-with-laser ray (list me))
         (quickhit it))
    (aif (min-t (cons sector-distance distances))
         it
         0.0)))


(defmethod update ((p player))
  (incf last-laser 1/120)
  (with-slots (client pos up dir) p
    (when (and (> (s-input-jump client) 0)
               (not (is-jumping p)))
      (setf (is-jumping p) t)
      
      (if (eql (minor-mode (attached-cam p)) :free)
          (setf (velocity p) (vec-scale4 up .1))
          (setf (velocity p) (vec-scale4 (dir (attached-cam p)) .1)))
      ;;(setf (new-up p) (vec-neg4 (velocity p)))
      )
      
    (when (and (> last-laser laser-delay)
               (> (s-input-attack client) 0))
      (setf last-laser 0.0)
      (let* ((here (lookup-sector (current-sector p)))
	     (my-camera (attached-cam p))
	     (camera-dir (to-vec4 (dir my-camera)))
             (start-pos (vec4+ (vec4+ pos (vec-scale4 up 0.3)) dir))
             (ray (norm4 camera-dir))
             ;(distance (run-into-something p (vec4+ pos up) dir here)))
	     (distance (run-into-something p start-pos ray here)))
	     ;(distance 5.0))
        (send-play-laser
         :broadcast :human
         start-pos
         (vec-scale4 ray distance))
        (blt3d-snd:send-sound :broadcast :laser nil 128)
        )
      )

                                        ;#+disabled
    (when (> (s-input-xbox-y client) 0)
      (quickhit p))
    
    (try-die p)
    
    ))

(defun is-alive-p (thing)
  (oid-in-use-p (oid thing)))


(defmethod update ((c camera))
  (let* ((player (target c))
         (client (player-client player)))

    ;; camera should remove itself after the player disconnects
    (when (not (is-alive-p player))
      (kill-entity c)
      (return-from update))
    
    (setf (minor-mode c)
      (if (and (not (is-jumping player)) (> (s-input-camera-mode client) 0))
        :strafe
        :free))
    

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
           (format t "Ray hit sector!: ~a~%" it))




      (update-camera c (/ 1.0 120.0) (vector (s-input-view-x client)
                                             (s-input-view-y client)))

      ;; Note: I use t-sector to avoid awkward behavior when the target
      ;; moves around a wall but the camera is in a different sector
      ;; not that this can't cause problems either..
      (let ((movement-vec (car (collide-sector 
                                c (velocity c) t-sector 1))))
        (blt3d-phy::move-camera c movement-vec))

      ;; do sector check for camera
      (collide-sector-portals c c-sector))))
      



(defun next-frame ()
  ;; (sleep) call moved to with-timer-loop
  (socket-flush-all)
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

;; FIXME: Massive hack
(defvar *client-sound-state* (make-hash-table))
(defvar *client-sound-score*
  '((0.1 :boot nil 128)
    (18 :music-intro nil 30)
    (207 :intro nil 128)
    (213 :music-exploration t 60)))
(defun sound-add-client (client)
  (setf (gethash client *client-sound-state*) (list (get-real-time) 0)))
(defun sound-rem-client (client)
  (remhash client *client-sound-state*))
(defun sound-update-client (client start-and-old-time)
  (let* ((start-time (first start-and-old-time))
         (old-time (second start-and-old-time))
         (new-time (get-real-time))
         (old-delta (- old-time start-time))
         (new-delta (- new-time start-time))
         (sound-args
          (iter (for (trigger-time sound loop volume) in *client-sound-score*)
                (finding (list sound loop volume)
                         such-that (and (< old-delta trigger-time)
                                        (< trigger-time new-delta))))))
    (when sound-args
      (apply #'blt3d-snd:send-sound client sound-args))
    (list start-time new-time)))
(defun sound-update-all-clients ()
  (iter (for (client value) in
             (iter (for (client old-time) in-hashtable *client-sound-state*)
                   (collect
                    (list client (sound-update-client client old-time)))))
        (setf (gethash client *client-sound-state*) value)))

(defun handle-disconnect (client)
  (remove-server-controller client)
  (push client *delay-disconnected-clients*)
  (decf *client-count*)
  (sound-rem-client client)
  (format t "Client ~a disconnected. (Total: ~a)~%" client *client-count*))
  
(defun new-camera (player-entity)
    (make-server-entity
        'camera
        :pos (make-point3 0.0 0.0 0.0)
        :dir (make-vec3 1.0 0.0 0.0)
        :up  (make-vec3 0.0 1.0 0.0)
        :ideal-coord (list 0.0 (/ pi 6) 4.0)
        :target player-entity
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
        
        (setf (attached-cam the-new-player) camera)
        
        (add-to-sector the-new-player :start-sector)
        (add-to-sector camera :start-sector)
        (push (make-camera-relative-player-mover new-client camera) 
              (displacers the-new-player))
        
        (message-send :broadcast (make-event :entity-create))
        (send-camera new-client camera)
        (sound-add-client new-client))))
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
      

(defvar *octree*)

(defun construct-octree ()
  (let* ((min-max (blt3d-sec::find-min-max-sector))
	 (center (vec-scale4 (vec4+ (aref min-max 0) (aref min-max 1)) 0.5))
	 (width (/ (- (aref (aref min-max 1) 0) 
				   (aref (aref min-max 0) 0)) 0.5))
	 (depth 5)) ; depth 8 broke it
    (make-octree center width depth)))

(defun make-hash-key (e1 e2)
  (if (< (oid e1) (oid e2))
    (return-from make-hash-key (list e1 e2))
    (return-from make-hash-key (list e2 e1))))

(defun check-collisions-octree ()
  (setf *octree* (construct-octree))
  (iter (for e1 in (list-entities))
    (when (bounding-volume e1)
      (octree-insert *octree* e1)))
  (setf collision-hash (make-hash-table))
  (iter (for each-entity in (list-entities))
    (when (bounding-volume each-entity)
      (let ((potential (coerce (octree-query *octree* each-entity) 'list)))
	(iter (for (e1 e2) in (combinations potential))
	  (when (not (gethash (make-hash-key e1 e2) collision-hash))
	    ; test for collision, otherwise already done
	    (collide e1 e2)))))))

;#+disabled ;old check
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
      (update-entities)
      (sound-update-all-clients)
      ;(check-collisions-octree)
      (check-collisions)
      (synchronize-clients))))
