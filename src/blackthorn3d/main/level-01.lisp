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

;;;
;;; Level 1.  For the demo.  This file contains the information
;;;           that is loaded by the server and client to add all
;;;           all the assets to the game.  this includes the level
;;;           layout
;;;

(defun load-level ()
;;;
;;; Load models here
;;;
  
  ;; maps
  (format t "##### Loading Maps~%")
  (load-model :dead-end-room :level #p "res/models/DeadEndRoom.dae")
  (load-model :hallway-straight :level #p "res/models/Hallway1a.dae")
  (load-model :slide-room :level #p "res/models/WallRoomSlide.dae")


;;;
;;; Level layout
;;;

  (make-start-sector (get-model :slide-room))

  (progn
    (add-sector-relative
     :start-sector
     :north
     (make-sector :hall-01 (get-model :hallway-straight)))
    
    (add-sector-relative
     :hall-01
     :north
     (make-sector :end-room (get-model :dead-end-room)
                  (axis-rad->quat +y-axis+ pi)))

    ;; Now link them all
    (link-sectors :start-sector :hall-01)
    (link-sectors :hall-01 :end-room)
    ))