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
  
  ;; monsters & stuff like that
  ;#+disabled
  ;(load-model :fire-beast :dae #p "res/models/FireBeastAnimated.dae")
  ;(load-model :ghost :dae #p "res/models/FireBeastAnimated.dae")
  (load-model :ghost :dae #p "res/models/characters/GhostFinal.dae")
  (blt3d-phy:apply-transform (get-model :ghost) 
                             (make-scale #(0.15 0.15 0.15)))
 
  (load-model :human-gun :dae #p "res/models/characters/ShooterFinal.dae")
  (load-model :gun :dae #p "res/models/props/GunFinal.dae")
  (blt3d-phy:apply-transform (get-model :human-gun) 
                             (make-scale #(0.01 0.01 0.01)))
  (blt3d-phy:apply-transform (get-model :gun)
                             (make-translate (make-point3 -4.0 -10.0 20.0)))
  (blt3d-phy:apply-transform (get-model :gun)
                             (make-x-rot (- (/ pi 6))))
  (blt3d-phy:apply-transform (get-model :gun)
                             (make-inv-ortho-basis
                              (make-point3 -1.0 0.0 0.0)
                              (make-point3 0.0 0.0 -1.0)
                              (make-point3 0.0 1.0 0.0)))
  (blt3d-phy:attach-node-to-model (car (blt3d-phy:mesh-nodes (get-model :gun)))
                                  "Bip001_R_Hand" (get-model :human-gun))
  
  ;; maps
  (format t "##### Loading Maps~%")
  (load-model :dead-end-room :level #p "res/models/DeadEndRoom.dae")
 ; (load-model :hallway-straight :level #p "res/models/Hallway1a.dae")
 ; (load-model :slide-room :level #p "res/models/WallRoomSlide.dae")
 ; (load-model :cylinder-room :level #p "res/models/CylinderRoom1.dae")
  (load-model :maze-room :level #p "res/models/MazeRoom.dae")
  (load-model :hallway-five :level #p "res/models/HallwayFiveway.dae")
  (load-model :battle-room :level #p "res/models/BattleRoom1.dae")
  (load-model :rotating-ring-room :level
              #p "res/models/maps/RotatingRingRoom.dae")



;;;
;;; Level layout
;;;

  (make-start-sector (get-model :dead-end-room))

 ; #+disabled
  (progn
    (add-sector-relative
     :start-sector
     :south
     (make-sector :ring-01 (get-model :rotating-ring-room)))
    
    (add-sector-relative
     :ring-01
     :south
     (make-sector :maze-room (get-model :maze-room)))
    
    ;; Now link them all
    (link-sectors :start-sector :ring-01)
    (link-sectors :ring-01 :maze-room)
   ; (link-sectors :hall-01 :battle-room)

    #+disabled
    (progn
      (link-sectors :hall-01 :end-room)
      (link-sectors :hall-01 :end-room2)
      (link-sectors :hall-01 :end-room3))
    ))