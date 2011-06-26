;;; This file was automatically generated by SWIG (http://www.swig.org).
;;; Version 2.0.3
;;;
;;; Do not make changes to this file unless you know what you are doing--modify
;;; the SWIG interface file instead.

(in-package :blackthorn3d-input)

;;;SWIG wrapper code starts here

(cl:defmacro defanonenum (&body enums)
   "Converts anonymous enums to defconstants."
  `(cl:progn ,@(cl:loop for value in enums
                        for index = 0 then (cl:1+ index)
                        when (cl:listp value) do (cl:setf index (cl:second value)
                                                          value (cl:first value))
                        collect `(cl:defconstant ,value ,index))))

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:unless (cl:fboundp 'swig-lispify)
    (cl:defun swig-lispify (name flag cl:&optional (package cl:*package*))
      (cl:labels ((helper (lst last rest cl:&aux (c (cl:car lst)))
                    (cl:cond
                      ((cl:null lst)
                       rest)
                      ((cl:upper-case-p c)
                       (helper (cl:cdr lst) 'upper
                               (cl:case last
                                 ((lower digit) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:lower-case-p c)
                       (helper (cl:cdr lst) 'lower (cl:cons (cl:char-upcase c) rest)))
                      ((cl:digit-char-p c)
                       (helper (cl:cdr lst) 'digit
                               (cl:case last
                                 ((upper lower) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:char-equal c #\_)
                       (helper (cl:cdr lst) '_ (cl:cons #\- rest)))
                      (cl:t
                       (cl:error "Invalid character: ~A" c)))))
        (cl:let ((fix (cl:case flag
                        ((constant enumvalue) "+")
                        (variable "*")
                        (cl:t ""))))
          (cl:intern
           (cl:concatenate
            'cl:string
            fix
            (cl:nreverse (helper (cl:concatenate 'cl:list name) cl:nil cl:nil))
            fix)
           package))))))

;;;SWIG wrapper code ends here


(cffi:defcfun ("xbox360_vibrate" xbox360_vibrate) :void
  (controller :int)
  (speed_left :unsigned-short)
  (speed_right :unsigned-short))

(cffi:defcfun ("xbox360_poll" xbox360_poll) :void
  "Call this once per step of the game, before accessing input."
  (controller :int))

(cffi:defcfun ("xbox360_get_a" xbox360_get_a) :int
  "Returns whether or not the A button is pressed."
  (controller :int))

(cffi:defcfun ("xbox360_get_b" xbox360_get_b) :int
  "Returns whether or not the B button is pressed."
  (controller :int))

(cffi:defcfun ("xbox360_get_x" xbox360_get_x) :int
  "Returns whether or not the X button is pressed."
  (controller :int))

(cffi:defcfun ("xbox360_get_y" xbox360_get_y) :int
  "Returns whether or not the Y button is pressed."
  (controller :int))

(cffi:defcfun ("xbox360_get_lx" xbox360_get_lx) :short
  "Returns left thumb's x-axis position."
  (controller :int))

(cffi:defcfun ("xbox360_get_ly" xbox360_get_ly) :short
  "Returns left thumb's y-axis position."
  (controller :int))

(cffi:defcfun ("xbox360_get_rx" xbox360_get_rx) :short
  "Returns right thumb's x-axis position."
  (controller :int))

(cffi:defcfun ("xbox360_get_ry" xbox360_get_ry) :short
  "Returns right thumb's y-axis position."
  (controller :int))

(defun xbox360-vibrate (controller left-motor right-motor)
    "Makes the xbox360 controller vibrate. Provide values in range [0,65535]."
        (setf left-motor  (min left-motor 65535))   ; xbox360_vibrate takes shorts
        (setf right-motor (min right-motor 65535))
        (xbox360_vibrate controller left-motor right-motor))


(cffi:defcfun ("xbox360_get_ltrig" xbox360_get_ltrig) :int
  "Returns the left trigger position (range 0 - 255)"
  (controller :int))

(cffi:defcfun ("xbox360_get_rtrig" xbox360_get_rtrig) :int
  "Returns the right trigger position (range 0 - 255)"
  (controller :int))


(cffi:defcfun ("xbox360_get_lbump" xbox360_get_lbump) :int
  "Returns the left shoulder button"
  (controller :int))

(cffi:defcfun ("xbox360_get_rbump" xbox360_get_rbump) :int
  "Returns the right shoulder button"
  (controller :int))
