(in-package :common-lisp-user)

(defpackage :uhppoted 
   (:use :common-lisp
         :ccl)
   (:export uhppoted
            uhppoted-error
            uhppoted-get-devices
            uhppoted-get-device
            uhppoted-set-address
            uhppoted-get-status))

(defpackage :examples
  (:use :common-lisp 
        :uhppoted)
  (:export debug
           get-devices
           get-device
           set-address
           get-status))

