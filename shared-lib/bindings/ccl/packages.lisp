(in-package :common-lisp-user)

(defpackage :uhppoted 
   (:use :common-lisp
         :ccl)
   (:export uhppoted
            uhppoted-get-devices
            uhppoted-get-device
            uhppoted-set-address
            uhppoted-get-status
            uhppoted-error
            with-warning
            message))

