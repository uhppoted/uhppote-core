(in-package :common-lisp-user)

(defpackage :examples
  (:use :common-lisp 
		:ccl
		:uhppoted)
  (:export get-devices
		   get-device
		   set-address
		   get-status
           get-time))

