(in-package :common-lisp-user)

(defpackage :tests
  (:use :common-lisp 
		:ccl
		:uhppoted)
  (:export get-devices
		       get-device
		       set-address
		       get-status
           get-time
           set-time
           get-listener
           set-listener
           get-door-control
           set-door-control
           get-cards
           get-card
           get-card-by-index
           put-card
           delete-card
           failed
           message))

