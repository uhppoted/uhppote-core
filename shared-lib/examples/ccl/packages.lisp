(in-package :common-lisp-user)

(defpackage :examples
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
           open-door
           get-cards
           get-card
           get-card-by-index
           put-card
           delete-card
           delete-cards
           get-event-index
           set-event-index
           get-event
           record-special-events
           get-time-profile
           set-time-profile))

