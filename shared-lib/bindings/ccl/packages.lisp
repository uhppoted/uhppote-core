(in-package :common-lisp-user)

(defpackage :uhppoted 
  (:use :common-lisp
		    :ccl)
  (:export uhppoted
           uhppoted-get-devices
           uhppoted-get-device
           uhppoted-set-address
           uhppoted-get-status
           uhppoted-get-time
           uhppoted-set-time
           uhppoted-get-listener
           uhppoted-set-listener
           uhppoted-get-door-control
           uhppoted-set-door-control
           uhppoted-get-cards
           uhppoted-get-card
           uhppoted-error
           with-warning
           message

           device-id
           device-address
           device-subnet
           device-gateway
           device-MAC
           device-version
           device-date
       
           status-id
           status-timestamp
           status-doors
           status-buttons
           status-relays
           status-inputs
           status-syserror
           status-info
           status-seqno
           status-event

           event-timestamp
           event-index
           event-type
           event-granted
           event-door
           event-direction
           event-card
           event-reason

           door-control-mode
           door-control-delay

           card-card-number
           card-from
           card-to
           card-doors
           ))

