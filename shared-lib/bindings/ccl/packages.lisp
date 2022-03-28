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
           event-reason))

