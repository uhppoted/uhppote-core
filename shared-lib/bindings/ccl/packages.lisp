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
           uhppoted-open-door
           uhppoted-get-cards
           uhppoted-get-card
           uhppoted-get-card-by-index
           uhppoted-put-card
           uhppoted-delete-card
           uhppoted-delete-cards
           uhppoted-get-event-index
           uhppoted-set-event-index
           uhppoted-get-event
           uhppoted-record-special-events
           uhppoted-get-time-profile
           uhppoted-set-time-profile
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

           make-time-profile
           time-profile-id
           time-profile-linked
           time-profile-from
           time-profile-to
           time-profile-monday
           time-profile-tuesday
           time-profile-wednesday
           time-profile-thursday
           time-profile-friday
           time-profile-saturday
           time-profile-sunday
           time-profile-segment1start
           time-profile-segment1end
           time-profile-segment2start
           time-profile-segment2end
           time-profile-segment3start
           time-profile-segment3end

           normally-open
           normally-closed
           controlled))

