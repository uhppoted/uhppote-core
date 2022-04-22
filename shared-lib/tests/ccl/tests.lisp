(in-package :tests)

(defconstant TEST-DEVICE-ID   405419896)
(defconstant TEST-CARD-NUMBER 8165538)
(defconstant TEST-CARD-INDEX  19)
(defconstant TEST-EVENT-INDEX 51)
(defconstant TEST-DOOR        4)
(defconstant TEST-PROFILE-ID  49)

(define-condition failed (error)
  ((message :initarg :message :reader message)))

(defun exec (f) "" 
  (handler-bind
  ((uhppoted-error
     #'(lambda (c) 
       (error 'failed :message  (uhppoted:message c)))))
  (uhppoted f 
        :bind-addr      "192.168.1.100"
        :broadcast-addr "192.168.1.100"
        :listen-addr    "192.168.1.100:60001"
        :timeout        2500
        :controllers    (list '(405419896 "192.168.1.100") '(303986753 "192.168.1.100"))
        :debug          T)))


(defun get-devices () "" 
  (let ((tag      "get-devices")
        (ok       T)
        (devices (exec #'(lambda (u) (uhppoted-get-devices u)))))
       (cond ((/= 3 (length devices))
                    (progn
                      (format t "get-devices: incorrect device count - expected:~a, got:~a~%" 3 (length devices))
                      (setf ok NIL)))
             ((not (equal (coerce devices 'list) '(201020304 303986753 405419896))) 
                    (progn
                      (format t "get-devices: incorrect device list - expected:~a, got:~a~%" '(201020304 303986753 405419896) devices)
                      (setf ok NIL))))
       (if ok 
          (passed tag)
          (failed tag))))


(defun get-device () "" 
  (let ((tag      "get-device")
        (ok       T)
        (device (exec #'(lambda (u) (uhppoted-get-device u TEST-DEVICE-ID)))))
       (if (/= 405419896 (device-id device)) 
           (progn
             (format t "get-device: incorrect device ID - expected:~a, got:~a~%" 405419896 (device-id device))
             (setf ok NIL)))

       (if (not (equal "192.168.1.101" (device-address device))) 
           (progn
             (format t "get-device: incorrect device address - expected:~a, got:~a~%" "192.168.1.101" (device-address device))
             (setf ok NIL)))

       (if (not (equal "255.255.255.0" (device-subnet device))) 
           (progn
             (format t "get-device: incorrect device subnet mask - expected:~a, got:~a~%" "255.255.255.0" (device-subnet device))
             (setf ok NIL)))

       (if (not (equal "192.168.1.1" (device-gateway device))) 
           (progn
             (format t "get-device: incorrect device gateway address - expected:~a, got:~a~%" "192.168.1.1" (device-gateway device))
             (setf ok NIL)))

       (if (not (equal "00:12:23:34:45:56" (device-MAC device))) 
           (progn
             (format t "get-device: incorrect device MAC address - expected:~a, got:~a~%" "00:12:23:34:45:56" (device-MAC device))
             (setf ok NIL)))

       (if (not (equal "v8.92" (device-version device))) 
           (progn
             (format t "get-device: incorrect device version - expected:~a, got:~a~%" "v8.92" (device-version device))
             (setf ok NIL)))

       (if (not (equal "2018-11-05" (device-date device))) 
           (progn
             (format t "get-device: incorrect device date - expected:~a, got:~a~%" "v8.92" (device-date device))
             (setf ok NIL)))
       (if ok 
          (passed tag )
          (failed tag))))


(defun set-address () "" 
  (let ((tag      "set-address")
        (ok       T))
       (exec #'(lambda (u) (uhppoted-set-address u TEST-DEVICE-ID "192.168.1.125" "255.255.254.0" "192.168.1.0")))
       (if ok 
          (passed tag )
          (failed tag))))


(defun get-status () "" 
  (let ((tag      "get-status")
        (ok       T)
        (status (exec #'(lambda (u) (uhppoted-get-status u TEST-DEVICE-ID)))))
       (if (/= 405419896 (status-id status)) 
           (progn
             (format t "get-status:  incorrect device ID - expected:~a, got:~a~%" 405419896 (status-id status))
             (setf ok NIL)))

       (if (not (equal "2022-03-19 15:48:32" (status-timestamp status))) 
           (progn
             (format t "get-status:  incorrect system date/time - expected:~a, got:~a~%" "2022-03-19 15:48:32" (status-timestamp status))
             (setf ok NIL)))

       (if (not (equal '(1 0 0 1) (status-doors status))) 
           (progn
             (format t "get-status:  incorrect doors state - expected:~a, got:~a~%" '(1 0 0 1) (status-doors status))
             (setf ok NIL)))

       (if (not (equal '(1 0 1 0) (status-buttons status))) 
           (progn
             (format t "get-status:  incorrect buttons state - expected:~a, got:~a~%" '(1 0 1 0) (status-buttons status))
             (setf ok NIL)))

       (if (/= 18 (status-relays status)) 
           (progn
             (format t "get-status:  incorrect relays state - expected:~a, got:~a~%" 18 (status-relays status))
             (setf ok NIL)))

       (if (/= 52 (status-inputs status)) 
           (progn
             (format t "get-status:  incorrect inputs state - expected:~a, got:~a~%" 18 (status-inputs status))
             (setf ok NIL)))

       (if (/= 86 (status-syserror status)) 
           (progn
             (format t "get-status:  incorrect system error - expected:~a, got:~a~%" 18 (status-syserror status))
             (setf ok NIL)))

       (if (/= 253 (status-info status)) 
           (progn
             (format t "get-status: incorrect system info - expected:~a, got:~a~%" 253 (status-info status))
             (setf ok NIL)))

       (if (/= 9876 (status-seqno status)) 
           (progn
             (format t "get-status:  incorrect status sequence number - expected:~a, got:~a~%" 9876 (status-seqno status))
             (setf ok NIL)))

       (let ((event (status-event status)))
            (if (not (equal "2022-01-02 12:34:56" (event-timestamp event))) 
                (progn
                  (format t "get-status:  incorrect event timestamp - expected:~a, got:~a~%" "2022-01-02 12:34:56" (event-timestamp event))
                  (setf ok NIL)))

            (if (/= 135 (event-index event)) 
                (progn
                  (format t "get-status:  incorrect event index - expected:~a, got:~a~%" 135 (event-index event))
                  (setf ok NIL)))

            (if (/= 6 (event-type event))
                (progn
                  (format t "get-status:  incorrect event type - expected:~a, got:~a~%" 6 (event-type event))
                  (setf ok NIL)))

            (if (/= 1 (event-granted event))
                (progn
                  (format t "get-status:  incorrect event granted - expected:~a, got:~a~%" 1 (event-granted event))
                  (setf ok NIL)))

            (if (/= 3 (event-door event))
                (progn
                  (format t "get-status:  incorrect event door - expected:~a, got:~a~%" 3 (event-door event))
                  (setf ok NIL)))

            (if (/= 1 (event-direction event))
                (progn
                  (format t "get-status:  incorrect event direction - expected:~a, got:~a~%" 1 (event-direction event))
                  (setf ok NIL)))

            (if (/= 8100023 (event-card event))
                (progn
                  (format t "get-status: incorrect event card - expected:~a, got:~a~%" 8100023 (event-card event))
                  (setf ok NIL)))

            (if (/= 21 (event-reason event))
                (progn
                  (format t "get-status:  incorrect event reason - expected:~a, got:~a~%" 21 (event-reason event))
                  (setf ok NIL))))
       (if ok 
          (passed tag )
          (failed tag))))


(defun get-time () "" 
  (let ((tag      "get-time")
        (ok       T)
        (datetime (exec #'(lambda (u) (uhppoted-get-time u TEST-DEVICE-ID)))))
       (if (not (equal "2022-01-02 12:34:56" datetime)) 
           (progn
             (format t "get-time:    incorrect date/time - expected:~a, got:~a~%" "2022-01-02 12:34:56" datetime)
             (setf ok NIL)))
       (if ok 
          (passed tag )
          (failed tag))))


(defun set-time () "" 
  (let ((tag      "set-time")
        (ok       T))
  (exec #'(lambda (u) (uhppoted-set-time u TEST-DEVICE-ID "2022-03-23 12:24:17")))
       (if ok 
          (passed tag )
          (failed tag))))


(defun get-listener () "" 
  (let ((tag      "get-listener")
        (ok       T)
        (listener (exec #'(lambda (u) (uhppoted-get-listener u TEST-DEVICE-ID)))))
       (if (not (equal "192.168.1.100:60001" listener)) 
           (progn
             (format t "get-time:    incorrect event listener address - expected:~a, got:~a~%" "192.168.1.100:60001" listener)
             (setf ok NIL)))
       (if ok 
          (passed tag )
          (failed tag))))


(defun set-listener () "" 
  (let ((tag      "set-listener")
        (ok       T))
       (exec #'(lambda (u) (uhppoted-set-listener u TEST-DEVICE-ID "192.168.1.100:60001")))
       (if ok 
          (passed tag )
          (failed tag))))


(defun get-door-control () "" 
  (let ((tag      "get-door-control")
        (ok       T)
        (control (exec #'(lambda (u) (uhppoted-get-door-control u TEST-DEVICE-ID TEST-DOOR)))))
       (if (/= uhppoted:controlled (door-control-mode control)) 
           (progn
             (format t "get-door-control: incorrect door control mode - expected:~a, got:~a~%" uhppoted:controlled (door-control-mode control))
             (setf ok NIL)))

       (if (/= 7 (door-control-delay control)) 
           (progn
             (format t "get-door-control: incorrect door open delay - expected:~a, got:~a~%" 7 (door-control-delay control))
             (setf ok NIL)))
       (if ok 
          (passed tag )
          (failed tag))))


(defun set-door-control () "" 
  (let ((tag      "set-door-control")
        (ok       T))
       (exec #'(lambda (u) (uhppoted-set-door-control u TEST-DEVICE-ID TEST-DOOR uhppoted:normally-closed 6)))
       (if ok 
          (passed tag )
          (failed tag))))


(defun open-door () "" 
  (let ((tag      "open-door")
        (ok       T))
       (exec #'(lambda (u) (uhppoted-open-door u TEST-DEVICE-ID TEST-DOOR)))
       (if ok 
          (passed tag )
          (failed tag))))


(defun get-cards () "" 
  (let ((tag      "get-cards")
        (ok       T)
        (cards (exec #'(lambda (u) (uhppoted-get-cards u TEST-DEVICE-ID)))))
       (if (not (equal 39 cards)) 
           (progn
             (format t "get-cards:    incorrect card count - expected:~a, got:~a~%" 39 cards)
             (setf ok NIL)))
       (if ok 
          (passed tag )
          (failed tag))))


(defun get-card () "" 
  (let ((tag      "get-card")
        (ok       T)
        (card (exec #'(lambda (u) (uhppoted-get-card u TEST-DEVICE-ID TEST-CARD-NUMBER)))))
       (if (not (equal 8165538 (card-card-number card))) 
           (progn
             (format t "get-card:     incorrect card number - expected:~a, got:~a~%" 8165538 (card-card-number card))
             (setf ok NIL)))

       (if (not (string= "2022-01-01" (card-from card))) 
           (progn
             (format t "get-card:     incorrect card 'from' date - expected:~a, got:~a~%" "2022-01-01" (card-from card))
             (setf ok NIL)))

       (if (not (string= "2022-12-31" (card-to card))) 
           (progn
             (format t "get-card:     incorrect card 'to' date - expected:~a, got:~a~%" "2022-12-31" (card-to card))
             (setf ok NIL)))

       (if (not (equal '(0 1 31 75) (card-doors card))) 
           (progn
             (format t "get-card:     incorrect card doors - expected:~a, got:~a~%" '(0 1 31 75) (card-doors card))
             (setf ok NIL)))

       (if ok 
          (passed tag )
          (failed tag))))


(defun get-card-by-index () "" 
  (let ((tag      "get-card-by-index")
        (ok       T)
        (card (exec #'(lambda (u) (uhppoted-get-card-by-index u TEST-DEVICE-ID TEST-CARD-INDEX)))))
       (if (not (equal 8165538 (card-card-number card))) 
           (progn
             (format t "get-card-by-index:  incorrect card number - expected:~a, got:~a~%" 8165538 (card-card-number card))
             (setf ok NIL)))

       (if (not (string= "2022-01-01" (card-from card))) 
           (progn
             (format t "get-card-by-index: incorrect card 'from' date - expected:~a, got:~a~%" "2022-01-01" (card-from card))
             (setf ok NIL)))

       (if (not (string= "2022-12-31" (card-to card))) 
           (progn
             (format t "get-card-by-index: incorrect card 'to' date - expected:~a, got:~a~%" "2022-12-31" (card-to card))
             (setf ok NIL)))

       (if (not (equal '(0 1 31 75) (card-doors card))) 
           (progn
             (format t "get-card-by-index: incorrect card doors - expected:~a, got:~a~%" '(0 1 31 75) (card-doors card))
             (setf ok NIL)))

       (if ok 
          (passed tag )
          (failed tag))))


(defun put-card () "" 
  (let ((tag      "put-card")
        (ok       T)
        (doors (make-array 4 :initial-contents '(0 1 31 75))))
       (exec #'(lambda (u) (uhppoted-put-card u TEST-DEVICE-ID TEST-CARD-NUMBER "2022-01-01" "2022-12-31" doors)))
       (if ok 
          (passed tag )
          (failed tag))))



(defun delete-card () "" 
  (let ((tag      "delete-card")
        (ok       T))
       (exec #'(lambda (u) (uhppoted-delete-card u TEST-DEVICE-ID TEST-CARD-NUMBER)))
       (if ok 
          (passed tag )
          (failed tag))))


(defun delete-cards () "" 
  (let ((tag      "delete-cards")
        (ok       T))
       (exec #'(lambda (u) (uhppoted-delete-cards u TEST-DEVICE-ID)))
       (if ok 
          (passed tag )
          (failed tag))))


(defun get-event-index () "" 
  (let ((tag      "get-event-index")
        (ok       T)
        (expected 47)
        (index    (exec #'(lambda (u) (uhppoted-get-event-index u TEST-DEVICE-ID)))))
       (if (not (equal expected index)) 
           (progn
             (format t "~a: incorrect event index - expected:~a, got:~a~%" tag expected index)
             (setf ok NIL)))
       (if ok 
          (passed tag )
          (failed tag))))


(defun set-event-index () "" 
  (let ((tag      "set-event-index")
        (ok       T))
       (exec #'(lambda (u) (uhppoted-set-event-index u TEST-DEVICE-ID TEST-EVENT-INDEX)))
       (if ok 
          (passed tag )
          (failed tag))))


(defun get-event () "" 
  (let ((tag "get-event")
        (ok T)
        (event (exec #'(lambda (u) (uhppoted-get-event u TEST-DEVICE-ID TEST-EVENT-INDEX)))))
       (if (not (equal 51 (event-index event))) 
           (progn
             (format t "~a:     incorrect event index - expected:~a, got:~a~%" tag 51 (event-index event))
             (setf ok NIL)))

       (if (not (string= "2022-04-15 12:29:15" (event-timestamp event))) 
           (progn
             (format t "~a:     incorrect event timestamp - expected:~a, got:~a~%" tag "2022-04-15 12:29:15" (event-timestamp event))
             (setf ok NIL)))

       (if (not (equal 6 (event-type event))) 
           (progn
             (format t "~a:     incorrect event type - expected:~a, got:~a~%" tag 6 (event-type event))
             (setf ok NIL)))

       (if (not (equal 1 (event-granted event))) 
           (progn
             (format t "~a:     incorrect event granted - expected:~a, got:~a~%" tag 1 (event-type event))
             (setf ok NIL)))

       (if (not (equal 3 (event-door event))) 
           (progn
             (format t "~a:     incorrect event door - expected:~a, got:~a~%" tag 3 (event-door event))
             (setf ok NIL)))

       (if (not (equal 1 (event-direction event))) 
           (progn
             (format t "~a:     incorrect event direction - expected:~a, got:~a~%" tag 1 (event-direction event))
             (setf ok NIL)))

       (if (not (equal 8165538 (event-card event))) 
           (progn
             (format t "~a:     incorrect event card number - expected:~a, got:~a~%" tag 8165538 (event-card event))
             (setf ok NIL)))

       (if (not (equal 21 (event-reason event))) 
           (progn
             (format t "~a:     incorrect event reason - expected:~a, got:~a~%" tag 21 (event-reason event))
             (setf ok NIL)))

       (if ok 
          (passed tag )
          (failed tag))))


(defun record-special-events () "" 
  (let ((tag      "record-special-events")
        (ok       T))
       (exec #'(lambda (u) (uhppoted-record-special-events u TEST-DEVICE-ID t)))
       (if ok 
          (passed tag )
          (failed tag))))


(defun get-time-profile () "" 
  (let ((tag      "get-time-profile")
        (ok       T)
        (profile (exec #'(lambda (u) (uhppoted-get-time-profile u TEST-DEVICE-ID TEST-PROFILE-ID)))))
       (if (not (equal 49 (time-profile-id profile))) 
           (progn
             (format t "~a:  incorrect profile ID- expected:~a, got:~a~%" tag 49 (time-profile-id profile))
             (setf ok NIL)))

       (if (not (equal 71 (time-profile-linked profile))) 
           (progn
             (format t "~a:  incorrect linked profile - expected:~a, got:~a~%" tag 71 (time-profile-linked profile))
             (setf ok NIL)))

       (if (not (string= "2022-02-01" (time-profile-from profile))) 
           (progn
             (format t "~a: incorrect profile 'from' date - expected:~a, got:~a~%" tag "2022-02-01" (time-profile-from profile))
             (setf ok NIL)))

       (if (not (string= "2022-06-30" (time-profile-to profile))) 
           (progn
             (format t "~a: incorrect profile 'to' date - expected:~a, got:~a~%" tag "2022-06-30" (time-profile-to profile))
             (setf ok NIL)))

       (if (not (time-profile-monday profile))
           (progn
             (format t "~a:  incorrect profile Monday - expected:~a, got:~a~%" tag t (time-profile-monday profile))
             (setf ok NIL)))

       (if (time-profile-tuesday profile)
           (progn
             (format t "~a:  incorrect profile Tuesday - expected:~a, got:~a~%" tag nil (time-profile-tuesday profile))
             (setf ok NIL)))

       (if (not (time-profile-wednesday profile))
           (progn
             (format t "~a:  incorrect profile Wednesday - expected:~a, got:~a~%" tag t (time-profile-wednesday profile))
             (setf ok NIL)))

       (if (not (time-profile-thursday profile))
           (progn
             (format t "~a:  incorrect profile Thursday - expected:~a, got:~a~%" tag t (time-profile-thursday profile))
             (setf ok NIL)))

       (if (time-profile-friday profile)
           (progn
             (format t "~a:  incorrect profile Friday - expected:~a, got:~a~%" tag nil (time-profile-friday profile))
             (setf ok NIL)))

       (if (time-profile-saturday profile)
           (progn
             (format t "~a:  incorrect profile Saturday - expected:~a, got:~a~%" tag nil (time-profile-saturday profile))
             (setf ok NIL)))

       (if (not (time-profile-sunday profile))
           (progn
             (format t "~a:  incorrect profile Sunday - expected:~a, got:~a~%" tag t (time-profile-sunday profile))
             (setf ok NIL)))
       
       (if (not (string= "08:30" (time-profile-segment1start profile))) 
           (progn
             (format t "~a: incorrect profile segment 1 start - expected:~a, got:~a~%" tag "08:30" (time-profile-segment1start profile))
             (setf ok NIL)))

       (if (not (string= "11:30" (time-profile-segment1end profile))) 
           (progn
             (format t "~a: incorrect profile segment 1 end - expected:~a, got:~a~%" tag "11:30" (time-profile-segment1end profile))
             (setf ok NIL)))

       (if (not (string= "00:00" (time-profile-segment2start profile))) 
           (progn
             (format t "~a: incorrect profile segment 2 start - expected:~a, got:~a~%" tag "00:00" (time-profile-segment2start profile))
             (setf ok NIL)))

       (if (not (string= "00:00" (time-profile-segment2end profile))) 
           (progn
             (format t "~a: incorrect profile segment 2 end - expected:~a, got:~a~%" tag "00:00" (time-profile-segment2end profile))
             (setf ok NIL)))

       (if (not (string= "00:00" (time-profile-segment3start profile))) 
           (progn
             (format t "~a: incorrect profile segment 3 start - expected:~a, got:~a~%" tag "00:00" (time-profile-segment3start profile))
             (setf ok NIL)))

       (if (not (string= "18:00" (time-profile-segment3end profile))) 
           (progn
             (format t "~a: incorrect profile segment 3 end - expected:~a, got:~a~%" tag "18:00" (time-profile-segment3end profile))
             (setf ok NIL)))

       (if ok 
          (passed tag )
          (failed tag))))


(defun set-time-profile () "" 
  (let ((tag     "set-time-profile")
        (profile (make-time-profile :ID        49
                                    :linked    71
                                    :from      "2022-02-01"
                                    :to        "2022-06-30"
                                    :monday    t
                                    :tuesday   nil
                                    :wednesday t
                                    :thursday  t
                                    :friday    nil
                                    :saturday  nil
                                    :sunday    t
                                    :segment1start "08:30"
                                    :segment1end   "11:30"
                                    :segment2start ""
                                    :segment2end   ""
                                    :segment3start ""
                                    :segment3end   "18:00"))
        (ok       T))
       (exec #'(lambda (u) (uhppoted-set-time-profile u TEST-DEVICE-ID profile)))
       (if ok 
          (passed tag )
          (failed tag))))


(defun passed (tag) ""
  (format t "~21a ok~%" tag))

(defun failed (tag) ""
  (format t "~21a failed~%" tag))
; (error 'failed :message  "get-event    FAILED"))))
