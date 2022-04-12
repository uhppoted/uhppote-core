(in-package :tests)

(defconstant TEST-DEVICE-ID   405419896)
(defconstant TEST-DOOR        4)
(defconstant TEST-CARD-NUMBER 8165538)
(defconstant TEST-CARD-INDEX  19)

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
  (let ((devices (exec #'(lambda (u) (uhppoted-get-devices u)))))
       (cond ((/= 3 (length devices))
                    (progn
                      (format t "get-devices: incorrect device count - expected:~a, got:~a~%" 3 (length devices)))
                      (error 'failed :message  "get-devices: FAILED"))
             ((not (equal (coerce devices 'list) '(201020304 303986753 405419896))) 
                    (progn
                      (format t "get-devices: incorrect device list - expected:~a, got:~a~%" '(201020304 303986753 405419896) devices))
                      (error 'failed :message  "get-devices: FAILED"))
             (t (result "get-devices" t) ))))
  

(defun get-device () "" 
  (let ((ok T)
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
          (result "get-device" t) 
          (error 'failed :message  "get-device: FAILED"))))


(defun set-address () "" 
  (exec #'(lambda (u) (uhppoted-set-address u TEST-DEVICE-ID "192.168.1.125" "255.255.255.254" "192.168.1.5")))
  (result "set-address" t))


(defun get-status () "" 
  (let ((ok T)
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
          (result "get-status" t)
          (error 'failed :message  "get-status:  FAILED"))))


(defun get-time () "" 
  (let ((ok T)
        (datetime (exec #'(lambda (u) (uhppoted-get-time u TEST-DEVICE-ID)))))
       (if (not (equal "2022-01-02 12:34:56" datetime)) 
           (progn
             (format t "get-time:    incorrect date/time - expected:~a, got:~a~%" "2022-01-02 12:34:56" datetime)
             (setf ok NIL)))
       (if ok 
          (result "get-time" t)
          (error 'failed :message  "get-time:    FAILED"))))


(defun set-time () "" 
  (exec #'(lambda (u) (uhppoted-set-time u TEST-DEVICE-ID "2022-03-23 12:24:17")))
  (result "set-time" t))


(defun get-listener () "" 
  (let ((ok T)
        (listener (exec #'(lambda (u) (uhppoted-get-listener u TEST-DEVICE-ID)))))
       (if (not (equal "192.168.1.100:60001" listener)) 
           (progn
             (format t "get-time:    incorrect event listener address - expected:~a, got:~a~%" "192.168.1.100:60001" listener)
             (setf ok NIL)))
       (if ok 
          (format t "get-listener      ok~%")
          (error 'failed :message  "get-listener  FAILED"))))


(defun set-listener () "" 
  (exec #'(lambda (u) (uhppoted-set-listener u TEST-DEVICE-ID "192.168.1.100:60001")))
  (result "set-listener" t))


(defun get-door-control () "" 
  (let ((ok T)
        (control (exec #'(lambda (u) (uhppoted-get-door-control u TEST-DEVICE-ID TEST-DOOR)))))
       (if (/= 3 (door-control-mode control)) 
           (progn
             (format t "get-door-control: incorrect door control mode - expected:~a, got:~a~%" 3 (door-control-mode control))
             (setf ok NIL)))

       (if (/= 7 (door-control-delay control)) 
           (progn
             (format t "get-door-control: incorrect door open delay - expected:~a, got:~a~%" 7 (door-control-delay control))
             (setf ok NIL)))

       (if ok 
          (result "get-door-control" t)
          (error 'failed :message  "get-door-control: FAILED"))))


(defun set-door-control () "" 
  (exec #'(lambda (u) (uhppoted-set-door-control u TEST-DEVICE-ID TEST-DOOR 2 6)))
  (format t "set-door-control  ok~%"))


(defun get-cards () "" 
  (let ((ok T)
        (cards (exec #'(lambda (u) (uhppoted-get-cards u TEST-DEVICE-ID)))))
       (if (not (equal 39 cards)) 
           (progn
             (format t "get-cards:    incorrect card count - expected:~a, got:~a~%" 39 cards)
             (setf ok NIL)))
       (if ok 
          (result "get-cards" t)
          (error 'failed :message  "get-cards     FAILED"))))


(defun get-card () "" 
  (let ((ok T)
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
          (result "get-card" t)
          (error 'failed :message  "get-card     FAILED"))))


(defun get-card-by-index () "" 
  (let ((ok T)
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
          (result "get-card-by-index" t)
          (error 'failed :message  "get-card-by-index  FAILED"))))


(defun put-card () "" 
  (let ((doors (make-array 4 :initial-contents '(0 1 31 75))))
    (exec #'(lambda (u) (uhppoted-put-card u TEST-DEVICE-ID TEST-CARD-NUMBER "2022-01-01" "2022-12-31" doors)))
    (result "put-card" t)))


(defun delete-card () "" 
  (exec #'(lambda (u) (uhppoted-delete-card u TEST-DEVICE-ID TEST-CARD-NUMBER)))
  (result "delete-card" t))


(defun delete-cards () "" 
  (exec #'(lambda (u) (uhppoted-delete-cards u TEST-DEVICE-ID)))
  (result "delete-cards" t))

(defun result (tag ok) ""
  (if ok
      (format t "~17a ok~%" tag)
      (format t "~17a failed~%" tag)))
