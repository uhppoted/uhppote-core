(in-package :tests)

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
             (t (format t "get-devices       ok~%")))))
  

(defun get-device () "" 
  (let ((ok T)
        (device (exec #'(lambda (u) (uhppoted-get-device u 405419896)))))
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
          (format t "get-device        ok~%") 
          (error 'failed :message  "get-device: FAILED"))))


(defun set-address () "" 
  (exec #'(lambda (u) (uhppoted-set-address u 405419896 "192.168.1.125" "255.255.255.254" "192.168.1.5")))
  (format t "set-address       ok~%"))


(defun get-status () "" 
  (let ((ok T)
        (status (exec #'(lambda (u) (uhppoted-get-status u 405419896)))))
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
          (format t "get-status        ok~%") 
          (error 'failed :message  "get-status:  FAILED"))))


(defun get-time () "" 
  (let ((ok T)
        (datetime (exec #'(lambda (u) (uhppoted-get-time u 405419896)))))
       (if (not (equal "2022-01-02 12:34:56" datetime)) 
           (progn
             (format t "get-time:    incorrect date/time - expected:~a, got:~a~%" "2022-01-02 12:34:56" datetime)
             (setf ok NIL)))
       (if ok 
          (format t "get-time          ok~%")
          (error 'failed :message  "get-time:    FAILED"))))


(defun set-time () "" 
  (exec #'(lambda (u) (uhppoted-set-time u 405419896 "2022-03-23 12:24:17")))
  (format t "set-time          ok~%"))


(defun get-listener () "" 
  (let ((ok T)
        (listener (exec #'(lambda (u) (uhppoted-get-listener u 405419896)))))
       (if (not (equal "192.168.1.100:60001" listener)) 
           (progn
             (format t "get-time:    incorrect event listener address - expected:~a, got:~a~%" "192.168.1.100:60001" listener)
             (setf ok NIL)))
       (if ok 
          (format t "get-listener      ok~%")
          (error 'failed :message  "get-listener  FAILED"))))


(defun set-listener () "" 
  (exec #'(lambda (u) (uhppoted-set-listener u 405419896 "192.168.1.100:60001")))
  (format t "set-listener      ok~%"))


(defun get-door-control () "" 
  (let ((ok T)
        (control (exec #'(lambda (u) (uhppoted-get-door-control u 405419896 4)))))
       (if (/= 3 (door-control-mode control)) 
           (progn
             (format t "get-door-control: incorrect door control mode - expected:~a, got:~a~%" 3 (door-control-mode control))
             (setf ok NIL)))

       (if (/= 7 (door-control-delay control)) 
           (progn
             (format t "get-door-control: incorrect door open delay - expected:~a, got:~a~%" 7 (door-control-delay control))
             (setf ok NIL)))

       (if ok 
          (format t "get-door-control  ok~%") 
          (error 'failed :message  "get-door-control: FAILED"))))


(defun set-door-control () "" 
  (exec #'(lambda (u) (uhppoted-set-door-control u 405419896 4 2 6)))
  (format t "set-door-control  ok~%"))
