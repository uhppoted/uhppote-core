(in-package :examples)

(defun exec (f) "" 
  (handler-bind
	((uhppoted-error
	   #'(lambda (c) 
		   (format t "*** ERROR: ~a~%" (uhppoted:message c))
		   (invoke-restart 'ignore))))
	(uhppoted f 
			  :bind-addr      "192.168.1.100"
			  :broadcast-addr "192.168.1.100"
			  :listen-addr    "192.168.1.100:60001"
			  :timeout        2500
			  :controllers    (list '(405419896 "192.168.1.100") '(303986753 "192.168.1.100"))
			  :debug          T)))

(defun get-devices () "" 
  (exec #'(lambda (u) (uhppoted-get-devices u))))

(defun get-device (device-id) "" 
  (exec #'(lambda (u) (uhppoted-get-device u device-id))))

(defun set-address (device-id addr subnet gateway) "" 
  (exec #'(lambda (u) (uhppoted-set-address u device-id addr subnet gateway)))
  t)

(defun get-status (device-id) "" 
  (exec #'(lambda (u) (uhppoted-get-status u device-id))))

(defun get-time (device-id) "" 
  (exec #'(lambda (u) (uhppoted-get-time u device-id))))

(defun set-time (device-id datetime) "" 
  (exec #'(lambda (u) (uhppoted-set-time u device-id datetime)))
  t)

(defun get-listener (device-id) "" 
  (exec #'(lambda (u) (uhppoted-get-listener u device-id))))

(defun set-listener (device-id listener) "" 
  (exec #'(lambda (u) (uhppoted-set-listener u device-id listener)))
  t)

(defun get-door-control (device-id door) "" 
  (exec #'(lambda (u) (uhppoted-get-door-control u device-id door))))

(defun set-door-control (device-id door mode delay) "" 
  (exec #'(lambda (u) (uhppoted-set-door-control u device-id door mode delay)))
  t)

(defun get-cards (device-id) "" 
  (exec #'(lambda (u) (uhppoted-get-cards u device-id))))

(defun get-card (device-id card-number) "" 
  (exec #'(lambda (u) (uhppoted-get-card u device-id card-number))))

(defun get-card-by-index (device-id index) "" 
  (exec #'(lambda (u) (uhppoted-get-card-by-index u device-id index))))

(defun put-card (device-id card-number from to doors) "" 
  (exec #'(lambda (u) (uhppoted-put-card u device-id card-number from to doors)))
  t)

(defun delete-card () "" 
  (let ((device-id   405419896)
        (card-number 8000001))
    (format t 
            "  delete-card:~%    ~:w~%~%" 
            (exec #'(lambda (u) (uhppoted-delete-card u device-id card-number))))))
