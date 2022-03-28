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
			  :timeout        1
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
