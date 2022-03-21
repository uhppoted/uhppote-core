(in-package :examples)

(defun help () ""
  (format t "~%  Examples:~%~{    ~a~^~%~}~%" '("(examples:get-devices)"
                           "(examples:get-device)"
                           "(examples:set-address)"
                           "(examples:get-status)"))
)

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
  (list "get-devices" (exec #'(lambda (u) (uhppoted-get-devices u)))))

(defun get-device () "" 
  (list "get-device" (exec #'(lambda (u) (uhppoted-get-device u 405419896)))))

(defun set-address () "" 
  (list "set-address" (exec #'(lambda (u) (uhppoted-set-address u 405419896 "192.168.1.125" "255.255.255.254" "192.168.1.5")))))

(defun get-status () "" 
  (list "get-status" (exec #'(lambda (u) (uhppoted-get-status u 405419896)))))
