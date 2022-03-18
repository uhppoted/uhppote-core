(in-package :examples)

;(define-condition uhppoted-error (error)
;   ((message :initarg :message :reader message)))

(defun debug () "" 
   (handler-bind
      ((uhppoted-error
         #'(lambda (c) 
              (format t "~%   *** ERROR: ~a~%~%" (message c))
              (invoke-restart 'uhppoted:with-warning "oh noes i can has problems"))))
      (list "debug" (uhppoted #'(lambda (u) (uhppoted-get-status u 405419896))
                                            :bind-addr "qwerty"
                                            :debug T))))

(defun get-devices () "" 
   (handler-bind
      ((uhppoted-error
         #'(lambda (c) 
              (format t "*** ERROR: ~a~%" (message c))
              (invoke-restart 'ignore))))
      (list "get-devices" (uhppoted #'(lambda (u) (uhppoted-get-devices u))
                                    :bind-addr      "192.168.1.100"
                                    :broadcast-addr "192.168.1.100"
                                    :listen-addr    "192.168.1.100:60001"
                                    :timeout        1
                                    :controllers    (list '(405419896 "192.168.1.100") '(303986753 "192.168.1.100"))
                                    :debug          T))))

(defun get-device () "" 
   (handler-bind
      ((uhppoted-error
         #'(lambda (c) 
              (format t "*** ERROR: ~a~%" (message c))
              (invoke-restart 'ignore))))
      (list "get-device" (uhppoted #'(lambda (u) (uhppoted-get-device u 405419896))
                                   :bind-addr      "192.168.1.100"
                                   :broadcast-addr "192.168.1.100"
                                   :listen-addr    "192.168.1.100:60001"
                                   :timeout        1
                                   :controllers   (list '(405419896 "192.168.1.100") '(303986753 "192.168.1.100"))
                                   :debug          T))))

(defun set-address () "" 
   (handler-bind
      ((uhppoted-error
         #'(lambda (c) 
              (format t "*** ERROR: ~a~%" (message c))
              (invoke-restart 'ignore))))
      (list "set-address" (uhppoted #'(lambda (u) (uhppoted-set-address u 405419896 "192.168.1.125" "255.255.255.254" "192.168.1.5"))
                                    :bind-addr      "192.168.1.100"
                                    :broadcast-addr "192.168.1.100"
                                    :listen-addr    "192.168.1.100:60001"
                                    :timeout        1
                                    :controllers   (list '(405419896 "192.168.1.100") '(303986753 "192.168.1.100"))
                                    :debug          T))))

(defun get-status () "" 
   (handler-bind
      ((uhppoted-error
         #'(lambda (c) 
              (format t "*** ERROR: ~a~%" (message c))
              (invoke-restart 'ignore))))
      (list "get-status" (uhppoted #'(lambda (u) (uhppoted-get-status u 405419896))
                                   :bind-addr      "192.168.1.100"
                                   :broadcast-addr "192.168.1.100"
                                   :listen-addr    "192.168.1.100:60001"
                                   :timeout        1
                                   :controllers   (list '(405419896 "192.168.1.100") '(303986753 "192.168.1.100"))
                                   :debug          T))))

