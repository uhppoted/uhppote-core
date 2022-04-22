(in-package :examples)

(defun now () ""
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
     (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" year month day hour minute second)))

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
  (format t 
          "~%  get-devices:~%~{    ~a~^~%~}~%~%"
          (coerce (exec #'(lambda (u) (uhppoted-get-devices u))) 'list)))

(defun get-device () "" 
  (let ((tag         "get-device")
        (device-id   405419896))
    (format t 
            "  ~a:~%    ~:w~%~%" 
            tag
            (exec #'(lambda (u) (uhppoted-get-device u device-id))))))


(defun set-address () "" 
  (let ((tag       "set-listener")
        (device-id 405419896)
        (address   "192.168.1.125")
        (subnet    "255.255.254.0")
        (gateway   "192.168.1.5"))
    (format t 
            "  ~a:~%    ~:w~%~%" 
            tag
            (exec #'(lambda (u) (uhppoted-set-address u device-id address subnet gateway))))))

(defun get-status () "" 
  (let ((tag       "get-status")
        (device-id 405419896))
    (format t 
            "  ~a:~%    ~:w~%~%" 
            tag
            (exec #'(lambda (u) (uhppoted-get-status u device-id))))))

(defun get-time () "" 
  (let ((tag       "get-time")
        (device-id 405419896))
    (format t 
            "  ~a:~%    ~:w~%~%" 
            tag
            (exec #'(lambda (u) (uhppoted-get-time u device-id))))))

(defun set-time () "" 
  (let ((tag       "set-listener")
        (device-id 405419896)
        (datetime  (now)))
    (format t 
            "  ~a:~%    ~:w~%~%" 
            tag
            (exec #'(lambda (u) (uhppoted-set-time u device-id datetime))))))

(defun get-listener () "" 
  (let ((tag       "get-listener")
        (device-id 405419896))
    (format t 
            "  ~a:~%    ~:w~%~%" 
            tag
            (exec #'(lambda (u) (uhppoted-get-listener u device-id))))))

(defun set-listener () "" 
  (let ((tag       "set-listener")
        (device-id 405419896)
        (listener  "192.168.1.100:60001"))
    (format t 
            "  ~a:~%    ~:w~%~%" 
            tag
            (exec #'(lambda (u) (uhppoted-set-listener u device-id listener))))))

(defun get-door-control () "" 
  (let ((tag       "get-door-control")
        (device-id 405419896)
        (door      4))
    (format t 
            "  ~a:~%    ~:w~%~%" 
            tag
            (exec #'(lambda (u) (uhppoted-get-door-control u device-id door))))))

(defun set-door-control () "" 
  (let ((tag       "set-door-control")
        (device-id 405419896)
        (door      4)
        (mode      uhppoted:normally-open)
        (delay     9))
    (format t 
            "  ~a:~%    ~:w~%~%" 
            tag
            (exec #'(lambda (u) (uhppoted-set-door-control u device-id door mode delay))))))

(defun open-door () "" 
  (let ((tag       "open-door")
        (device-id 405419896)
        (door      4))
    (format t 
            "  ~a:~%    ~:w~%~%" 
            tag
            (exec #'(lambda (u) (uhppoted-open-door u device-id door))))))

(defun get-cards () "" 
  (let ((tag       "get-cards")
        (device-id 405419896))
    (format t 
            "  ~a:~%    ~:w~%~%" 
            tag
            (exec #'(lambda (u) (uhppoted-get-cards u device-id))))))

(defun get-card () "" 
  (let ((tag         "get-card")
        (device-id   405419896)
        (card-number 8000001))
    (format t 
            "  ~a:~%    ~:w~%~%" 
            tag
            (exec #'(lambda (u) (uhppoted-get-card u device-id card-number))))))

(defun get-card-by-index () "" 
  (let ((tag       "get-card-by-index")
        (device-id 405419896)
        (index     7))
    (format t 
            "  ~a:~%    ~:w~%~%" 
            tag
            (exec #'(lambda (u) (uhppoted-get-card-by-index u device-id index))))))

(defun put-card () "" 
  (let ((tag         "put-card")
        (device-id   405419896)
        (card-number 8000001)
        (from        "2022-01-01")
        (to          "2022-12-31")
        (doors       (make-array 4 :initial-contents '(0 1 31 75))))
    (format t 
            "  ~a:~%    ~:w~%~%" 
            tag
            (exec #'(lambda (u) (uhppoted-put-card u device-id card-number from to doors))))))

(defun delete-card () "" 
  (let ((tag         "delete-card")
        (device-id   405419896)
        (card-number 8000001))
    (format t 
            "  ~a:~%    ~:w~%~%" 
            tag
            (exec #'(lambda (u) (uhppoted-delete-card u device-id card-number))))))


(defun delete-cards () "" 
  (let ((tag       "delete-cards")
        (device-id 405419896))
    (format t 
            "  ~a:~%    ~:w~%~%" 
            tag
            (exec #'(lambda (u) (uhppoted-delete-cards u device-id))))))


(defun get-event-index () "" 
  (let ((tag       "get-event-index")
        (device-id 405419896))
    (format t 
            "  ~a:~%    ~:w~%~%" 
            tag
            (exec #'(lambda (u) (uhppoted-get-event-index u device-id))))))


(defun set-event-index () "" 
  (let ((tag       "set-event-index")
        (device-id 405419896)
        (index     91))
    (format t 
            "  ~a:~%    ~:w~%~%" 
            tag
            (exec #'(lambda (u) (uhppoted-set-event-index u device-id index))))))


(defun get-event () "" 
  (let ((tag       "get-event")
        (device-id 405419896)
        (index     43))
    (format t 
            "  ~a:~%    ~:w~%~%" 
            tag
            (exec #'(lambda (u) (uhppoted-get-event u device-id index))))))


(defun record-special-events () "" 
  (let ((tag       "record-special-events")
        (device-id 405419896)
        (enabled   t))
    (format t 
            "  ~a:~%    ~:w~%~%" 
            tag
            (exec #'(lambda (u) (uhppoted-record-special-events u device-id enabled))))))

(defun get-time-profile () "" 
  (let ((tag        "get-time-profile")
        (device-id  405419896)
        (profile-id 29))
    (format t 
            "  ~a:~%    ~:w~%~%" 
            tag
            (exec #'(lambda (u) (uhppoted-get-time-profile u device-id profile-id))))))

(defun set-time-profile () "" 
  (let ((tag        "set-time-profile")
        (device-id  405419896)
        (profile    (make-time-profile :ID        29
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
                                       :segment3end   "18:00")))
    (format t 
            "  ~a:~%    ~:w~%~%" 
            tag
            (exec #'(lambda (u) (uhppoted-set-time-profile u device-id profile))))))
