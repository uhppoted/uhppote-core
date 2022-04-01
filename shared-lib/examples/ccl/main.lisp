(load "../../bindings/ccl/packages.lisp")
(load "../../bindings/ccl/uhppoted.lisp")
(load "packages.lisp")
(load "examples.lisp")

(defun usage () ""
  (format t "~%  Usage: ./examples <command>~%") 
  (format t "~%") 
  (format t "    Supported commands:~%") 
  (format t "      get-devices       Retrieves a list of devices on the local LAN~%") 
  (format t "      get-device        Retrieves the information for a UHPPOTE controller~%") 
  (format t "      set-address       Sets a controller IP address, subnet mask and gateway address~%") 
  (format t "      get-status        Retrieves a controller status~%") 
  (format t "      get-time          Retrieves a controller date/time~%") 
  (format t "      set-time          Sets a controller date/time~%") 
  (format t "      get-listener      Retrieves a controller's configured event listener address~%") 
  (format t "      set-listener      Sets a controller event listener address and port~%") 
  (format t "      get-door-control  Retrieves the door control state and open delay for a controller door~%") 
  (format t "~%"))

(defun help () ""
  (format t "~%  Examples:~%~{    ~a~^~%~}~%~%" 
           (list "(get-devices)"
                 "(get-device)"
                 "(set-address)"
                 "(get-status)"
                 "(get-time)"
                 "(set-time)"
                 "(get-listener)"
                 "(set-listener)"
                 "(get-door-control)"
           )))

(defun get-devices () ""
  (format t "~%  get-devices:~%~{    ~a~^~%~}~%~%" (coerce (examples:get-devices) 'list)))

(defun get-device () ""
  (format t "  get-device:~%    ~:w~%~%" (examples:get-device 405419896)))

(defun set-address () ""
  (format t "  set-address:~%    ~a~%~%" (examples:set-address 405419896 "192.168.1.125" "255.255.255.254" "192.168.1.5")))

(defun get-status () ""
  (format t "  get-status:~%    ~:w~%~%" (examples:get-status 405419896)))

(defun get-time () ""
  (format t "  get-time:~%    ~:w~%~%" (examples:get-time 405419896)))

(defun set-time () ""
  (format t "  set-time:~%    ~a~%~%" (examples:set-time 405419896 (now))))

(defun get-listener () ""
  (format t "  get-listener:~%    ~:w~%~%" (examples:get-listener 405419896)))

(defun set-listener () ""
  (format t "  set-listener:~%    ~a~%~%" (examples:set-listener 405419896 "192.168.1.100:60001")))

(defun get-door-control () ""
  (format t "  get-door-control:~%    ~:w~%~%" (examples:get-door-control 405419896 4)))

(defun all () "Executes all examples with a 'warning' condition handler"
  (get-devices)
  (get-device)
  (set-address)
  (get-status)
  (get-time)
  (set-time)
  (get-listener)
  (set-listener)
  (get-door-control))

(defun main () ""
  (let ((args (parse-command-line)))
    (if (not args)
        (all)
        (loop for arg in args
          do (cond ((string= arg "get-devices")      (get-devices))
                   ((string= arg "get-device")       (get-device))
                   ((string= arg "set-address")      (set-address))
                   ((string= arg "get-status")       (get-status))
                   ((string= arg "get-time")         (get-time))
                   ((string= arg "set-time")         (set-time))
                   ((string= arg "get-listener")     (get-listener))
                   ((string= arg "set-listener")     (set-listener))
                   ((string= arg "get-door-control") (get-door-control))
                   ((string= arg "all")              (all))
                   ((string= arg "help")             (help))
                   (t (help)))))))

;;;; Workaround to skip command line arguments for REPL - invoking (main) in the REPL is
;;;; peculiarly pointless so:
;;;; - if *unproccessed-command-line-arguments* is not NIL just discard them
;;;; - if (car *command-line-arguments*) is not (example) just discard them.
;;;;
;;;; Ref. https://github.com/Clozure/ccl/issues/177
(defun parse-command-line () ""
  (let ((args       *command-line-argument-list*)
        (executable "examples"))
    (cond (*unprocessed-command-line-arguments* ())
          ((eq (search executable (car args)) NIL) ())
          ((/= (+ (coerce (search executable (car args)) 'fixnum) (length executable)) (length (car args))) ())
          (t (cdr args)))))


(defun now () ""
  (multiple-value-bind (second minute hour day month year) (get-decoded-time)
     (format nil "~4,'0d-~2,'0d-~2,'0d ~2,'0d:~2,'0d:~2,'0d" year month day hour minute second)))


(defun make-app () ""
  (save-application "examples" :toplevel-function #'main :prepend-kernel t))

