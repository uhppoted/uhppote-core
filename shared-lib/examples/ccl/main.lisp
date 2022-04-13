(load "../../bindings/ccl/packages.lisp")
(load "../../bindings/ccl/uhppoted.lisp")
(load "packages.lisp")
(load "examples.lisp")

(defun commands () ""
  (list '("get-devices"       "Retrieves a list of UHPPOTE controller IDs findable on the local LAN.")
        '("get-device"        "Retrieves the basic device information for a single UHPPOTE controller.")
        '("set-address"       "Sets the controller IPv4 address, subnet mask and gateway address.")
        '("get-status"        "Retrieves a controller status.")
        '("get-time"          "Retrieves a controller current date/time (YYYY-MM-DD HH:mm:ss).")
        '("set-time"          "Sets a controller current date/time (YYYY-MM-DD HH:mm:ss).")
        '("get-listener"      "Retrieves a controller's configured event listener address.")
        '("set-listener"      "Configures a controller's event listener address and port.")
        '("get-door-control"  "Retrieves the control state and open delay for a controller door.")
        '("set-door-control"  "Sets the control mode and delay for a controller door.")
        '("get-cards"         "Retrieves the number of cards stored on a controller.")
        '("get-card"          "Retrieves the card detail for card number from a controller.")
        '("get-card-by-index" "Retrieves the card detail for the card stored at an index on a controller.")
        '("put-card"          "Adds or updates the card detail stored on a controller.")
        '("delete-card"       "Deletes a card from a controller.")
        '("delete-cards"      "Deletes all cards from a controller.")
        '("get-event-index"   "Retrieves the current event index from a controller.")
))


(defun usage () ""
  (let ((cmds (commands)))
    (format t "~%   Usage: ./examples <command>~%")   
    (format t "~%   Suppported commands:~%")
    (loop for cmd in cmds
      do (format t "     ~a~%" (first cmd)))
    (format t "~%")))


(defun help () ""
  (let ((cmds (commands)))
    (format t "~%   Usage: ./examples <command>~%")   
    (format t "~%   Commands:~%")
    (loop for cmd in cmds
      do (format t "     ~17,a ~a~%" (first cmd) (second cmd)))
    (format t "~%")))


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

(defun set-door-control () ""
  (format t "  set-door-control:~%    ~:w~%~%" (examples:set-door-control 405419896 4 1 9)))

(defun get-cards () ""
  (format t "  get-cards:~%    ~:w~%~%" (examples:get-cards 405419896)))

(defun get-card () ""
  (format t "  get-card:~%    ~:w~%~%" (examples:get-card 405419896 8000001)))

(defun get-card-by-index () ""
  (format t "  get-card-byindex:~% ~:w~%~%" (examples:get-card-by-index 405419896 7)))

(defun put-card () ""
  (let ((doors (make-array 4 :initial-contents '(0 1 31 75))))
    (format t "  put-card:~%    ~:w~%~%" (examples:put-card 405419896 8000001 "2022-01-01" "2022-12-31" doors))))


(defun main () ""
  (let ((args (parse-command-line)))
    (if (not args)
        (usage)
        (loop for arg in args
          do (cond ((string= arg "help")              (help))
                   ((string= arg "get-devices")       (examples:get-devices))
                   ((string= arg "get-device")        (get-device))
                   ((string= arg "set-address")       (set-address))
                   ((string= arg "get-status")        (get-status))
                   ((string= arg "get-time")          (get-time))
                   ((string= arg "set-time")          (set-time))
                   ((string= arg "get-listener")      (get-listener))
                   ((string= arg "set-listener")      (set-listener))
                   ((string= arg "get-door-control")  (get-door-control))
                   ((string= arg "set-door-control")  (set-door-control))
                   ((string= arg "get-cards")         (get-cards))
                   ((string= arg "get-card")          (get-card))
                   ((string= arg "get-card-by-index") (get-card-by-index))
                   ((string= arg "put-card")          (put-card))
                   ((string= arg "delete-card")       (examples:delete-card))
                   ((string= arg "delete-cards")      (examples:delete-cards))
                   ((string= arg "get-event-index")   (examples:get-event-index))
                   (t (progn
                        (format t "~%   *** ERROR: invalid command (~a)~%"  arg) 
                        (usage))))))))

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

