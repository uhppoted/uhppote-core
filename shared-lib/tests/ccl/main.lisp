(load "../../bindings/ccl/packages.lisp")
(load "../../bindings/ccl/uhppoted.lisp")
(load "packages.lisp")
(load "tests.lisp")

(defun usage () ""
  (format t "~%  Usage: ./test <command>~%") 
  (format t "~%") 
  (format t "    Supported commands:~%") 
  (format t "      get-devices   Retrieves a list of devices on the local LAN~%") 
  (format t "      get-device    Retrieves the information for a UHPPOTE controller~%") 
  (format t "      set-address   Sets a controller IP address, subnet mask and gateway address~%") 
  (format t "      get-status    Retrieves a controller status~%") 
  (format t "      get-time      Retrieves a controller date/time~%") 
  (format t "~%") 
  (format t "    Defaults to running all tests~%") 
  (format t "~%"))

(defun get-devices () ""
  (tests:get-devices))

(defun get-device () ""
  (tests:get-device))

(defun set-address () ""
  (tests:set-address))

(defun get-status () ""
  (tests:get-status))

(defun get-time () ""
  (tests:get-time))

(defun all () ""
  (let ((ok T))
       (if (not (get-devices)) (setf ok NIL))
       (if (not (get-device))  (setf ok NIL))
       (if (not (set-address)) (setf ok NIL))
       (if (not (get-status))  (setf ok NIL))
       (if (not (get-time))    (setf ok NIL))
       ok))

(defun main () ""
  (let ((args (parse-command-line)))
    (loop for arg in args
       do (cond ((string= arg "get-devices") (get-devices))
                ((string= arg "get-device")  (get-device))
                ((string= arg "set-address") (set-address))
                ((string= arg "get-status")  (get-status))
                ((string= arg "get-time")    (get-time))
                (t (all))))))

;;;; Workaround to skip command line arguments for REPL - invoking (main) in the REPL is
;;;; peculiarly pointless so:
;;;; - if *unproccessed-command-line-arguments* is not NIL just discard them
;;;; - if (car *command-line-arguments*) is not (example) just discard them.
;;;;
;;;; Ref. https://github.com/Clozure/ccl/issues/177
(defun parse-command-line () ""
  (let ((args       *command-line-argument-list*)
        (executable "test"))
    (cond (*unprocessed-command-line-arguments* ())
          ((eq (search executable (car args)) NIL) ())
          ((/= (+ (coerce (search executable (car args)) 'fixnum) (length executable)) (length (car args))) ())
          (t (cdr args)))))

(defun make-app () ""
  (save-application "test" :toplevel-function #'main :prepend-kernel t))

