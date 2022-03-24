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
  (if (not (tests:get-devices))
      (error 'tests:failed :message  "get-devices: FAILED")))

(defun get-device () ""
  (if (not (tests:get-device))
      (error 'tests:failed :message  "get-device: FAILED")))

(defun set-address () ""
  (if (not (tests:set-address))
      (error 'tests:failed :message  "set-address: FAILED")))

(defun get-status () ""
  (if (not (tests:get-status))
      (error 'tests:failed :message  "get-status: FAILED")))

(defun get-time () ""
  (if (not (tests:get-time))
      (error 'tests:failed :message  "get-time: FAILED")))

(defun all () ""
  (let ((ok T))
       (if (not (get-devices)) (setf ok NIL))
       (if (not (get-device))  (setf ok NIL))
       (if (not (set-address)) (setf ok NIL))
       (if (not (get-status))  (setf ok NIL))
       (if (not (get-time))    (setf ok NIL))
       ok))

(defun main () ""
  (handler-bind
    ((tests:failed #'(lambda (err) 
                     (progn
                       (format *error-output* "~% *** ERROR: ~a~%~%" (tests:message err))
                       (invoke-restart 'exit-fail)))))

    (restart-case 
      (let ((args (parse-command-line)))
        (loop for arg in args
           do (cond ((string= arg "get-devices") (get-devices))
                    ((string= arg "get-device")  (get-device))
                    ((string= arg "set-address") (set-address))
                    ((string= arg "get-status")  (get-status))
                    ((string= arg "get-time")    (get-time))
                    (t (all)))))

      (ignore       ()      nil)
      (use-value    (value) value)
      (with-warning (err)   (warn err))
      (exit-fail    ()      (quit -1)))))

;;;; Workaround to skip command line arguments for REPL - invoking (main) in the REPL is
;;;; peculiarly pointless so:
;;;; - if *unproccessed-command-line-arguments* is not NIL just discard them
;;;; - if (car *command-line-arguments*) is not (test) just discard all command line arguments.
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


(defun test (f) "Invoke test function with 'warning only' condition handler"
  (handler-bind
    ((tests:failed #'(lambda (err) 
                     (invoke-restart 'with-warning (tests:message err)))))
    
    (restart-case (funcall f)
      (ignore       ()      nil)
      (use-value    (value) value)
      (with-warning (err)   (warn err))
      (exit-fail    ()      (quit -1)))
    )
)
