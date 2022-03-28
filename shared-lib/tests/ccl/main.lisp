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
  (format t "      set-time      Sets a controller date/time~%") 
  (format t "~%") 
  (format t "    Defaults to running all tests~%") 
  (format t "~%"))


(defun test (f) "Invokes 'tests' function with 'exit-fail' condition handler"
  (handler-bind
    ((tests:failed #'(lambda (err) 
                     (progn
                       (format *error-output* "~%  *** ERROR: ~a~%~%" (tests:message err))
                       (invoke-restart 'exit-fail)))))
    
    (restart-case (funcall f)
      (exit-fail () (quit -1)))))


(defun all () "Invokes all test functions with 'exit-fail' condition handler"
  (let* ((tests '( tests:get-devices 
                   tests:get-device 
                   tests:set-address 
                   tests:get-status 
                   tests:get-time 
                   tests:set-time 
                 ))
         (result (loop for test in tests collect (all-x test))))
        (if (some #'null result)
            (quit -1))))

(defun all-x (fn) ""
  (handler-bind
    ((tests:failed #'(lambda (err) 
                       (progn
                         (format *error-output* "~% *** ERROR: ~a~%~%" (tests:message err))
                         (use-value NIL)))))
    (restart-case (progn (funcall fn) T)
      (use-value (value) value))))


(defun main () ""
  (let ((args (parse-command-line)))
    (if (not args)
        (all)
        (loop for arg in args
          do (cond ((string= arg "get-devices") (test #'tests:get-devices))
                   ((string= arg "get-device")  (test #'tests:get-device))
                   ((string= arg "set-address") (test #'tests:set-address))
                   ((string= arg "get-status")  (test #'tests:get-status))
                   ((string= arg "get-time")    (test #'tests:get-time))
                   ((string= arg "set-time")    (test #'tests:set-time))
                   ((string= arg "all")         (all))
                   (t (progn
                        (format t "~%   *** ERROR invalid command (~a)~%" arg)
                        (usage))
                   )
                   )))))


;;;; Workaround to skip command line arguments for REPL - invoking (main) in the REPL is
;;;; particularly pointless so:
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


(defun debug (f) "Invoke test function with 'warning only' condition handler"
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
