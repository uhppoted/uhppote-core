(load "../../bindings/ccl/packages.lisp")
(load "../../bindings/ccl/uhppoted.lisp")
(load "packages.lisp")
(load "tests.lisp")


(defun commands () "Returns a list of the commands and their associated functions"
  (list '("get-devices"           tests:get-devices)
        '("get-device"            tests:get-device)
        '("set-address"           tests:set-address)
        '("get-status"            tests:get-status)
        '("get-time"              tests:get-time)
        '("set-time"              tests:set-time)
        '("get-listener"          tests:get-listener)
        '("set-listener"          tests:set-listener)
        '("get-door-control"      tests:get-door-control)
        '("set-door-control"      tests:set-door-control)
        '("open-door"             tests:open-door)
        '("get-cards"             tests:get-cards)
        '("get-card"              tests:get-card)
        '("get-card-by-index"     tests:get-card-by-index)
        '("put-card"              tests:put-card)
        '("delete-card"           tests:delete-card)
        '("delete-cards"          tests:delete-cards)
        '("get-event-index"       tests:get-event-index)
        '("set-event-index"       tests:set-event-index)
        '("get-event"             tests:get-event)
        '("record-special-events" tests:record-special-events)
        '("get-time-profile"      tests:get-time-profile)
        '("set-time-profile"      tests:set-time-profile)
))


(defun usage () ""
  (let ((ll (commands)))
       (format t "~%  Usage: ./test <command>~%") 
       (format t "~%") 
       (format t "    Supported commands:~%") 
       (loop for cmd in ll
          do (format t "      ~a~%" (first cmd)))
       (format t "~%") 
       (format t "    Defaults to running all tests~%") 
       (format t "~%")))


(defun test (f) "Invokes 'tests' function with 'exit-fail' condition handler"
  (handler-bind
    ((tests:failed #'(lambda (err) 
                     (progn
                       (format *error-output* "~%  *** ERROR: ~a~%~%" (tests:message err))
                       (invoke-restart 'exit-fail)))))
    
    (restart-case (funcall f)
      (exit-fail () (quit -1)))))


(defun all () "Invokes all test functions, exiting with -1 if any them failed"
  (let* ((tests (commands)))
    (handler-bind
      ((tests:failed #'(lambda (err) 
                         (progn
                           (format *error-output* "~% *** ERROR: ~a~%~%" (tests:message err))
                           (use-value NIL)))))
      (if (some #'null 
                (loop for test in tests 
                  collect (restart-case 
                            (progn 
                              (funcall (second test)) 
                              t)
                            (use-value (value) value))))
          (quit -1)))))


(defun main () ""
  (let* ((ll (commands))
         (args (parse-command-line))
         (arg  (if args (first args) "all")))
    (cond ((string= arg "help") (usage))
          ((string= arg "all")  (all))
          (t (block single-command
               (loop for (cmd fn) in ll
                 do (if (string= arg cmd)
                        (progn
                          (test fn)
                          (return-from single-command))))

               (format *error-output* "~%   *** ERROR invalid command (~a)~%" arg)
               (usage))))))

;;;; Workaround to skip command line arguments for REPL - invoking (main) in the REPL is
;;;; particularly pointless so:
;;;; - if *unproccessed-command-line-arguments* is not NIL just discard them
;;;; - if (car *command-line-arguments*) is not (test) just discard all command line arguments.
;;;;
;;;; Ref. https://github.com/Clozure/ccl/issues/177
(defun parse-command-line () ""
  (let ((args       *command-line-argument-list*)
        (executable "tests"))
    (cond (*unprocessed-command-line-arguments* ())
          ((eq (search executable (car args)) NIL) ())
          ((/= (+ (coerce (search executable (car args)) 'fixnum) (length executable)) (length (car args))) ())
          (t (cdr args)))))


(defun make-app () ""
  (save-application "tests" :toplevel-function #'main :prepend-kernel t))


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
