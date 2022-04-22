(load "../../bindings/ccl/packages.lisp")
(load "../../bindings/ccl/uhppoted.lisp")
(load "packages.lisp")
(load "examples.lisp")

(defun commands () ""
  (list '("get-devices"           examples:get-devices           "Retrieves a list of UHPPOTE controller IDs findable on the local LAN.")
        '("get-device"            examples:get-device            "Retrieves the basic device information for a single UHPPOTE controller.")
        '("set-address"           examples:set-address           "Sets the controller IPv4 address, subnet mask and gateway address.")
        '("get-status"            examples:get-status            "Retrieves a controller status.")
        '("get-time"              examples:get-time              "Retrieves a controller current date/time (YYYY-MM-DD HH:mm:ss).")
        '("set-time"              examples:set-time              "Sets a controller current date/time (YYYY-MM-DD HH:mm:ss).")
        '("get-listener"          examples:get-listener          "Retrieves a controller's configured event listener address.")
        '("set-listener"          examples:set-listener          "Configures a controller's event listener address and port.")
        '("get-door-control"      examples:get-door-control      "Retrieves the control state and open delay for a controller door.")
        '("set-door-control"      examples:set-door-control      "Sets the control mode and delay for a controller door.")
        '("open-door"             examples:open-door             "Remotely opens a controller door.")
        '("get-cards"             examples:get-cards             "Retrieves the number of cards stored on a controller.")
        '("get-card"              examples:get-card              "Retrieves the card detail for card number from a controller.")
        '("get-card-by-index"     examples:get-card-by-index     "Retrieves the card detail for the card stored at an index on a controller.")
        '("put-card"              examples:put-card              "Adds or updates the card detail stored on a controller.")
        '("delete-card"           examples:delete-card           "Deletes a card from a controller.")
        '("delete-cards"          examples:delete-cards          "Deletes all cards from a controller.")
        '("get-event-index"       examples:get-event-index       "Retrieves the current event index from a controller.")
        '("set-event-index"       examples:set-event-index       "Sets the current event index on a controller.")
        '("get-event"             examples:get-event             "Retrieves the event at the index from a controller.")
        '("record-special-events" examples:record-special-events "Enables/disables recording additional events for a controller.")
        '("get-time-profile"      examples:get-time-profile      "Retrieves a time profile from a controller.")
        '("set-time-profile"      examples:set-time-profile      "Adds or updates a time profile on a controller.")
))


(defun main () ""
  (let ((args (parse-command-line))
        (cmds (commands)))
    (if (not args)
        (usage)
        (let ((arg (first args)))
          (if (string= arg "help")   
              (help)
              (block execute
                (progn 
                  (loop for (cmd fn) in cmds              
                        do (when (string= cmd arg) 
                                 (progn
                                    (funcall fn)
                                    (return-from execute t))))
                  (progn
                    (format t "~%   *** ERROR: invalid command (~a)~%"  arg) 
                    (usage)))))))))


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
      do (format t "     ~17,a ~a~%" (first cmd) (third cmd)))
    (format t "~%")))


; Workaround to skip command line arguments for REPL - invoking (main) in the REPL is
; peculiarly pointless so:
; - if *unproccessed-command-line-arguments* is not NIL just discard them
; - if (car *command-line-arguments*) is not (example) just discard them.
;
; Ref. https://github.com/Clozure/ccl/issues/177
(defun parse-command-line () ""
  (let ((args       *command-line-argument-list*)
        (executable "examples"))
    (cond (*unprocessed-command-line-arguments* ())
          ((eq (search executable (car args)) NIL) ())
          ((/= (+ (coerce (search executable (car args)) 'fixnum) (length executable)) (length (car args))) ())
          (t (cdr args)))))


(defun make-app () ""
  (save-application "examples" :toplevel-function #'main :prepend-kernel t))

