(load "../../bindings/ccl/packages.lisp")
(load "../../bindings/ccl/uhppoted.lisp")
(load "packages.lisp")
(load "examples.lisp")

(defun usage () ""
  (format t "~%  Usage: ./examples <command>~%") 
  (format t "~%") 
  (format t "    Supported commands:~%") 
  (format t "      get-devices   Retrieves a list of devices on the local LAN~%") 
  (format t "      get-device    Retrieves the information for a UHPPOTE controller~%") 
  (format t "      set-address   Sets a controller IP address, subnet mask and gateway address~%") 
  (format t "      get-status    Retrieves a controller status~%") 
  (format t "~%"))

(defun help () ""
  (format t "~%  Examples:~%~{    ~a~^~%~}~%~%" 
           (list "(get-devices)"
                 "(get-device)"
                 "(set-address)"
                 "(get-status)")))

(defun get-devices () ""
  (format t "~%  get-devices:~%~{    ~a~^~%~}~%~%" (coerce (examples:get-devices) 'list)))

(defun get-device () ""
  (format t "  get-device:~%    ~:w~%" (examples:get-device 405419896)))

(defun set-address () ""
  (format t "  set-address:~%    ~a~%" (examples:set-address 405419896 "192.168.1.125" "255.255.255.254" "192.168.1.5")))

(defun get-status () ""
  (format t "  get-status:~%    ~:w~%" (examples:get-status 405419896)))

;;;; Workaround to get command line arguments for executable. Assumes you're running it 
;;;; as ./examples <commmand> and just treats everything after the first *command-line-argument*
;;;; as the command(s) to be executed. Not going to work for every variation but 'good enough'
;;;; for now.
;;;;
;;;; Ref. https://github.com/Clozure/ccl/issues/177
(defun main () ""
  (cond (*unprocessed-command-line-arguments* (main-x *unprocessed-command-line-arguments*))
        (t (let ((commands (parse-command-line)))
                (cond ((eq commands NIL) (usage))
                      (t (main-x commands)))))))

(defun parse-command-line () ""
  (cond ((eq *command-line-argument-list* NIL) ())
        (t (cdr *command-line-argument-list*))
  )
)

(defun main-x (commands) ""
  (loop for arg in commands
    do (progn
         (cond ((string= arg "get-devices") (get-devices))
               ((string= arg "get-device")  (get-device))
               ((string= arg "set-address") (set-address))
               ((string= arg "get-status")  (get-status))
               ((string= arg "help")        (help))
               (t (help))))))

(defun make-app () ""
  (save-application "examples" :toplevel-function #'main :prepend-kernel t))

