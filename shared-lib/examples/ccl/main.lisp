(load "../../bindings/ccl/packages.lisp")
(load "../../bindings/ccl/uhppoted.lisp")
(load "packages.lisp")
(load "examples.lisp")

(defun usage () ""
  (format t "~%Usage: ccl64 --load main.lisp [-- command]~%") 
  (format t "~%") 
  (format t "  Supported commands:~%") 
  (format t "~%") 
  (format t "    help~%") 
  (format t "    get-device~%") 
  (format t "    set-address~%") 
  (format t "    get-status~%") 
  (format t "    get-status~%") 
  (format t "~%"))

(defun help () ""
  (format t "~%  Examples:~%~{    ~a~^~%~}~%~%" 
           (list "(get-devices)"
                 "(get-device)"
                 "(set-address)"
                 "(get-status)"
)))

(defun get-devices () ""
  (format t "~%  get-devices:~%~{    ~a~^~%~}~%~%" (coerce (examples:get-devices) 'list)))

(defun get-device () ""
  (format t "  get-device:~%    ~:w~%" (examples:get-device 405419896)))

(defun set-address () ""
  (format t "  set-address:~%    ~a~%" (examples:set-address 405419896 "192.168.1.125" "255.255.255.254" "192.168.1.5")))

(defun get-status () ""
  (format t "  get-status:~%    ~:w~%" (examples:get-status 405419896)))

(defun main () ""
  (loop for arg in *unprocessed-command-line-arguments*
    do (progn
         (cond ((string= arg "get-devices") (get-devices))
               ((string= arg "get-device")  (get-device))
               ((string= arg "set-address") (set-address))
               ((string= arg "get-status")  (get-status))
               ((string= arg "help")        (help))
               (t (usage))))))

(defun make-app () ""
  (save-application "examples" :toplevel-function #'main :prepend-kernel t))

