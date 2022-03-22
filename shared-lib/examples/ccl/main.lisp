(load "../../bindings/ccl/packages.lisp")
(load "../../bindings/ccl/uhppoted.lisp")
(load "packages.lisp")
(load "examples.lisp")

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
  (format t "  get-device:~%    ~a~%" (examples:get-device 405419896)))

(defun set-address () ""
  (format t "  set-address:~%    ~a~%" (examples:set-address 405419896 "192.168.1.125" "255.255.255.254" "192.168.1.5")))

(defun get-status () ""
  (format t "  get-status:~%    ~a~%" (examples:get-status 405419896)))

(defun main () ""
  (loop for arg in *unprocessed-command-line-arguments*
    do (progn
         (cond ((string= arg "get-devices") (get-devices))
               ((string= arg "get-device")  (get-device))
               ((string= arg "set-address") (set-address))
               ((string= arg "get-status")  (get-status))
               ((string= arg "help")        (help))
               (t (help))))))

(defun make-app () ""
  (save-application "examples" :toplevel-function #'main :prepend-kernel t))

