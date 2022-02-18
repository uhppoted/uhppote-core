(open-shared-library "libuhppoted-debug.so")

(defun uhppoted-externs () ""
   (external "GetDevices")
)

(define-condition on-uhppote-error (error)
   ((message :initarg :message :reader message))
)

(defun get-devices (N lp) ""
   (with-macptrs ((v (external-call "GetDevices" :address (%null-ptr) :address N :address lp :address)))
       (unless (%null-ptr-p v)
         (error 'on-uhppote-error :message (go-string v))
       )
   )
)

(defun debug () "" 
   (handler-bind
      ((on-uhppote-error
         #'(lambda (c) 
              (format t "*** ERROR: ~a" (message c))
              (invoke-restart 'return-value nil)
           )
         )
      )
      (multiple-value-bind (l lp) (make-heap-ivector 10 '(unsigned-byte 32))
         (rlet ((N :signed-long 10))
            (get-devices N lp)
            (print (list "get-devices" (%get-signed-long N) l lp))
            "ok"
         )
      )
   )
)

(defun debug-error () "" 
   (handler-bind
      ((on-uhppote-error
         #'(lambda (c) 
              (format t "*** ERROR: ~a" (message c))
              (invoke-restart 'return-value nil)
           )
         )
      )
      (multiple-value-bind (l lp) (make-heap-ivector 10 '(unsigned-byte 32))
         (rlet ((N :signed-long 10))
            (get-devices N (%null-ptr))
            (print (list "get-devices" (%get-signed-long N) l lp))
            "ok"
         )
      )
   )
)

(defun go-err (err) ""
   (format T "~% *** ERROR ~a~2%" err)
   nil
)

(defun go-string (cstr) "Converts a 'C' char * returned by the Go FFI to a string"
   (cond ((%null-ptr-p cstr) "")
         (T (format nil "~{~A~}" (go-string-char cstr 0)))
   )
)

(defun go-string-char (cstr ix) "Accumulates characters from a 'C' char * until it reaches the \0 terminator"
   (let ((ch (%get-unsigned-byte cstr ix)))
        (cond ((eq ch 0) ())
              (T (cons (code-char ch) (go-string-char cstr (+ ix 1))))
        )
   )
)

