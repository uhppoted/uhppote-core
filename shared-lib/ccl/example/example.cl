(open-shared-library "libuhppoted.so")

(defun uhppoted-externs () ""
   (external "GetDevicesN")
)

(defun demo () ""                                                  
   (external-call "GetDevicesN" :void)
)

(defun get-devices (N lp) ""
   (with-macptrs ((v (external-call "GetDevicesN" :address (%null-ptr) :address N :address lp :address)))
       (unless (%null-ptr-p v)
         (go-err (go-string v))
       )
   )
)

(defun debug () "" 
   (multiple-value-bind (l lp) (make-heap-ivector 10 '(unsigned-byte 32))
    (rlet ((N :signed-long 10))
         (get-devices N lp)
         (print (list "get-devices" (%get-signed-long N) l lp))
         "ok"
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

