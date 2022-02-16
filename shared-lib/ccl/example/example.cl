(open-shared-library "libuhppoted.so")

(defun uhppoted-externs () ""
   (external "Woot")
)

(defun demo () ""                                                  
   (external-call "Woot" :void)
)

(defun woot () ""
   (with-macptrs ((v (external-call "Woot" :address)))
       (unless (%null-ptr-p v)
         (go-err (go-string v))
       )
   )
)

(defun debug () "" 
   (woot)
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

