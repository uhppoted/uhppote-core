(in-package uhppoted)

(open-shared-library (native-translated-namestring (make-pathname :directory (getenv "DYLD_LIBRARY_PATH") 
                                                                  :name "libuhppoted" 
                                                                  :type "so")))

(defstruct device
           ID 
           address
           subnet
           gateway
           MAC
           version
           date)

(defstruct status
           ID 
           timestamp
           doors
           buttons
           relays
           inputs
           syserror
           info
           seqno
           event
)

(defstruct event
           timestamp
           index
           type
           granted
           door
           direction
           card
           reason
)

(def-foreign-type nil
  (:struct :UDEVICE
           (:id      :int)
           (:address :address)))

(def-foreign-type nil
  (:struct :UDEVICES
           (:N       :int)
           (:devices :address)))

(def-foreign-type nil
  (:struct :UHPPOTE
           (:bind      :address)
           (:broadcast :address)
           (:listen    :address)
           (:timeout   :int)
           (:devices   :address)
           (:debug     :int)))

(def-foreign-type nil
  (:struct :GoDevice
           (:id      :unsigned-long)
           (:address :address)
           (:subnet  :address)
           (:gateway :address)
           (:MAC     :address)
           (:version :address)
           (:date    :address)))

(def-foreign-type nil
  (:struct :GoEvent
           (:timestamp :address)
           (:index     :unsigned-fullword)
           (:type      :unsigned-byte)
           (:granted   :unsigned-byte)
           (:door      :unsigned-byte)
           (:direction :unsigned-byte)
           (:card      :unsigned-fullword)
           (:reason    :unsigned-byte)
))

(def-foreign-type nil
  (:struct :GoStatus
           (:id        :unsigned-fullword)
           (:timestamp :address)
           (:doors     :address)
           (:buttons   :address)
           (:relays    :unsigned-byte)
           (:inputs    :unsigned-byte)
           (:syserror  :unsigned-byte)
           (:info      :unsigned-byte)
           (:seqno     :unsigned-fullword)
           (:event     :address)
))

(define-condition uhppoted-error (error)
   ((message :initarg :message :reader message)))

(defun go-error (cstr) "Converts a 'C' char * returned by the Go FFI to a string and frees the 'C' string"
   (with-macptrs ((p cstr))
      (%get-cstring p)))

(defun go-string (cstr) "Converts a 'C' char * returned by the Go FFI to a string and frees the 'C' string"
   (with-macptrs ((p cstr))
      (%get-cstring p)))

(defun go-sizeof (type) "" 
   (cond ((eq type :udevice) 16)
         (T 0)))

(defun uhppoted (f &key (bind-addr "") (broadcast-addr "") (listen-addr "") (timeout 5) (controllers NIL) (debug NIL)) ""
    (let ((cstrings ()))
       (%stack-block ((devices (* (length controllers) (go-sizeof :udevice)   )))
       (rletz ((udevices (:struct UDEVICES) :N (length controllers) :devices devices)
               (uhppote (:struct :UHPPOTE) 
                         :bind      (ccl::make-cstring bind-addr)
                         :broadcast (ccl::make-cstring broadcast-addr)
                         :listen    (ccl::make-cstring listen-addr)
                         :timeout   timeout
                         :devices   udevices
                         :debug     (cond (debug 1) (T 0))))

          (loop for (id addr) in controllers
              do (progn
                    (setf (pref devices :udevice.id) id)
                    (setf (pref devices :udevice.address) (ccl::make-cstring addr))
                    (push (pref devices :udevice.address) cstrings)
                          (%setf-macptr devices (%inc-ptr devices 16))))

          (unwind-protect
             (restart-case (funcall f uhppote)
                           (ignore () nil)
                           (use-value  (value) value)
                           (with-warning (err) (warn err)))
             (progn
                 (free (pref uhppote :UHPPOTE.bind))
                 (free (pref uhppote :UHPPOTE.broadcast))
                 (free (pref uhppote :UHPPOTE.listen))          
                 (loop for addr in cstrings 
                    do (free addr))))))))

(defun uhppoted-get-devices (uhppote &optional (N 16)) ""
   (destructuring-bind  (p q) (uhppoted-get-devices-n uhppote N)
      (cond ((>= N p) (subseq q 0 p))
             (T (uhppoted-get-devices uhppote (+ N 16))))))

(defun uhppoted-get-devices-n (uhppote max) ""
   (multiple-value-bind (array parray) (make-heap-ivector max '(unsigned-byte 32))
      (unwind-protect
         (rletz ((N :signed-long max))
            (with-macptrs ((err (external-call "GetDevices" 
                                               :address uhppote 
                                               :address N 
                                               :address parray 
                                               :address)))
               (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err))))
            (list (%get-signed-long N) array))
         (dispose-heap-ivector array))))


(defun uhppoted-get-device (uhppote device-id) ""
   (rletz ((device (:struct :GoDevice)))
      (with-macptrs ((err (external-call "GetDevice" 
                                         :address uhppote 
                                         :address device 
                                         :unsigned-long device-id 
                                         :address)))
         (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
         (make-device :id      (%get-unsigned-long device)
                      :address (go-string (pref device :GoDevice.address))
                      :subnet  (go-string (pref device :GoDevice.subnet))
                      :gateway (go-string (pref device :GoDevice.gateway))
                      :MAC     (go-string (pref device :GoDevice.MAC))
                      :version (go-string (pref device :GoDevice.version))
                      :date    (go-string (pref device :GoDevice.date))))))


(defun uhppoted-set-address (uhppote device-id ip-addr subnet-mask gateway-addr) ""
    (with-cstrs ((address ip-addr)
                 (subnet  subnet-mask)
                 (gateway gateway-addr))
       (with-macptrs ((err (external-call "SetAddress" :address uhppote 
                                                       :unsigned-long device-id 
                                                       :address address  
                                                       :address subnet
                                                       :address gateway
                                                       :address)))
          (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err))))))


(defun uhppoted-get-status (uhppote device-id) ""
   (%stack-block ((doors   4)
                  (buttons 4))
      (rletz ((event (:struct :GoEvent))
              (status (:struct :GoStatus)
                       :doors   doors
                       :buttons buttons
                       :event   event))
         (with-macptrs ((err (external-call "GetStatus" 
                                            :address uhppote 
                                            :address status
                                            :unsigned-long device-id 
                                            :address)))
            (unless (%null-ptr-p err) (error 'uhppoted-error :message (go-error err)))
            (make-status :id        (%get-unsigned-long status)
                         :timestamp (go-string (pref status :GoStatus.timestamp))
                         :doors     (list (%get-unsigned-byte doors 0)
                                          (%get-unsigned-byte doors 1)
                                          (%get-unsigned-byte doors 2)
                                          (%get-unsigned-byte doors 3))
                         :buttons   (list (%get-unsigned-byte buttons 0)
                                          (%get-unsigned-byte buttons 1)
                                          (%get-unsigned-byte buttons 2)
                                          (%get-unsigned-byte buttons 3))
                        :relays    (pref status :GoStatus.relays)
                        :inputs    (pref status :GoStatus.inputs)
                        :syserror  (pref status :GoStatus.syserror)
                        :info      (pref status :GoStatus.info)
                        :seqno     (pref status :GoStatus.seqno)
                        :event     (make-event :timestamp (go-string (pref event :GoEvent.timestamp))
                                               :index     (pref event :GoEvent.index)
                                               :type      (pref event :GoEvent.type)
                                               :granted   (pref event :GoEvent.granted)
                                               :door      (pref event :GoEvent.door)
                                               :direction (pref event :GoEvent.direction)
                                               :card      (pref event :GoEvent.card)
                                               :reason    (pref event :GoEvent.reason)))))))
(defun debug () "" 
   (handler-bind
      ((uhppoted-error
         #'(lambda (c) 
              (format t "~%   *** ERROR: ~a~%~%" (message c))
              (invoke-restart 'with-warning "oh noes i can has problems"))))
      (list "debug" (uhppoted #'(lambda (u) (uhppoted-get-status u 405419896))
                                            :bind-addr "qwerty"
                                            :debug T))))
