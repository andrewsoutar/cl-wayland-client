(uiop:define-package #:com.andrewsoutar.cl-wayland-client/core
  (:use #:cl #:cffi)
  (:export #:libwayland-client)
  (:export #:wayland-proxy #:pointer #:version #:wayland-interface #:dispatch-wayland-event #:wayland-destroy)
  (:export #:find-proxy #:set-proxy-pointer #:destroy-proxy)
  (:export #:wayland-array-arg #:size #:data)
  (:export #:wayland-argument #:int #:uint #:fixed #:string #:object #:array #:fd)
  (:export #:wayland-message #:name #:signature #:types)
  (:export #:wayland-interface #:name #:version #:method-count #:methods #:event-count #:events)
  (:export #:populate-wayland-interface #:clear-wayland-interface)
  (:export #:wl-proxy-marshal-array #:wl-proxy-marshal-array-constructor-versioned))
(cl:in-package #:com.andrewsoutar.cl-wayland-client/core)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library libwayland-client
    (:linux (:or "libwayland-client.so.0" (:default "libwayland-client")))
    (t (:default "libwayland-client"))))
(use-foreign-library libwayland-client)


(defclass wayland-proxy ()
  ((pointer :type foreign-pointer :reader pointer)
   (version :type (unsigned-byte 32) :reader version)))

(defgeneric wayland-interface (proxy-object))

(defgeneric dispatch-wayland-event (target opcode arguments))

(defgeneric wayland-destroy (object))


(defvar *lisp-proxies* (make-hash-table :test 'eql))

(defun find-proxy (pointer)
  (gethash (pointer-address pointer) *lisp-proxies*))

(defcallback wayland-event-dispatcher :int
    ((impl :pointer) (target :pointer) (opcode :uint32) (message :pointer) (arguments :pointer))
  (declare (ignore impl message))
  (let ((ret -1))
    (unwind-protect
         (progn (multiple-value-bind (lisp-target foundp) (find-proxy target)
                  (if foundp
                      (dispatch-wayland-event lisp-target opcode arguments)
                      (error "No wayland-proxy found for pointer: ~A" target)))
                (setf ret 0))
      (return-from wayland-event-dispatcher ret))))


(defcfun ("wl_proxy_add_dispatcher" :library libwayland-client) :int
  (proxy :pointer) (dispatcher :pointer) (implementation :pointer) (data :pointer))

(defcfun ("wl_proxy_destroy" :library libwayland-client) :void
  (proxy :pointer))

(defun set-proxy-pointer (proxy pointer &key (install-dispatcher t))
  (declare (type wayland-proxy proxy)
           (type foreign-pointer pointer))
  (when (null-pointer-p pointer)
    (error "Failed to allocate proxy for proxy object: ~A" proxy))
  (when install-dispatcher
    (unless (zerop (wl-proxy-add-dispatcher pointer (callback wayland-event-dispatcher)
                                            (null-pointer) (null-pointer)))
      (unwind-protect (error "Failed to add dispatcher to proxy: ~A" proxy)
        (wl-proxy-destroy pointer))))
  (setf (slot-value proxy 'pointer) pointer)
  (setf (gethash (pointer-address pointer) *lisp-proxies*) proxy))

(defun destroy-proxy (proxy &key (destroy-pointer t))
  (declare (type wayland-proxy proxy))
  (when (slot-boundp proxy 'pointer)
    (let ((pointer (pointer proxy)))
      (when destroy-pointer
        (wl-proxy-destroy pointer))
      (remhash (pointer-address pointer) *lisp-proxies*))
    (slot-makunbound proxy 'pointer)))

(defmethod wayland-destroy ((object wayland-proxy))
  (destroy-proxy object))


(defcstruct wayland-array-arg
  (size :size)
  (alloc :size)
  (data :pointer))
(defcunion wayland-argument
  (int :int32)
  (uint :uint32)
  (fixed :int32)
  (string :string)
  (object :pointer)
  (array (:pointer (:struct wayland-array-arg)))
  (fd :int32))

(defcstruct wayland-message
  (name :string)
  (signature :string)
  (types (:pointer :pointer)))
(defun populate-wayland-message (message name signature &rest types)
  (setf (mem-ref message '(:struct wayland-message))
        (list 'name (foreign-string-alloc name)
              'signature (foreign-string-alloc signature)
              'types (foreign-array-alloc (coerce types 'vector) `(:array :pointer ,(length types))))))
(defun clear-wayland-message (message)
  (with-foreign-slots (((:pointer name) (:pointer signature) (:pointer types))
                       message (:struct wayland-message))
    (foreign-string-free (mem-ref name :pointer))
    (foreign-string-free (mem-ref signature :pointer))
    (foreign-array-free (mem-ref types :pointer))))

(defcstruct wayland-interface
  (name :string)
  (version :int)
  (method-count :int)
  (methods (:pointer (:struct wayland-message)))
  (event-count :int)
  (events (:pointer (:struct wayland-message))))
(defun populate-wayland-interface (interface name version methods events)
  (flet ((alloc-messages (messages)
           (loop with array = (foreign-alloc '(:struct wayland-message) :count (length messages))
                 for i from 0
                 for message in messages
                 do (apply #'populate-wayland-message (mem-aptr array '(:struct wayland-message) i) message)
                 finally (return array))))
    (setf (mem-ref interface '(:struct wayland-interface))
          (list 'name (foreign-string-alloc name)
                'version version
                'method-count (length methods)
                'methods (alloc-messages methods)
                'event-count (length events)
                'events (alloc-messages events)))))
(defun clear-wayland-interface (interface)
  (with-foreign-slots (((:pointer name) method-count methods event-count events)
                       interface (:struct wayland-interface))
    (foreign-string-free (mem-ref name :pointer))
    (dotimes (i method-count)
      (clear-wayland-message (mem-aptr methods '(:struct wayland-message) i)))
    (foreign-free methods)
    (dotimes (i event-count)
      (clear-wayland-message (mem-aptr events '(:struct wayland-message) i)))
    (foreign-free events)))

(defmethod initialize-instance :after ((proxy wayland-proxy) &key version &allow-other-keys)
  (when (and version (not (slot-boundp proxy 'version)))
    (setf (slot-value proxy 'version)
          (min version (foreign-slot-value (wayland-interface proxy) '(:struct wayland-interface) 'version)))))

(defcfun ("wl_proxy_marshal_array" :library libwayland-client) :void
  (proxy :pointer) (opcode :uint32) (arguments (:pointer (:union wayland-argument))))

(defcfun ("wl_proxy_marshal_array_constructor_versioned" :library libwayland-client) :pointer
  (proxy :pointer) (opcode :uint32) (arguments (:pointer (:union wayland-argument)))
  (interface :pointer) (version :uint32))
