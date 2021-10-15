(uiop:define-package #:com.andrewsoutar.cl-wayland-client/core
  (:use #:cl #:cffi)
  (:export #:libwayland-client)
  (:export #:wayland-proxy #:pointer #:version #:wayland-interface #:dispatch-wayland-event)
  (:export #:find-proxy #:set-proxy-pointer #:destroy-proxy)
  (:export #:wayland-array-arg #:size #:data)
  (:export #:wayland-argument #:int #:uint #:fixed #:string #:object #:array #:fd)
  (:export #:wayland-interface #:name #:version)
  (:export #:wl-proxy-marshal-array #:wl-proxy-marshal-array-constructor-versioned))
(cl:in-package #:com.andrewsoutar.cl-wayland-client/core)

(define-foreign-library libwayland-client
  (t (:default "libwayland-client")))
(use-foreign-library libwayland-client)


(defclass wayland-proxy ()
  ((pointer :type foreign-pointer :reader pointer)
   (version :type (unsigned-byte 32) :reader version)))

(defgeneric wayland-interface (proxy-object))

(defgeneric dispatch-wayland-event (target opcode arguments))


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


(defcfun (wl-proxy-add-dispatcher :library libwayland-client) :int
  (proxy :pointer) (dispatcher :pointer) (implementation :pointer) (data :pointer))

(defcfun (wl-proxy-destroy :library libwayland-client) :void
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
  (let ((pointer (pointer proxy)))
    (when destroy-pointer
      (wl-proxy-destroy pointer))
    (remhash (pointer-address pointer) *lisp-proxies*))
  (slot-makunbound proxy 'pointer))


;;; HACK
(defctype size-t #+64-bit :uint64 #+32-bit :uint32)

(defcstruct wayland-array-arg
  (size size-t)
  (alloc size-t)
  (data :pointer))
(defcunion wayland-argument
  (int :int32)
  (uint :uint32)
  (fixed :int32)
  (string :string)
  (object :pointer)
  (array (:pointer (:struct wayland-array-arg)))
  (fd :int32))

(defcstruct wayland-interface
  (name :string)
  (version :int)
  ;; Don't need the rest
  )

(defmethod initialize-instance :after ((proxy wayland-proxy) &key version &allow-other-keys)
  (when (and version (not (slot-boundp proxy 'version)))
    (setf (slot-value proxy 'version)
          (min version (foreign-slot-value (wayland-interface proxy) '(:struct wayland-interface) 'version)))))

(defcfun (wl-proxy-marshal-array :library libwayland-client) :void
  (proxy :pointer) (opcode :uint32) (arguments (:pointer (:union wayland-argument))))

(defcfun (wl-proxy-marshal-array-constructor-versioned :library libwayland-client) :pointer
  (proxy :pointer) (opcode :uint32) (arguments (:pointer (:union wayland-argument)))
  (interface :pointer) (version :uint32))
