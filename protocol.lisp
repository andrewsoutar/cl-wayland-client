(uiop:define-package #:com.andrewsoutar.cl-wayland-client
  (:nicknames #:com.andrewsoutar.cl-wayland-client/protocol)
  (:use #:cl #:cffi #:com.andrewsoutar.cl-wayland-client/core)
  (:use-reexport #:com.andrewsoutar.cl-wayland-client.protocol/wayland)
  (:export #:wl-display-connect #:wl-display-disconnect #:wl-display-dispatch #:wayland-destroy
           #:wl-display-flush))
(cl:in-package #:com.andrewsoutar.cl-wayland-client/protocol)

(defcfun (%wl-display-connect "wl_display_connect" :library libwayland-client) :pointer
  (name :string))
(defun wl-display-connect (name)
  (set-proxy-pointer
   (make-instance 'wl-display :version 0)
   (let ((pointer (%wl-display-connect (or name (null-pointer)))))
     (when (null-pointer-p pointer)
       (error "Couldn't connect to ~:[default wayland display~;wayland display ~:*~A~]" name))
     pointer)
   :install-dispatcher nil))

(defcfun (%wl-display-disconnect "wl_display_disconnect" :library libwayland-client) :void
  (display :pointer))
(defun wl-display-disconnect (display)
  (%wl-display-disconnect (pointer display))
  (destroy-proxy display :destroy-pointer nil)
  (values))

(defcfun (%wl-display-dispatch "wl_display_dispatch" :library libwayland-client) :int
  (display :pointer))
(defun com.andrewsoutar.cl-wayland-client/protocol:wl-display-dispatch (display)
  (let ((ret (%wl-display-dispatch (pointer display))))
    (if (minusp ret)
        (error "Error dispatching events")
        ret)))

(defcfun (%wl-display-flush "wl_display_flush") :int
  (display :pointer))
(defun wl-display-flush (display)
  (let ((ret (%wl-display-flush (pointer display))))
    (if (minusp ret)
        (error "Error flushing display queue")
        ret)))
