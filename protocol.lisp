(uiop:define-package #:com.andrewsoutar.cl-wayland-client
  (:nicknames #:com.andrewsoutar.cl-wayland-client/protocol)
  (:use #:cl)
  (:import-from #:cffi)
  (:import-from #:com.andrewsoutar.cl-wayland-client/core #:wayland-destroy)
  (:import-from #:com.andrewsoutar.cl-wayland-client/codegen)
  (:export #:wl-display-connect #:wl-display-disconnect #:wl-display-dispatch #:wayland-destroy))
(uiop:define-package #:com.andrewsoutar.cl-wayland-client/protocol/helpers
  (:use #:cl)
  (:import-from #:cffi #:defcfun #:null-pointer #:null-pointer-p)
  (:import-from #:com.andrewsoutar.cl-wayland-client/core #:libwayland-client #:pointer #:set-proxy-pointer #:destroy-proxy))
(cl:in-package #:com.andrewsoutar.cl-wayland-client/protocol)

(com.andrewsoutar.cl-wayland-client/codegen:define-from-xml "/usr/share/wayland/wayland.xml" :export)

(cl:in-package #:com.andrewsoutar.cl-wayland-client/protocol/helpers)

(defcfun (wl-display-connect :library libwayland-client) :pointer
  (name :string))
(defun com.andrewsoutar.cl-wayland-client/protocol:wl-display-connect (name)
  (set-proxy-pointer
   (make-instance 'com.andrewsoutar.cl-wayland-client/protocol:wl-display :version 0)
   (let ((pointer (wl-display-connect (or name (null-pointer)))))
     (when (null-pointer-p pointer)
       (error "Couldn't connect to ~:[default wayland display~;wayland display ~:*~A~]" name))
     pointer)
   :install-dispatcher nil))

(defcfun (wl-display-disconnect :library libwayland-client) :void
  (display :pointer))
(defun com.andrewsoutar.cl-wayland-client/protocol:wl-display-disconnect (display)
  (wl-display-disconnect (pointer display))
  (destroy-proxy display :destroy-pointer nil)
  (values))

(defcfun (wl-display-dispatch :library libwayland-client) :int
  (display :pointer))
(defun com.andrewsoutar.cl-wayland-client/protocol:wl-display-dispatch (display)
  (let ((ret (wl-display-dispatch (pointer display))))
    (if (minusp ret)
        (error "Error dispatching events")
        ret)))
