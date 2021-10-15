(uiop:define-package #:com.andrewsoutar.cl-wayland-client/helpers
  (:use #:cl #:cffi)
  (:use #:com.andrewsoutar.cl-wayland-client/core)
  (:export))
(cl:in-package #:com.andrewsoutar.cl-wayland-client/helpers)

(defcfun (%wl-display-connect "wl_display_connect" :library libwayland-client) :pointer
  (name :string))
(defun wl-display-connect (name)
  (set-proxy-pointer (make-instance )))
