(uiop:define-package #:com.andrewsoutar.cl-wayland-client/egl
    (:use #:cl #:cffi)
  (:export #:libwayland-egl)
  (:export #:wl-egl-window-create
           #:wl-egl-window-destroy
           #:wl-egl-window-resize
           #:wl-egl-window-get-attached-size))

(cl:in-package #:com.andrewsoutar.cl-wayland-client/egl)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-foreign-library libwayland-egl
    (t (:default "libwayland-egl"))))

(use-foreign-library libwayland-egl)

(defcfun ("wl_egl_window_create" :library libwayland-egl) :pointer
  (surface :pointer)
  (width :int)
  (height :int))

(defcfun ("wl_egl_window_destroy" :library libwayland-egl) :void
  (egl-window :pointer))

(defcfun ("wl_egl_window_resize" :library libwayland-egl) :void
  (egl-window :pointer)
  (width :int)
  (height :int)
  (dx :int)
  (dy :int))

(defcfun ("wl_egl_window_get_attached_size" :library libwayland-egl) :void
  (egl-window :pointer)
  (width (:pointer :int))
  (height (:pointer :int)))
