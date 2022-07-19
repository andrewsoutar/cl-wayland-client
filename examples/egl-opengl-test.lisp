;;; This is the wl_shm example adapted to use EGL for its surface buffer.


;; (ql:quickload '(cl-egl cl-opengl
;;                 com.andrewsoutar.cl-wayland-client
;;                 com.andrewsoutar.cl-wayland-client.protocol/stable/xdg-shell/xdg-shell
;;                 com.andrewsoutar.cl-wayland-client/egl))

(uiop:define-package #:wayland-tests
    (:use #:common-lisp #:cffi
          #:com.andrewsoutar.cl-wayland-client
          #:com.andrewsoutar.cl-wayland-client.protocol/stable/xdg-shell/xdg-shell)
  (:local-nicknames (#:wl-egl #:com.andrewsoutar.cl-wayland-client/egl)
                    (#:wl-core #:com.andrewsoutar.cl-wayland-client/core)))

(cl:in-package #:wayland-tests)
;;; Load the core protocol and the xdg-shell extension (from
;;; /usr/share/wayland-protocols/stable/xdg-shell/xdg-shell.XML).

;;; Apologies for the long example; Wayland can be rather verbose.

;;; Create a registry object which records all globals
(defclass recording-registry (wl-registry)
  ((globals :type list :accessor globals :initform ())))

;;; React to events on the registry
(defmethod wl-registry-global ((self recording-registry) name interface version)
  (push (list name interface version) (globals self)))

(defmethod wl-registry-global-remove ((self recording-registry) name)
  (setf (globals self) (delete name (globals self) :key #'first)))

(defun wl-registry-find-or-lose (registry interface &optional version)
  (or (dolist (global (globals registry))
        (destructuring-bind (gname ginterface gversion) global
          (when (and (equal ginterface interface)
                     (or (null version) (>= gversion version)))
            (return (values gname gversion)))))
      (error "Wayland: could not find interface ~A~@[ version ~A~] in registry"
             interface version)))


(defparameter *devices* nil)

(defclass recording-output (wl-output) ())

(defmethod wl-output-geometry ((output recording-output) x y physical-width physical-height
                               subpixel make model transform)
  (push (list :geometry x y physical-width physical-height
              subpixel make model transform)
        *devices*)
  (format t "wl-output-geometry received ~a~%" *devices*))

(defmethod wl-output-mode ((output recording-output) flags width height
                           refresh)
  (push (list :mode flags width height refresh) *devices*))

(defmethod wl-output-scale ((output recording-output) scale-factor)
  (push (list :scale scale-factor) *devices*))

(defmethod wl-output-done ((output recording-output))
  (format t "ALL OUTPUT EVENTS FINISHED ~a~%" *devices*))

;;; Create a xdg_wm_base subclass which responds to pings
(defclass xdg-wm-base-pingpong (xdg-wm-base) ())

;;; Every time we receive a ping, send back a pong
(defmethod xdg-wm-base-ping ((self xdg-wm-base-pingpong) serial)
  (xdg-wm-base-pong self serial))

;;; wl-callbacks created from this class will call the callback fun
(defclass invoking-callback (wl-callback)
  ((fun :type (function ((unsigned-byte 32)) *) :accessor fun :initarg :fun)))

(defmethod wl-callback-done ((self invoking-callback) data)
  (funcall (fun self) data))

(defun roundtrip (display)
  "Wait for all previous requests to be processed by the wayland compositor"
  (let (callback done-p)
    (unwind-protect
         (flet ((set-done (x)
                  (declare (ignore x))
                  (setf done-p t)))
           ;; This request simply invokes the provided callback as
           ;; soon as it's processed. Since Wayland processes requests
           ;; in order, it won't be processed until all prior requests
           ;; are done being processed.
           (setf callback (wl-display-sync display (make-instance 'invoking-callback :fun #'set-done)))
           (loop until done-p do (wl-display-dispatch display)))
      (when callback (wayland-destroy callback)))))


;;; Something to keep track of the window we're going to create
(defvar *window*)
(defclass window (wl-surface)
  ((open-p :initform t :accessor open-p)
   (needs-redraw-p :initform nil :accessor needs-redraw-p)))

(defvar *egl-window*)

(defclass window-xdg-surface (xdg-surface) ())

(defmethod xdg-surface-configure ((self window-xdg-surface) serial)
  ;; We handle configuration events immediately, so we can acknowledge
  ;; right away
  (setf (needs-redraw-p *window*) t)
  (xdg-surface-ack-configure self serial))

(defmethod xdg-surface-configure :after (surface serial)
  (format t "xdg surface configured ~a  surface: ~a~%" serial surface))

(defclass window-xdg-toplevel (xdg-toplevel) ())
(defmethod xdg-toplevel-configure ((self window-xdg-toplevel) width height states)
  (declare (ignore states))
  (format t "toplevel configure event: w: ~a  h: ~a~%" width height)
  (wl-egl:wl-egl-window-resize *egl-window* width height 0 0)
  (setf (needs-redraw-p *window*) t))

(defmethod xdg-toplevel-configure :after (proxy width height states)
  (format t "generic toplevel configure event fired: w: ~a  h: ~a
  :target ~a
  :states ~a~%" width height proxy states))

(defmethod xdg-toplevel-close ((self window-xdg-toplevel))
  (setf (open-p *window*) nil))

(defun create-native-window (width height compositor)
  "Our fn for creating a wl-egl-window for the wl-compositor"
  (let ((region
          (wl-compositor-create-region compositor (make-instance 'wl-region))))
    (wl-region-add region 0 0 width height)
    (wl-surface-set-opaque-region *window* region)
    (wl-egl:wl-egl-window-create (wl-core:pointer *window*) width height)))

(defun create-egl-context (native-display)
  "Initialize an EGL context and make it active"
  (let* ((egl-display (egl:get-display (wl-core:pointer native-display))))
    (format t "egl init successful! version: (major minor) ~s~%"
            (multiple-value-list (egl:initialize egl-display)))
    (egl:bind-api :opengl-api)

    (let* ((config (egl:choose-config egl-display 1
                                      :surface-type :window-bit
                                      :renderable-type :opengl-bit
                                      :red-size 8
                                      :green-size 8
                                      :blue-size 8
                                      :none))
           (surface (egl:create-window-surface egl-display
                                               (first config)
                                               *egl-window*
                                               (null-pointer)))
           (context (egl:create-context egl-display
                                        (first config)
                                        (null-pointer)
                                        :context-major-version 2
                                        :none)))
      (egl:make-current egl-display surface surface context)
      (values egl-display surface context))))

(defparameter *height* 400)
(defparameter *width* 600)

(defun refresh-window (egl-display egl-surface)
  (egl:swap-buffers egl-display egl-surface))

(defun init-gl ()
  (gl:clear-color 0.3 0.3 0.3 1.0)
  (gl:ortho 0.0 *width* 0.0 *height* -1.0 1.0))

(defun draw-gl ()
  (gl:clear :color-buffer-bit)
  (let ((stride 8))
    (loop for y from 0 below *height* by stride do
      (loop for x from 0 below *width* by stride
            for x+stride = (+ x stride)
            for y+stride = (+ y stride)
            do
               (if (zerop (mod (+ (floor x stride) (floor y stride)) 2))
                   (gl:color 0.4 0.4 0.4)
                   (gl:color 0.933 0.933 0.933))
               (gl:with-primitives :quads
                 (gl:vertex x y)
                 (gl:vertex x y+stride)
                 (gl:vertex x+stride y+stride)
                 (gl:vertex x+stride y)))))
  (gl:flush))

(defun main ()
  (let #1=(display registry device mode compositor egl-display egl-surface egl-context
                   wm-base xdg-surface xdg-toplevel)
    (unwind-protect
         (progn
           ;;
           ;; Connect to the default display
           (setf display (wl-display-connect nil))
           ;; Make a new recording-registry, and connect it to the
           ;; display. Since the registry is a new_id parameter, it is
           ;; returned from the request.
           (setf registry
                 (wl-display-get-registry display
                                          (make-instance 'recording-registry)))
           ;; Wait for all wl_registry_global callbacks to be received
           (roundtrip display)

           (format t  "~a~%" (globals registry))

           (setf device (wl-registry-bind
                         registry
                         (wl-registry-find-or-lose registry "wl_output" 3)
                         (make-instance 'recording-output :version 3)))

           ;; Boilerplate for creating a window:

           ;; We don't need to do anything special with the compositor
           ;; or the surface, so we can just use the base classes
           (setf compositor
                 (wl-registry-bind
                  registry
                  (wl-registry-find-or-lose registry "wl_compositor" 4)
                  (make-instance 'wl-compositor :version 4)))

           ;; Create the surface for the window itself - this is where
           ;; we'll draw everything
           (setf *window*
                 (wl-compositor-create-surface compositor
                                               (make-instance 'window)))

           ;; Bind the global xdg_wm_base to an instance of our pingpong
           ;; class
           (setf wm-base
                 (wl-registry-bind
                  registry
                  (wl-registry-find-or-lose registry "xdg_wm_base" 1)
                  (make-instance 'xdg-wm-base-pingpong :version 1)))

           ;; Create a xdg_surface for our window
           (setf xdg-surface
                 (xdg-wm-base-get-xdg-surface wm-base
                                              (make-instance 'window-xdg-surface)
                                              *window*))

           ;; Make the xdg_surface a toplevel window
           (setf xdg-toplevel
                 (xdg-surface-get-toplevel xdg-surface
                                           (make-instance 'window-xdg-toplevel)))

           (wl-surface-commit *window*)

           ;; Create EGL window and context
           (setf *egl-window*
                 (create-native-window *width* *height* compositor))

           (multiple-value-setq (egl-display egl-surface egl-context)
             (create-egl-context display))

           (init-gl)
           ;; main render loop
           (loop while (open-p *window*) do
             (let ((event (wl-display-dispatch display)))
               ;; (format t "dispatched event?: ~a~%~%" event)
               (when (needs-redraw-p *window*)
                 (format t "Drawing...~%")
                 (draw-gl)
                 (refresh-window egl-display egl-surface)
                 (wl-surface-commit *window*)
                 (setf (needs-redraw-p *window*) nil)))))

      ;; Clean up all objects in reverse order that they were created
      (dolist (obj #.`(list ,@(reverse '#1#)))
        (when obj
          (cond ((eql obj display)
                 (wl-display-disconnect obj))
                ((eql obj egl-surface)
                 (egl:destroy-surface egl-display egl-surface))
                ((eql obj egl-context)
                 (egl:destroy-context egl-display egl-context))
                ((eql obj egl-display)
                 (egl:terminate egl-display))
                (t (wayland-destroy obj))))))))
