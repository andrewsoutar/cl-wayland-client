(uiop:define-package #:com.andrewsoutar.cl-wayland-client/codegen
  (:use #:cl #:alexandria #:cffi)
  (:import-from #:uiop #:define-package)
  (:use #:com.andrewsoutar.cl-wayland-client/utils #:com.andrewsoutar.cl-wayland-client/core)
  (:export #:define-interface #:initialize-interface #:define-enum #:define-request #:define-event)
  (:export #:int #:uint #:fixed #:string #:object #:new-id #:array #:fd))
(cl:in-package #:com.andrewsoutar.cl-wayland-client/codegen)

(define-package #:com.andrewsoutar.cl-wayland-client/interface (:use)
  (:documentation "Each symbol in this package represents a Wayland interface"))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun interface-var-name (interface-name)
    (intern (lispify interface-name) "COM.ANDREWSOUTAR.CL-WAYLAND-CLIENT/INTERFACE")))

(defmacro define-interface (interface-name &key export description)
  (let ((class-name (intern (lispify interface-name)))
        (interface-var (interface-var-name interface-name)))
    `(progn
       (defclass ,class-name (wayland-proxy) ()
         ,@(when description `((:documentation ,description))))
       ,@(when export `((eval-when (:compile-toplevel :load-toplevel :execute)
                          (export ',class-name ',(package-name *package*)))))
       (if (boundp ',interface-var)
           (when (get ',interface-var 'interface-initialized)
             (clear-wayland-interface (symbol-value ',interface-var)))
           (setf (symbol-value ',interface-var) (foreign-alloc '(:struct wayland-interface))))
       (setf (get ',interface-var 'interface-initialized) nil)
       (defmethod wayland-interface ((proxy-object ,class-name)) (symbol-value ',interface-var)))))

(defmacro initialize-interface (interface-name interface-version &body (requests events))
  (flet ((create-message (message)
           (destructuring-bind (name since &rest args) message
             ``(,,name
                ,,(apply #'concatenate 'string (format nil "~@[~A~]" since)
                         (mapcar (lambda (arg)
                                   (destructuring-bind (type allow-null-p interface) arg
                                     (format nil "~:[~;?~]~A" allow-null-p
                                             (ecase type
                                               (int "i")
                                               (uint "u")
                                               (fixed "f")
                                               (string "s")
                                               (object "o")
                                               (new-id (if interface "n" "sun"))
                                               (array "a")
                                               (fd "h")))))
                                 args))
                ,,@(mapcar (lambda (arg)
                             (if (third arg) `(symbol-value ',(interface-var-name (third arg))) '(null-pointer)))
                           args)))))
    (let ((interface-var (interface-var-name interface-name)))
      `(progn (populate-wayland-interface (symbol-value ',interface-var)
                                          ,interface-name ,interface-version
                                          `(,,@(mapcar #'create-message requests))
                                          `(,,@(mapcar #'create-message events)))
              (setf (get ',interface-var 'interface-initialized) t)))))

(defmacro define-enum ((interface-name enum-name &key description bitfield-p export) &body entries)
  (let* ((lisp-type (if bitfield-p '(unsigned-byte 32) 'integer))
         (marshal-fun-name (intern (lispify interface-name 'marshal enum-name)))
         (unmarshal-fun-name (intern (lispify interface-name 'unmarshal enum-name)))
         (keywords (mapcar #'first entries))
         (values (mapcar #'second entries))
         (docstring (format nil "~@[~A~:[~;~%~%~]~:*~]~:{~A (~A) : ~:[Undocumented~;~:*~A~]~:^~%~}"
                            description entries)))
    `(progn
       (declaim (ftype (function (t) ,lisp-type) ,marshal-fun-name))
       (defun ,marshal-fun-name (arg)
         ,(format nil "Marshal ~:[enum~;bitfield~] ~A::~A to an integer~@[~%~%~A~]"
                  bitfield-p interface-name enum-name docstring)
         (flet ((marshal-atom (arg)
                  (etypecase arg
                    (integer arg)
                    ,@(mapcar (lambda (keyword value) `((eql ,keyword) ,value)) keywords values))))
           ,(if bitfield-p
                `(if (listp arg) (reduce #'logior arg :key #'marshal-atom) (marshal-atom arg))
                `(marshal-atom arg))))

       (declaim (ftype (function (,lisp-type) (cons integer)) ,unmarshal-fun-name))
       (defun ,unmarshal-fun-name (arg)
         (declare (type ,lisp-type arg))
         ,(format nil "Unmarshal ~:[enum~;bitfield~] ~A::~A from an integer~%~
The CAR of the result is the original integer; the CDR is the ~
~:[associated keyword~;list of associated keywords~], if any~
~@[~%~%~A~]" bitfield-p interface-name enum-name bitfield-p docstring)
         ,(if bitfield-p
              `(let* ((ret (cons arg nil))
                      (last ret))
                 ,@(mapcar (lambda (keyword value)
                             `(when (logtest arg ,value) (setf last (setf (cdr last) (list ,keyword)))))
                           keywords values)
                 ret)
              `(cons arg (case arg ,@(mapcar (lambda (keyword value) `((,value) ,keyword)) keywords values)))))

       ,@(when export `((eval-when (:compile-toplevel :load-toplevel :execute)
                          (export '(,marshal-fun-name ,unmarshal-fun-name) ',(package-name *package*))))))))

(defmacro define-request ((interface-name request-name &key opcode description destructor-p since export)
                          &body args)
  (do ((function-name (intern (lispify interface-name request-name)))
       (remaining-args args (rest remaining-args))
       (arg-index 0 (1+ arg-index))
       (n-arrays 0)
       (lambda-list (make-collector))
       (arg-lisp-types (make-collector))
       (pointer-bindings (make-collector))
       (arg-setup-forms (make-collector))
       (object (make-symbol "OBJECT"))
       (wayland-arguments (gensym "WAYLAND-ARGUMENTS"))
       (wayland-arrays (gensym "WAYLAND-ARRAYS"))
       new-object-var)
      ((endp remaining-args)
       `(progn
          (declaim (ftype (function (wayland-proxy ,@(collect arg-lisp-types))
                                    ,(if new-object-var 'wayland-proxy '(values)))
                          ,function-name))
          (defun ,function-name (,object ,@(collect lambda-list))
            ,(format nil "Send request ~A::~A~@[~%~%~A~]" interface-name request-name description)
            ,@(when since `((when (< (version ,object) ,since)
                              (error "~A::~A requires version ~A, but only have version ~A"
                                     ,interface-name ,request-name ,since (version ,object)))))
            (with-foreign-objects ((,wayland-arguments '(:union wayland-argument) ,arg-index)
                                   ,@(unless (zerop n-arrays) `(,wayland-arrays (:struct wayland-array) ,n-arrays)))
              (macrolet ((with-maybe-vector-data ((pointer array) &body body)
                           `(flet ((thunk (,pointer) ,@body))
                              (let ((array ,array))
                                (if array
                                    (with-pointer-to-vector-data (pointer array) (thunk pointer))
                                    (thunk array)))))
                         (with-constant-maybe-string ((pointer string) &body body)
                           `(flet ((thunk (,pointer) ,@body))
                              (let ((string ,string))
                                (if string
                                    (with-constant-string (pointer string) (thunk pointer))
                                    (thunk string))))))
                (nest ,@(collect pointer-bindings)
                  (progn
                    ,@(collect arg-setup-forms)
                    (unwind-protect
                         ,(if new-object-var
                              `(set-proxy-pointer ,new-object-var
                                                  (wl-proxy-marshal-array-constructor-versioned
                                                   (pointer ,object) ,opcode ,wayland-arguments
                                                   (wayland-interface ,new-object-var)
                                                   (if (slot-boundp ,new-object-var 'version)
                                                       (version ,new-object-var)
                                                       (setf (slot-value ,new-object-var 'version)
                                                             (version ,object)))))
                              `(wl-proxy-marshal-array (pointer ,object) ,opcode ,wayland-arguments))
                      ,@(when destructor-p `((destroy-proxy ,object)))))
                  (values ,@(when new-object-var `(,new-object-var)))))))
          ,@(when (string= request-name "destroy")
              (assert destructor-p)
              (assert (null args))
              `((defmethod wayland-destroy ((object ,(intern (lispify interface-name))))
                  (,function-name object))))
          ,@(when export `((eval-when (:compile-toplevel :load-toplevel :execute)
                             (export ',function-name ',(package-name *package*)))))))
    (destructuring-bind (name type &key enum interface allow-null-p &allow-other-keys
                         &aux (var (make-symbol (lispify name))))
        (first remaining-args)
      (when enum (assert (member type '(int uint))))
      (when allow-null-p (assert (member type '(string object array))))
      (collect lambda-list var)
      (collect arg-lisp-types
        (let ((base-type (ecase type
                           (int '(signed-byte 32))
                           (uint '(unsigned-byte 32))
                           (fixed 'rational)
                           (string 'string)
                           ((object new-id) 'wayland-proxy)
                           (array '(simple-array (unsigned-byte 8) (*)))
                           (fd '(signed-byte 32)))))
          (cond (enum '*)
                (allow-null-p `(or ,base-type null))
                (t base-type))))
      (case type
        (string
         (let ((string-pointer (gensym (lispify name 'string-pointer))))
           (collect pointer-bindings
             (if allow-null-p
                 `(with-constant-maybe-string (,string-pointer ,var))
                 `(with-constant-string (,string-pointer ,var))))
           (collect arg-setup-forms
             `(setf (c-access (:union wayland-argument) ,wayland-arguments (,arg-index) :. string) ,string-pointer))))
        (array
         (let ((array-pointer (gensym (lispify name 'array-pointer)))
               (array-index (prog1 n-arrays (incf n-arrays))))
           (collect pointer-bindings
             (if allow-null-p
                 `(with-maybe-vector-data (,array-pointer ,var))
                 `(with-pointer-to-vector-data (,array-pointer ,var))))
           (collect arg-setup-forms
             `(macrolet ((arr (&rest stuff)
                           `(c-access (:struct wayland-array) ,',wayland-arrays (,',array-index) ,@stuff)))
                (setf (c-access (:union wayland-argument) ,wayland-arguments (,arg-index))
                      (if ,(if allow-null-p var t)
                          (progn (setf (arr :. size) (length ,var)
                                       (arr :. alloc) (length ,var)
                                       (arr :. data) ,array-pointer)
                                 (arg :&))
                          (null-pointer)))))))
        (new-id
         (assert (null (shiftf new-object-var var)))
         (if interface
             `(assert (pointer-eq
                       (foreign-symbol-pointer ,(format nil "~A_interface" interface) :library libwayland-client)
                       (wayland-interface ,var)))
             (progn
               (collect arg-setup-forms
                 `(setf (c-access (:union wayland-argument) ,wayland-arguments (,arg-index) :. string)
                        (c-access (:struct wayland-interface) (wayland-interface ,var) :-> name)
                        (c-access (:union wayland-argument) ,wayland-arguments (,(incf arg-index)) :. uint)
                        (version ,var)))
               (incf arg-index))))
        (t (collect arg-setup-forms
             `(setf (c-access (:union wayland-argument) ,wayland-arguments (,arg-index) :. ,type)
                    ,(if enum
                         `(,(intern (multiple-value-bind (interface enum) (parse-dotted enum interface-name)
                                      (lispify interface 'marshal enum)))
                           ,var)
                         (ecase type
                           ((int uint fd) var)
                           (fixed `(the integer (* ,var 256)))
                           (object `(if ,(if allow-null-p var t) (pointer ,var) (null-pointer))))))))))))

(defmacro define-event ((interface-name event-name &key opcode description destructor-p since export) &body args)
  (declare (ignore since))
  (let ((function-name (intern (lispify interface-name event-name))))
    `(progn
       ,(let ((lambda-args (mapcar (compose #'make-symbol #'lispify #'first) args)))
          `(defgeneric ,function-name (target ,@lambda-args)
             ,@(when description `((:documentation ,description)))
             (:method ((target ,(intern (lispify interface-name))) ,@lambda-args)
               (declare (ignore ,@lambda-args))
               nil)))
       (defmethod dispatch-wayland-event ((target ,(intern (lispify interface-name))) (opcode (eql ,opcode))
                                          arguments-pointer)
         ,@(when destructor-p `((destroy-proxy target)))
         (,function-name
          target
          ,@(loop for arg in args
                  for i from 0
                  collect (destructuring-bind (name type &key enum interface &allow-other-keys) arg
                            (declare (ignore name))
                            (when enum (assert (member type '(int uint))))
                            (cond ((eql type 'new-id) (assert interface))
                                  (interface (assert (eql type 'object))))
                            (flet ((getter (&rest stuff)
                                     `(c-access (:union wayland-argument) arguments-pointer (,i) ,@stuff)))
                              (if enum
                                  `(,(intern (multiple-value-bind (interface enum) (parse-dotted enum interface-name)
                                               (lispify interface 'unmarshal enum)))
                                    ,(getter :. type))
                                  (ecase type
                                    ((int uint string fd) (getter :. type))
                                    (fixed `(/ ,(getter :. type) 256))
                                    (object `(let ((p ,(getter :. type)))
                                               (unless (null-pointer-p p)
                                                 (or (find-proxy p)
                                                     (error "Could not find proxy for object at ~A" p)))))
                                    (new-id
                                     `(set-proxy-pointer (make-instance ',(intern (lispify interface))
                                                                        :version (version target))
                                                         ,(getter :. 'object)))
                                    (array
                                     `(cons ,(getter :. 'array :-> 'size) ,(getter :. 'array :-> 'data))))))))))
       ,@(when export `((eval-when (:compile-toplevel :load-toplevel :execute)
                          (export ',function-name ',(package-name *package*))))))))
