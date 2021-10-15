(uiop:define-package #:com.andrewsoutar.cl-wayland-client/codegen
  (:use #:cl #:alexandria #:cffi)
  (:use #:com.andrewsoutar.cl-wayland-client/utils #:com.andrewsoutar.cl-wayland-client/core)
  (:export #:define-from-xml #:define-enum #:define-request #:define-event))
(cl:in-package #:com.andrewsoutar.cl-wayland-client/codegen)

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
                          (export '(,marshal-fun-name ,unmarshal-fun-name))))))))

(defmacro define-request ((interface-name request-name &key opcode description destructor-p since export)
                             &body args)
  (do ((function-name (intern (lispify interface-name request-name)))
       (remaining-args args (rest remaining-args))
       (arg-index 0 (1+ arg-index))
       (n-arrays 0)
       (lambda-list (make-collector))
       (arg-lisp-types (make-collector))
       (array-pointer-bindings (make-collector))
       (arg-setup-forms (make-collector))
       (object (gensym "OBJECT"))
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
                                    (thunk array))))))
                (nest ,@(collect array-pointer-bindings)
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
          ,@(when export `((eval-when (:compile-toplevel :load-toplevel :execute)
                             (export ',function-name))))))
    (destructuring-bind (name type &key enum interface allow-null-p &allow-other-keys
                         &aux (var (gensym (lispify name))))
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
        (array
         (let ((array-pointer (gensym (lispify name 'array-pointer)))
               (array-index (prog1 n-arrays (incf n-arrays))))
           (collect array-pointer-bindings
             (if allow-null-p
                 `(with-maybe-vector-data (,array-pointer ,var))
                 `(with-pointer-to-vector-data (,array-pointer ,var))))
           (collect arg-setup-forms
             `(when ,(if allow-null-p var t)
                (macrolet ((arr (&rest stuff)
                             `(c-access (:pointer (:struct wayland-array))
                                        ,',wayland-arrays (,',array-index) ,@stuff)))
                  (setf (arr :. size) (length ,var)
                        (arr :. alloc) (length ,var)
                        (arr :. data) ,array-pointer
                        (c-access (:pointer (:union wayland-argument))
                                  ,wayland-arguments (,arg-index) :. array)
                        (arr :&)))))))
        (new-id
         (assert (null (shiftf new-object-var var)))
         (if interface
             `(assert (pointer-eq
                       (foreign-symbol-pointer ,(format nil "~A_interface" interface) :library libwayland-client)
                       (wayland-interface ,var)))
             (progn
               (collect arg-setup-forms
                 `(setf (c-access (:pointer (:union wayland-argument)) ,wayland-arguments (,arg-index) :. string)
                        (c-access (:pointer (:struct wayland-interface)) (wayland-interface ,var) :-> name)
                        (c-access (:pointer (:union wayland-argument)) ,wayland-arguments (,(incf arg-index)) :. uint)
                        (version ,var)))
               (incf arg-index))))
        (t (collect arg-setup-forms
             `(when ,(if allow-null-p var t)
                (setf (c-access (:pointer (:union wayland-argument)) ,wayland-arguments (,arg-index) :. ,type)
                      ,(if enum
                           `(,(intern (multiple-value-bind (interface enum) (parse-dotted enum interface-name)
                                        (lispify interface 'marshal enum)))
                             ,var)
                           (ecase type
                             ((int uint string fd) var)
                             (fixed `(the integer (* ,var 256)))
                             (object `(pointer ,var))))))))))))

(defmacro define-event ((interface-name event-name &key opcode description destructor-p since export) &body args)
  (declare (ignore since))
  (let ((function-name (intern (lispify interface-name event-name))))
    `(progn
       (defgeneric ,function-name (target ,@(mapcar (compose #'make-symbol #'lispify #'first) args)))
       (defmethod dispatch-wayland-event ((target ,(intern (lispify interface-name))) (opcode (eql ,opcode))
                                          arguments-pointer)
         ,@(when description `(,description))
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
                                     `(c-access (:pointer (:union wayland-argument)) arguments-pointer (,i) ,@stuff)))
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
                          (export ',function-name)))))))

(defmacro define-from-xml (xml-file &optional export)
  (macrolet ((maybe-pop-node (node-list &rest tags &environment env)
               (multiple-value-bind (vars vals store-vars writer-form reader-form)
                   (get-setf-expansion node-list env)
                 `(multiple-value-bind (,@vars) (values ,@vals)
                    (let ((list ,reader-form))
                      (when (member (dom:tag-name (car list)) ',tags :test #'string=)
                        (multiple-value-bind (,@store-vars) (cdr list) ,writer-form)
                        (car list)))))))
    (labels ((text-contents (node)
               (when node
                 (apply 'concatenate 'string
                        (delete nil (map 'list (lambda (node)
                                                 (cond ((dom:comment-p node) ())
                                                       ((dom:text-node-p node) (dom:data node))
                                                       (t (cerror "Skip" "Non-text found: ~A" node) ""))) 
                                         (dom:child-nodes node))))))
             (child-elems (node)
               (loop for child across (dom:child-nodes node)
                     when (dom:element-p child) collect child))

             (compile-enum (interface-name enum)
               (let* ((entries (child-elems enum))
                      (description (text-contents (maybe-pop-node entries "description"))))
                 `(define-enum (,interface-name ,(get-attribute enum "name")
                                   :description ,(unless (equal description "") description)
                                   :bitfield-p ,(equal (get-attribute enum "bitfield") "true")
                                   :export ,export)
                    ,@(do-collecting ((entry entries))
                        (assert (string= (dom:tag-name entry) "entry"))
                        (list (make-keyword (lispify (get-attribute entry "name")))
                              (let ((value-str (get-attribute entry "value")))
                                (if (and (>= (length value-str) 2) (string= value-str "0x" :end1 2))
                                    (parse-integer value-str :radix 16 :start 2)
                                    (parse-integer value-str)))
                              (get-attribute entry "summary"))))))

             (compile-request-or-event (definer interface-name thing opcode)
               (let* ((args (child-elems thing))
                      (description (text-contents (maybe-pop-node args "description"))))
                 `(,definer (,interface-name ,(get-attribute thing "name")
                                             :opcode ,opcode
                                             :description ,(unless (equal description "") description)
                                             :destructor-p ,(equal (get-attribute thing "type") "destructor")
                                             :since ,(when-let ((since (get-attribute thing "since")))
                                                       (parse-integer since))
                                             :export ,export)
                            ,@(do-collecting ((arg args))
                                (assert (string= (dom:tag-name arg) "arg"))
                                (list (get-attribute arg "name")
                                      (if (string= (get-attribute arg "type") "new_id")
                                          'new-id
                                          (find (get-attribute arg "type")
                                                '(int uint fixed string object array fd)
                                                :key #'string-downcase :test #'equal))
                                      :enum (get-attribute arg "enum")
                                      :interface (get-attribute arg "interface")
                                      :allow-null-p (equal (get-attribute arg "allow-null") "true")))))))
      (let* ((root (dom:document-element (cxml:parse-file xml-file (cxml-dom:make-dom-builder))))
             (interfaces (child-elems root))
             (copyright (text-contents (maybe-pop-node interfaces "copyright")))
             (description (text-contents (maybe-pop-node interfaces "description"))))
        (declare (ignore copyright))
        `(progn
           ;; FIXME this doesn't really do anything except look good in the macroexpansion
           ,@(when description `(,description))
           ,@(do-collecting ((interface interfaces))
               (assert (string= (dom:tag-name interface) "interface"))
               (let* ((interface-name (get-attribute interface "name"))
                      (class-name (intern (lispify interface-name)))
                      (children (child-elems interface))
                      (description (text-contents (maybe-pop-node children "description"))))
                 `(progn
                    (defclass ,class-name (wayland-proxy) () ,@(when description `((:documentation ,description))))
                    ,@(when export `((eval-when (:compile-toplevel :load-toplevel :execute)
                                       (export ',class-name))))
                    (let ((interface (foreign-symbol-pointer ,(format nil "~A_interface" interface-name)
                                                             :library 'libwayland-client)))
                      (defmethod wayland-interface ((proxy-object ,class-name)) interface))
                    ,@(loop for child in children
                            for tag = (dom:tag-name child)
                            if (string= tag "request")
                              collect child into requests
                            else if (string= tag "event")
                                   collect child into events
                            else if (string= tag "enum")
                                   collect child into enums
                            else do (error "Unknown element ~A" child)
                            finally (return
                                      `(,@(loop for enum in enums
                                                collect (compile-enum interface-name enum))
                                        ,@(loop for opcode from 0
                                                for request in requests
                                                collect (compile-request-or-event 'define-request interface-name
                                                                                  request opcode))
                                        ,@(loop for opcode from 0
                                                for event in events
                                                collect (compile-request-or-event 'define-event interface-name
                                                                                  event opcode)))))))))))))
