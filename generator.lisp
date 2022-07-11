(uiop:define-package #:com.andrewsoutar.cl-wayland-client.generator
  (:nicknames #:com.andrewsoutar.cl-wayland-client.generator/generator)
  (:use #:cl #:alexandria #:asdf #:com.andrewsoutar.asdf-generated-system)
  (:use #:com.andrewsoutar.cl-wayland-client/utils #:com.andrewsoutar.cl-wayland-client/codegen)
  (:import-from #:uiop #:ensure-package #:define-package #:subpathname #:with-staging-pathname
                #:xdg-data-pathname)
  (:import-from #:cxml-dom)
  (:export #:protocol-system))
(cl:in-package #:com.andrewsoutar.cl-wayland-client.generator/generator)

(defun get-attribute (element name)
  (when-let ((attr (dom:get-attribute-node element name)))
    (dom:value attr)))

(defun generate-from-xml (xml-file package-name use-list)
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

             (parse-enum (interface-name enum)
               (let* ((entries (child-elems enum))
                      (description (text-contents (maybe-pop-node entries "description"))))
                 `((,interface-name ,(get-attribute enum "name")
                                    :description ,(unless (equal description "") description)
                                    :bitfield-p ,(equal (get-attribute enum "bitfield") "true")
                                    :export t)
                   ,@(do-collecting ((entry entries))
                       (assert (string= (dom:tag-name entry) "entry"))
                       (list (make-keyword (lispify (get-attribute entry "name")))
                             (let ((value-str (get-attribute entry "value")))
                               (if (and (>= (length value-str) 2) (string= value-str "0x" :end1 2))
                                   (parse-integer value-str :radix 16 :start 2)
                                   (parse-integer value-str)))
                             (get-attribute entry "summary"))))))

             (parse-request-or-event (interface-name thing opcode)
               (let* ((args (child-elems thing))
                      (description (text-contents (maybe-pop-node args "description"))))
                 `((,interface-name ,(get-attribute thing "name")
                                    :opcode ,opcode
                                    :description ,(unless (equal description "") description)
                                    :destructor-p ,(equal (get-attribute thing "type") "destructor")
                                    :since ,(when-let ((since (get-attribute thing "since")))
                                              (parse-integer since))
                                    :export t)
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
             (description (text-contents (maybe-pop-node interfaces "description")))
             (fixup-forms (make-collector))
             (*package* (ensure-package package-name :use use-list)))
        (declare (ignore copyright))
        `(progn
           ;; FIXME this doesn't really do anything except look good in the macroexpansion
           ,@(when description `(,description))
           ,@(do-collecting ((interface interfaces))
               (assert (string= (dom:tag-name interface) "interface"))
               (do* ((interface-name (get-attribute interface "name"))
                     (interface-version (parse-integer (get-attribute interface "version")))
                     (children (child-elems interface) (cdr children))
                     (description (text-contents (maybe-pop-node children "description")))
                     (enums (make-collector))
                     (requests (make-collector))
                     (request-opcode 0)
                     (events (make-collector))
                     (event-opcode 0))
                    ((endp children)
                     (collect fixup-forms
                       (labels ((strip-unnecessary-arg (arg)
                                  (destructuring-bind (name type &key allow-null-p interface &allow-other-keys) arg
                                    (declare (ignore name))
                                    `(,type ,allow-null-p ,interface)))
                                (strip-unnecessary-msg (thing)
                                  (destructuring-bind ((interface-name name &key since &allow-other-keys) &rest args)
                                      thing
                                    (declare (ignore interface-name))
                                    `(,name ,since ,@(mapcar #'strip-unnecessary-arg args)))))
                         `(initialize-interface ,interface-name ,interface-version
                            ,(mapcar #'strip-unnecessary-msg (collect requests))
                            ,(mapcar #'strip-unnecessary-msg (collect events)))))
                     `(progn
                        (define-interface ,interface-name :export t :description ,description)
                        ,@(do-collecting ((enum (collect enums))) `(define-enum ,@enum))
                        ,@(do-collecting ((request (collect requests))) `(define-request ,@request))
                        ,@(do-collecting ((event (collect events))) `(define-event ,@event))
                        ,@(unless (or (member "destroy" (collect requests) :key #'cadar :test #'string=)
                                      (string= interface-name "wl_display"))
                            (let ((destroy-name (intern (lispify interface-name 'destroy))))
                              `((defun ,destroy-name (object)
                                  (destroy-proxy object))
                                (eval-when (:compile-toplevel :load-toplevel :execute)
                                  (export ',destroy-name ',package-name)))))))
                 (let* ((child (first children))
                        (tag (dom:tag-name child)))
                   (cond ((string= tag "request")
                          (collect requests (parse-request-or-event interface-name child
                                                                    (prog1 request-opcode (incf request-opcode)))))
                         ((string= tag "event")
                          (collect events (parse-request-or-event interface-name child
                                                                  (prog1 event-opcode (incf event-opcode)))))
                         ((string= tag "enum")
                          (collect enums (parse-enum interface-name child)))
                         (t (error "Unknown element ~A" child))))))
           ,@(collect fixup-forms))))))


(defclass protocol-system (generated-system) ())

(defun wayland-xml-name (subname)
  (let ((subname-directory (if (string= subname "wayland")
                               "wayland/"
                               "wayland-protocols/")))
    (list
     (or (xdg-data-pathname
          (subpathname subname-directory subname :type "xml"))
         (error "Unable to locate protocol definition for ~S.~%~
                 Are you sure you installed the relevant Wayland protocol ~
                 definition file?~%~
                 (If you installed it to a nonstandard location, make sure ~
                 that the location is in your $XDG_DATA_DIRS.)"
                subname)))))

(defmethod generated-system-dependencies append ((s protocol-system) subname)
  (unless (equal subname "wayland") (list (concatenate 'string (component-name s) "/wayland"))))

(defmethod input-files ((o generate-op) (c generated-system))
  (multiple-value-bind (primary primary-name subname) (find-primary-system c)
    (declare (ignore primary primary-name))
    (when subname (wayland-xml-name subname))))

(defmethod perform ((o generate-op) (c generated-system))
  (multiple-value-bind (primary primary-name subname) (find-primary-system c)
    (declare (ignore primary-name))
    (when-let (in (input-files o c))
      (with-staging-pathname (tmp (output-file o c))
        (with-open-file
            (tmp tmp :direction :output :if-exists :overwrite
                     :element-type 'character
                     :external-format (component-external-format (find-component c "generated")))
          (let* ((package-name (string-upcase (component-name c)))
                 (use-list (mapcar #'string-upcase (generated-system-dependencies primary subname)))
                 (*package* (find-package :keyword)))
            (format tmp "~@{~S~%~}"
                    `(define-package ,package-name (:use ,@use-list))
                    `(define-package "COM.ANDREWSOUTAR.CL-WAYLAND-CLIENT/INTERFACE" (:use))
                    `(in-package ,package-name)
                    (generate-from-xml (first in) package-name use-list))))))))
