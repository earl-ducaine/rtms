;;; -*- Mode:Common-Lisp; Package:RTMS; Base:10; Fonts:(*CODE-FONT* *COMMENT-FONT* *STRING-FONT*) -*-
;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved.
;;; DEFINE-ATT
;;;
;;; This file contains the following Explorer extensions to CommonLisp Standard as Indicated in the June 1985 Explorer Lisp
;;; Reference
;;;    deff
;;;
;;; The following function contains flavor references and thus are incompatable with CommonLisp. Their removal will not
;;; effect the functionality of RTMS.
;;;     destroy-att-flavor-hash
;;;     destroy-att-flavor-heap
;;;     destroy-att-flavor-avl
;;;     destroy-att-utility-redef-rel-flavor-heap
;;;     destroy-att-utility-redef-rel-flavor-hash
;;;     destroy-att-utility-redef-rel-flavor-avl
;;;
(deff defattr 'define-attribute)

(defun define-attribute (relation-name attribute-descriptor &rest keyword-list
 &key &optional key &allow-other-keys
 &aux (relation-tuple nil) (result nil) attr? attribute-list key-value rel-owner-id
 save-dir rel-implementation-type status rel-storage-structure (new-attrs nil))
  "Add a new attribute to a relation.
    All its tuples will get the default value of the attribute for the attribute value.

   RELATION-NAME - Name of the relation.
   ATTRIBUTE-DESCRIPTOR - List of attributes and their descriptions.
   KEY           - If the key for this relation is to be changed, specify it."
  key
 (block define-attribute
       ;;
       ;; Verify that the user is logged into a database before proceeding
       (if (not (active-database))
   (return-from define-attribute nil))
       ;;
       ;; Check the parameters for validity
       ;;
       (cond ((null (setf relation-name (validate-sym relation-name t)))
      (return-from define-attribute nil)))
       (cond ((not (listp attribute-descriptor))
      (if *provide-error-messages*
  (format *standard-output*
  "~%ERROR - Illegally specified attribute descriptor list, expected a list --> ~s"
  attribute-descriptor))
      (return-from define-attribute nil)))
       (setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
      ((or (null keyword-list)(not (listp (car keyword-list)))) keyword-list)))
       ;;
       ;; Get the system-relation tuple which defines the relation
       (setf relation-tuple (cadr (get-relation relation-name '("OWNER-ID" "TUPLE-FORMAT" "ATTRIBUTES"
     "DOMAINS" "KEY" "SAVE-DIRECTORY" "DOC"
     "IMPLEMENTATION-TYPE" "STORAGE-STRUCTURE")
   t)))
       (cond ((not relation-tuple)
      (if *provide-error-messages*
  (format *standard-output* "~%ERROR - The relation ~S is not defined in the database ~S"
  relation-name *active-db*))
      (return-from define-attribute nil)))
       ;; later, verify here that the user owns this relation
       (setf rel-owner-id (first relation-tuple)
     save-dir (sixth relation-tuple)
     ;;
     ;; Ignore attribute definition which attribute name already exists in the relation
     attribute-list (third relation-tuple))
       (setf attribute-descriptor
     (do ((attr-des attribute-descriptor (cdr attr-des)))
 ((null attr-des)  result)
       (setf attr? (car attr-des))
       (if (and (listp attr?) (string-equal "QUOTE" (first attr?)))
   (setf attr? (validate-sym attr? t)))
       (cond ((or (stringp attr?)(atom attr?))
      (setf attr? (string-upcase attr?))
      (cond ((member attr? attribute-list :test 'string-equal) ;e.g. attribute already defined
     (if *provide-error-messages*
  (format *standard-output*
   "~%WARNING - The attribute ~S already exists. It will not be added."
   attr?)))
    (t
     (setf new-attrs (cons attr? new-attrs))
     (setf result (append result
     (cons attr?
    (if (or (atom (cadr attr-des))
     (string-equal "QUOTE" (first (cadr attr-des))))
        nil
        (list (cadr attr-des))))))))))))
       (cond ((null attribute-descriptor)
      (if *provide-error-messages*
  (format *standard-output*
  "~%ERROR - Legal attribute descriptor not provided"))
      (return-from define-attribute nil)))
       (cond ((car keyword-list)
      (setf keyword-list (get-keyword-value-prereq '(key) keyword-list))
      (setf key-value (convert-attributes (car (get-keyword-value '(key) keyword-list))))
      (cond ((or (not key-value) (not (car key-value))
 (and (stringp key-value)(string-equal key-value "NIL"))
 (string-equal (car key-value) "NIL"))
     (setf key-value (fifth relation-tuple)))
    ((not (listp key-value))
     (setf key-value (list key-value))))
      (if (null
    (do ((key% key-value (cdr key%)))
((null key%) t)
      (cond ((and (not (member (car key%) attribute-list :test 'string-equal))
   (not (member (car key%) new-attrs :test 'string-equal)))
     (cond (*provide-error-messages*
     (format *standard-output*
      "~%ERROR - ~s is not an attribute of the ~s relation"
      (car key%) relation-name)
     (format *standard-output*
      "~%         and it is included illegally in the key list")))
     (return-from define-attribute nil)))))
  (return-from define-attribute nil))
      (setf rel-implementation-type (nth 7 relation-tuple)
    rel-storage-structure (nth 8 relation-tuple))
      (setf status (not (funcall (find-symbol (concatenate 'string "DEFATT-UTILITY-REDEF-REL-"
        rel-implementation-type "-"
        rel-storage-structure) *pkg-string*)
   relation-name attribute-descriptor relation-tuple key-value)))
      (if status
  (return-from define-attribute nil)))
     (t
      (setf rel-implementation-type (nth 7 relation-tuple)
    rel-storage-structure (nth 8 relation-tuple))
      (setf status (not (funcall (find-symbol (concatenate 'string "DEFATT-" rel-implementation-type "-"
        rel-storage-structure) *pkg-string*)
   relation-name attribute-descriptor relation-tuple nil)))

      (if status
  (return-from define-attribute nil))))
       (cond (*auto-save*
      (save-system-relations)
      (save-relation relation-name (list 'dir save-dir))))
       (if *provide-status-messages*
   (format *standard-output* "~%Attribute definition complete"))
       (return-from define-attribute relation-name)))

(defun delete-index-tuples (relation-name relation-implementation
    &aux attributes domains indices)
  ;;
  ;;  Must determine if there are any indices defined on this relation. If there are, the tuples must be deleted from the indices vefore they
  ;; are modified. After the modification is complete, the tuples modified are sent back. These must then be inserted.
  ;;
  (cond ((setf indices (retrieve 'system-index 'tuples t 'project '("INDEX-NAME" "KEY" "INDEX-TYPE")
  'where `(string-equal relation-name ,(string-upcase relation-name))))
 (setf attributes (car (funcall (find-symbol (concatenate 'string "RETRIEVE-"
       *system-relation-base-implementation*
       "-"
       *system-relation-storage-structure*)
        *pkg-string*)
  'system-relation *system-relation-attributes* '("ATTRIBUTES" "DOMAINS")
  *system-relation-key*
  `(string-equal relation-name ,(string-upcase relation-name))
  nil 'system-relation)))
 (setf domains (second attributes)
       attributes (first attributes))
 (mapc (function (lambda (index-element)
   (funcall (find-symbol (concatenate 'string "DELETE-" relation-implementation "-"
          (third index-element)) *pkg-string*)
     relation-name attributes (second index-element) domains t
     (first index-element))))
       indices)))
  indices)

(defun insert-index-tuples (relation-name relation-implementation relation-storage-structure indices
    &aux attributes tuples)
  (setf tuples (funcall (find-symbol (concatenate 'string "OBTAIN-TUPLES-" relation-storage-structure)
      *pkg-string*)
relation-name))
  (setf attributes (caar (funcall (find-symbol (concatenate 'string "RETRIEVE-"
        *system-relation-base-implementation*
        "-" *system-relation-storage-structure*)
         *pkg-string*)
  'system-relation *system-relation-attributes* '("ATTRIBUTES")
  *system-relation-key*
  `(string-equal relation-name ,(string-upcase relation-name))
  nil 'system-relation)))
  (mapc (function (lambda (index-element)
    (funcall (find-symbol (concatenate 'string "INDEX-INSERT-" relation-implementation "-"
          (third index-element)) *pkg-string*)
     (first index-element) tuples attributes (second index-element) relation-name)))
indices))


(defun defatt-array-hash (relation-name attribute-descriptor relation-tuple
  &aux array-name att-default-list)
  (block defatt-array-hash
;;
;;  (i)   insert system-attribute tuples
;;  (ii)  modify system-relation tuple
;;
  (setf att-default-list (defatt-utility-array-list relation-name attribute-descriptor relation-tuple))
  (if (not att-default-list)
      (return-from defatt-array-hash nil))
  ;;
  ;;  (iii) modify relation-name tuples
  ;;
  (multiple-value-setq (array-name)
     (intern (read-from-string (concatenate 'string relation-name "ARRAY"))))
  (maphash (function (lambda (key bucket)
       key
       (mapcar (function (lambda (row)
  (setf (aref (symbol-value array-name) row)
        (append (aref (symbol-value array-name) row) att-default-list))))
       bucket)))
   (getp relation-name 'entry-point))
  (return-from defatt-array-hash relation-name)))

(defun defatt-array-heap (relation-name attribute-descriptor relation-tuple
  &aux array-name att-default-list)
  (block defatt-array-heap
;;
;;  (i)   insert system-attribute tuples
;;  (ii)  modify system-relation tuple
  (setf att-default-list (defatt-utility-array-list relation-name attribute-descriptor relation-tuple))
  (if (not att-default-list)
      (return-from defatt-array-heap nil))
  ;;
  ;;  (iii) modify relation-name tuples
  (multiple-value-setq (array-name)
     (intern (read-from-string (concatenate 'string relation-name "ARRAY"))))
   (mapcar (function (lambda (row)
      (setf (aref (symbol-value array-name) row)
        (append (aref (symbol-value array-name) row) att-default-list))))
   (getp relation-name 'entry-point))

   (return-from defatt-array-heap relation-name)))

(defun defatt-flavor-avl (relation-name attribute-descriptor relation-tuple key-value)
  (defatt-utility-redef-rel relation-name attribute-descriptor relation-tuple key-value))

(defun defatt-flavor-hash (relation-name attribute-descriptor relation-tuple key-value)
  (defatt-utility-redef-rel relation-name attribute-descriptor relation-tuple key-value))

(defun defatt-flavor-heap (relation-name attribute-descriptor relation-tuple key-value)
  (defatt-utility-redef-rel relation-name attribute-descriptor relation-tuple key-value))

(defun defatt-list-hash (relation-name attribute-descriptor relation-tuple key-value)
  (defatt-utility-redef-rel relation-name attribute-descriptor relation-tuple key-value))

(defun defatt-list-heap (relation-name attribute-descriptor relation-tuple key-value)
  (defatt-utility-redef-rel relation-name attribute-descriptor relation-tuple key-value))

(defun defatt-list-avl (relation-name attribute-descriptor relation-tuple key-value)
  (defatt-utility-redef-rel relation-name attribute-descriptor relation-tuple key-value))

(defun defatt-struct-hash (relation-name attribute-descriptor relation-tuple key-value)
  (defatt-utility-redef-rel relation-name attribute-descriptor relation-tuple key-value))

(defun defatt-struct-avl (relation-name attribute-descriptor relation-tuple key-value)
  (defatt-utility-redef-rel relation-name attribute-descriptor relation-tuple key-value))

(defun defatt-struct-heap (relation-name attribute-descriptor relation-tuple key-value)
  (defatt-utility-redef-rel relation-name attribute-descriptor relation-tuple key-value))


(defun defatt-utility (relation-name attribute-descriptor relation-tuple system-attribute-list
       &aux default doc domain attribute global-attr att-default-list new-att-list
       attribute-list domain-list tuple-format-list temp-format)
  (setf new-att-list nil
att-default-list nil)
  (setf attribute-list (third relation-tuple)
domain-list (fourth relation-tuple)
tuple-format-list (second relation-tuple))
  (do ((att-des attribute-descriptor (if (listp (cadr att-des))
   (cddr att-des)
   (cdr att-des))))
      ((null att-des) (values t))
    (setf attribute (string-upcase (car att-des)))
    (cond ((member attribute new-att-list :test 'string-equal)
   (if *provide-warning-messages*
       (format *standard-output* "~%WARNING - The attribute ~S is defined more than once" attribute))
   (setf attribute nil)))
    (if attribute
(block defatt-utility
      (setf *domain-list*
    (mapcar (function (lambda (dom)
  (car dom)))
    (qtrieve 'system-domain *system-domain-attributes* '("DOMAIN-NAME")
      *system-domain-key* t)))
      (cond ((listp (cadr att-des))
     ;;
     ;;  Determine the domain of the attribute
     ;;
     (setf domain (string-upcase (or (car (get-keyword-value '(dom) (cadr att-des))) "ANYP")))
     (cond ((not (member domain *domain-list* :test 'string-equal))
    (if *provide-error-messages*
 (format *standard-output* "~%ERROR - ~s is an unrecognized domain" domain))
    (return-from defatt-utility nil)))
     (setf domain-list (append domain-list (list domain)))
     ;;
     ;;  Add the print width of this attribute into the tuple format list
     ;;
     (if (not (listp (setf temp-format (or (car (get-keyword-value '(format) (cadr att-des)))
       (default-tuple-format (list domain))))))
 (setf temp-format (list temp-format)))
     (cond ((<= (car temp-format) 0)
    (cond (*provide-error-messages*
    (format *standard-output* "~%ERROR - ~s is not a legal format specification"
     (car temp-format))
    (format *standard-output* "~%         It must be a positive numeric value")))
    (return-from defatt-utility nil)))
     (setf tuple-format-list (append tuple-format-list temp-format))
     ;;
     ;;  Determine the default value of the domain
     ;;
     (setf default
   (cond ((setf default (car (get-keyword-value '(def) (cadr att-des))))
   (if *validity-checking*
       (cond ((funcall
         (if (setf global-attr (find-symbol (string-upcase domain)
         "GLOBAL"))
      global-attr
      (find-symbol (string-upcase domain) *pkg-string*))
         default)
       default)
      (t
       (if *provide-error-messages*
    (format *standard-output* "~%ERROR - The default value specified ~S is not in the domain ~S"
     default domain))
       (return-from defatt-utility nil)))
       default))
  (t
   (get-default-value domain))))
     ;;
     ;;  Determine the documentation for the attribute
     ;;
     (setf doc (car (get-keyword-value '(doc) (cadr att-des)))))
    (t
     (setf domain-list (append domain-list  (list "ANYP"))
   domain "ANYP"
   default "?"
   tuple-format-list (append tuple-format-list (default-tuple-format '("ANYP")))
   doc nil)))
      (setf new-att-list (append new-att-list (list attribute))
    attribute-list (append attribute-list (list attribute))
    att-default-list (append att-default-list (list default))
    system-attribute-list
      (append system-attribute-list (list (list (concatenate 'string relation-name)
     (string (car att-des)) domain default doc
     user-id)))))))
  (values att-default-list system-attribute-list tuple-format-list attribute-list domain-list))

(defun defatt-utility-array-list (relation-name attribute-descriptor relation-tuple
   &aux att-default-list attribute-list domain-list tuple-format-list
   sys-att-list system-attribute-list where-c
   (status? *provide-status-messages*))
  (block defatt-utility-array-list
;;
;; (i) system-attribute relation
;;
;;     -- Define the tuples for the system-attribute relation
;;
(setf system-attribute-list nil)
(multiple-value-setq (att-default-list sys-att-list tuple-format-list attribute-list domain-list)
  (defatt-utility relation-name attribute-descriptor relation-tuple system-attribute-list))
(if (not att-default-list)(return-from defatt-utility-array-list nil))
;;
;;     -- Insert the tuples in the system-attribute relation
;;
(setf *provide-status-messages* nil)
(insert  'system-attribute (list 'tuples sys-att-list))
(setf *provide-status-messages* status?)
;;
;;  (ii) modify the system-relation tuple which defines this relation
;;
(setf where-c (list 'string-equal 'relation-name `(quote ,relation-name)))
(delete-or-modify 'system-relation t where-c '("MODIFIEDP" "TUPLE-FORMAT" "ATTRIBUTES" "DOMAINS")
  (list 't `(quote ,tuple-format-list) `(quote ,attribute-list) `(quote ,domain-list)))
(return-from defatt-utility-array-list att-default-list)))

(defun defatt-utility-redef-rel (relation-name attribute-descriptor relation-tuple new-key
  &aux att-default-list system-attribute-list attribute-list tuple-format-list
  tuple-list dir imp ss key doc attr-des-pair)
  (block defatt-utility-redef-rel
(setf system-attribute-list
      (get-system-attribute-list relation-name))
(multiple-value-setq (att-default-list system-attribute-list tuple-format-list attribute-list)
  (defatt-utility relation-name attribute-descriptor relation-tuple system-attribute-list))
(if (not att-default-list)
    (return-from defatt-utility-redef-rel nil))
(setf dir (nth 5 relation-tuple)
      imp (nth 7 relation-tuple)
      ss (nth 8 relation-tuple)
      key (if new-key
      new-key
      (nth 4 relation-tuple))
      doc (nth 6 relation-tuple))
;;
;; Create the attribute-descriptor list to redefine the relation
(setf attr-des-pair  (create-attr-descriptor attribute-list system-attribute-list)
      tuple-list nil)
(maptuple (function (lambda (tuple)
      (setf tuple-list (append (list (append tuple att-default-list)) tuple-list))))
  (read-from-string relation-name))
(redefine-rel relation-name attr-des-pair imp ss key tuple-format-list doc dir (nreverse tuple-list))
(return-from defatt-utility-redef-rel relation-name)))

(defun defatt-utility-redef-rel-list-heap (relation-name attribute-descriptor relation-tuple key-value)
  (defatt-list-heap relation-name attribute-descriptor relation-tuple key-value))

(defun defatt-utility-redef-rel-list-avl (relation-name attribute-descriptor relation-tuple key-value)
  (defatt-list-avl relation-name attribute-descriptor relation-tuple key-value))

(defun defatt-utility-redef-rel-list-hash (relation-name attribute-descriptor relation-tuple key-value)
  (defatt-utility-redef-rel relation-name attribute-descriptor relation-tuple key-value))

(defun defatt-utility-redef-rel-flavor-heap (relation-name attribute-descriptor relation-tuple key-value)
  (defatt-utility-redef-rel relation-name attribute-descriptor relation-tuple key-value))

(defun defatt-utility-redef-rel-flavor-hash (relation-name attribute-descriptor relation-tuple key-value)
  (defatt-utility-redef-rel relation-name attribute-descriptor relation-tuple key-value))

(defun defatt-utility-redef-rel-flavor-avl (relation-name attribute-descriptor relation-tuple key-value)
  (defatt-utility-redef-rel relation-name attribute-descriptor relation-tuple key-value))

(defun defatt-utility-redef-rel-struct-heap (relation-name attribute-descriptor relation-tuple key-value)
  (defatt-utility-redef-rel relation-name attribute-descriptor relation-tuple key-value))

(defun defatt-utility-redef-rel-struct-hash (relation-name attribute-descriptor relation-tuple key-value)
  (defatt-utility-redef-rel relation-name attribute-descriptor relation-tuple key-value))

(defun defatt-utility-redef-rel-struct-avl (relation-name attribute-descriptor relation-tuple key-value)
  (defatt-utility-redef-rel relation-name attribute-descriptor relation-tuple key-value))


;;; fragments



(node-compare key current-node-key domain-key-list))
  (cond ((equal comparison-operator 'less-than)
 (multiple-value-setq (mod-tree rebalancep)
   (insert-avl-struct new-element (caddr tree) key key-list a
