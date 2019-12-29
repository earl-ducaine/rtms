;;; -*- Mode:Common-Lisp; Package:RTMS; Base:10  -*-

;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved.
;;; DEFINE-REL
;;;
;;; This file contains the following Explorer extensions to CommonLisp
;;; Standard as Indicated in the June 1985 Explorer Lisp Reference
;;;     firstn
;;;     errset
;;;     deff
;;;
;;; The following function contains flavor references and thus are
;;; incompatable with CommonLisp. Their removal will not effect the
;;; functionality of RTMS.
;;;     defrel-flavor

(deff defrel 'define-relation)

(defun define-relation (relation-name attribute-descriptor &rest keyword-list
			&key &optional directory documentation format implementation-type key storage-structure
			       &allow-other-keys)
  "Define relations in the active database.
   relation-name        - Name of the relation to be defined.
   attribute-descriptor - List of attributes and their descriptions.
   directory            - Name of the directory in which this relation is to
                          be saved.
   documentation        - Description of this relation.
   format               - List of print-width values correponding to the
                          attribute-list.
   implementation-type  - Name of the implementation for this relation.
   key                  - List of attributes comprising the key for this
                          relation.
   storage-structure    - Name of the storage structure to be used for this
                          relation."
  (declare (ignore directory documentation format implementation-type key
		   storage-structure))
  (let (default doc domain key-format implementation-type1 domain-list
		error-p attribute-list system-attribute-list
		system-relation-list storage-structure1 attribute
		temp-format tuple-format-list)
    (block define-relation
      (cond ((and *parameter-checking*
		  (not (member relation-name *system-relations*
			       :test 'string=)))
	     (if (not (active-database relation-name))
		 (return-from define-relation nil))
	     (if (not (setf relation-name (validate-sym relation-name t)))
		 (return-from define-relation nil))
	     (cond ((null attribute-descriptor)
		    (if *provide-error-messages*
			(format *standard-output*
				"~%ERROR - No attribute descriptor provided."))
		    (return-from define-relation nil)))
	     (if (not (listp attribute-descriptor))
		 (setf attribute-descriptor
		       (list attribute-descriptor))))
	    (t
	     (setf relation-name (string-upcase relation-name))))
      (setf keyword-list (de-nest-keyword-list keyword-list))
      (if *provide-status-messages*
	  (format *standard-output* "~%Define relation ~s in database ~s"
		  (read-from-string relation-name)
		  (read-from-string *active-db*)))
      (if *parameter-checking*
	  (setf keyword-list
		(get-keyword-value-prereq '(dir dom def cardinality doc imp
					    owner-id tup mod key sto
					    format) keyword-list)))
      ;; Determine if the implementation and storage structure
      ;; specified for the current user relation are valid.
      (multiple-value-setq (implementation-type1 storage-structure1)
	(obtain-imp-&-sto relation-name keyword-list))
      (if (or (not implementation-type1)(not storage-structure1))
	  (return-from define-relation nil))
      ;; Does a relation owned by this user already exist? If it does,
      ;; a new one may not be defined until the current one is
      ;; destroyed. In the future, location transparency issues need
      ;; to be introduced here.
      (if (relation-exist-p relation-name)
	  (return-from define-relation nil))
      ;; Define the tuples for the system-attribute relation. If an
      ;; attribute descriptor is not specified, define a relation with
      ;; one attribute.
      (setf system-attribute-list nil
	    tuple-format-list nil)
      (if (not (member relation-name *system-relations*
		       :test 'string))
	  (setf *domain-list*
		(mapcar (function (lambda (dom)
			  (car dom)))
			(qtrieve 'system-domain *system-domain-attributes*
				 '("DOMAIN-NAME")
				 *system-domain-key* t))))
      (if (null
	   (do ((att-des attribute-descriptor
			 (if (and (listp (cadr att-des))
				  (not (string= "QUOTE"
						(first (cadr att-des)))))
			     (cddr att-des)
			     (cdr att-des))))
	       ((null att-des) t)
	     ;; Determine if the attribute has already been defined
	     ;; for this relation
	     (cond ((not *parameter-checking*)
		    (setf attribute (string-upcase (car att-des))))
		   ((null (setf attribute (validate-sym (car att-des) t)))
		    (return-from define-relation nil))
		   ((member (setf attribute (string-upcase attribute))
			    attribute-list :test 'string-equal)
		    (if *provide-error-messages*
			(format *standard-output*
				(str "~%ERROR - The attribute ~s is "
				     "defined more than once")
				(read-from-string attribute)))
		    (return-from define-relation nil)))
	     (setf attribute-list (cons attribute attribute-list))
	     ;; If the second part of the attribute descriptor is
	     ;; provided, parse it and set the appropriate values.
	     (cond ((and (listp (cadr att-des)) (not (string-equal "QUOTE" (first (cadr att-des)))))
		    (multiple-value-setq (domain domain-list)
		      (obtain-domain relation-name (cadr att-des) domain-list))
		    (if (null domain)
			(return-from define-relation nil))
		    (if (listp (setf temp-format (or (car (get-keyword-value '(format) (cadr att-des)))
						     (default-tuple-format (list domain)))))
			(setf temp-format (car temp-format)))
		    (setf tuple-format-list (cons temp-format tuple-format-list))
		    (multiple-value-setq (default error-p)
		      (obtain-domain-default-value domain (cadr att-des)))
		    (setf doc (car (get-keyword-value '(doc) (cadr att-des)))))
		   (t
		    (setf domain-list (cons "ANYP" domain-list)
			  domain "ANYP"
			  tuple-format-list (cons (car (default-tuple-format (list domain))) tuple-format-list)
			  default "?"
			  doc nil)))
	     (setf system-attribute-list (cons (list relation-name (string attribute) domain default doc user-id)
					       system-attribute-list))))
	  (return-from define-relation nil))
      ;; Create the system-relation tuple which defines the new
      ;; relation. This will temporarly be stored in a property list
      ;; and will be inserted later as described below. The manner of
      ;; the implementation of the tuple depends on the method in
      ;; which the system-relation relation is implemented. The tuple
      ;; created will be of the appropriate type. The attributes are
      ;; the same regardless of implementation and can be done for all
      ;; types and then passed to the correct function.  If any
      ;; keywords and their values are provided (ex. in defrel call in
      ;; RESTORE from QFASL) we should not use default. Specifically,
      ;; we should not set cardinality to 0.
      (setf attribute-list (nreverse attribute-list)
	    domain-list (nreverse domain-list)
	    system-attribute-list (nreverse system-attribute-list))
      (cond ((null (errset
		    (setf key
			  (funcall (find-symbol
				    (concatenate 'string "DEFREL-"
						 storage-structure1)
				    *pkg-string*)
				   relation-name attribute-list keyword-list))
		    nil))
	     (if *provide-error-messages*
		 (format *standard-output*
			 "~%ERROR - ~s is an undefined storage structure"
			 storage-structure1))
	     (return-from define-relation nil))
	    ((null key)
	     (putp 'system-attribute nil 'commit-tuples)
	     (return-from define-relation nil)))
      (setf key-format (car (get-keyword-value '(format) keyword-list)))
      (if (and key-format (not (listp key-format)))
	  (setf key-format (list key-format)))
      (setf tuple-format-list (nreverse tuple-format-list))
      (if key-format
	  (if (< (length key-format)(length tuple-format-list))
	      (setf tuple-format-list (append key-format (nthcdr (length key-format) tuple-format-list)))
	      (setf tuple-format-list (firstn (length tuple-format-list) key-format))))
      (cond ((and (not (member relation-name *system-relations* :test 'string-equal)) *parameter-checking*)
	     (setf tuple-format-list (mapcar #'(lambda (value)
						 (cond ((or (not (numberp value)) (<= value 0))
							(if *provide-warning-messages*
							    (format *standard-output*
								    "~%WARNING - ~s is not a valid format value."
								    value))
							*default-anyp-width*)
						       (t
							value)))
					     tuple-format-list))))
      (setf system-relation-list  (list (concatenate 'string relation-name) user-id
					(or (car (get-keyword-value '(mod) keyword-list)) t) ;modifiedp
					(or (car (get-keyword-value '(cardinality) keyword-list)) 0) ; cardinality
					tuple-format-list attribute-list domain-list key
					(get-directory keyword-list) (car (get-keyword-value '(doc) keyword-list ))
					(string-upcase implementation-type1) (string-upcase storage-structure1)
					nil))
      ;;
      ;;  Call the implementation dependent function which will define the structure which will be used to implement the new relation.
      ;;
      (cond ((null (errset (funcall (find-symbol (concatenate 'string "DEFREL-" implementation-type1)
						 *pkg-string*)
				    relation-name attribute-list keyword-list) nil))
	     (if *provide-error-messages*
		 (format *standard-output* "~%ERROR - ~s is an undefined implementation type" implementation-type1))
	     (return-from define-relation nil)))
      ;;
      ;;  Determine if the system-relation tuple may be inserted into the system relations thus completing the relation definition. If it is one of
      ;; the basic system relations it may not be commited until all have been defined.
      ;;
      (putp 'system-relation (append (getp 'system-relation 'commit-tuples) (list system-relation-list))
	    'commit-tuples)
      (putp 'system-attribute (append (getp 'system-attribute 'commit-tuples) system-attribute-list) 'commit-tuples)
      (if (not (member relation-name *system-relations* :test 'string-equal))
	  (commit-system-relation))
      (return-from define-relation relation-name))))

(defun defrel-restore* (relation-name attributes &optional keyword-list
			&aux  implementation-type storage-structure)
  (block defrel-restore*
    (if (not (active-database relation-name))
	(return-from defrel-restore* nil))
    (setf keyword-list (get-keyword-value-prereq '(dir dom def cardinality doc imp owner-id tup mod key sto)
						 (car keyword-list)))
    (setf storage-structure (string-upcase (car (get-keyword-value '(sto) keyword-list))))
    ;;
    ;;  Define the structure which will house the new relation where applicable. The implementation type must be obtained as the selection
    ;; criteria. If the implementation type is not specified in the keyword list, extract it from the system default.
    ;;
    (setf implementation-type (string-upcase (car (get-keyword-value '(imp) keyword-list))))
    (funcall (find-symbol (concatenate 'string "DEFREL-" storage-structure) *pkg-string*) relation-name attributes
	     keyword-list)
    ;;
    ;;  Call the implementation dependent function which will define the structure which will be used to implement the new relation.
    ;;
    (funcall (find-symbol (concatenate 'string "DEFREL-" implementation-type) *pkg-string*) relation-name
	     attributes keyword-list)
    (return-from defrel-restore* relation-name)))

(defun defrel-flavor (relation-name attribute-list keyword-list
		      &aux attr-val-list atom-attribute-list )
  keyword-list
  (setf relation-name (read-from-string (concatenate 'string *pkg-name* relation-name))
	atom-attribute-list (unconvert-attributes attribute-list *pkg-name*))
  (eval `(defflavor ,relation-name ,atom-attribute-list ()
		    :inittable-instance-variables
		    :settable-instance-variables
		    :gettable-instance-variables
		    :special-instance-variables))
  ;;Using CONDITION instead of fasd-form inorder to solve the problem of the values being evaluated when saved and restored, has a
  ;; problem. And that is the problem of :A1 versus plain A1 which was taken care of by the following modification to the attribute-list
  ;; before dumping.
  (setf attr-val-list nil)
  (mapc (function (lambda (attr)
	  (setf attr-val-list (append attr-val-list
				      (list (list 'quote (list 'quote (read-from-string
								       (concatenate 'string ":"
										    (string-upcase attr)))))
					    `(list 'quote ,attr))))))
	atom-attribute-list)
  (setf attr-val-list (cons (list 'quote (list 'quote relation-name)) attr-val-list))
  (eval `(defmethod (,relation-name :fasd-form) ()
	   (list 'make-instance ,@attr-val-list))))

(defun defrel-hash (relation-name attribute-list keyword-list &aux key)
  (cond ((setf key (defrel-validate-key attribute-list keyword-list))
	 (putp relation-name (make-hash-table :test 'equal :size 1000) 'entry-point)))
  key)

(defun defrel-validate-key (attribute-list keyword-list &aux key)
  (block defrel-validate-key
    (setf attribute-list (convert-attributes attribute-list))
    (setf key (car (get-keyword-value '(key) keyword-list)))
    (cond (*parameter-checking*
	   (if (and key (not (listp key)))
	       (setf key (list key)))
	   (if (null (car key))
	       (setf key nil))
	   (setf key (mapcar #'(lambda (attr)
				 (validate-sym attr t))
			     key))
	   (if (member nil key)
	       (return-from defrel-validate-key nil))
	   (cond (key
		  (cond ((and key (not (listp key)))
			 (setf key (list key))))
		  (if (null (do ((key (convert-attributes key) (cdr key)))
				((null key) t)
			      (cond ((not (member (car key) attribute-list :test 'string-equal))
				     (if *provide-error-messages*
					 (format *standard-output* "~%ERROR - ~s is given as a key attribute but it is not contained in the attribute list" (car key)))
				     (return-from defrel-validate-key nil)))))
		      (return-from defrel-validate-key nil))
		  (return-from defrel-validate-key key))
		 (t
		  (setf key (list (car attribute-list)))
		  (return-from defrel-validate-key key))))
	  ((null key)
	   (setf key (list (car attribute-list)))))
    (return-from defrel-validate-key key)))

(defun defrel-heap (relation-name attribute-list keyword-list)
  (putp relation-name nil 'entry-point)
  (defrel-validate-key attribute-list keyword-list))

(defun defrel-avl (relation-name attribute-list keyword-list)
  (putp relation-name nil 'entry-point)
  (defrel-validate-key attribute-list keyword-list))

(defun defrel-list (relation-name attribute-list keyword-list &aux
								attr-val-list)
  attribute-list keyword-list attr-val-list relation-name)

;;        IMPORTANT: Since DEFSTRUCT creates a function (accessor function) for each of the components of the structure, if two
;;  relations have same attributes and their implementation type is structure, then when restored there will be a warning message to notify
;; the fact that some functions are being redefined.
(defun defrel-struct (relation-name attribute-list keyword-list)
  keyword-list
  ;;
  ;;create a structure which will be used to implement the tuples in this relation
  ;;
  (setf attribute-list
	(mapcar (function (lambda (attr)
		  (read-from-string (concatenate 'string *pkg-name* relation-name attr))))
		attribute-list))
  (setf relation-name (read-from-string (concatenate 'string *pkg-name* relation-name)))
  (eval `(defstruct (,relation-name (:conc-name nil)
				    (:constructor ,(read-from-string (concatenate 'string *pkg-name* "MAKE-"
										  (string relation-name))))) ,@attribute-list)))

(defun obtain-imp-&-sto (relation-name keyword-list)
  (let (implementation-type storage-structure)
    (cond ((member relation-name *system-relations* :test 'string=)
	   (setf storage-structure *system-relation-storage-structure*
		 implementation-type *system-relation-base-implementation*))
	  (t
	   (cond ((null (setf implementation-type
			      (car (get-keyword-value '(imp) keyword-list))))
		  (setf implementation-type
			(validate-sym (subseq *relation-implementation* 0
					      (search "-" *relation-implementation*))
				      t)))
		 ((not (car (qtrieve 'system-implementation *system-implementation-attributes*
				     '("IMPLEMENTATION-NAME") *system-implementation-key*
				     `(string-equal implementation-name ,(string-upcase implementation-type)))))
		  (if *provide-error-messages*
		      (format *standard-output*  "~%ERROR - ~s is an undefined implementation type"
			      implementation-type))
		  (setf implementation-type nil))
		 (implementation-type
		  (setf implementation-type (string-upcase implementation-type))))
	   (cond ((null (setf storage-structure (car (get-keyword-value '(sto) keyword-list))))
		  (setf storage-structure (validate-sym (subseq *relation-implementation*
								(+ (search "-" *relation-implementation*) 1))
							t)))
		 ((not (car (qtrieve 'system-storage-structure *system-storage-structure-attributes*
				     '("STORAGE-STRUCTURE-NAME") *system-storage-structure-key*
				     `(string-equal storage-structure-name
						    ,(string-upcase storage-structure)))))
		  (if *provide-error-messages*
		      (format *standard-output* "~%ERROR - ~s is an undefined storage structure"
			      storage-structure))
		  (setf storage-structure nil))
		 (storage-structure
		  (setf storage-structure (string-upcase storage-structure))))))
    (values implementation-type storage-structure)))

(defun relation-exist-p (relation-name)
  (let (qtrieve-var)
    (cond ((not (member relation-name *system-relations* :test 'string-equal))
	   (setf qtrieve-var (caar (qtrieve 'system-relation *system-relation-attributes*
					    '("OWNER-ID") *system-relation-key*
					    `(string-equal relation-name ,relation-name))))
	   (cond ((equal qtrieve-var user-id)
		  (if *provide-error-messages*
		      (format *standard-output* "~%ERROR - The ~S relation already exists in database ~s."
			      (read-from-string relation-name) (read-from-string *active-db*)))
		  t)
		 ;;
		 ;;  Should let the user define a new relation with the same name. This may cause problems
		 ;; in other places. Think about this before doing it.
		 ;;
		 (qtrieve-var
		  (if *provide-error-messages*
		      (format *standard-output* "~%ERROR - The ~S relation exists and is defined by ~S. "
			      (read-from-string relation-name) qtrieve-var))
		  t)
		 ;;See if there is a view by the same name.
		 ((caar (qtrieve 'system-view *system-view-attributes* '("OWNER-ID") *system-view-key*
				 `(and (string-equal view-name ,relation-name)(string-equal owner-id
											    ,user-id))))
		  (if *provide-error-messages*
		      (format *standard-output*
			      "~%ERROR - There exists a view named ~S and defining a relation with the same name causes conflicts."
			      (read-from-string relation-name)))
		  t)
		 (t
		  nil))))))

(defun obtain-domain (relation-name keyword-list domain-list)
  (let (domain)
    (setf domain (car (get-keyword-value '(dom) keyword-list)))
    (cond ((null domain)
	   (setf domain "ANYP"))
	  ((null (setf domain (validate-sym domain t)))
	   nil)
	  ((not *parameter-checking*)
	   (setf domain (string-upcase domain)))
	  ((and (not (member relation-name *system-relations* :test 'string-equal))
		(not (member (setf domain (string-upcase domain)) *domain-list* :test 'string-equal)))
	   (if *provide-error-messages*
	       (format *standard-output* "~%ERROR - ~s is an unrecognized domain" domain))
	   (setf domain nil)))
    (setf domain-list (cons domain domain-list))
    (values domain domain-list)))

(defun obtain-domain-default-value (domain keyword-list)
  (let (default global-attr error-p)
    (cond ((setf default (car (get-keyword-value '(def) keyword-list)))
	   (if *validity-checking*
	       (cond ((not (funcall (if (setf global-attr (find-symbol (string-upcase domain) "GLOBAL"))
					global-attr
					(find-symbol (string-upcase domain) *pkg-string*))
				    default))
		      (if *provide-error-messages*
			  (format *standard-output* "~%ERROR - The default value specified ~S is not in the domain ~S"
				  default domain))
		      (setf error-p t)))))
	  (t
	   (setf default (get-default-value domain))))
    (values default error-p)))


;;; fragments

;; (return-from destroy-relation nil))
;; (cond ((null (setf relation (validate-sym relation t)))
;;        (return-from destroy-relation nil)))
;; (setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
;; 		       ((or (null keyword-list)(not (listp (car keyword-list)))) keyword-list)))
;; (setf qtrieve-var (car (qtrieve 'system-relation *system-relation-attributes*
;; 				'("ATTRIBUTES" "IMPLEMENTATION-TYPE" "STORAGE-STRUCTURE" "SAVE-DIRECTORY")
;; 				*system-relation-key*
;; 				(return-from defrel-restore* nil))
;; 		       (setf keyword-list (get-keyword-value-prereq '(dir dom def cardinality doc imp owner-id tup mod key sto)
;; 								    (car keyword-list)))
;; 		       (setf storage-structure (string-upcase (car (get-keyword-value '(sto)
