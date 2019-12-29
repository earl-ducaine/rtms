;;; -*- Mode:Common-Lisp; Package:RTMS; Base:10 -*-

;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved
;;; INSERT
;;;
;;; This file contains the following Explorer extensions to CommonLisp
;;; Standard as Indicated in the June 1985 Explorer Lisp Reference
;;;     firstn
;;;     deff
;;;     :string-in
;;;
;;; The following function contains flavor references and thus are
;;; incompatable with CommonLisp. Their removal will not effect the
;;; functionality of RTMS.
;;;     insert-flavor-hash
;;;     insert-flavor-heap
;;;

;;; Change History --
;;; 04.20.87 MRR  Removed &optional from INSERT lambda list.


(defun rtms-read-insert-file (pathname)
  (let* ((beg-index 0)
	 (extend-size 4096)
	 (total-length 0)
	 (value (make-array extend-size))
	 (end-index extend-size)
	 index value)
    (with-open-file (stream1 pathname)
      (do ((eof-flag nil))
	  (eof-flag t)
	(multiple-value-setq (index eof-flag)
	  (funcall stream1 ':string-in nil value beg-index end-index))
	(setf total-length (+ total-length index))
	(cond ((not eof-flag)
	       (setf value (adjust-array value (+ total-length extend-size)))
	       (setf beg-index end-index
		     end-index (+ total-length extend-size))))))
    (read-from-string value nil nil :start 0 :end total-length)))

;; for the sake of old references to this function             *
(defun insert1 (rel tuples &optional attribute-list)
  (insert rel (list 'tuples tuples 'attr attribute-list)))

(deff insert-tuples 'insert)

(defun insert (relation-name &rest keyword-list
	       &key tuples attributes pathname
	       &allow-other-keys
	       &aux (attr-val-list nil) (sub-list nil) tuple project-list path attribute-list (key nil) ss imp
		 card qtrieve-var mod-tuples qtrieve-var1 qtrieve-var2 indices)
  "Insert a list of tuples or data from a file.

   RELATION-NAME   - Name of the relation into which the data is to be inserted.
   TUPLES     - List of tuples to be inserted. Tuples are expected to be in the list-of-values format.
   ATTRIBUTES - If the values in the tuples do not correspond to the attribute-list specified during
                relation-defintion, specify a list of attributes to determine the order.
   PATHNAME   - If the data is in a file, specify the name of the file."
  tuples attributes pathname

  (block insert
    (setf keyword-list (de-nest-keyword-list keyword-list))
    (cond (*parameter-checking*
	   (if (or (not (active-database)) (null (setf relation-name (validate-sym relation-name))))
	       (return-from insert nil))
	   (setf keyword-list (get-keyword-value-prereq '(tuple attr path) keyword-list))))
    (setf tuple (car (get-keyword-value '(tuple) keyword-list))
	  project-list (car (get-keyword-value '(attr) keyword-list))
	  path (car (get-keyword-value '(path) keyword-list)))
    (if (and *parameter-checking* project-list (not (listp project-list)))
	(setf project-list (list project-list)))
    (cond ((and *parameter-checking* tuple path)
	   (if *provide-error-messages*
	       (format *standard-output* "~%ERROR - List of tuples as well as a pathname provided."))
	   (return-from insert nil)))
    ;; INSERT has been called by one of the restore operations
    ;; (LOAD-RELATION) and in reference to one of the system-relations
    ;; insert the tuples without further processing and return.
    (if (and *restore-operation* (member (string-upcase relation-name) *system-relations*
					 :test 'string-equal))
	(return-from insert
	  (funcall
	   (find-symbol (concatenate 'string "INSERT-" *system-relation-base-implementation* "-"
				     *system-relation-storage-structure*) *pkg-string*)
	   relation-name
	   (eval (read-from-string (concatenate 'string *pkg-name* "*" (string relation-name)
						"-ATTRIBUTES*")))
	   tuple
	   (eval (read-from-string (concatenate 'string *pkg-name* "*" (string relation-name) "-KEY*")))
	   relation-name)))
    ;;
    ;;  If there multiple indices defined on this relation, the tuples inserted into the base relation must also be inserted into each of the
    ;; secondary indice relations. Not only do the tuples have to be inserted but the SAME tuples.
    ;;
    (cond ((not (member (string relation-name) *system-relations* :test 'string-equal))
	   (setf indices (qtrieve 'system-index *system-index-attributes* '("INDEX-NAME" "INDEX-TYPE" "KEY")
				  *system-index-key*
				  `(string-equal relation-name ,(string relation-name))))))
    ;;
    ;;  Obtain some information of the relation into which the tuples will be inserted.
    ;;
    (setf qtrieve-var (get-relation relation-name
				    '("ATTRIBUTES" "IMPLEMENTATION-TYPE" "STORAGE-STRUCTURE" "KEY" "CARDINALITY")
				    t))
    (cond ((null (cadr qtrieve-var))
	   (if *provide-error-messages*
	       (format *standard-output* "~%ERROR - Relation ~S is not defined in the database ~S"
		       relation-name *active-db*))
	   (return-from insert nil)))
    (setf relation-name (car qtrieve-var)
	  qtrieve-var (cadr qtrieve-var)
	  imp (second qtrieve-var)
	  ss (third qtrieve-var)
	  key (fourth qtrieve-var)
	  card (fifth qtrieve-var)
	  attribute-list (convert-attributes (first qtrieve-var)))
    ;;
    ;;  Validate that the attribues specified in the project list supplied by the user are actually attributes of the
    ;; relation and place them into the proper form.
    ;;
    (if *parameter-checking*
	(if project-list
	    (setf project-list (mapcar #'(lambda (attr)
					   (if (null (validate-sym attr t))
					       (return-from insert nil)
					       (validate-sym attr t)))
				       project-list)))
	(setf project-list (convert-attributes project-list)))
    ;; If the data is stored in a file, read it into the TUPLE.
    (cond (path
	   (if (probe-file path)
	       (setf tuple (rtms-read-insert-file path))
	       (if *provide-error-messages*
		   (format *standard-output* "~%ERROR - File ~S does not exist." path)))))
    (cond ((null tuple)
	   (if *provide-error-messages*
	       (format *standard-output* "~%ERROR - No tuples provided."))
	   (return-from insert nil)))
    ;; Obtain information about the attributes of the insert relation
    (if (not (or (member (string relation-name) *system-relations* :test 'string-equal) *restore-operation*
		 (not *validity-checking*)))
	(setf qtrieve-var
	      (funcall (find-symbol (concatenate 'string "RETRIEVE-" *system-relation-base-implementation* "-"
						 *system-relation-storage-structure*) *pkg-string*)
		       'system-attribute *system-attribute-attributes*
		       '("ATTRIBUTE-NAME" "DOMAIN-FUNCTION" "DEFAULT-VALUE") *system-attribute-key*
		       (list 'string-equal 'relation-name  (string relation-name)) nil 'system-attribute)))
    ;; Check for various possibilities of INSERT format. First see if
    ;; tuple is a list of tuples. Store the attribute names
    (cond ((or (member (string-upcase relation-name) *system-relations* :test 'string-equal) *restore-operation*)
	   (setf attr-val-list tuple))
	  ((null (listp tuple))
	   (if *provide-error-messages*
	       (format *standard-output* "~%ERROR - List of tuples not provided."))
	   (return-from insert nil))
	  ;;
	  ;; Check for form 2 ...Multiple inserts AND the optional attribute list is provided,
	  ;;
	  (project-list
	   ;;
	   ;; Make sure that all attributes provided are actually the attributes in the relation.
	   ;;
	   (if *parameter-checking*
	       (mapl (function (lambda (cdr-attr &aux attr)
		       (setf attr (car cdr-attr))
		       (cond ((not (member attr attribute-list :test 'string-equal))
			      (if *provide-error-messages*
				  (format *standard-output*
					  "~%ERROR - ~S is not an attribute in the relation ~S"
					  attr relation-name))
			      (return-from insert nil))
			     ((member attr (cdr cdr-attr) :test 'string-equal)
			      (if *provide-warning-messages*
				  (format *standard-output*
					  "~%WARNING - Attribute ~S has been specified more than once in the attribute list."
					  attr))))))
		     project-list))
	   ;;
	   ;;  CHeck the length of the tuple provided against the length of the attribute list provided. If they are different in length
	   ;; inform the user that RTMS is substituting the default values for the missing attributes.
	   ;;
	   (mapc
	    (function (lambda (sub-tuple &aux (actual-p-l project-list))
	      (cond
		((listp sub-tuple)
		 (if (and *parameter-checking* (< (length actual-p-l)(length sub-tuple)))
		     (mapc #'(lambda (attr)
			       (if (not (or (member attr actual-p-l :test 'string-equal)
					    (equal (length actual-p-l) (length sub-tuple))))
				   (setf actual-p-l (append actual-p-l (list attr)))))
			   attribute-list))
		 (if (and *parameter-checking* (> (length actual-p-l)(length sub-tuple)))
		     (progn
		       (setf actual-p-l (firstn (length sub-tuple) actual-p-l))
		       (if *provide-warning-messages*
			   (format *standard-output*
				   "~%WARNING - The tuple ~S is smaller in length than the attribute-list. The extra attributes will get the default values for this tuple."
				   sub-tuple))))
		 (setf sub-list nil)
		 (mapc
		  (function
		   (lambda (attr &aux test)
		    (cond ((member attr actual-p-l :test 'string-equal)
			   (setf sub-list
				 (cons
				  (if (setf test (caar (project-list (list sub-tuple)
								     actual-p-l
								     (list attr))))
				      test
				      (caddr (assoc (string-upcase attr) qtrieve-var
						    :test 'string-equal)))
				  sub-list)))
			  (t
			   (setf sub-list
				 (cons (caddr (assoc (string-upcase attr) qtrieve-var
						     :test 'string-equal))
				       sub-list))))))
		  (reverse attribute-list))
		 (setf attr-val-list (cons sub-list attr-val-list)))
		(t
		 (cond (*provide-warning-messages*
			(format *standard-output* "~%WARNING - The tuple ~S is not a list."
				sub-tuple)
			(format *standard-output* "~%          It will not be inserted.")))))))
	    tuple))
	  ;;
	  ;;It is of form 1.
	  ;;
	  (*parameter-checking*
	   (mapc
	    (function
	     (lambda (sub-tuple)
	      (cond ((not (listp sub-tuple))
		     (cond (*provide-warning-messages*
			    (format *standard-output* "~%WARNING - The tuple ~S is not a list."
				    sub-tuple)
			    (format *standard-output* "~%          It will not be inserted."))))
		    ((< (length sub-tuple) (length attribute-list))
		     (setf attr-val-list
			   (cons
			    (append sub-tuple
				    (mapcar
				     (function
				      (lambda (attr)
				       (caddr (assoc (string-upcase attr) qtrieve-var
						     :test 'string-equal))))
				     (nthcdr (length sub-tuple) attribute-list)))
			    attr-val-list)))
		    ((> (length sub-tuple) (length attribute-list))
		     (setf attr-val-list (cons (firstn (length attribute-list) sub-tuple) attr-val-list)))
		    (t
		     (setf attr-val-list (cons sub-tuple attr-val-list))))))
	    tuple))
	  (t
	   (setf attr-val-list tuple)))
    ;;
    ;;  Perform validity checking on the tuples to be inserted if some requested
    ;;
    (cond ((and *validity-checking* (not (member (string-upcase relation-name) *system-relations*
						 :test 'string-equal))
		(not *restore-operation*))
	   (setf mod-tuples attr-val-list
		 attr-val-list nil
		 qtrieve-var1 nil
		 qtrieve-var2 nil)
	   (mapc #'(lambda (attr &aux fun dom)
		     (setf fun (read-from-string
				(concatenate 'string *pkg-name*
					     (setf dom (cadr (assoc attr qtrieve-var
								    :test 'string-equal))))))
		     (push fun qtrieve-var1)
		     (push dom qtrieve-var2))
		 (reverse attribute-list))
	   (do ((tuples mod-tuples (cdr tuples)))
	       ((null tuples) t)
	     (if (domain-check attribute-list qtrieve-var1 qtrieve-var2 (car tuples))
		 (setf attr-val-list (cons (car tuples) attr-val-list))
		 (cond (*provide-warning-messages*
			(format *standard-output* "~%WARNING - ~S is not a valid tuple." (car tuples))
			(format *standard-output* "~%          It will not be inserted."))))))
	  (t
	   (setf attr-val-list (reverse attr-val-list))))
    (if (null attr-val-list)
	(return-from insert (format *standard-output* "~%ERROR - No valid tuples to be inserted.")))
    ;;
    ;;  Perform the actual insertation by calling the low level insert functions. The tuple must be inserted into all of
    ;; secondary index structures as well as the base relation. The low level accessor functions return a list of the tuples
    ;; which were inserted. Use this list to insert into the index relations.
    ;;
    ;;  These insert functions need to be surrounded by a UNWIND-PROTECT. The entry points need to saved else
    ;; where and restored if there is a problem
    ;;
    (setf tuples (funcall (find-symbol (concatenate 'string "INSERT-" imp "-" ss) *pkg-string*)
			  (string relation-name) attribute-list attr-val-list key (string relation-name)))
    (cond (indices
	   (mapc (function (lambda (key%)
		   (funcall (find-symbol (concatenate 'string "INDEX-INSERT-" imp "-" (second key%))
					 *pkg-string*)
			    (first key%) tuples attribute-list (third key%) relation-name)))
		 indices)))
    ;;
    ;;Reset the modified flag and increment the cardinality.
    ;;
    (cond ((not *restore-operation*)
	   (delete-or-modify 'system-relation t (list 'string-equal 'relation-name (string-upcase relation-name))
			     '("MODIFIEDP" "CARDINALITY") (list t (+ card (length attr-val-list))))
	   (delete-or-modify 'system-relation t (list 'string-equal 'relation-name "SYSTEM-RELATION")
			     '("MODIFIEDP") (list t))
	   (if *provide-status-messages*
	       (format *standard-output* "~%~s tuple~:P inserted into the ~s relation"
		       (length attr-val-list) relation-name))
	   (return-from insert relation-name))
	  (t
	   (return-from insert relation-name)))))

(defun insert-flavor-hash (relation-name attr-list tuples key index-name &aux hash-relation)
  (setf key (project-list tuples attr-list key)
	hash-relation (getp index-name 'entry-point))
  (setf attr-list (unconvert-attributes attr-list))
  (setf relation-name (read-from-string (concatenate 'string *pkg-name* (string-upcase relation-name))))
  (mapcar (function (lambda (tuple keyval &aux %tuple)
	    ;;
	    ;;Insert the tuple into the hash table using heap formation for collissions. Form the instance to be stored in the
	    ;; hash table.
	    ;;
	    (setf %tuple (make-instance relation-name))
	    (do ((tuple tuple (cdr tuple))
		 (attr-list attr-list (cdr attr-list)))
		((null tuple) %tuple)
	      (set-in-instance %tuple (car attr-list) (car tuple)))
	    (puthash keyval (cons %tuple (gethash keyval hash-relation)) hash-relation)
	    %tuple))
	  tuples key))

(defun insert-flavor-heap (relation-name attr-list tuples key index-name)
  key attr-list
  (setf attr-list (unconvert-attributes attr-list))
  (setf relation-name (read-from-string (concatenate 'string *pkg-name* (string-upcase relation-name))))
  (let (flavor-tuples (relation-tuples (getp index-name 'entry-point)) (tuples-length (length tuples)))
    (if (nth tuples-length relation-tuples)
	(putp index-name (append (setf flavor-tuples (mapcar #'(lambda (tuple &aux %tuple)
								 (setf %tuple (make-instance relation-name))
								 (do ((tuple tuple (cdr tuple))
								      (attr-list attr-list (cdr attr-list)))
								     ((null tuple) %tuple)
								   (set-in-instance %tuple (car attr-list)
										    (car tuple))))
							     tuples))
				 (getp index-name 'entry-point))
	      'entry-point)
	(putp index-name (append (getp index-name 'entry-point)
				 (setf flavor-tuples (mapcar #'(lambda (tuple &aux %tuple)
								 (setf %tuple (make-instance relation-name))
								 (do ((tuple tuple (cdr tuple))
								      (attr-list attr-list (cdr attr-list)))
								     ((null tuple) %tuple)
								   (set-in-instance %tuple (car attr-list)
										    (car tuple))))
							     tuples)))
	      'entry-point))
    flavor-tuples))

(defun insert-list-hash (relation attr-list tuples key index-name &aux hash-relation)
  relation
  (setf key (project-list tuples attr-list key)
	hash-relation (getp index-name 'entry-point))
  (mapc
   (function (lambda (tuple keyval)
     ;;
     ;;Here the tuple (val.1 val.2 .......val.n) itself is stored in the the hash table.
     ;;
     (puthash keyval (cons tuple (gethash keyval hash-relation)) hash-relation)))
   tuples key)
  tuples)

(defun insert-list-heap (relation attr-list tuples key index-name)
  key attr-list relation
  (let ((relation-tuples (getp index-name 'entry-point)) (tuples-length (length tuples)))
    ;; The idea here is that append copies all arguments except the last, therefore for speed reasons the small list should
    ;; be the first argument to append. Length takes too long so a faster determination of the probable shortest list must be made.
    (if (nth tuples-length relation-tuples)
	(putp index-name (append tuples relation-tuples) 'entry-point)
	(putp index-name (append relation-tuples tuples) 'entry-point)))
  tuples)

(defun insert-struct-hash (relation-name attr-list tuples key index-name
			   &aux hash-relation relation-macro (string-relation-name (string relation-name)))
  (setf key (project-list tuples attr-list key)
	hash-relation (getp index-name 'entry-point))
  ;;
  ;;Instead of calling the project for each tuple after the instance is created we are calling PROJECT-LIST so that we need to call
  ;; PROJECT only once.
  ;;
  (setf relation-macro (read-from-string (concatenate 'string *pkg-name* "MAKE-"
						      string-relation-name)))
  (setf attr-list
	(mapcar #'(lambda (attr)
		    (read-from-string (concatenate 'string ":" string-relation-name attr)))
		attr-list))
  (mapcar (function (lambda (tuple keyval &aux %tuple attr-val)
	    ;;
	    ;;Insert the instance into the hash table
	    ;;
	    (do ((tuple tuple (cdr tuple))
		 (attr-list attr-list (cdr attr-list)))
		((null tuple) attr-val)
	      (push `(quote ,(car tuple)) attr-val)
	      (push (car attr-list) attr-val))
	    (setf %tuple (eval `(,relation-macro ,@attr-val)))
	    (puthash keyval (cons %tuple (gethash keyval hash-relation)) hash-relation)
	    %tuple))
	  tuples key))

(defun insert-struct-heap (relation-name attr-list tuples key index-name
			   &aux relation-macro struct-tuples
			     (string-relation-name (string relation-name)))
  key attr-list
  (setf relation-macro (read-from-string (concatenate 'string *pkg-name* "MAKE-"
						      string-relation-name)))
  (setf attr-list
	(mapcar #'(lambda (attr)
		    (read-from-string (concatenate 'string ":" string-relation-name attr)))
		attr-list))
  (setf struct-tuples (mapcar (function (lambda (tuple &aux attr-val)
				(do ((tuple tuple (cdr tuple))
				     (attr-list attr-list (cdr attr-list)))
				    ((null tuple) attr-val)
				  (push `(quote ,(car tuple)) attr-val)
				  (push (car attr-list) attr-val))
				(eval `(,relation-macro ,@attr-val))))
			      tuples))
  (let ((relation-tuples (getp index-name 'entry-point)) (tuples-length (length struct-tuples)))
    ;; The idea here is that append copies all arguments except the last, therefore for speed reasons the small list should
    ;; be the first argument to append. Length takes too long so a faster determination of the probable shortest list must be made.
    (if (nth tuples-length relation-tuples)
	(putp index-name (append struct-tuples relation-tuples) 'entry-point)
	(putp index-name (append relation-tuples struct-tuples) 'entry-point)))
  struct-tuples)


;;; fragments


;; E") *system-index-key*
;;        `(and (string-equal index-name ,(string-upcase new-index-name))
;;       (string-equal relation-name ,(string-upcase relation-name)))
;;        nil 'system-index))
;; (if *provide-error-messages*
;;     (format *standard-output*
;;     "~%ERROR - An index with the name of ~s has already been defined on the relation ~s"
;;     new-index-name relation-name))
;; (return-from modify-index nil)))))
;;   ;;
;;   ;;  Determine if the requested storage structure is defined in the current database
;;   ;;
;;   (cond ((null index-type)
;;  (setf index-type (first index-info)))
;; (t
;;  (setf index-type (string-upcase index-type))



;; XR-BQ-LISTB DOCUMENTATION Insert a list of tuples or data from a file.

;;    RELATION-NAME   - Name of the relation into which the data is to be inserted.
;;    TUPLES     - List of tuples to be inserted. Tuples are expected to be in the list-of-values format.
;;    ATTRIBUTES - If the values in the tuples do not correspond to the attribute-list specified during
;;                 relation-defintion, specify a list of attributes to determine the order.
;;    PATHNAME   - If the data is in a file, specify the name of the file.'(lambda (attr)
;;   (if (not (or (member attr actual-p-l :test 'string-equal)
;;         (equal (length actual-p-l) (length sub-tuple))))
;;       (setf actual-p-l (append actual-p-l (list attr)))))
;;      attribute-list))
;;   (if (and *parameter-checking* (> (length actual-p-l)(length sub-tuple)))
;;       (progn
;;  (setf actual-p-l (firstn (length sub-tuple) actual-p-l))
;;  (if *provide-warning-messages*
;;      (format *standard-output*
;;       "~%WARNING - The tuple ~S is smaller in length than
