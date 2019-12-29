;;; -*- Mode:Common-Lisp; Package:RTMS; Fonts:(*CODE-FONT* *COMMENT-FONT* *STRING-FONT*); Base:10 -*-
;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved
;;; ATTACH-DETACH
;;;
;;; This file contains the following Explorer extensions to CommonLisp
;;; Standard as Indicated in the June 1985 Explorer Lisp
;;; Reference
;;;     errset
;;;     dump-forms-to-file
;;;     fs:directory-list
;;;
;;; The following function contains flavor references and thus are
;;; incompatable with CommonLisp. Their removal will not effect the
;;; functionality of RTMS.
;;;     validate-attach-flavor-avl
;;;     validate-attach-flavor-heap
;;;     validate-attach-flavor-hash
;;;

(defun attach-relation-load (relation-name pathname)
  (cond ((probe-file pathname)
	 (load pathname :verbose (if *provide-status-messages* t nil))
	 '*attach-detach-data*)
	((and (not (pathname-type pathname)) (probe-file (string-append pathname ".XLD")))
	 (load pathname :verbose (if *provide-status-messages* t nil)) '*attach-detach-data*)
	(t
	 (cond (*provide-error-messages*
		(format *standard-output* "~%ERROR - The file ~s does not exist" pathname)
		(format *standard-output* "~%      - The relation ~s can not be attached." relation-name)))
	 nil)))

(defun attach-relation (relation-name &rest keyword-list
			&key &optional attributes directory documentation format implementation-type key memory
			       pathname storage-structure
			&allow-other-keys
			&aux attribute-descriptor dir end-index imp pathname1 (attr nil) relation-info sto mem
			  hold-warning (card 0) temp-card)
  "Attach some existing data to relation.

   RELATION-NAME - The name of the relation to which the data is to be attached.
   ATTRIBUTES    - A list that describes the attributes in this relation.
   DIRECTORY     - The directory in which RTMS saves the attached data.
   DOCUMENTATION - A string that describes the specified relation.
   FORMAT        - A list corresponding to the ATTRIBUTES specifying their print width.
   IMPLEMENTATION-TYPE - Name of the implementation type.
   KEY           - A list of attributes that are to form the key.
   MEMORY        - Specifies a variable where the data to be attached is stored.
   PATHNAME      - If the data is stored in a file, specify its name here.
   STORAGE-STRUCTURE - Name of the storage-structure type."

  attributes directory documentation format implementation-type key memory pathname storage-structure

  (block attach-relation
    (multiple-value-setq (relation-name keyword-list relation-info)
      (initial-validation relation-name keyword-list '(att path imp sto key dir doc format mem)
			  '(save-directory implementation-type storage-structure cardinality attributes)
			  t))
    (setf imp (or (nth 1 relation-info) (car (get-keyword-value '(imp) keyword-list)) *rel-imp*)
	  sto (or (nth 2 relation-info) (car (get-keyword-value '(sto) keyword-list)) *rel-sto*)
	  dir (or (nth 0 relation-info) (car (get-keyword-value '(dir) keyword-list)))
	  card (or (nth 3 relation-info) 0)
	  attribute-descriptor (or (nth 4 relation-info) (car (get-keyword-value '(att) keyword-list)))
	  mem (car (get-keyword-value '(mem) keyword-list))
	  pathname1 (or (car (get-keyword-value '(path) keyword-list))
			(concatenate 'string *save-directory* relation-name ".lisp")))
    ;; Collect the names of the attributes.
    (if (car (get-keyword-value '(att) keyword-list))
	(mapc #'(lambda (attr-desc)
		  (if (not (listp attr-desc))
		      (push attr-desc attr)))
	      (reverse attribute-descriptor))
	(setf attr attribute-descriptor))
    (if (and (not mem) (not (car (get-keyword-value '(path) keyword-list))))
	(setf mem '*attach-detach-data*))
    (if (not mem)
	(progn
	  (if (setf end-index (search ";" pathname1))
	      (setf dir (subseq pathname1 0 (+ end-index 1)))
	      (setf pathname (concatenate 'string dir pathname1)))
	  (cond ((not (search "pages used in" (third (caar (errset (fs:directory-list dir) nil)))
			      ':test 'string-equal))
		 (if *provide-error-messages*
		     (format *standard-output* "~%ERROR - Directory ~s does not exist" dir))
		 (return-from attach-relation nil)))))
    (if (equal mem t)
	(setf mem '*attach-detach-data*))
    ;;
    ;;  Attach the relation in the appropriate manner
    (cond (relation-info
	   ;;
	   ;;  The relation exists, determine if it is empty
	   (cond ((relation-emptyp relation-name (nth 1 relation-info) (nth 2 relation-info))
		  (if (not mem)
		      (setf mem (attach-relation-load relation-name pathname1)))
		  (if (car (errset (symbol-value mem) t))
		      (return-from attach-relation (process-attach relation-name sto imp mem attr))
		      (return-from attach-relation nil)))
		 (t
		  (if (not mem)
		      (setf mem (attach-relation-load relation-name pathname1)))
		  (if (car (errset (symbol-value mem) t))
		      (progn
			(if
			 (not (setf temp-card
				    (validate-attach relation-name (string-upcase sto)
						     (string-upcase imp) (symbol-value mem) attr)))
			 (progn
			   (if *provide-error-messages*
			       (format *standard-output* "~%ERROR - ~S is not valid data."
				       (symbol-value mem)))
			   (return-from attach-relation nil)))
			(modify 'system-relation
				'where `(string-equal relation-name (string-upcase ',relation-name))
				'attributes '(cardinality modifiedp) 'values (list (+ temp-card card) t))
			(if *provide-status-messages*
			    (format *standard-output* "~%Relation ~s has been successfully attached"
				    relation-name))
			(return-from attach-relation relation-name))
		      (return-from attach-relation nil)))))
	  (t
	   ;;
	   ;; -----> The keyword-list is not formed properly here  <-------
	   ;;
	   (setf hold-warning *provide-warning-messages*
		 *provide-warning-messages* nil
		 keyword-list (get-keyword-value-prereq '(imp sto key dir doc format)  keyword-list)
		 *provide-warning-messages* hold-warning)
	   (if (not (define-relation relation-name attribute-descriptor keyword-list))
	       (return-from attach-relation nil))
	   (if (not mem)
	       (setf mem (attach-relation-load relation-name pathname1)))
	   (if (car (errset (symbol-value mem) t))
	       (return-from attach-relation (process-attach relation-name sto imp mem attr))
	       (return-from attach-relation nil))))))

(defun process-attach (relation-name sto imp mem attr)
  (let (card)
    (cond ((not (setf card (validate-attach relation-name (string-upcase sto) (string-upcase imp)
					    (symbol-value mem)  attr)))
	   (if *provide-error-messages*
	       (format *standard-output* "~%ERROR - ~S is not valid data." (symbol-value mem)))
	   (destroy-relation relation-name)
	   nil)
	  (t
	   (modify 'system-relation 'where `(string-equal relation-name (string-upcase ',relation-name))
		   'attributes '(cardinality modifiedp) 'values (list card t))
	   (if *provide-status-messages*
	       (format *standard-output* "~%Relation ~s has been successfully attached" relation-name))
	   relation-name))))

(defun validate-attach (relation-name sto imp data attr)
  (funcall (find-symbol (concatenate 'string "VALIDATE-ATTACH" "-" imp "-" sto) *pkg-string*) relation-name
	   attr data))


(defun validate-attach-list-heap (relation-name attr data &aux (len (length attr)))
  (cond (*validity-checking*
	 (cond ((or (not (listp data))
		    (apply 'or (mapcar #'(lambda (%data)
					   (or (not (listp %data)) (not (equal (length %data) len))))
				       data)))
		nil)
	       (t
		;;Later on, we will have to go through each tuple, call dom-check - just like INSERT.
		(putp relation-name (append (getp relation-name 'entry-point) data) 'entry-point)
		(length data))))
	(t
	 (insert-list-heap relation-name attr data nil relation-name)
	 (length data))))

(defun validate-attach-list-hash (relation-name attr data)
  (block validate-attach-list-hash
    (let ((table (getp relation-name 'entry-point)) (card 0) (len (length attr)))
      (cond (*validity-checking*    ;Some validation,like in Insert, needs to be done.
	     (if (not (hash-table-p data))
		 (return-from validate-attach-list-hash nil))
	     (progn
	       (maphash #'(lambda (key tuples)
			    (if (or (not (listp tuples))
				    (apply 'or (mapcar #'(lambda (%data)
							   (or (not (listp %data))
							       (not (equal (length %data) len))))
						       tuples)))
				(return-from validate-attach-list-hash nil))
			    (puthash key tuples table)
			    (setf card (+ card (length tuples))))
			data)
	       (return-from validate-attach-list-hash card)))
	    (t
	     (maphash #'(lambda (key tuples)
			  (puthash key tuples table)
			  (setf card (+ card (length tuples))))
		      data)
	     (return-from validate-attach-list-hash card))))))

(defun validate-attach-list-avl (relation-name attr data)
  (let ((len (length attr)) list-data)
    (cond ((not (car (errset (setf list-data (avl-inorder-traversal data)) t)))
	   nil)
	  (t
	   (cond (*validity-checking*
		  (cond ((or (not (listp list-data))
			     (apply 'or (mapcar #'(lambda (%data)
						    (or (not (listp %data)) (not (equal (length %data) len))))
						list-data)))
			 nil)
			(t
			 (cond ((null (get relation-name 'entry-point))
				(putp relation-name data 'entry-point))
			       (t
				(insert relation-name 'tuples list-data)))
			 (length list-data))))
		 (t
		  (cond ((null (get relation-name 'entry-point))
			 (putp relation-name data 'entry-point))
			(t
			 (insert relation-name 'tuples list-data)))
		  (length list-data)))))))

(defun validate-attach-flavor-avl (relation-name attr data
				   &aux temp list-data)
  (block validate-attach-flavor-avl
    (cond ((not (car (errset (setf list-data (avl-inorder-traversal data)) t)))
	   (return-from validate-attach-flavor-avl nil))
	  (t
	   (cond (*validity-checking*
		  (cond ((not (member (read-from-string (concatenate 'string *pkg-name* relation-name))
				      *all-flavor-names* :test 'string-equal))
			 (return-from validate-attach-flavor-avl nil))
			(t
			 (setf temp (send (make-instance (read-from-string (concatenate 'string *pkg-name*
											relation-name)))
					  ':which-operations))
			 (mapc #'(lambda (x)
				   (if (not (member (read-from-string (concatenate 'string ":"
										   (string-upcase x))) temp
										   :test 'string-equal))
				       (return-from validate-attach-flavor-avl nil)))
			       attr)
			 (if (not (car (errset
					(mapc #'(lambda (%data)
						  (if (not (equal temp (send %data ':which-operations)))
						      (return-from validate-attach-flavor-avl nil)))
					      list-data) t)))
			     (return-from validate-attach-flavor-avl nil))
			 (cond ((null (getp relation-name 'entry-point))
				(putp relation-name data 'entry-point))
			       (t
				(insert relation-name
					'tuples (project-flavor list-data attr attr relation-name))))
			 (return-from validate-attach-flavor-avl (length list-data)))))
		 (t
		  (cond ((null (getp relation-name 'entry-point))
			 (putp relation-name data 'entry-point))
			(t
			 (insert relation-name 'tuples (project-flavor list-data attr attr relation-name))))
		  (return-from validate-attach-flavor-avl (length list-data))))))))

(defun validate-attach-flavor-heap (relation-name attr data &aux temp)
  (block validate-attach-flavor-heap
    (cond ((not (listp data))
	   (return-from validate-attach-flavor-heap nil)))
    (cond (*validity-checking*
	   (cond ((not (member (read-from-string (concatenate 'string *pkg-name* relation-name))
			       *all-flavor-names* :test 'string-equal))
		  (return-from validate-attach-flavor-heap nil))
		 (t
		  (setf temp (send (make-instance
				    (read-from-string (concatenate 'string *pkg-name* relation-name)))
				   ':which-operations))
		  (mapc #'(lambda (x)
			    (if (not (member (read-from-string (concatenate 'string ":" (string-upcase x)))
					     temp :test 'string-equal))
				(return-from validate-attach-flavor-heap nil)))
			attr)
		  (if (not (car (errset (mapc #'(lambda (%data)
						  (if (not (equal temp (send %data ':which-operations)))
						      (return-from validate-attach-flavor-heap nil)))
					      data) t)))
		      (return-from validate-attach-flavor-heap nil))
		  (putp relation-name (append (getp relation-name 'entry-point) data) 'entry-point)
		  (return-from validate-attach-flavor-heap (length data)))))
	  (t
	   (putp relation-name (append (getp relation-name 'entry-point)  data) 'entry-point)
	   (return-from validate-attach-flavor-heap (length data))))))

(defun validate-attach-flavor-hash (relation-name attr data
				    &aux (table (getp relation-name 'entry-point)) (card 0) temp)
  (block validate-attach-flavor-hash
    (if (not (hash-table-p data))
	(return-from validate-attach-flavor-hash nil))
    (cond (*validity-checking*
	   (cond ((not (member (read-from-string (concatenate 'string *pkg-name* relation-name))
			       *all-flavor-names* :test 'string-equal))
		  (return-from validate-attach-flavor-hash nil))
		 (t (setf temp (send (make-instance
				      (read-from-string (concatenate 'string *pkg-name* relation-name)))
				     ':which-operations))
		    (mapc #'(lambda (x)
			      (if (not (member (read-from-string (concatenate 'string ":"
									      (string-upcase x))) temp
									      :test 'string-equal))
				  (Return-from validate-attach-flavor-hash nil)))
			  attr)
		    (maphash
		     #'(lambda
			   (key tuples)
			 (if (not (car (errset (mapc
						#'(lambda (%data)
						    (if (not (equal temp (send %data ':which-operations)))
							(return-from validate-attach-flavor-hash nil)))
						tuples) t)))
			     (return-from validate-attach-flavor-hash nil))
			 (puthash key tuples table)
			 (setf card (+ card (length tuples))))
		     data)
		    (return-from validate-attach-flavor-hash card))))
	  (t
	   (maphash #'(lambda (key tuples)
			(puthash key tuples table)
			(setf card (+ card (length tuples))))
		    data)
	   (return-from validate-attach-flavor-hash card)))))

(defun validate-attach-struct-avl (relation-name attr data
				   &aux temp list-data)
  (block validate-attach-struct-avl
    (cond ((not (car (errset (setf list-data (avl-inorder-traversal data)) t)))
	   nil)
	  (t
	   (cond (*validity-checking*
		  (cond ((not (setf temp (fourth (get (read-from-string
						       (concatenate 'string *pkg-name* relation-name))
						      'si:defstruct-description))))
			 (return-from validate-attach-struct-avl nil))
			(t
			 (mapc
			  #'(lambda (x &aux data fn)
			      (if (not (setf data (assoc
						   (setf fn (read-from-string
							     (concatenate 'string *pkg-name*
									  relation-name
									  (string-upcase x))))
						   temp)))
				  (return-from validate-attach-struct-avl nil)
				  (if (not (equal (seventh data) fn))
				      (return-from validate-attach-struct-avl nil))))
			  attr)
			 (cond ((null (get relation-name 'entry-point))
				(putp relation-name data 'entry-point))
			       (t
				(insert relation-name
					'tuples (project-struct list-data attr attr relation-name))))
			 (return-from validate-attach-struct-avl (length list-data)))))
		 (t
		  (cond ((null (get relation-name 'entry-point))
			 (putp relation-name data 'entry-point))
			(t
			 (insert relation-name 'tuples (project-struct list-data attr attr relation-name))))
		  (return-from validate-attach-struct-avl (length list-data))))))))

(defun validate-attach-struct-heap (relation-name attr data
				    &aux temp )
  (block validate-attach-struct-heap
    (cond ((not (listp data))
	   (return-from validate-attach-struct-heap nil)))
    (cond (*validity-checking*
	   (cond ((not (setf temp (fourth
				   (get (read-from-string (concatenate 'string *pkg-name* relation-name))
					'si:defstruct-description))))
		  (return-from validate-attach-struct-heap nil))
		 (t
		  (mapc
		   #'(lambda (x &aux data fn)
		       (if (not (setf data (assoc
					    (setf fn (read-from-string
						      (concatenate 'string *pkg-name*  relation-name
								   (string-upcase x))))
					    temp)))
			   (return-from validate-attach-struct-heap nil)
			   (if (not (equal (seventh data) fn))
			       (return-from validate-attach-struct-heap nil))))
		   attr)
		  (putp relation-name (append (getp relation-name 'entry-point) data) 'entry-point)
		  (return-from validate-attach-struct-heap (length data)))))
	  (t
	   (putp relation-name (append (getp relation-name 'entry-point) data)'entry-point)
	   (return-from validate-attach-struct-heap (length data))))))

(defun validate-attach-struct-hash (relation-name attr data
				    &aux (table (getp relation-name 'entry-point)) (card 0) temp)
  (block validate-attach-struct-hash
    (if (not (hash-table-p data))
	(return-from validate-attach-struct-hash nil))
    (cond (*validity-checking*
	   (cond ((not (setf temp (fourth
				   (get (read-from-string (concatenate 'string *pkg-name* relation-name))
					'si:defstruct-description))))
		  (return-from validate-attach-struct-hash nil))
		 (t
		  (mapc
		   #'(lambda (x &aux data fn)
		       (if (not (setf data (assoc
					    (setf fn (read-from-string
						      (concatenate 'string *pkg-name* relation-name
								   (string-upcase x))))
					    temp)))
			   (return-from validate-attach-struct-hash nil)
			   (if (not (equal (seventh data) fn))
			       (return-from validate-attach-struct-hash nil))))
		   attr)
		  (maphash #'(lambda (key tuples)
			       (if (not (listp tuples))
				   (return-from validate-attach-struct-hash nil))
			       (puthash key tuples table)
			       (setf card (+ card (length tuples))))
			   data)
		  (return-from validate-attach-struct-hash card))))
	  (t
	   (maphash #'(lambda (key tuples)
			(puthash key tuples table)
			(setf card (+ card (length tuples))))
		    data)
	   (return-from validate-attach-struct-hash card)))))

(defun detach-relation-save (relation-name pathname)
  (dump-forms-to-file pathname `((setf *attach-detach-data* ',(getp relation-name 'entry-point)))))

(defun detach-relation (relation-name &rest keyword-list
			&key &optional disk memory pathname
			&allow-other-keys
			&aux dir end-index imp pathname1 sto mem)
  "Detach data in an existing relation into a variable or onto the disk.

   RELATION-NAME - The name of the relation from which the data is to be detached.
   DISK          - If T, RTMS stores the data in the file specified in the PATHNAME.
   MEMORY        - If set to T, the detached data is stored in the variable rtms:*attach-detach-data*.
                   If any variable name is supplied, the data will be stored in it.
   PATHNAME      - Name of the file in which the detached data is to be saved."
  disk memory pathname
  (block detach-relation
    (if *parameter-checking*
	(if (or (not (active-database relation-name))
		(not (setf relation-name (validate-name relation-name))))
	    (return-from detach-relation nil)
	    (setf relation-name (string-upcase relation-name)))
	(setf relation-name (string-upcase relation-name)))
    (setf keyword-list  (de-nest-keyword-list keyword-list))
    ;;
    ;;  Must deteremine if the relation exists. May not detach a view
    ;;
    (cond ((not (setf dir (car (qtrieve 'system-relation *system-relation-attributes*
					'(save-directory implementation-type storage-structure)
					*system-relation-key*
					`(string-equal relation-name ,relation-name)))))
	   (if *provide-error-messages*
	       (format *standard-output* "~%ERROR - ~S is not a relation in database ~s."
		       relation-name *active-db*))
	   (return-from detach-relation nil)))
    (setf imp (nth 1 dir)
	  sto (nth 2 dir)
	  dir (nth 0 dir))
    (if *parameter-checking*
	(setf keyword-list (get-keyword-value-prereq '(path disk mem) keyword-list)))
    ;;
    ;;  Form the pathname where the file will be saved
    ;;
    (setf mem (car (get-keyword-value '(mem) keyword-list)))
    (setf pathname1 (car (get-keyword-value '(path) keyword-list)))
    (if (and (not mem) (not pathname1))
	(setf mem '*attach-detach-data*))
    (setf keyword-list (append '(disk) (get-keyword-value '(disk) keyword-list)))
    ;;
    ;;  Determine if the specified directory exists
    ;;
    (if pathname1
	(progn
	  (cond ((setf end-index (search ";" pathname1))
		 (setf dir (subseq pathname1 0 (+ end-index 1))))
		(t
		 (setf pathname1 (concatenate 'string dir pathname1))))
	  (cond ((not (search "pages used in" (third (caar (errset (fs:directory-list pathname1) nil)))))
		 (if *provide-error-messages*
		     (format *standard-output* "~%ERROR - Directory ~s does not exist" dir))
		 (return-from detach-relation nil)))))
    ;;
    ;;  Everything looks alright, get the data, write it out and then destroy the relation
    ;;
    (if *provide-status-messages*
	(format *standard-output* "~%Relation ~s will now be detached from the ~s database"
		relation-name *active-db*))
    (cond ((relation-emptyp relation-name imp sto)
	   (cond ((destroy-relation relation-name keyword-list)
		  (if *provide-status-messages*
		      (format *standard-output* "~%The relation ~s is empty, nothing to output"
			      relation-name))
		  (return-from detach-relation relation-name))
		 (t
		  (return-from detach-relation nil))))
	  (t
	   (if (or (not mem) (equal mem t))
	       (setf *attach-detach-data* (getp relation-name 'entry-point))
	       (set mem (getp relation-name 'entry-point)))
	   (if pathname1
	       (detach-relation-save relation-name pathname1))
	   (cond ((destroy-relation relation-name keyword-list)
		  (if *provide-status-messages*
		      (format *standard-output* "~%Relation ~s has been sucessfully detached"
			      relation-name))
		  (return-from detach-relation relation-name))
		 (t
		  (return-from detach-relation nil)))))))

(defun relation-emptyp (relation-name implementation-type storage-structure)
  implementation-type
  (funcall (find-symbol (concatenate 'string "RELATION-" storage-structure "-EMPTYP") *pkg-string*)
	   relation-name))

(defun relation-hash-emptyp (relation-name)
  (block relation-hash-emptyp
    (maphash (function (lambda (key tuple)
	       key tuple
	       (return-from relation-hash-emptyp  (not tuple))))
	     (getp relation-name 'entry-point))
    (return-from relation-hash-emptyp  t)))

(defun relation-heap-emptyp (relation-name)
  (not (getp relation-name 'entry-point)))

(defun relation-avl-emptyp (relation-name)
  (not (getp relation-name 'entry-point)))
