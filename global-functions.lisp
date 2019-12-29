;;; -*- Mode:Common-Lisp; Package:RTMS; Base:10 -*-

;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved
;;;
;;; global-functions
;;;
;;; This file contains the following Explorer extensions to CommonLisp
;;; Standard as Indicated in the June 1985 Explorer Lisp
;;; Reference
;;;
;;;     firstn
;;;     arglist
;;;     fs:directory-list
;;;     errset
;;;     ed
;;;     tyo
;;;
;;; The following function contains flavor references and thus are
;;; incompatable with CommonLisp. Their removal will not effect the
;;; functionality of RTMS.
;;;
;;;     project-flavor-prereq
;;;     fast-project-flavor
;;;     put-flavor
;;;     qtrieve-flavor-hash
;;;     qtrieve-flavor-heap
;;;     qtrieve-flavor-avl
;;;     project-flavor
;;;
;;; Change History
;;; 04.09.87 MRR Changed a line in HELP-ATTRIBUTE, so that print-width
;;; is not made into a mouse-sensitive item.  Fixed MODIFY-TRANSACTION
;;; and SAVE-TRANSACTION so that the fs:directory-list works for
;;; remote files, too.


(defun dbp (&optional (operation nil))
  (active-database operation))

(defun active-db (&optional (operation nil))
  (active-database operation))

(defun active-dbp (&optional (operation nil))
  (active-databasep operation))

(defun active-database (&optional (operation nil) &aux active-system-relations)
  ;; Determine if the system relations have been completely defined
  (cond (*restore-operation*
	 t)
	(t
	 (setf active-system-relations
	       (mapcar (function (lambda (sys-rel)
			 (not (and (not (getp sys-rel 'entry-point))
				   (not (string-equal sys-rel "SYSTEM-VIEW"))
				   (not (string-equal sys-rel "SYSTEM-INDEX"))))))
		       *system-relations*))
	 (setf active-system-relations (not (member 'nil active-system-relations)))
	 (cond ((or (null *active-db*)
		    (and (null active-system-relations) (not (member operation *system-relations*
								     :test 'string-equal))))
		(cond ((and *provide-error-messages*
			    (and (not (string-equal operation "DEFDB"))
				 (not (string-equal operation "RESTORE"))))
		       (format *standard-output*  "~%ERROR - There is currently no active database")
		       (format *standard-output*
			       "~%        Please either restore a saved database or define a new one")))
		(recover-all)
		nil)
	       (t
		*active-db*)))))

;; determine if the system relations have been completely defined
(defun active-databasep (&optional (operation nil))
  (let (active-system-relations)
    (cond (*restore-operation*
	   t)
	  (t
	   (setf active-system-relations
		 (mapcar (lambda (sys-rel)
			   (not (and (not (getp sys-rel 'entry-point))
				     (not (string= sys-rel "system-view"))
				     (not (string= sys-rel "system-index")))))
			 *system-relations*))
	   (setf active-system-relations
		 (not (member 'nil active-system-relations)))
	   (cond ((or (null *active-db*)
		      (and (null active-system-relations)
			   (not (member operation *system-relations*
					:test 'string-equal))))
		  (recover-all)
		  nil)
		 (t
		  *active-db*))))))

(defun anyp (value)
  value
  t)

(defun domain-check (attributes attr-fun attr-dom values &aux original-fun)
  (block domain-check
    (mapc (function (lambda (value attr fun dom)
	    (setf original-fun fun)
	    (if (setf fun (find-symbol (string-upcase fun) "GLOBAL"))
		fun
		(setf fun (find-symbol (string-upcase original-fun) *pkg-string*)))
	    (cond ((null (funcall fun value))
		   (if *provide-warning-messages*
		       (format *standard-output*
			       "~%WARNING - ~s is not a legal value for the ~s attribute whose domain is ~s"
			       value (read-from-string attr) (read-from-string dom)))
		   (return-from domain-check nil)))))
	  values attributes attr-fun attr-dom)
    (return-from domain-check t)))

(defun project-tuples (tuples attribute-list imp relation-name)
  ;;  Convert the tuples into a list-heap format so that they may be processed
  (if (not (string-equal "LIST" imp))
      (funcall (find-symbol (concatenate 'string "PROJECT-" imp) *pkg-string*) tuples attribute-list
	       attribute-list relation-name)
      tuples))

(defun eval-where (tuples attribute-list where-clause imp relation-name
		   &aux temp-attribute-list)
  (if (not (string-equal imp "LIST"))
      (setf tuples (project-tuples tuples attribute-list imp relation-name)))  ;;
  ;;  Determine if the tuples may be returned unchaged.
  (cond ((or (null tuples) (equal where-clause t))
	 tuples)
	((not (listp where-clause))
	 (if (eval where-clause)
	     tuples
	     nil))
	(t
	 (multiple-value-setq (where-clause temp-attribute-list)
	   (eval-where-prereq where-clause attribute-list relation-name))
	 (fast-eval-where tuples where-clause temp-attribute-list))))

(defun eval-where-prereq (where-clause attribute-list relation-name
			  &aux reader-package-name temp-attribute-list)
  ;; Determine if any optimization may be performed on the where
  ;; clause. This is usually dealing with aggregrate functions.
  (cond ((symbolp relation-name)
	 (setf reader-package-name (package-name (symbol-package relation-name))))
	(t
	 (setf reader-package-name *pkg-string*)))
  (if (not (equal where-clause t))
      (setf where-clause (parse-where where-clause)))
  ;; Determine the attributes which have global variables defined for
  ;; them. These will have to be take into account in the where clause
  ;; since it comes from the reader.
  (setf temp-attribute-list
	(mapcar #'(lambda (attr &aux global-attr)
		    ;; This currently has a problem in that if a
		    ;; relaiton name is contained in the global
		    ;; package this will cause a problem. The only
		    ;; solution seems to rewrite eval-where to parse
		    ;; the where-clause and locate the attributes
		    ;; which are used and there package.
		    (if (setf global-attr (find-symbol (string-upcase attr) "GLOBAL"))
			global-attr
			(read-from-string (concatenate 'string reader-package-name ":" attr))))
		attribute-list))
  (values where-clause temp-attribute-list))

(defun fast-eval-where (val-list where-clause temp-attribute-list
			&aux (result nil))
  (progv temp-attribute-list nil
    (mapc
     (function (lambda (%tuple)
       (mapc #'(lambda (x z)
		 (set x z))
	     temp-attribute-list %tuple)
       (if (eval where-clause)
	   (setf result (cons %tuple result)))))
     val-list))
  (reverse result))

(defun get-directory (keyword-list &optional (default-directory nil))
  (let (directory num dir)
    (cond ((null (setf directory
		       (car (get-keyword-value '(dir) keyword-list))))
	   (setf directory (or default-directory
			       *save-directory*
			       (concatenate 'string user-id ";"))))
	  ((not (stringp directory))
	   (setf directory (string directory))
	   (cond ((and (null (search ";" directory))
		       (null (search "." directory)))
		  (setf directory (concatenate 'string directory ";")))))
	  ((and (null (search ";" directory))
		(null (search "." directory)))
	   (setf directory (concatenate 'string directory ";"))))
    (when (setf num (search ":;" directory))
      (setf directory
	    (concatenate 'string (subseq directory 0 (1+ num)) user-id ";")))
    (setf directory
	  (parse-pathname directory))
    (concatenate 'string (send (send directory :host) :short-name) ":"
		 (if (setf directory (send directory :directory))
		     (if (listp directory)
			 (progn
			   (setf dir (concatenate 'string (car directory)))
			   (mapc #'(lambda (sub-dir)
				     (setf dir (concatenate 'string dir "." sub-dir)))
				 (cdr directory))
			   dir)
			 directory)
		     user-id)
		 ";")))

(defun get-keyword-value (keyword-list keyword-value-list
			  &aux (result nil) (temp-result nil) key-found exit-var string-value
			    default-variable)
  (setf keyword-list (convert-attributes keyword-list))
  (do ((keyword keyword-list (cdr keyword)))
      ((null keyword) (nreverse result))
    (setf key-found nil)
    (setf temp-result (do ((value-list keyword-value-list (cddr value-list)))
			  ((or (null value-list) key-found) exit-var)
			(if (not (listp (car value-list)))
			    (setf string-value (string-upcase (car value-list))))
			(if (setf key-found (equal (search (car keyword) string-value) 0))
			    (setf exit-var (cadr value-list))
			    (setf exit-var nil))))
    (setf result (if (null key-found)
		     (progn
		       (setf default-variable (read-from-string (concatenate 'string *pkg-name* "*"
									     (car keyword) "*")))
					;that keyword is not in keyword-value-list. value will be default value
		       (cons (if (boundp default-variable)
				 (symbol-value default-variable)
				 *default-keyword-value*)
			     result))
		     (cons temp-result result)))))


(defun get-keyword-value-prereq (command-keywords-list keyword-value-list
				 &aux  (result nil) this-is-keyword (first-is-keyword nil)  first-value-list
				   second-value-list third-value-list second-is-keyword third-is-keyword
				   first-value)
  ;;
  ;; the keyword-value-list has to begin with a keyword to be treated
  ;;
  ;;  The keywords are made into string so that complications will not arise from keywords being interned into a different package.
  (setf command-keywords-list (mapcar (function (lambda (%keyword)
					(string-upcase %keyword)))
				      command-keywords-list))
  ;;
  ;;  Remove any extra sets of parens that may be around the keyword list which may be there if the macro version of the function is
  ;; called also for older versions of RTMS which allow the keyword list to really be a list of keywords.
  ;;
  (setf keyword-value-list (do ((value-list (de-nest-keyword-list keyword-value-list) (cdr value-list)))
			       ((or (null value-list) first-is-keyword (equal value-list '(nil)))
				(if first-is-keyword
				    (cons first-value value-list)
				    nil))
			     (setf this-is-keyword nil)
			     (if (and (first value-list)
				      (or (symbolp (first value-list)) (stringp (first value-list))))
				 (progn
				   (setf first-value-list (string-upcase (first value-list))
					 first-value (car value-list))))
			     (setf first-is-keyword (and (first value-list) (symbolp (first value-list))
							 ;; This "DO" is to allow to use abbreviated keywords
							 (do ((commands command-keywords-list (cdr commands)))
							     ((or (null commands) this-is-keyword)
							      this-is-keyword)
							   (setf this-is-keyword (equal
										  (search (car commands)
											  first-value-list)
										  0)))))
			     (if (not first-is-keyword)
				 (if *provide-warning-messages*
				     (format *standard-output* "~%WARNING unrecognized keyword -->~S"
					     (car value-list))))))
  (do ((value-list keyword-value-list (cddr value-list)))
      ((null value-list) (nreverse result))
    (setf third-is-keyword nil)
    (do () (third-is-keyword)
      (setf second-is-keyword nil)
      (cond
					;third word is a keyword
	((and (third value-list) (symbolp (third value-list))
	      (setf third-value-list (string-upcase (third value-list)))
	      (do ((commands command-keywords-list (cdr commands)))
		  ((or (null commands) third-is-keyword) third-is-keyword)
		(setf third-is-keyword (equal (search (car commands) third-value-list) 0))))
	 nil)
	;; no value for that keyword. add default value to keyword-value-list. I had to change the test (NOT(SECOND value-list))
	;; because it didn't allowed to have the last value of keyword-value-list equal to NIL.
	((equal (length value-list) 1)   ;no value for the last keyword
	 (setf second-value-list (string-upcase (second value-list))
	       first-value-list (string-upcase (first value-list)))
	 (cond ((and (symbolp (second value-list)) ;next word: keyword instead of value
		     (do ((commands command-keywords-list (cdr commands)))
			 ((or (null commands) second-is-keyword) second-is-keyword)
		       (setf second-is-keyword (and (equal (search (car commands) second-value-list) 0)
						    (not (equal (search (car commands) first-value-list)
								0))))))
		(setf third-is-keyword t)
		;;
		;;  There is a problem here if the keyword is the same as the global variable
		;;
		(setf value-list
		      (cons (car value-list)
			    (cons (if (boundp (read-from-string (concatenate 'string *pkg-name* "*"
									     first-value-list "*")))
				      (symbol-value (read-from-string (concatenate 'string *pkg-name* "*"
										   first-value-list "*")))
				      *default-keyword-value*)
				  (cdr value-list)))))))
					;last pair of keyword-value-list
	((not (third value-list))
	 (setf third-is-keyword t))
					;third word will be ignored as it and second word are not keyword
	(t
	 (if *provide-warning-messages*
	     (format *standard-output* "~%WARNING unrecognized keyword -->~S" (third value-list)))
	 (setf value-list (cons (first value-list) (cons (second value-list) (cdddr value-list)))))))
    (setf result (cons (second value-list)(cons (first value-list) result)))))

(defun rtms-describe (&optional object &rest ignore)
  (help object))

(defun help (&optional object &rest ignore)
  (if object (help-object (string-upcase object))
      (if (not (car (errset (send *output-window* ':exposed-p) nil)))
	  (format *standard-output*  "~&(HELP <object>) will provide help on <object>.")
	  (format *typeout-window*"~&
  You are in the user interface to the database. The interaction pane
  is used to accept your input and the results are displayed in the output window.
  Any trivial output is displayed in the typeout window and when
  you type in any character the typeout window disappears.

  The interface menu is at the bottom. In the submenu \"help\" the menu item
  \"inspect-dbms-object\" lets you seek information on any database object
  (currently - a relation, command or an attribute). If the object is any
  database command, syntax for that command is provided. In the submenu \"help\"
  the menu item \"introduction\" prints out this information.

  The menu item \"command-menu\" pops up a menu of all database
  commands and when a command is selected all the input data required
  to execute that command is obtained using a choose-variables-value
  window and the command is executed. The menu item \"exit\" allows you to exit
  the interface by burying it and the item \"kill\" kills the interface process.
  The item \"display\" lets you scroll in the output window. For more information
  on any of these items, see the documentation provided by the command tables.

  Each of the attribute names  and relation names is made mouse-sensitive. If you
  click on any mouse-sensitive item, either that item is inspected or some stated
  (as seen in the who-line-documentation) operation is performed. Each line of
  display is sensitive to line-area-scrolling and clicking the left button in the
  zone of line-area-scrolling (when -> appears) on any line will cause the
  entire line to be displayed in the typeout-window.

  The interaction pane runs under universal command loop. It has preemptable read
  facility - lets you typein part of a lisp expression and click on a menu or
  a mouse-sensitive item and when the execution of the mouse command is over,
  continue with the unfinished expression. It also has command completion
  capability and pops up a choose-variables-values window if all the arguments are
  not provided."))))

					;**************************************************************************
					;             Functions used in inspecting database objects.                       *
					;**************************************************************************
(defun help-object (object
		    &aux attribute-list (found-object nil) help-object atomic-object (db-test t) out-window
		      attribute-test)
  (block help-object
    (if (not (active-database))
	(return-from help-object nil))
    (setf out-window (car (errset (send *output-window* ':exposed-p) nil)))
    (cond ((null (setf object (validate-sym object t)))
	   (return-from help-object nil)))
    (setf db-test nil
	  attribute-test nil
	  atomic-object (read-from-string object))
    ;;
    ;;  Determine if the type of the object and call the appropriate function. It is possible that the name is used for more than one
    ;; object.
    ;;
    (cond-every ((string-equal object *active-db*)
		 (help-database out-window atomic-object)
		 (setf found-object t))
		(t
		 ;;
		 ;;  Determine if the object name is a relation or an attribute
		 ;;
		 (mapc (function
			(lambda (systup &aux rel)
			 (cond-every ((string-equal
				       (setf rel (caar (project-list (list systup)
								     *system-relation-attributes*
								     '("RELATION-NAME"))))
				       object)
				      (help-relation out-window object
						     (car (project-list (list systup)
									*system-relation-attributes*
									'("MODIFIEDP"
									  "SAVE-DIRECTORY"
									  "IMPLEMENTATION-TYPE"
									  "STORAGE-STRUCTURE"
									  "KEY"
									  "TUPLE-FORMAT"
									  "DOC"
									  "CARDINALITY"
									  "ATTRIBUTES"))))
				      (setf found-object t))
				     (t
				      (setf attribute-list (caar (project-list
								  (list systup)
								  *system-relation-attributes*
								  '("ATTRIBUTES"))))
				      (cond ((member object attribute-list :test 'string-equal)
					     (help-attribute out-window object rel attribute-test)
					     (setf attribute-test t)))))))
		       (qtrieve 'system-relation *system-relation-attributes* *system-relation-attributes*
				*system-relation-key* t)))
		;;
		;;  Determine if the object is a view
		;;
		((setf help-object (assoc object (qtrieve 'system-view *system-view-attributes*
							  *system-view-attributes* *system-view-key* t)
					  :test 'string-equal))
		 (help-view out-window help-object atomic-object)
		 (setf found-object t))
		;;
		;;  Determine if the object is an index
		;;
		((setf help-object
		       (funcall
			(find-symbol (concatenate 'string "RETRIEVE-" *system-relation-base-implementation*
						  "-" *system-relation-storage-structure*) *pkg-string*)
			'system-index *system-index-attributes* *system-index-attributes* *system-index-key*
			`(string-equal index-name ,(string-upcase object))
			nil 'system-index))
		 (help-index out-window help-object atomic-object)
		 (setf found-object t))
		;;
		;;  Determine if the object is an implementation
		;;
		((setf help-object (assoc object (qtrieve 'system-implementation
							  *system-implementation-attributes*
							  *system-implementation-attributes*
							  *system-implementation-key* t)
					  :test 'string-equal))
		 (help-implementation out-window help-object atomic-object)
		 (setf found-object t))
		;;
		;;  Determine if the object is a storage-structure
		;;
		((setf help-object (assoc object (qtrieve 'system-storage-structure
							  *system-storage-structure-attributes*
							  *system-storage-structure-attributes*
							  *system-storage-structure-key* t)
					  :test 'string-equal))
		 (help-storage-structure out-window help-object atomic-object)
		 (setf found-object t))
		;;
		;;  Determine if the object is a domain
		;;
		((setf help-object (assoc object (qtrieve 'system-domain *system-domain-attributes*
							  *system-domain-attributes*
							  *system-domain-key* t)
					  :test 'string-equal))
		 (help-domain out-window help-object atomic-object)
		 (setf found-object t))
		;;
		;;  Determine if the object is a function
		;;
		((functionp object)
		 (help-function out-window atomic-object)
		 (setf found-object t)))
    (cond ((or found-object attribute-test)
	   (return-from help-object object))
	  (t
	   (if out-window
	       (format *typeout-window*  "~%~S is not a valid object in the database ~S."
		       (read-from-string object) (read-from-string *active-db*))
	       (format *standard-output* "~%~S is not a valid object in the database ~S."
		       (read-from-string object) (read-from-string *active-db*)))
	   (return-from help-object nil)))))


;; The object is a relation.
(defun help-relation (out-window object qtrieve-var
		      &aux dir imp ss key tup-fmt doc card attr atomic-object indices)
  (setf dir (second qtrieve-var)
	imp (third qtrieve-var)
	ss (fourth qtrieve-var)
	key (fifth qtrieve-var)
	tup-fmt (sixth qtrieve-var)
	doc (seventh qtrieve-var)
	card (nthcdr 7 qtrieve-var)
	attr (second card)
	card (first card)
	atomic-object (read-from-string object)
	indices (qtrieve 'system-index *system-index-attributes* *system-index-attributes* *system-index-key*
			 `(string-equal relation-name ,(string-upcase object))))
  (cond (out-window
	 (scroll-to-bottom)
	 (send *output-window* ':append-item " ")
	 (send *output-window* ':append-item  (list (list ':item1 atomic-object 'relation) " is a relation"))
	 (send *output-window* ':append-item  (format nil "Characteristics of this relation are :"))
	 (send *output-window* ':append-item " ")
	 (send *output-window* ':append-item (list "Database :   "
						   (list ':item1 (read-from-string *active-db*) 'database)))
	 (send *output-window* ':append-item (list (list ':item1 'attributes 'attribute) "  : "))
	 (mapcar (function (lambda (atr)
		   (send *output-window* ':append-item (list " : " (list ':item1 atr 'attribute)))))
		 attr)
	 (send *output-window* ':append-item
	       (list (list ':item1 'implementation-type 'attribute) "  : " (list ':item1 imp 'dbms-object)))
	 (send *output-window* ':append-item
	       (list (list ':item1 'storage-structure 'attribute) "  : " (list ':item1 ss 'dbms-object)))
	 (send *output-window* ':append-item
	       (list (list ':item1 'cardinality 'attribute) "  : " (list card card)))
	 (send *output-window* ':append-item
	       (list (list ':item1 'save-directory 'attribute) "  : " (list dir dir)))
	 (send *output-window* ':append-item
	       (list (list ':item1 'tuple-format 'attribute) "  : " (list tup-fmt tup-fmt)))
	 (send *output-window* ':append-item
	       (list (list ':item1 'key 'attribute) "  : " (list key key)))
	 (send *output-window* ':append-item
	       (list (list ':item1 'doc 'attribute) "umentation: " (list doc doc)))
	 (cond (indices
		(send *output-window* ':append-item
		      (list (list ':item1 'secondary-indices 'attribute) "  : "))
		(mapcar (function (lambda (index)
			  (send *output-window* ':append-item
				(list "                  "
				      (list ':item1 (second index) 'attribute)))))
			indices))))
	(t
	 (format *standard-output* "~% ")
	 (format *standard-output* "~%~S is a relation." atomic-object)
	 (format *standard-output* "~%Characteristics of this relation are :")
	 (format *standard-output* "~%~%Database : ~s" (read-from-string *active-db*))
	 (format *standard-output* "~%~%Attributes: ")
	 (mapcar (function (lambda (atr)
		   (format *standard-output* "~%      ~S" atr)))
		 attr)
	 (format *standard-output* "~%Implementation type: ~S" imp)
	 (format *standard-output* "~%Storage structure: ~S" ss)
	 (format *standard-output* "~%Cardinality: ~S" card)
	 (format *standard-output* "~%Save-Directory: ~S" dir)
	 (format *standard-output* "~%Tuple-Format: ~S" tup-fmt)
	 (format *standard-output* "~%Key: ~S" key)
	 (format *standard-output* "~%Documentation: ~S" doc)
	 (cond (indices
		(format *standard-output* "~%Secondary Indices: ")
		(mapcar (function (lambda (index)
			  (format *standard-output* "~%      ~S" (second index))))
			indices))))))


;; The object is an attribute
(defun help-attribute (out-window object relation attribute-test
		       &aux  doc qtrieve-var dom def rel)
  (setf qtrieve-var (car (qtrieve 'system-attribute *system-attribute-attributes*
				  '(relation-name domain-function default-value doc )
				  *system-attribute-key*
				  `(and (string-equal relation-name ,(string relation))
					(string-equal attribute-name ,(string object))))))
  (setf dom (second qtrieve-var)
	def (third qtrieve-var)
	doc (fourth qtrieve-var)
	rel (first qtrieve-var))
  (setf qtrieve-var (car (qtrieve 'system-relation *system-relation-attributes* '(attributes tuple-format)
				  *system-relation-key*
				  `(string-equal relation-name ,(string-upcase relation)))))
  (cond (out-window
	 (send *output-window* ':append-item " ")
	 (cond (attribute-test
		(send *output-window* ':append-item  (format nil "~S is a duplicate attribute." object)))
	       (t
		(scroll-to-bottom)
		(send *output-window* ':append-item (format nil "~S is an attribute." object))))
	 (send *output-window* ':append-item " ")
	 (send *output-window* ':append-item (format nil "It has the following characteristics." ))
	 (send *output-window* ':append-item " ")
	 (send *output-window* ':append-item
	       (list (list ':item1 'relation-name 'attribute) "  : " (list ':item1 relation 'relation)))
	 (send *output-window* ':append-item
	       (list (list ':item1 'domain-function 'attribute) "  : " (list ':item1 dom 'dbms-object)))
	 (send *output-window* ':append-item
	       (list (list ':item1 'default-value 'attribute) "  : " (list def def)))
	 (setf def (caar (project-list (cdr qtrieve-var) (car qtrieve-var) (list object))))
	 (send *output-window* ':append-item
	       (list (list ':item1 'print-width 'other-info) "  : " (list def def))) ;mrr 04.09.87
	 (send *output-window* ':append-item
	       (list (list ':item1 'doc 'attribute) "umentation  : " (list doc doc))))
	(t
	 (format *standard-output* "~% ")
	 (cond (attribute-test
		(format *standard-output* "~S is a duplicate attribute." object))
	       (t
		(format *standard-output* "~%~S is an attribute." object)))
	 (format *standard-output* "~% ")
	 (format *standard-output* "~%It has the following characteristics. ")
	 (format *standard-output* "~%Relation-name  : ~S" relation)
	 (format *standard-output* "~%Domain-function  : ~S" dom)
	 (format *standard-output* "~%Default-value  : ~S" def)
	 (setf def (caar (project-list (cdr qtrieve-var) (car qtrieve-var) (list object))))
	 (format *standard-output* "~%Print-width    : ~s" def)
	 (format *standard-output* "~%Documentation  : ~S" doc))))


(defun parse-where (where-c &aux a-list by? where-clause)
  (block parse-where
    (setf where-clause (copy-list where-c))
    (cond ((null where-clause)
	   (return-from parse-where where-clause))
	  ((listp (car where-clause))
	   (return-from parse-where where-clause))
	  ((and (not (stringp (car where-clause))) (not (symbolp (car where-clause))))
	   (return-from parse-where where-clause))
	  ((member (string-upcase (car where-clause)) *where-opt-macros* :test 'string-equal)
	   (setf a-list (eval (append where-clause (list 'tuples t))))
	   (cond ((setf by? (member 'by where-clause :test 'member-pkgind))
		  (if (listp (setf by? (cadr by?)))
		      (setf by? (car by?)))
		  (setf a-list (list 'cadr (list 'assoc (read-from-string (string-upcase by?))
						 `(quote ,a-list))))))
	   (return-from parse-where a-list))
	  ((member (string-upcase (car where-clause)) *where-opt* :test 'string-equal)
	   (setf a-list (eval (append where-clause (list '(quote tuples) t))))
	   ;;
	   ;;  Need to determine if there is a by clause and pull out the attribute name for use in the
	   ;; association list
	   ;;
	   (cond ((setf by? (member 'by where-clause :test 'member-pkgind))
		  (if (listp (setf by? (cadr (cadr by?))))
		      (setf by? (car by?)))
		  (setf a-list (list 'cadr (list 'assoc (read-from-string (string-upcase by?))
						 `(quote ,a-list))))))
	   (return-from parse-where a-list)))
    (do ((sub-clause (cdr where-clause) (cdr sub-clause)))
	((null sub-clause) t)
      (cond ((listp (car sub-clause))
	     (rplaca sub-clause (parse-where (car sub-clause))))))
    (return-from parse-where where-clause)))


(defun member-pkgind (item1 item2)
  (cond ((and (listp item2) (equal 'quote (car item2)))
	 (setf item2 (cadr item2))))
  (cond ((or (listp item1) (listp item2))
	 (equal item1 item2))
	(t
	 (string-equal (string-upcase item1) (string-upcase item2)))))

(defun project-array (tuples attribute-list project-list &optional relation-name)
  relation-name
  (project-list tuples attribute-list project-list))

(defun project-flavor (tuples attribute-list project-list &optional relation-name)
  relation-name attribute-list
  (setf project-list (project-flavor-prereq project-list))
  (fast-project-flavor tuples project-list))

(defun project-flavor-prereq (project-list)
  (mapcar #'(lambda (attr)
	      (read-from-string (concatenate 'string ":" (string-upcase attr))))
	  project-list))

(defun fast-project-flavor (tuples project-list)
  (mapcar (function (lambda (tuple)
	    (mapcar (function (lambda (attr)
		      (funcall tuple attr)))
		    project-list)))
	  tuples))



(defun project-list (tuples attribute-list project-attr-list
		     &optional relation-name &aux attribute-length)
  relation-name
  (cond ((equal attribute-list project-attr-list)
	 tuples)
	(t
	 (setf attribute-length (length attribute-list))
	 (setf project-attr-list (mapcar (function (lambda (attr &aux pos)
					   (setf pos (- attribute-length
							(length
							 (member attr attribute-list
								 :test 'string-equal))))))
					 project-attr-list))
	 (mapcar (function (lambda (tuple)
		   (mapcar (function (lambda (attr-pos)
			     (nth attr-pos tuple)))
			   project-attr-list)))
		 tuples))))

(defun project-struct (tuples attribute-list project-list relation-name)
  attribute-list
  (let ((string-relation-name (string relation-name)))
    (setf project-list (unconvert-attributes (mapcar #'(lambda (attr)
							 (concatenate 'string string-relation-name
								      (string attr))) project-list))))
  (mapcar (function (lambda (tuple)
	    (mapcar (function (lambda (attr)
		      (funcall attr tuple)))
		    project-list)))
	  tuples))

(defun put-array (relation-name array-attribute-init-list
		  &aux array-name current-point)
  ;;
  ;;form the array name and current-point.
					;
  (multiple-value-setq (array-name)
    (intern (read-from-string (concatenate 'string relation-name "ARRAY"))))
  (multiple-value-setq (current-point)
    (intern (read-from-string (concatenate 'string relation-name "CURRENT-POINT"))))
  ;;
  ;;Call the function to do the insertion.
  ;;
  (put-array* (symbol-value array-name) (symbol-value current-point) array-attribute-init-list)
  ;;
  ;;Increment the current-point.
  ;;
  (set current-point (+ (symbol-value current-point) 1))
  ;;
  ;;Return the row in which this tuple was stored.
  ;;
  (- (symbol-value current-point) 1))

(defun put-array* (array-name current-point attribute-init-list)
  ;;
  ;;if the current-point equals current-size increase the size of the array
  ;;
  (cond ((equal current-point (array-total-size array-name))
	 (adjust-array array-name (+ (array-total-size array-name) 10))))
  ;;
  ;;Store the tuple in the array.
  ;;
  (setf (aref array-name current-point) attribute-init-list))


(defun put-flavor (relation-name attr-list attribute-init-list &aux tuple)
  (setf tuple (make-instance (read-from-string (string-upcase relation-name))))
  (do ((attribute-init-list attribute-init-list (cdr attribute-init-list))
       (attr-list attr-list (cdr attr-list)))
      ((null attribute-init-list) tuple)
    (set-in-instance tuple (read-from-string (string-upcase (car attr-list))) (car attribute-init-list)))
  tuple)

(defun put-struct (relation-name attr-list attribute-init-list
		   &aux structure-tuple alter-macro temp1 temp2)
  ;;
  ;;make an instance of the relation-structure.
  ;;
  (setf structure-tuple  (eval `(,(read-from-string (concatenate 'string "MAKE-" relation-name)))))
  ;;
  ;;Form the name of the macro to be used to set the values in the instance.
  ;;
  (setf alter-macro (read-from-string (concatenate 'string "ALTER-" relation-name)))
  ;;
  ;;Set the attribute values in the instance. (Initialize)
  ;;
  (do ((attribute-init-list attribute-init-list (cdr attribute-init-list))
       (attr-list attr-list (cdr attr-list)))
      ((null attribute-init-list) structure-tuple)
    (setf temp1 (read-from-string (concatenate 'string (string relation-name) (car attr-list)))
	  temp2 (car attribute-init-list))
    (eval `(,alter-macro structure-tuple ,temp1 temp2)))
  structure-tuple)

(defun getp (relation-name property)
  (get (find-symbol (string relation-name) *pkg-string*)
       (find-symbol (string property) *pkg-string*)))

(defun putp (relation-name value property)
  (if (not (find-symbol (string relation-name) *pkg-string*))
      (intern (string relation-name) *pkg-string*))
  (putprop (find-symbol (string relation-name) *pkg-string*)
	   value
	   (find-symbol (string property) *pkg-string*)))

(defun qtrieve (relation-name attribute-list project-list secondary-key where-clause)
  (if (equal where-clause t)
      (funcall (find-symbol (concatenate 'string "RETRIEVE-" *system-relation-base-implementation*
					 "-" *system-relation-storage-structure*) *pkg-string*)
	       relation-name attribute-list project-list secondary-key where-clause nil relation-name)
      (funcall (find-symbol (concatenate 'string "QTRIEVE-" *system-relation-base-implementation*
					 "-" *system-relation-storage-structure*) *pkg-string*)
	       relation-name attribute-list project-list secondary-key where-clause)))

;;;  A simplification may be made by enforcing the rule which us stated for all QTRIEVEs, the where-clause is assumed to be an AND-EQUAL
;;; clause containing all of the attributes contained in the secondary-key. Thus the where-clause specifies a single bucket. The tuples in the
;;; bucket do not necessarly satisfy  the where-clause however and each tuple in the bucket must be checked for validity.
;;;
(defun qtrieve-flavor-hash (relation-name attribute-list project-list secondary-key where-clause
			    &aux (key nil) result)
  secondary-key
  (do ((where-c (cdr where-clause) (cdr where-c)))
      ((null where-c) t)
    (cond ((listp (car where-c))
	   (setf key (append key (list (caddar where-c)))))
	  (t
	   (setf key (append key (list (cadr where-c))))
	   (setf where-c (cdddr where-c)))))
  (setf result (eval-where (gethash key (getp relation-name 'entry-point))
			   attribute-list where-clause "FLAVOR" relation-name))
  (project-list result attribute-list project-list))

(defun qtrieve-flavor-heap (relation-name attribute-list project-list secondary-key where-clause
			    &optional (tuple-list (getp relation-name 'entry-point)))
  secondary-key
  (if tuple-list
      (project-list (eval-where tuple-list  attribute-list where-clause "FLAVOR" relation-name)
		    attribute-list project-list)
      (project-flavor tuple-list attribute-list project-list relation-name)))

(defun qtrieve-struct-avl (relation-name attribute-list project-list secondary-key where-clause
			   &optional (tuple-list (avl-inorder-traversal (getp relation-name 'entry-point))))
  secondary-key
  (if tuple-list
      (project-list (eval-where tuple-list  attribute-list where-clause "STRUCT" relation-name)
		    attribute-list project-list)
      (project-struct tuple-list attribute-list project-list relation-name)))

(defun qtrieve-list-avl (relation-name attribute-list project-list secondary-key where-clause
			 &optional (tuple-list (avl-inorder-traversal (getp relation-name 'entry-point))))
  secondary-key
  (if tuple-list
      (project-list (eval-where tuple-list  attribute-list where-clause "LIST" relation-name)
		    attribute-list project-list)))

(defun qtrieve-flavor-avl (relation-name attribute-list project-list  secondary-key where-clause
			   &optional (tuple-list (avl-inorder-traversal (getp relation-name 'entry-point))))
  secondary-key
  (if tuple-list
      (project-list (eval-where tuple-list  attribute-list where-clause "FLAVOR" relation-name)
		    attribute-list project-list)
      (project-flavor tuple-list attribute-list project-list relation-name)))


(defun qtrieve-list-hash (relation-name attribute-list project-list secondary-key where-clause
			  &aux (key nil) result)
  secondary-key
  (do ((where-c (cdr where-clause) (cdr where-c)))
      ((null where-c) t)
    (cond ((listp (car where-c))
	   (setf key (append key (list (caddar where-c)))))
	  (t
	   (setf key (append key (list (cadr where-c))))
	   (setf where-c (cdddr where-c)))))
  (setf result (eval-where (gethash key (getp relation-name 'entry-point)) attribute-list where-clause "LIST"
			   relation-name))
  (project-list result attribute-list project-list))

(defun qtrieve-list-heap (relation-name attribute-list project-list secondary-key where-clause
			  &optional (tuple-list (getp relation-name 'entry-point)))
  secondary-key
  (if tuple-list
      (project-list (eval-where tuple-list attribute-list where-clause "LIST" relation-name)
		    attribute-list project-list)))


(defun qtrieve-struct-hash (relation-name attribute-list project-list secondary-key where-clause
			    &aux (key nil) result)
  secondary-key
  (do ((where-c (cdr where-clause) (cdr where-c)))
      ((null where-c) t)
    (cond ((listp (car where-c))
	   (setf key (append key (list (caddar where-c)))))
	  (t
	   (setf key (append key (list (cadr where-c))))
	   (setf where-c (cdddr where-c)))))
  (setf result
	(eval-where (gethash key (getp relation-name 'entry-point)) attribute-list where-clause "STRUCT"
		    relation-name))
  (project-list result attribute-list project-list))

(defun qtrieve-struct-heap (relation-name attribute-list project-list secondary-key where-clause
			    &optional (tuple-list (getp relation-name 'entry-point)))
  secondary-key
  (if tuple-list
      (project-list (eval-where tuple-list attribute-list where-clause "STRUCT" relation-name)
		    attribute-list project-list)
      (project-struct tuple-list attribute-list project-list relation-name)))

(defun validate-name (object-name &optional (donot-flag-error nil))
  ;;
  ;;  List testing needs to be done before the actual name can be validated
  ;;
  (cond ((and (listp object-name) (string-equal "QUOTE" (car object-name)))
	 (setf object-name (cadr object-name)))
	((and (listp object-name) (equal (length object-name) 1))
	 (setf object-name (car object-name))))
  (cond ((not (atom object-name))
	 (cond ((and (not donot-flag-error) *provide-error-messages*)
		(format *standard-output* "~%ERROR - improperly specified name --> ~s" object-name)
		(setf object-name nil))))
	((stringp object-name)
	 (setf object-name (read-from-string object-name))))
  object-name)

(defun validate-sym (object-name &optional (allow-strings nil))
  ;;
  ;;  List testing needs to be done before the actual name can be validated
  ;;
  (cond ((and (listp object-name) (string-equal "QUOTE" (car object-name)))
	 (setf object-name (cadr object-name))))
  (cond ((and (symbolp object-name) (not allow-strings)))
	((and allow-strings (or (symbolp object-name)(stringp object-name)))
	 (setf object-name (string-upcase object-name)))
	(t
	 (cond (*provide-error-messages*
		(format *standard-output* "~%ERROR - Improperly specified name --> ~s" object-name)
		(format *standard-output* "~%        All names in RTMS must be SYMBOLS")))
	 (setf object-name nil)))
  object-name)

(defun define-transaction (transaction forms &rest keyword-list
			   &key &optional directory pathname
			   &allow-other-keys
			   &aux dir)
  "Define a transaction, a list of database calls.

   TRANSACTION - Name of the transaction.
   FORMS       - List of RTMS calls.
   DIRECTORY   - Name of the directory in which this transaction will be stored.
   PATHNAME    - Name of the file in which it will be stored."
  directory pathname
  (block define-transaction
    (if (not (active-database))
	(return-from define-transaction nil))
    (cond ((null (setf transaction (validate-sym transaction t)))
	   (return-from define-transaction nil)))
    (setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
			   ((or (null keyword-list) (not (listp (car keyword-list)))) keyword-list)))
    (setf keyword-list (get-keyword-value-prereq '(dir path) keyword-list))
    (putp transaction (if (setf dir (car (get-keyword-value '(dir) keyword-list)))
			  dir
			  *save-directory*)
	  ':save-directory)
    (putp transaction (car (get-keyword-value '(path) keyword-list)) ':file)
    (putp transaction forms ':forms)
    (return-from define-transaction transaction)))

;;;  Unless you call an explicit (BEGIN-TRANSACTION), the forms in a given transaction file are treated as a set
;;; of database calls. In effect, a sequence of db calls can be made to fall under the category of a transaction
;;; just by calling the following function and the transaction period is over when you call (END-TRANSACTION).
;;; Just what exactly is a transaction?. In our context, we will postpone all the disk modifiable functions
;;; until the user ends the transaction. They include (as of now) all SAVE operations. Since destroy operations
;;; call SAVE they are taken care of. If any files on the disk are deleted due to any of the destroy operations
;;; (DISK T), they can always be undeleted using DIRED.
(defun begin-transaction (&rest ignore)
  "Start transaction processing - postpone any save-related operations."
  (setf *transaction-on* t))

(defun end-transaction (&rest ignore)
  "End transaction processing. Revert to normal processing after executing any save-related operations
    postponed during transaction processing."
  (if *transaction-on*
      (progn
	(setf *transaction-on* nil)
	(mapc #'(lambda (x) (eval x))
	      *transaction-forms-postponed*)
	(setf *transaction-forms-postponed* nil))
      (progn
	(if *provide-status-messages*
	    (format *standard-output* "~%WARNING - There is no active transaction."))
	nil)))

(defun abort-transaction (&rest ignore)
  "Abort the ongoing transaction processing - Do not perform any postponed save-related operations."
  (if (not *transaction-on*)
      (progn
	(setf *transaction-on* nil)
	(setf *transaction-forms-postponed* nil))
      (progn
	(if *provide-status-messages*
	    (format *standard-output* "~%WARNING - There is no active transaction."))
	nil)))

(defun modify-transaction (transaction &rest keyword-list
			   &key &optional directory pathname
				  &allow-other-keys)
  "Edit the database calls in a transaction.
   TRANSACTION - Name of the transaction.
   DIRECTORY   - Name of the directory in which this transaction can be found.
   PATHNAME    - Name of the file in which it is stored."
  (declare (ignore directory pathname))
  (let (dir file)
    (block modify-transaction
      (if (not (active-database))
	  (return-from modify-transaction nil))
      (cond ((null (setf transaction (validate-sym transaction t)))
	     (return-from modify-transaction nil)))
      (setf keyword-list
	    (do ((keyword-list keyword-list (car keyword-list)))
		((or (null keyword-list)
		     (not (listp (car keyword-list))))
		 keyword-list)))
      (setf keyword-list (get-keyword-value-prereq '(dir path) keyword-list))
      (if (not (getp transaction ':save-directory))
	  (progn
	    (setf file (or (car (get-keyword-value '(path) keyword-list))
			   (concatenate
			    'string
			    (if (setf dir
				      (car (get-keyword-value
					    '(dir) keyword-list)))
				(get-directory keyword-list)
				*save-directory*)
			    transaction)))
	    (if (probe-file file)
		(progn
		  (ed (string-upcase file))
		  (return-from modify-transaction transaction))
		(progn
		  (if *provide-error-messages*
		      (format *standard-output*
			      (str "~%ERROR - The transaction file ~S does not "
				   "exist; ~@~%"
				   "~7T the transaction ~S has not been defined "
				   " yet.")
			      file transaction))
		  (return-from modify-transaction nil)))))
      (if (not (probe-file
		(setf file (or (car (get-keyword-value '(path) keyword-list))
			       (if (setf dir
					 (car (get-keyword-value
					       '(dir) keyword-list)))
				   (concatenate 'string
						(get-directory keyword-list)
						transaction))
			       (getp transaction ':file)
			       (concatenate 'string
					    (getp transaction ':save-directory)
					    transaction)))))
	  (progn
	    ;; mrr 04.09.87
	    (unless (errset (directory-list file) nil)
	      (if *provide-error-messages*
		  (format *standard-output*
			  "~%ERROR - The directory of file ~S does not exist."
			  file))
	      (return-from modify-transaction nil))
	    (with-open-file (path file :characters t :direction 'output)
	      (write-char #\( path)
	      (mapc #'(lambda (form)
			(print form path))
		    (getp transaction ':forms))
	      (terpri)
	      (write-char #\) path))))
      (ed (string-upcase file))
      (return-from modify-transaction transaction))))

(defun save-transaction (transaction &rest keyword-list
			 &key &optional directory pathname
			 &allow-other-keys
			 &aux dir file keys)
  "Save a transaction on disk.

   TRANSACTION - Name of the transaction.
   DIRECTORY   - Name of the directory in which this transaction is to be stored.
   PATHNAME    - Name of the file in which it is to be stored."
  directory pathname
  (block save-transaction
    (if (not (active-database))
	(return-from save-transaction nil))
    (cond ((null (setf transaction (validate-sym transaction t)))
	   (return-from save-transaction nil)))
    (setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
			   ((or (null keyword-list) (not (listp (car keyword-list)))) keyword-list)))
    (setf keys (copy-list keyword-list))
    (if (not (getp transaction ':save-directory))
	(progn
	  (if *provide-error-messages*
	      (format *standard-output* "~%ERROR - The transaction ~S has not been defined yet."
		      transaction))
	  (return-from save-transaction nil)))
    (if *transaction-on*
	(progn
	  (setf *transaction-forms-postponed* (append *transaction-forms-postponed*
						      (list `(save-transaction ',transaction ',keys))))
	  (return-from save-transaction transaction)))
    (setf keyword-list (get-keyword-value-prereq '(dir path) keyword-list))
    (setf file (or (car (get-keyword-value '(path) keyword-list))
		   (if (setf dir (car (get-keyword-value '(dir) keyword-list)))
		       (concatenate 'string (get-directory keyword-list) transaction))
		   (getp transaction ':file)
		   (concatenate 'string (getp transaction ':save-directory) transaction)))
    (unless (errset (directory-list file) nil)  ;mrr 04.09.87
      (if *provide-error-messages*
	  (format *standard-output* "~%ERROR - The directory of file ~S does not exist." file))
      (return-from save-transaction nil))
    (with-open-file (path file :characters t :direction :output)
      (write-char #\( path)
      (mapc #'(lambda (form) (print form path))
	    (getp transaction ':forms))
      (terpri)
      (write-char #\) path))
    (return-from save-transaction transaction)))

(defun commit-transaction (transaction &rest keyword-list
			   &key &optional directory pathname
			   &allow-other-keys
			   &aux dir path file forms)
  "Execute the database calls in a transaction.

   TRANSACTION - Name of the transaction to be commited.
   DIRECTORY   - Name of the directory in which this transaction can be found, if not in memory.
   PATHNAME    - Name of the file in which it can be found."
  directory pathname
  (block commit-transaction
    (if (not (active-database))
	(return-from commit-transaction nil))
    (cond ((null (setf transaction (validate-sym transaction t)))
	   (return-from commit-transaction nil)))
    (setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
			   ((or (null keyword-list) (not (listp (car keyword-list)))) keyword-list)))
    (begin-transaction)
    (setf keyword-list (get-keyword-value-prereq '(dir path) keyword-list))
    (if (not (getp transaction ':save-directory))
	(progn
	  (setf path (or (car (get-keyword-value '(path) keyword-list))
			 (concatenate 'string (if (setf dir (car (get-keyword-value '(dir) keyword-list)))
						  (get-directory keyword-list)
						  *save-directory*)
				      transaction)))
	  (if (probe-file path)
	      (progn
		(setf forms (rtms-read-insert-file path))
		(mapc #'(lambda (form) (eval form))
		      forms)
		(end-transaction)
		(return-from commit-transaction transaction))
	      (progn
		(if *provide-error-messages*
		    (format *standard-output* "~%ERROR - The transaction file ~S does not exist; ~@
                              ~7T the transaction ~S has not been defined yet."
			    path transaction))
		(end-transaction)
		(return-from commit-transaction nil)))))
    (if (setf file
	      (or (car (get-keyword-value '(path) keyword-list))
		  (if (setf dir (car (get-keyword-value '(dir) keyword-list)))
		      (concatenate 'string (get-directory keyword-list) transaction))))
	(if (probe-file file)
	    (putp transaction (rtms-read-insert-file file) ':forms)
	    (progn
	      (if *provide-error-messages*
		  (format *standard-output* "~%ERROR - The transaction file ~S does not exist." file))
	      (end-transaction)
	      (return-from commit-transaction nil)))
	(progn
	  (setf file (or (getp transaction ':file)
			 (concatenate 'string (getp transaction ':save-directory) transaction)))
	  (if (probe-file file)
	      (putp transaction (rtms-read-insert-file file) ':forms))))
    (mapc #'(lambda (form) (eval form))
	  (getp transaction ':forms))
    (end-transaction)
    (return-from commit-transaction transaction)))

(defun memtuple (tuple relation &aux where-clause)
  (setf where-clause
	(cons 'and
	      (mapcar #'(lambda (attr val)
			  (list 'equal attr `(quote ,val)))
		      (first (second (get-relation relation '(attributes) nil)))
		      tuple)))
  (car (retrieve relation 'where where-clause 'tuples t)))

(defun dblessp (x y)
  (cond ((and (not (equal (type-of x) (type-of y))) (not (and (numberp x) (numberp y))))
	 nil)
	((numberp x)
	 (<= x y))
	((or (atom x) (stringp x))
	 (or (string-lessp x y) (string-equal x y)))
	((listp x)
	 (and (equal (length x)(length y)) (apply 'and (mapcar #'(lambda (x1 y1) (dblessp x1 y1)) x y))))
	(t
	 nil)))

(defun dbgtp (x y)
  (cond ((and (not (equal (type-of x) (type-of y))) (not (and (numberp x) (numberp y))))
	 nil)
	((numberp x) (>= x y))
	((or (atom x)
	     (stringp x))
	 (or (string-greaterp x y)
	     (string-equal x y)))
	((listp x) (and (equal (length x)(length y))
			(apply 'and (mapcar #'(lambda (x1 y1) (dbgtp x1 y1))
					    x y))))
	(t nil)))

(defun quick-sort (tuples sort-attrs attributes
		   &aux sort-attributes tuple-attributes)
  ;;
  ;; This is a temporary solution
  ;;
  (setf sort-attributes sort-attrs
	sort-attrs nil
	tuple-attributes attributes
	attributes nil)
  (do ((attribute% sort-attributes (cdr attribute%)))
      ((null attribute%) t)
    (cond ((listp (car attribute%))
	   (setf sort-attrs (cons (list (read-from-string (caar attribute%)) (second (car attribute%)))
				  sort-attrs)))
	  ((stringp (car attribute%))
	   (setf sort-attrs (cons (read-from-string (car attribute%)) sort-attrs)))
	  (t
	   (setf sort-attrs (cons (read-from-string (string (car attribute%)))
				  sort-attrs)))))
  (do ((attribute% tuple-attributes (cdr attribute%)))
      ((null attribute%) t)
    (setf attributes (cons (read-from-string (car attribute%)) attributes)))
  (setf sort-attrs (reverse sort-attrs)
	attributes (reverse attributes))
  (cond ((equal sort-attrs t)
	 (sort tuples 'ltp))
	(t
	 (sort tuples
	       #'(lambda (x y)
		   (apply
		    'and
		    (mapcar
		     #'(lambda
			   (attr &aux (number (position (if (not (listp attr)) attr (car attr)) attributes)))
			 (cond ((null number) t)
			       ((equal attr (car sort-attrs))
				(if (or (not (listp attr)) (null (cadr attr)))
				    (ltp (nth number x) (nth number y))
				    (funcall (cadr attr) (nth number x) (nth number y))))
			       (t
				(if (apply 'and
					   (mapcar
					    #'(lambda
						  (at &aux num)
						(equal
						 (nth
						  (setf num (position (if (listp at) (car at) at)
								      attributes))
						  x)
						 (nth num y)))
					    (firstn (position attr sort-attrs)
						    sort-attrs)))
				    (if (or (not (listp attr))
					    (null (cadr attr)))
					(ltp (nth number x) (nth number y))
					(funcall (cadr attr) (nth number x)
						 (nth number y)))
				    t))))
		     sort-attrs)))))))

(defun define-implementation (implementation-name &rest keyword-list
			      &key &optional documentation
			      &allow-other-keys
			      &aux doc)
  "Define a new implementation.

   IMPLEMENTATION-NAME - Name of the implementation to be defined. All the implementation-specific
                         accessor functions are expected to be defined.
   DOCUMENTATION       - Description of this implementation."
  documentation
  (block define-implementation
    (cond (*parameter-checking*
	   (if (not (active-database implementation-name))
	       (return-from define-implementation nil))))
    (cond ((not (setf implementation-name (validate-sym implementation-name t)))
	   (return-from define-implementation nil)))
    ;;
    ;;Check to see if this implementation exists.
    (if (member (list implementation-name) (qtrieve 'system-implementation
						    *system-implementation-attributes*
						    '(implementation-name)
						    *system-implementation-key* t)
		:test 'equal)
	(progn
	  (cond (*provide-error-messages*
		 (format *standard-output*
			 "~%ERROR - The implementation ~s already exists in the ~s database"
			 implementation-name *active-db*)
		 (format *standard-output*
			 "~%        and may not be redefined")))
	  (return-from define-implementation nil)))
    (setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
			   ((or (null keyword-list) (not (listp (car keyword-list)))) keyword-list)))
    ;;
    ;; Everything seems to be in order, proceed
    ;;
    (if *provide-status-messages*
	(format *standard-output* "~%Define implementation ~s" implementation-name))
    (setf keyword-list (get-keyword-value-prereq '(doc) keyword-list))
    (setf doc (car (get-keyword-value '(doc) keyword-list)))
    (insert 'system-implementation (list 'tuples (list (list implementation-name user-id doc))))
    (if *provide-status-messages*
	(format *standard-output* "~%Definition of implementation ~s completed." implementation-name))
    implementation-name))

(defun define-storage-structure (storage-structure-name &rest keyword-list
				 &key &optional documentation
				 &allow-other-keys
				 &aux doc)
  "Define a new storage structure.

   STORAGE-STRUCTURE-NAME - Name of the storage-structure to be defined. All the storage-structure-specific
                            accessor functions are expected to be defined.
   DOCUMENTATION          - Description of this storage-structure."
  documentation
  (block define-storage-structure
    (cond (*parameter-checking*
	   (if (not (active-database storage-structure-name))
	       (return-from define-storage-structure nil))))
    (cond ((not (setf storage-structure-name (validate-sym storage-structure-name t)))
	   (return-from define-storage-structure nil)))
    ;;Check to see if this storage-structure exists.
    (if (member (list (setf storage-structure-name (string storage-structure-name)))
		(qtrieve 'system-storage-structure *system-storage-structure-attributes*
			 '(storage-structure-name) *system-storage-structure-key* t)
		:test 'equal)
	(progn
	  (cond (*provide-error-messages*
		 (format *standard-output*
			 "~%ERROR - The storage structure ~s already exists in the ~s database"
			 storage-structure-name *active-db*)
		 (format *standard-output*
			 "~%        and may not be redefined")))
	  (return-from define-storage-structure nil)))
    (setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
			   ((or (null keyword-list) (not (listp (car keyword-list)))) keyword-list)))
    ;;
    ;; Everything seems to be in order, proceed
    ;;
    (if *provide-status-messages*
	(format *standard-output* "~%Define storage-structure ~s" storage-structure-name))
    (setf keyword-list (get-keyword-value-prereq  '(doc) keyword-list))
    (setf doc (car (get-keyword-value '(doc) keyword-list)))
    (insert 'system-storage-structure (list 'tuples (list (list storage-structure-name user-id doc))))
    (if *provide-status-messages*
	(format *standard-output* "~%Definition of storage-structure ~s completed." storage-structure-name))
    (return-from define-storage-structure storage-structure-name)))

(defun help-database (out-window atomic-object &aux (none t))
  (cond (out-window
	 ;;
	 ;;  Send the output to the output window of the interface if the user is using the interface
	 ;;
	 (scroll-to-bottom)
	 (send *output-window* ':append-item (format nil "~S is the current database." atomic-object))
	 (if *database-documentation* (send *output-window* ':append-item (format nil "~S"
										  *database-documentation*))
	     (send *output-window* ':append-item " "))
	 (send *output-window* ':append-item (format nil "The relations in this database include:"))
	 (send *output-window* ':append-item " ")
	 ;;
	 ;;  Retrieve all of the relations in the current database and display the relations which are not system relations.
	 ;;
	 (mapc (function (lambda (rel)
		 (cond ((not (member (car rel) *system-relations* :test 'string-equal))
			(setf none nil)
			(send *output-window* ':append-item
			      (list (list ':item1  (read-from-string (car rel)) 'relation)))))))
	       (qtrieve 'system-relation *system-relation-attributes* '("RELATION-NAME") *system-relation-key*
			t))
	 (if none
	     (format *typeout-window* "~% There are no relations defined in ~S" (read-from-string *active-db*))
	     (send *output-window* ':append-item " ")))
	(t
	 ;;
	 ;;  The user is not using the interface, therefore send all output to the standard output
	 ;;
	 (format *standard-output* "~%~S is the current database." atomic-object)
	 (if *database-documentation*
	     (format *standard-output* "~%~S" *database-documentation*)
	     (format *standard-output* "~% "))
	 (format *standard-output* "~%The relations in this database include:")
	 (format *standard-output* "~% ")
	 (mapc (function (lambda (rel)
		 (if (not (member (car rel) *system-relations* :test 'string-equal))
		     (progn
		       (setf none nil)
		       (format *standard-output* "~%~S" (read-from-string (car rel)))))))
	       (qtrieve 'system-relation *system-relation-attributes* '("RELATION-NAME") *system-relation-key*
			t))
	 (if none
	     (format *standard-output* "~% There are no relations defined in ~S"
		     (read-from-string *active-db*)))
	 (format *standard-output* "~% "))))

(defun help-implementation (out-window implementation-name atomic-object)
  (cond (out-window
	 (scroll-to-bottom)
	 (send *output-window* ':append-item (format nil "~S is a type of implementation." atomic-object))
	 (send *output-window* ':append-item " ")
	 (send *output-window* ':append-item
	       (list (list ':item1 'owner-id 'attribute)
		     (format nil ":  ~S" (read-from-string (cadr implementation-name)))))
	 (send *output-window* ':append-item
	       (list (list ':item1 'doc 'attribute)
		     (format nil "UMENTATION:  ~S" (caddr implementation-name)))))
	(t
	 (format *standard-output* "~%~S is a type of implementation." atomic-object)
	 (format *standard-output* "~% ")
	 (format *standard-output* "~%OWNER-ID:  ~S" (read-from-string (cadr implementation-name)))
	 (format *standard-output* "~%DOCUMENTATION:  ~S" (caddr implementation-name)))))

(defun help-storage-structure (out-window storage-structure atomic-object)
  (cond (out-window
	 (scroll-to-bottom)
	 (send *output-window* ':append-item (format nil "~S is a type of storage structure." atomic-object))
	 (send *output-window* ':append-item " ")
	 (send *output-window* ':append-item
	       (list (list ':item1 'owner-id 'attribute)
		     (format nil ":  ~S" (read-from-string (cadr storage-structure)))))
	 (send *output-window* ':append-item
	       (list (list ':item1 'doc 'attribute) (format nil "UMENTATION:  ~S" (caddr storage-structure)))))
	(t
	 (format *standard-output* "~%~S is a type of storage structure." atomic-object)
	 (format *standard-output* "~% ")
	 (format *standard-output* "~%OWNER-ID:  ~S" (read-from-string (cadr storage-structure)))
	 (format *standard-output* "~%DOCUMENTATION:  ~S" (caddr storage-structure)))))

(defun help-view (out-window view-name atomic-object)
  (cond (out-window
	 (scroll-to-bottom)
	 (send *output-window* ':append-item (format nil "~S is a view." atomic-object))
	 (send *output-window* ':append-item " ")
	 (send *output-window* ':append-item
	       (list (list ':item1 'view-definition 'attribute) (format nil ":  ~S" (cadr view-name))))
	 (send *output-window* ':append-item
	       (list (list ':item1 'owner-id 'attribute)
		     (format nil ":  ~S" (read-from-string (caddr view-name)))))
	 (send *output-window* ':append-item
	       (list (list ':item1 'view-documentation 'attribute) (format nil ":  ~S" (cadddr view-name)))))
	(t
	 (format *standard-output* "~%~S is a type of view." atomic-object)
	 (format *standard-output* "~% ")
	 (format *standard-output* "~%VIEW DEFINITION:  ~S" (cadr view-name))
	 (format *standard-output* "~%OWNER-ID:  ~S" (read-from-string (caddr view-name)))
	 (format *standard-output* "~%VIEW DOCUMENTATION:  ~S" (cadddr view-name)))))

(defun help-index (out-window view-name atomic-object)
  (cond (out-window
	 (scroll-to-bottom)
	 (mapc (function (lambda (view-tuple)
		 (send *output-window* ':append-item " ")
		 (send *output-window* ':append-item " ")
		 (send *output-window* ':append-item
		       (format nil "~S is an index on relation ~s."
			       atomic-object (read-from-string (first view-tuple))))
		 (send *output-window* ':append-item " ")
		 (send *output-window* ':append-item
		       (list (list ':item1 'storage-structure 'attribute)
			     (format nil ":  ~S" (read-from-string (third view-tuple)))))
		 (send *output-window* ':append-item
		       (list (list ':item1 'key 'attribute) (format nil ":  ~S" (fourth view-tuple))))
		 (send *output-window* ':append-item
		       (list (list ':item1 'priority 'attribute)
			     (format nil ":  ~S" (fifth view-tuple))))
		 (send *output-window* ':append-item
		       (list (list ':item1 'doc 'attribute)
			     (format nil ":  ~S" (sixth view-tuple))))))
	       view-name))
	(t
	 (mapc (function (lambda (view-tuple)
		 (format *standard-output* "~% ")
		 (format *standard-output* "~% ")
		 (format *standard-output* "~%~S is an index on relation ~s."
			 atomic-object (read-from-string (first view-tuple)))
		 (format *standard-output* "~% ")
		 (format *standard-output* "~%STORAGE-STRUCTURE:  ~S"
			 (read-from-string (third view-tuple)))
		 (format *standard-output* "~%KEY:  ~S"
			 (fourth view-tuple))
		 (format *standard-output* "~%PRIORITY:  ~S"
			 (fifth view-tuple))
		 (format *standard-output* "~%DOC:  ~S"
			 (sixth view-tuple))))
	       view-name))))

(defun help-domain (out-window domain-name atomic-object)
  (cond (out-window
	 (scroll-to-bottom)
	 (send *output-window* ':append-item (format nil "~S is a domain." atomic-object))
	 (send *output-window* ':append-item " ")
	 (send *output-window* ':append-item
	       (list (list ':item1 'owner-id 'attribute)
		     (format nil ":  ~S" (read-from-string (cadr domain-name)))))
	 (send *output-window* ':append-item
	       (list (list ':item1 'default-print-width 'attribute)
		     (format nil ":  ~S" (caddr domain-name))))
	 (send *output-window* ':append-item
	       (list (list ':item1 'default-value 'attribute)
		     (format nil ":  ~S" (cadddr domain-name))))
	 (send *output-window* ':append-item
	       (list (list ':item1 'doc 'attribute)
		     (format nil "UMENTATION:  ~S" (fifth domain-name)))))
	(t
	 (format *standard-output* "~%~S is a domain." atomic-object)
	 (format *standard-output* "~% ")
	 (format *standard-output* "~%OWNER ID:  ~S" (read-from-string (cadr domain-name)))
	 (format *standard-output* "~%DEFAULT PRINT WIDTH:  ~S" (caddr domain-name))
	 (format *standard-output* "~%DEFAULT VALUE:  ~S" (cadddr domain-name))
	 (format *standard-output* "~%DOCUMENATATION:  ~S" (fifth domain-name)))))

(defun help-function (out-window atomic-object)
  (cond (out-window
	 (format *typeout-window* "~%~S is a function." atomic-object)
	 (format *typeout-window* "~%~S" (documentation atomic-object))
	 (format *typeout-window* "~%The syntax of ~S is:" atomic-object)
	 (format *typeout-window* "~%~S" (cons atomic-object (arglist atomic-object))))
	(t
	 (format *standard-output* "~%~S is a function." atomic-object)
	 (format *standard-output* "~%~S" (documentation atomic-object))
	 (format *standard-output* "~%The syntax of ~S is:" atomic-object)
	 (format *standard-output* "~%~S" (cons atomic-object (arglist atomic-object))))))


;;; fragments

;; reduced.
;; ;;
;; (cond ((<= (length beg-val-list) 1)
;;        (return-from reduce-avl-key (append (list extracted-key-attribute-list)
;; 					   (list beg-val-list)
;; 					   (list end-val-list)))))
;; ;;
;; ;;  This is a test section of this function, it only looks at the first attribute of the key to determine the range of values which
;; ;;will be selected.
;; ;;
;; (do ((begval% beg-val-list (cdr begval%))
;;      (endval% end-val-list (cdr endval%)))
;;     ((null begval%) t)
;;   (setf normalized-key-list (append (list (list (car key-attribute-list))) normalized-key-list)
;; 	normalized-begval-list (cons (list (caar begval%)) normalized-begval-list)
;; 	normalized-endval-list (cons (list (caar endval%)




;; 					   (project-list (eval-where tuple-list  attribute-list where-clause "FLAVOR" relation-name)
;; 							 attribute-list project-list)
;; 					   (project-flavor tuple-list attribute-list project-list relation-name)))

;; 	(defun qtrieve-struct-avl (relation-name attribute-list project-list secondary-key where-clause
;; 				   &optional (tuple-list (avl-inorder-traversal (getp relation-name 'entry-point))))
;; 	  secondary-key
;; 	  (if tuple-liLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540715. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "LISP" :NAME "GLOBAL-VARS" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :CHARACTERS T :NOT-BACKED-UP T :CREATION-DATE 2749846126. :AUTHOR "REL3" :LENGTH-IN-BYTES 9991. :LENGTH-IN-BLOCKS 10. :BYTE-SIZE 8.)
