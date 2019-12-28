
;;; -*- Mode:Common-Lisp; Package:RTMS; Base:10; Fonts:(*CODE-FONT* *COMMENT-FONT* *STRING-FONT*) -*-
;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved.
;;; DEFINE-DB
;;;
;;; This file contains the following Explorer extensions to CommonLisp Standard as Indicated in the June 1985 Explorer Lisp
;;; Reference
;;;     deff

(deff defdb 'define-database)

(defun define-database (db-name &rest keyword-list
&key &optional directory environment documentation
&allow-other-keys
&aux dir-keyword-list keyword-values temp-validity-checking)
  "Define a new database.

   DB-NAME     - Name of the database.
   DIRECTORY   - Name of the directory in which this database is to be saved.
   ENVIRONMENT - Name of the environment to be associated with this database.
   DOCUMENTATION - A string describing this database."

  directory environment documentation
  (block define-database
;;
;;  If there is an activedb, determine if any relation has been modified, if so do not do anything which would provoke those
;; relations, i.e. terminate the function.
;;
  (cond ((and *parameter-checking* (active-database "defdb"))
 (cond ((car (funcall
       (find-symbol (concatenate 'string "RETRIEVE-" *system-relation-base-implementation*
    "-" *system-relation-storage-structure*) *pkg-string*)
       'system-relation *system-relation-attributes* '("RELATION-NAME") *system-relation-key*
       `(string-equal modifiedp "T") nil 'system-relation))
(cond (*provide-error-messages*
       (format *standard-output*
       "~%ERROR - ~s is the current database and it has modified relations" *active-db*)
       (format *standard-output*
       "~%        Please resolve this conflict by either saving or destroying this database")
       (format *standard-output* "~%        before defining a new one")))
(return-from define-database nil)))))
  (cond ((not *parameter-checking*)
 (setf db-name (string-upcase db-name)
       keyword-list (get-keyword-value-prereq '(dir doc env) (de-nest-keyword-list keyword-list))))
((null (setf db-name (validate-sym db-name t)))
 (return-from define-database nil)))
  ;;
  ;;  Get the keyword values. NOTE : Do not change the order of the GET-KEYWORD-VALUE keyword
  ;; list. The code in this function depend on this positional relationship !!!
  ;;
  (setf     ; *save-directory* nil  -- don't do this -- MRR 5.27.87
*save-user-id* nil
keyword-values (get-keyword-value '(dir doc env) keyword-list)
*save-directory* (get-directory keyword-list))
  ;;
  ;;  If an environment was specified, activate it. Otherwise set the value of all variables to the system default values.
  ;;
  (cond ((equal *environment-name* (car (get-keyword-value '(env) keyword-list)))
 t)
((third keyword-values)
 (setf *environment-name* (third keyword-values))
 (cond ((listp *environment-name*)
(setf dir-keyword-list (append '(dir) (cdr *environment-name*)))
(setf *environment-name* (car *environment-name*))
(load-environment *environment-name* dir-keyword-list))
       (t
(load-environment *environment-name*))))
(t
 ;;
 ;;  This part of the code needs to be redone as the following variables are no longer
 ;; part of the environment
 ;;
 (setf *default-anyp-width* 20
       *default-atom-width* 10
       *default-listp-width* 20
       *default-numberp-width* 8
       *default-stringp-width* 10)))
  (setf *database-documentation* (second keyword-values))
  (cond ((string-equal user-id "")
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - You must be logged in before defining a database"))
 (return-from define-database nil)))
  (if *provide-status-messages*
      (format *standard-output* "~%Defining database ~s" db-name))
  ;;
  ;;  Define the system relations. The first four system relations are needed to define all of the others, therefore their structures are
  ;; defined and the tuple for the system-relation relation is created but not inserted until all four have been defined. Then the tuples will
  ;; be inserted and the other system relations will be defined.
  ;;
  (setf *active-db* db-name
        *save-user-id* user-id
user-id *pkg-string*
*domain-list* '("ANYP" "ATOM" "LISTP" "NUMBERP" "STRINGP")
;;
;;  We will be needing this because all the information is not provided in the system-domain relation while we are defining the
;; database. We will reset this to nil so that the user defined relations will refer to the system-domain relation.
;;
temp-validity-checking *validity-checking*
*validity-checking* nil)
  ;;
  ;;  Define the SYSTEM-RELATION relation
  ;;
  (cond ((null (define-system-relation))
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - SYSTEM-RELATION could not be defined"))
 (setf *active-db* nil)
 (return-from define-database nil)))
  ;;
  ;;  Define the SYSTEM-ATTRIBUTE relation
  ;;
  (cond ((null (define-system-attribute))
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - SYSTEM-ATTRIBUTE could not be defined"))
 (setf *active-db* nil)
 (return-from define-database nil)))
  ;;
  ;;  Define the SYSTEM-DOMAIN relation
  ;;
  (cond ((null (define-system-domain))
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - SYSTEM-DOMAIN could not be defined"))
 (setf *active-db* nil)
 (return-from define-database nil)))
  ;;
  ;;  Define the SYSTEM-VIEW relation
  ;;
  (cond ((null (define-system-view))
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - SYSTEM-VIEW could not be defined"))
 (setf *active-db* nil)
 (return-from define-database nil)))
  ;;
  ;;  Define the SYSTEM-IMPLEMENTATION relation
  ;;
  (cond ((null (define-system-implementation))
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - SYSTEM-IMPLEMENTATION could not be defined"))
 (setf *active-db* nil)
 (return-from define-database nil)))
  ;;
  ;;  Define the SYSTEM-STORAGE-STRUCTURE relation
  ;;
  (cond ((null (define-system-storage-structure))
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - SYSTEM-STORAGE-STRUCTURE could not be defined"))
 (setf *active-db* nil)
 (return-from define-database nil)))
  ;;
  ;;  Define the SYSTEM-OPTFUNC relation
  ;;
  (cond ((null (define-system-optfunc))
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - SYSTEM-OPTFUNC could not be defined"))
 (setf *active-db* nil)
 (return-from define-database nil)))
  ;;
  ;;  Define the SYSTEM-WHEREOPT relation
  ;;
  (cond ((null (define-system-whereopt))
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - SYSTEM-WHEREOPT could not be defined"))
 (setf *active-db* nil)
 (return-from define-database nil)))
  ;;
  ;;  Define the SYSTEM-INDEX relation
   ;;
  (cond ((null (define-system-index))
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - SYSTEM-INDEX could not be defined"))
 (setf *active-db* nil)
 (return-from define-database nil)))
  ;;
  ;;  All of the system relations have been defined, insert the tuples
  ;;
  (if *provide-status-messages*
      (format *standard-output* "~%Insert the tuples into the system relations"))
  (setf user-id (or *save-user-id* user-id))
  (cond ((not *donot-commit*)
 (commit-system-relation)
 (init-where-opt)))
  (setf *validity-checking* temp-validity-checking)
  (if *provide-status-messages*
      (format *standard-output* "~%Definition of database ~s has been completed successfully" *active-db*))
  ;;
  ;;  If the user has requested a save, then save the database
  ;;
  (if (and (or *auto-save* (nth 3 keyword-values)) (not *donot-commit*))
      (save-database *active-db*))
  (return-from define-database *active-db*)))

(defun define-system-attribute ()
  (setf *system-attribute-key* '("RELATION-NAME" "ATTRIBUTE-NAME")
*system-attribute-key-domains* '("STRINGP" "STRINGP")
*system-attribute-attributes* '("RELATION-NAME" "ATTRIBUTE-NAME" "DOMAIN-FUNCTION" "DEFAULT-VALUE"
  "DOC" "OWNER-ID")
*system-relations* (cons "SYSTEM-ATTRIBUTE" *system-relations*))
  (define-relation 'system-attribute
   '(relation-name (dom "STRINGP" doc "Name of the relation which the attribute belongs" def ""
     format 15)
     attribute-name (dom "STRINGP" doc "The name of the attribute" def "" format 15)
     domain-function (dom "STRINGP" doc "The membership domain predicate" def "" format 10)
     default-value (dom "ANYP" doc "the value to used if one is not specified" def "?"
  format 10)
     doc (dom "STRINGP" doc "The attribute documentation string" def "" format 15)
     owner-id (dom "STRINGP" doc "The owner of the relation which contains the attr" def ""
    format 10))
    'imp *system-relation-base-implementation* 'key *system-attribute-key*
    'sto *system-relation-storage-structure*
    'doc "Contains one entry for each attribute in the database"))

(defun define-system-domain (&aux system-domain-list)
  (setf *system-domain-key*  '("DOMAIN-NAME")
*system-domain-key-domains* '("STRINGP")
*system-domain-attributes* '("DOMAIN-NAME" "OWNER-ID" "DEFAULT-PRINT-WIDTH" "DEFAULT-VALUE" "DOC")
*system-relations* (cons "SYSTEM-DOMAIN" *system-relations*))
  (define-relation 'system-domain
   '(domain-name (dom stringp doc "The name of the domain" def "" format 20)
     owner-id (dom "STRINGP" doc "The owner of the domain definition" def "" format 15)
     default-print-width (dom "NUMBERP"  doc "The print width of the domain" def 0 format 6)
     default-value (dom "ANYP" doc "The default value of the domain" def "?" format 10)
     doc (dom "STRINGP" doc "The attribute documentation string" def "" format 25))
    'imp *system-relation-base-implementation* 'sto *system-relation-storage-structure*
    'key *system-domain-key* 'doc "Contains one entry for each domain in the database")
  ;;
  ;;  Define the tuples which will be inserted into the SYSTEM-DOMAIN relation
  ;;
  (setf system-domain-list
`(("ANYP" "RTMS" ,*default-anyp-width* ,*default-anyp-value* "Always returns T")
  ("ATOM" "RTMS" ,*default-atom-width* ,*default-atom-value* "The Lisp ATOM function")
  ("LISTP" "RTMS" ,*default-listp-width* ,*default-listp-value* "The Lisp LISTP function")
  ("NUMBERP" "RTMS" ,*default-numberp-width* ,*default-numberp-value* "The Lisp NUMBERP function")
  ("STRINGP" "RTMS" ,*default-stringp-width* ,*default-stringp-value* "The Lisp STRINGP function")))
  (putp 'system-domain system-domain-list 'commit-tuples))

(defun define-system-implementation (&aux system-implementation-list)
  (setf *system-implementation-key* '("IMPLEMENTATION-NAME")
*system-implementation-key-domains* '("STRINGP")
*system-implementation-attributes* '("IMPLEMENTATION-NAME" "OWNER-ID" "DOC")
*system-relations* (cons "SYSTEM-IMPLEMENTATION" *system-relations*))
  (define-relation 'system-implementation
   '(implementation-name (dom "STRINGP" doc "The name of the storage structure being defined"
    def "" format 20)
     owner-id (dom "STRINGP" doc "The owner of the implementation definition" def "" format 20)
     doc (dom "STRINGP" doc "The implementation documentation string" def "" format 35))
    'imp *system-relation-base-implementation* 'key *system-implementation-key*
    'sto *system-relation-storage-structure*
    'doc "Contains one entry for each implementation in the database")
  ;;
  ;;  Form the tuples which will be inserted into the SYSTEM-IMPLEMENTATION relation
  ;;
  (setf system-implementation-list '(("FLAVOR" "RTMS" "Tuples are flavor instances")
      ("LIST" "RTMS" "Tuples are implemented as a list")
      ("STRUCT" "RTMS" "Tuples are structure instances")))
  (putp 'system-implementation system-implementation-list 'commit-tuples))

(defun define-system-index ()
  (setf *system-index-key* '("RELATION-NAME")
*system-index-key-domains* '("STRINGP")
*system-index-attributes* '("RELATION-NAME" "INDEX-NAME" "INDEX-TYPE" "KEY" "PRIORITY" "DOC")
*system-relations* (cons "SYSTEM-INDEX" *system-relations*))
  (define-relation 'system-index
   '(relation-name (dom "STRINGP" doc "The name of the relation upon which the index is being defined"
     def "" format 20)
    index-name (dom "STRINGP" doc "The name of the index which is defined on the relation"
         def "" format 20)
    index-type (dom "STRINGP" doc "The storage structure type of the index" def "AVL"
         format 20)
    key (dom "LISTP" def '() format 10
  doc "A list containing the names of the attribute names which form the key")
    priority (dom "NUMBERP" def 2 format 8
       doc "A positive, non-zero number specifying the priority of the index which is used when attempting to extract a key. 1 is the highest priority.")
    doc (dom "STRINGP" doc "The index documentation string" def "" format 35))
    'imp *system-relation-base-implementation* 'key *system-index-key*
    'sto *system-relation-storage-structure* 'doc "Contains one entry for each index in the database"))

(defun define-system-optfunc (&aux system-optfunc-tuple-list)
  (setf *system-optfunc-key* '("SYMBOL-NAME" "STORAGE-STRUCTURE-TYPE")
*system-optfunc-key-domains* '("STRINGP" "STRINGP")
*system-optfunc-attributes* '("SYMBOL-NAME" "STORAGE-STRUCTURE-TYPE" "OPTIMIZE-FUNCTION" "OWNER-ID")
*system-relations* (cons "SYSTEM-OPTFUNC" *system-relations*))
  (define-relation 'system-optfunc
   '(symbol-name (dom "STRINGP" doc "The name of the symbol" def "" format 20)
  storage-structure-type (dom "STRINGP" def "" format 20
     doc "Optimizations are based on the ss type")
  optimize-function (dom "STRINGP" def "" format 25
       doc "The name of the optimization function for this symbol")
  owner-id (dom "STRINGP" def "" format 10
     doc "The originator of the optimization function" ))
    'imp *system-relation-base-implementation* 'key *system-optfunc-key*
    'sto *system-relation-storage-structure*
    'doc "Contains one entry for each defined function and storage structure")
  ;;
  ;;  Define the tuples which will be inserted into the SYSTEM-OPTFUNC relation
  ;;
  (setf system-optfunc-tuple-list
'(("=" "HASH" "OPT-HASH-EQUAL" "RTMS")
  ("=" "AVL" "OPT-AVL-EQUAL" "RTMS")
;   ("+" "HASH" "OPT-HASH-PLUS" "RTMS")
  ("<" "AVL" "OPT-AVL-LT" "RTMS")
  (">" "AVL" "OPT-AVL-GT" "RTMS")
  ("<=" "AVL" "OPT-AVL-LT" "RTMS")
  (">=" "AVL" "OPT-AVL-GT" "RTMS")
  ("AND" "HASH" "OPT-HASH-AND" "RTMS")
  ("AND" "AVL" "OPT-AVL-AND" "RTMS")
  ("EQUAL" "HASH" "OPT-HASH-EQUAL" "RTMS")
  ("EQUAL" "AVL" "OPT-AVL-EQUAL" "RTMS")
  ("LESSP" "AVL" "OPT-AVL-LT" "RTMS")
  ("STRING-LESSP" "AVL" "OPT-AVL-LT" "RTMS")
  ("GREATERP" "AVL" "OPT-AVL-GT" "RTMS")
  ("STRING-GREATERP" "AVL" "OPT-AVL-GT" "RTMS")
  ("OR" "HASH" "OPT-HASH-OR" "RTMS")
  ("OR" "AVL" "OPT-AVL-OR" "RTMS")
;   ("PLUS" "HASH" "OPT-HASH-PLUS" "RTMS")
  ("STRING-EQUAL" "HASH" "OPT-HASH-EQUAL" "RTMS")
  ("STRING-EQUAL" "AVL" "OPT-AVL-EQUAL" "RTMS")))
  (putp 'system-optfunc system-optfunc-tuple-list 'commit-tuples))

(defun define-system-relation ()
  (setf *system-relation-key* '("RELATION-NAME")
*system-relation-key-domains* '("STRINGP")
*system-relation-attributes* '("RELATION-NAME" "OWNER-ID" "MODIFIEDP" "CARDINALITY" "TUPLE-FORMAT"
        "ATTRIBUTES" "DOMAINS" "KEY" "SAVE-DIRECTORY" "DOC" "IMPLEMENTATION-TYPE"
        "STORAGE-STRUCTURE" "DISK")
        *system-relations* '("SYSTEM-RELATION"))
  (define-relation 'system-relation
     '(relation-name (dom "STRINGP" format (20) doc "The name of the relation" def "")
       owner-id (dom "STRINGP" doc "The owner of the relation" def "" format (10))
       modifiedp (dom "ANYP" format (3) doc "T if modified since last save" def "?")
       cardinality (dom "NUMBERP" doc "The number of tuples in the relation" format (6) def 0)
       tuple-format (dom "LISTP" doc "List of print widths of attributes" def '() format (20))
       attributes (dom "LISTP" doc "List of attribute descriptors" def '() format (20))
       domains (dom "LISTP" doc "List of the domains types of the attributes" def '() format (20))
       key (dom "LISTP" doc "List of secondary indicies" def '() format (20))
       save-directory (dom "STRINGP" doc "The directory where the relation will be saved"
       def "" format (20))
       doc (dom "STRINGP" doc "The documentation string" def "" format (20))
       implementation-type (dom "STRINGP" doc "The base implementation type" def "" format (6))
       storage-structure (dom "STRINGP" doc "The storage structure type of the relation" def ""
   format (6))
               disk (dom "ANYP" doc "T is the relation is on disk, else it is in memory" default '()
     format (4)))
    'imp *system-relation-base-implementation* 'sto *system-relation-storage-structure*
    'key *system-relation-key* 'doc "Contains one tuple for each relation in the database"))

(defun define-system-storage-structure (&aux system-storage-structure-list)
  (setf *system-storage-structure-key* '("STORAGE-STRUCTURE-NAME")
*system-storage-structure-key-domains* '("STRINGP")
*system-storage-structure-attributes* '("STORAGE-STRUCTURE-NAME" "OWNER-ID" "DOC")
*system-relations* (cons "SYSTEM-STORAGE-STRUCTURE" *system-relations*))
  (define-relation 'system-storage-structure
  '(storage-structure-name (dom "STRINGP" doc "The name of the storage structure being defined"
     def "" format 20)
    owner-id (dom "STRINGP" doc "The owner of the storage structure definition" def "" format 20)
    doc (dom "STRINGP" doc "The storage structure documentation string" def "" format 35))
    'imp *system-relation-base-implementation* 'sto *system-relation-storage-structure*
    'key *system-storage-structure-key*
    'doc "Contains one entry for each implementation in the database")
  ;;
  ;;  Define the tuples to be inserted into the SYSTEM-STORAGE-STRUCTURE relation
  ;;
  (setf system-storage-structure-list
'(("HEAP" "RTMS" "Tuples are stored in a list")
  ("HASH" "RTMS" "Tuples are stored in a hash table")
  ("AVL" "RTMS" "Tuples are stored in an avl height balanced binary tree")))
  (putp 'system-storage-structure system-storage-structure-list 'commit-tuples))

(defun define-system-view ()
  (setf *system-view-key* '("VIEW-NAME" "OWNER-ID")
*system-view-key-domains* '("STRINGP" "STRINGP")
*system-view-attributes* '("VIEW-NAME" "VIEW-DEFINITION" "OWNER-ID" "VIEW-DOCUMENTATION")
*system-relations* (cons "SYSTEM-VIEW" *system-relations*))
  (define-relation 'system-view '(view-name (dom "STRINGP" doc "the name of the view" def "" format 20)
         view-definition (dom "ANYP" doc "the definition of the view" def "?" format 30)
         owner-id (dom "STRINGP" doc "the owner of the view" def "" format 20)
        view-documentation (dom "STRINGP" doc "the documentation of the view" def "" format 10))
    'imp *system-relation-base-implementation* 'sto *system-relation-storage-structure*
    'key *system-view-key* 'doc "Contains an entry for each view."))

(defun define-system-whereopt ()
  (setf *system-whereopt-key* '("FUNCTION-NAME")
*system-whereopt-key-domains* '("STRINGP")
*system-whereopt-attributes*  '("FUNCTION-NAME" "DOCUMENTATION")
*system-relations* (cons "SYSTEM-WHEREOPT" *system-relations*))
  (define-relation 'system-whereopt
     '(function-name (dom stringp format 20 def ""
      doc "The name of the function which may be optimized for the where clause")
       documentation (dom "STRINGP" doc "The documentation string" def "" format 20))
    'imp *system-relation-base-implementation* 'sto *system-relation-storage-structure*
    'key *system-whereopt-key*
    'doc "Contains one tuple for each aggregate function in the database")
  ;;
  ;;  Define the tuples which will be inserted into the SYSTEM-OPTFUNC relation
  ;;
  (putp 'system-whereopt '(("AVERAGE" "RTMS average function")
   ("COUNT-RTMS" "RTMS count function")
   ("RTMS-COUNT" "RTMS count function")
   ("MAXIMUM" "RTMS maximum function")
   ("MINIMUM" "RTMS minimum function")
   ("SUM" "RTMS sum function"))
'commit-tuples))


t domain-key-list
       rebalancep relation-name))
 (rplaca (cddr tree) mod-tree)
 (cond (rebalancep
(multiple-value-setq (tree rebalancep)
  (left-balance tree rebalancep)))))
((equal comparison-operator 'greater-than)
 (multiple-value-setq (mod-tree rebalancep)
   (insert-avl-struct new-element (cadddr tree) key key-list attribute-list domain-key-list
       rebalancep relation-name))
 (rplaca (cdddr tree) mod-tree)
 (cond (rebalancep
(multiple-value-setq (tree rebalancep)
  (right-balance tree rebalancep)))))
((equal comparison-operator 'equal)
 (multiple-value-setq (tree rebalancep)
   (insert-avl-equal tree new-element rebalancep))))))
  (values tree rebalancep))

(defun insert-avl-equal (tree new-element rebalancep)
  (rplaca tree (append (car new-element) (car tree)))
  (setf rebalancep nil)
  (values tree rebalancep))

(defun left-balance (tree rebalancep
     &aux left-branch right-branch)
  (cond ((equal (cadr tree) -1)
	 (rplaca (cdr tree) 0))))
