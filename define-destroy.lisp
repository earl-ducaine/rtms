
;;; -*- Mode:Common-Lisp; Package:RTMS; Base:10; Fonts:(*CODE-FONT* *COMMENT-FONT* *STRING-FONT*) -*-
;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved.
;;; DEFINE-DESTROY
;;;
;;; This file contains the following Explorer extensions to CommonLisp Standard as Indicated in the June 1985 Explorer Lisp
;;; Reference
;;;     fdefinedp
;;;     errset
;;;     deff
(deff defdom 'define-domain)

(defun define-domain (domain-name &rest keyword-list
      &key &optional default documentation format
      &allow-other-keys
      &aux def width1 doc status? pred)
  "Define new domain. Corresponding predicate is expected to be defined prior to this operation.

   DOMAIN-NAME     - Name of the domain to be defined.
   DOCUMENTATION   - Describes the new domain.
   FORMAT          - Print width for attributes belonging to this domain."
  default documentation format
  (block define-domain
(cond (*parameter-checking*
       (if (not (active-database domain-name))
   (return-from define-domain nil))))
(cond ((not (setf domain-name (validate-sym domain-name t)))
       (return-from define-domain nil)))
(if (null (fdefinedp (setf pred (read-from-string (string-append *pkg-name* domain-name)))))
    (progn
      (if *provide-error-messages*
  (format *standard-output* "~%ERROR - The predicate ~S has not been defined yet." pred))
      (return-from define-domain nil)))
(if (member domain-name (mapcar (function (lambda (dom)
       (car dom)))
  (qtrieve 'system-domain *system-domain-attributes* '(domain-name)
    *system-domain-key* t)) :test 'string-equal)
    (progn
      (if *provide-error-messages*
  (format *standard-output* "~%ERROR - The domain ~s already exists and may not be redefined."
  domain-name))
      (return-from define-domain nil)))
(setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
       ((or (null keyword-list) (not (listp (car keyword-list)))) keyword-list)))
(if *provide-status-messages*
    (format *standard-output* "~%Define domain ~s" domain-name))
(setf keyword-list (get-keyword-value-prereq '("DEF" "FORMAT" "DOC") keyword-list))
(if (setf width1 (car (get-keyword-value '(format) keyword-list)))
    (validate-format (setf width1 (if (not (listp width1)) width1 (car width1))))
    (setf width1 *default-anyp-width*))
(setf def (car (get-keyword-value '(def) keyword-list)))
(cond (*validity-checking*
       (cond ((null (car (errset (funcall pred def) nil)))
      (cond (*provide-warning-messages*
     (format *standard-output*
      "~%WARNING - The default value ~S does not satisfy the domain ~S"
      def domain-name)
     (format *standard-output*
      "~%          Other values will be tested for validity")))
      (setf def nil)))))
(cond ((null def)
       (mapc (function (lambda (existing-def)
  (if (car (errset (funcall pred (car existing-def)) nil))
      (setf def (car existing-def)))))
     (qtrieve 'system-domain *system-domain-attributes* '(default-value) *system-domain-key*
      t))))
(if (null def)
    (progn
      (if *provide-error-messages*
  (format *standard-output* "~%ERROR - None of the existing default values are valid for ~S."
  domain-name))
      (return-from define-domain nil)))
(if (setf doc (car (get-keyword-value '(doc) keyword-list)))
    doc
    (setf doc nil))
(setf status? *provide-status-messages*
      *provide-status-messages* nil)
(insert 'system-domain 'tuples (list (list domain-name user-id width1 def doc)))
(setf *provide-status-messages* status?)
(if *provide-status-messages*
    (format *standard-output* "~%Definition of domain ~s completed." domain-name))
(return-from define-domain domain-name)))

(deff defenv 'define-environment)

(defun define-environment (environment &rest keyword-list
   &key &optional auto-save directory errors parameter-checking
   relation-implementation relation-storage-structure status system-implementation
   system-storage-structure validity warnings
   &allow-other-keys
   &aux all-key-values
   keyword-list-2 (legal-imp nil) (legal-sto nil) (sys-imp nil) (sys-sto nil)
   (keywords '("AUTO" "DIR" "ERR" "PARA" "REL-IMP" "REL-STO" "STAT" "SYS-IMP" "SYS-STO"
        "VAL" "WARN")))
  "Global variables defining an environment can be set using this function.

   ENVIRONMENT  - Name of the environment.
   AUTO-SAVE    - If T, RTMS saves the database whenever a relation is modified.
   DIRECTORY    - Name of the default directory in which the database is to be saved.
   ERRORS       - If T, error messages are generated.
   PARAMETER-CHECKING - If T, extensive parameter validity checking is done.
   RELATION-IMPLEMENTATION - The default implementation type for the user relations.
   RELATION-STORAGE-STRUCTURE -The default storage structure type for the user relations.
   STATUS       - If T, status messages are generated.
   SYSTEM-IMPLEMENTATION - If there is no active database, this value will be used as the implementation
                           type for implementing system-relations.
   SYSTEM-STORAGE-STRUCTURE - If there is no active database, this value will be used as the storage structure
                              for implementing system-relations.
   VALIDITY     - If T, extensive validity checking is done for user-supplied data.
   WARNINGS     - If T, warning messages are generated."
  auto-save directory errors parameter-checking relation-implementation relation-storage-structure status
  system-implementation system-storage-structure validity warnings
  (block define-environment
  (cond ((null (setf environment (validate-sym environment t)))
 (return-from define-environment nil)))
  (setf *environment-name* environment)
  (setf keyword-list (de-nest-keyword-list keyword-list))
  ;;
  ;;  Obtain the names of the domains which are currently defined in the current database. Form the names of all of the possible
  ;; combinations of domain widths and domain default values and add them to the keyword list. Only the system defined domains may be
  ;; set before there is an active database.
  ;;
  ;;
  ;;  Obtain the values of only the keywords specified
  ;;
  (setf keyword-list-2 (get-keyword-value-prereq keywords keyword-list))
  (setf keyword-list '())
  (do ((keywords keyword-list-2 (cddr keywords)))
      ((null keywords) t)
    (setf keyword-list (cons (string-upcase (car keywords)) keyword-list)))
  (setf all-key-values (get-keyword-value keywords keyword-list-2))
  ;;
  ;;  Have determined which environment variables have been specified and their values, now process the list and set the proper variables
  ;; after checking for validity of the specified values
  ;;
  (do ((keyword keyword-list (cdr keyword)))
      ((null keyword) t)
    (cond ((equal (search "AUTO" (car keyword)) 0)
   (setf *auto-save* (car (get-keyword-value '(auto) keyword-list-2))))
  ((equal (search "DIR" (car keyword)) 0)
   (setf *save-directory* (get-directory keyword-list-2)))
  ((equal (search "ERR" (car keyword)) 0)
   (setf *provide-error-messages* (car (get-keyword-value '(err) keyword-list-2))))
  ((equal (search "PARA" (car keyword)) 0)
   (setf *parameter-checking* (car (get-keyword-value '(para) keyword-list-2))))
  ((and (string-equal (car keyword) "REL-IMP")(null *active-db*))
   (setf sys-imp (string-upcase (car (get-keyword-value '(rel-imp) keyword-list-2)))
 *rel-imp* sys-imp)
   (setf *relation-implementation*
 (string-append sys-imp (subseq *relation-implementation*
   (position "-" *relation-implementation*
      :test 'string-equal)))))
  ((string-equal (car keyword) "REL-IMP")
   (if (not legal-imp)
       (setf legal-imp (qtrieve 'system-implementation *system-implementation-attributes*
  '(implementation-name) *system-implementation-key* t)))
   (setf sys-imp (string-upcase (car (get-keyword-value '(rel-imp) keyword-list-2)))
 *rel-imp* sys-imp)
   (cond ((member (list sys-imp) legal-imp :test 'equal)
  (setf *relation-implementation* (string-append sys-imp
      (subseq  *relation-implementation*
        (position "-"
           *relation-implementation*
           :test 'string-equal)))))
 (*provide-error-messages*
  (format *standard-output* "~%ERROR - ~s is an unrecognized implementation type" sys-imp))))
  ((and (string-equal (car keyword) "REL-STO") (null *active-db*))
   (setf sys-sto (string-upcase (car (get-keyword-value '(rel-sto) keyword-list-2)))
 *rel-sto* sys-sto)
   (setf *relation-implementation* (string-append (subseq *relation-implementation* 0
       (search "-" *relation-implementation*))
      "-" sys-sto)))
  ((string-equal (car keyword) "REL-STO")
   (if (not legal-sto)
       (setf legal-sto (qtrieve 'system-storage-structure *system-storage-structure-attributes*
    '(storage-structure-name) *system-storage-structure-key* t)))
   (setf sys-sto (string-upcase (car (get-keyword-value '(rel-sto) keyword-list-2)))
 *rel-sto* sys-sto)
   (cond ((member (list sys-sto) legal-sto :test 'equal)
  (setf *relation-implementation*
(string-append (subseq *relation-implementation* 0
     (search "-" *relation-implementation*))
        "-" sys-sto)))
 (*provide-error-messages*
  (format *standard-output* "~%ERROR - ~s is an unrecognized storage-structure" sys-sto))))
  ((equal (search "STAT" (car keyword)) 0)
   (setf *provide-status-messages* (car (get-keyword-value '(stat) keyword-list-2))))
  ((and (string-equal (car keyword) "SYS-IMP") *active-db* *provide-warning-messages*)
   (format *standard-output* "~%WARNING - Can not change system relation implementation")
   (format *standard-output* "~%          while there is an active database"))
  ((and (string-equal (car keyword) "SYS-IMP") (not *active-db*))
   (setf sys-imp (string-upcase (car (get-keyword-value '(sys-imp) keyword-list-2))))
   (setf *system-relation-base-implementation* sys-imp))
  ((and (string-equal (car keyword) "SYS-STO") *active-db* *provide-warning-messages*)
   (format *standard-output* "~%WARNING - Can not change system relation storage structure")
   (format *standard-output* "~%          while there is an active database"))
  ((and (string-equal (car keyword) "SYS-STO") (not *active-db*))
   (setf sys-sto (string-upcase (car (get-keyword-value '(sys-sto) keyword-list-2))))
   (setf *system-relation-storage-structure* sys-sto))
  ((equal (search "VAL" (car keyword)) 0)
   (setf *validity-checking* (car (get-keyword-value '(val) keyword-list-2))))
  ((equal (search "WARN" (car keyword)) 0)
   (setf *provide-warning-messages* (car (get-keyword-value '(warn) keyword-list-2))))))
  (if *provide-status-messages*
      (format *standard-output* "~%Environment ~s defined" *environment-name*))
  (return-from define-environment *environment-name*)))


(deff defview 'define-view)
(defun define-view (viewname view-def &rest keyword-list
    &key &optional documentation
    &allow-other-keys
    &aux doc temp-status)
  "Define views on the relations.

   VIEW-NAME - Name of the view.
   VIEW-DEF  - Definition of the view.
   DOCUMENTATION - Describes the view."
  documentation
  (block define-view
(if (not (active-database))
    (return-from define-view nil))
(cond ((null (setf viewname (validate-sym viewname t)))
       (return-from define-view nil)))
(if (relationp viewname)
    (progn
      (if *provide-error-messages*
  (format *standard-output*
  "~%ERROR - A relation with the name ~S already exists in the ~s database."
  viewname *active-db*))
      (return-from define-view nil)))
(if (member viewname (mapcar (function (lambda (dom)
    (car dom)))
      (qtrieve 'system-view *system-view-attributes* '(view-name)
        *system-view-key* t)) :test 'string-equal)
    (progn
      (if *provide-error-messages*
  (format *standard-output* "~%ERROR - The view ~s already exists in the ~s database"
   viewname *active-db*))
      (return-from define-view nil)))
(setf temp-status *provide-status-messages*
      *provide-status-messages* nil)
(setf keyword-list (get-keyword-value-prereq '(doc) keyword-list)
      doc (car (get-keyword-value '(doc) keyword-list)))
(insert 'system-view 'tuples (list (list viewname view-def (string-upcase user-id) doc)))
(setf *provide-status-messages* temp-status)
(if *provide-status-messages*
    (format *standard-output* "~%The ~s view has been defined in the ~s database" viewname *active-db*))
(return-from define-view viewname)))

(defun modify-view (view-name &rest keyword-list
    &key &optional view-definition view-documentation
    &allow-other-keys
    &aux temp1 temp2)
  "Modify a view definition or its documentation.

   VIEW-NAME       - Name of the view.
   VIEW-DEFINITION - New definition of the view.
   VIEW-DOCUMENTATION - New description of the view."
  view-definition view-documentation
  (block modify-view
(cond (*parameter-checking*
       (if (not (active-database view-name))
   (return-from modify-view nil))
       (if (not (setf view-name (validate-sym view-name t)))
   (return-from modify-view nil))))
(if (not (member view-name (mapcar (function (lambda (dom)
          (car dom)))
     (qtrieve 'system-view *system-view-attributes* '(view-name)
       *system-view-key* t)) :test 'string-equal))
    (progn
      (if *provide-error-messages*
  (format *standard-output* "~%ERROR - The view ~s does not exist." view-name))
      (return-from modify-view nil)))
(if (not (caar (qtrieve 'system-view *system-view-attributes* '("OWNER-ID") *system-view-key*
 `(and (string-equal view-name ,view-name) (string-equal owner-id ,user-id)))))
    (progn
      (if *provide-error-messages*
  (format *standard-output*
  "~%ERROR - The owner of the view ~s is not the same as the current user-id ~S."
  view-name user-id))
      (return-from modify-view nil)))
(setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
       ((or (null keyword-list)(not (listp (car keyword-list)))) keyword-list)))
;;
;;  Determine if the user wants a memory only destroy
;;
(setf keyword-list (get-keyword-value-prereq '(view-def view-doc doc) keyword-list))
(setf temp1 (car (get-keyword-value '(view-def) keyword-list))
      temp2 (or (car (get-keyword-value '(view-doc) keyword-list))
(car (get-keyword-value '(doc) keyword-list))))
(if (not (or temp1 temp2))
    (return-from modify-view nil))
(modify 'system-view 'where
`(string-equal view-name ',view-name)
'attributes (cond ((and temp1 temp2)
    (list 'view-definition 'view-documentation))
   (temp1 (list 'view-definition))
   (temp2 (list 'view-documentation)))
'values (cond ((and temp1 temp2)
       (list `(quote ,temp1) `(quote ,temp2)))
      (temp1 (list `(quote ,temp1)))
      (temp2 (list `(quote ,temp2)))))
(return-from modify-view view-name)))

(deff destroy-db 'destroy-database)

;;
;;  RTMS will now look first for xfasl extensions for relation names.  9/24/85 SMC
;;
(defun destroy-database (database &rest keyword-list
 &key &optional disk &allow-other-keys
 &aux memory-only path pathname)
  "Delete the specified database from memory and all the corresponding files from
   disk if the keyword DISK is T.

   DATABASE - Name of the database to be destroyed.
   DISK     - If T, all the relevant files will be deleted."
  disk
  (block destroy-database
  (cond ((null (setf database (validate-sym database t)))
 (return-from destroy-database nil)))
  (cond ((null *active-db*)
 (cond (*provide-error-messages*
(format *standard-output* "~%ERROR - Only the active database may currently be destroyed")
(format *standard-output* "~%        and there is currently no active database")))
 (return-from destroy-database nil))
((not (string-equal *active-db* database))
 (cond (*provide-error-messages*
(format *standard-output* "~%ERROR - Only the active database may currently be destroyed")
(format *standard-output* "~%        The active database is ~s" *active-db*)))
 (return-from destroy-database nil)))
  (setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
 ((or (null keyword-list)(not (listp (car keyword-list)))) keyword-list)))
  ;;
  ;;  Determine if the user wants a memory only destroy
  ;;
  (setf keyword-list (get-keyword-value-prereq '(disk) keyword-list))
  (cond ((setf memory-only (not (car (get-keyword-value '(disk) keyword-list))))
 (if *provide-status-messages*
     (format *standard-output* "~%The database ~s will be deleted from memory" database)))
(t
 (if *provide-status-messages*
     (format *standard-output* "~%Destroying the ~s database" database))))

  ;;
  ;;Right now the name database is not used. But we need it to delete files on the disk
  ;;and when the system relations have the database name as prefix or whatever.
  ;;
  ;;Delete the file for the database definition.
  (if (not memory-only)
      (progn
(setf pathname (string-append (get-save-directory) database))
;;
;;  This was added to handle the change in the extensions used by RTMS
;;
(cond ((probe-file (setf path (string-append pathname ".lisp")))
       (delete-file (string-append path "#*")))
      ((probe-file (setf path (string-append pathname ".xld")))
       (delete-file (string-append path "#*")))
      ((probe-file (setf path (string-append pathname ".xfasl")))
       (delete-file (string-append path "#*")))
      ((probe-file (setf path (string-append pathname ".qfasl")))
       (delete-file (string-append path "#*")))
      (t
       (if *provide-error-messages*
  (format *standard-output* "~%ERROR - The ~s database does not exist in the directory ~s"
  database (get-directory keyword-list)))
      (return-from destroy-database nil)))))
  (if (not memory-only)
      (mapt
(function
  (lambda (tuple &aux temp pathname path dir)
  (setf temp (read-from-string
       (caar (project-list (list tuple) *system-relation-attributes*
      '("RELATION-NAME")))))
  (setf dir (caar (project-list (list tuple) *system-relation-attributes*
   '("SAVE-DIRECTORY"))))
  ;;DELETE all the files for each relation.
  (setf pathname (string-append dir *active-db* "-" temp "."))
  (cond-every ((probe-file (setf path (string-append pathname "XLD")))
       (delete-file (string-append path "#*")))
      ((probe-file (setf path (string-append pathname "XFASL")))
       (delete-file (string-append path "#*")))
      ((probe-file (setf path (string-append pathname "QFASL")))
       (delete-file (string-append path "#*")))
      ((probe-file (setf path (string-append pathname "LISP")))
       (delete-file (string-append path "#*"))))))
  ;;
  ;;  If the relation is a flavor, must use the new undef-flavor. This will be done at a
  ;; later time
  ;;
    'system-relation))
  (recover-all)
  (if *provide-status-messages*
      (format *standard-output* "~%The ~s database has been destroyed" database))
  (return-from destroy-database database)))

(defun get-save-directory ()
  (caar (qtrieve 'system-relation *system-relation-attributes* '("SAVE-DIRECTORY") *system-relation-key*
 '(string-equal relation-name "SYSTEM-RELATION"))))

(deff destroy-rel 'destroy-relation)

;
;  RTMS will now look first for xfasl extensions for relation names.  9/24/85 SMC
;

(defun destroy-relation (relation &rest keyword-list
 &key &optional disk &allow-other-keys
 &aux indices qtrieve-var pathname path memory-only)
  "Deletes the specified relation from the active database.
   Deletes all the files on disk if keyword DISK is t.

   RELATION - Name of the relation to be destroyed.
   DISK     - If T, the relevant files will be deleted."
  disk
  (block destroy-relation
  (if (not (active-database))
      (return-from destroy-relation nil))
  (cond ((null (setf relation (validate-sym relation t)))
 (return-from destroy-relation nil)))
  (setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
 ((or (null keyword-list)(not (listp (car keyword-list)))) keyword-list)))
  (setf qtrieve-var (car (qtrieve 'system-relation *system-relation-attributes*
   '("ATTRIBUTES" "IMPLEMENTATION-TYPE" "STORAGE-STRUCTURE" "SAVE-DIRECTORY")
   *system-relation-key*
   (list 'string-equal 'relation-name (string-upcase relation)))))
  (if (null qtrieve-var)
      (progn
(if *provide-error-messages*
    (format *standard-output* "~%ERROR - The relation ~S is not defined in the database ~s."
    (read-from-string relation) (read-from-string *active-db*)))
(return-from destroy-relation nil)))
  ;;
  ;;  Determine if the user wants a memory only destroy
  ;;
  (setf keyword-list (get-keyword-value-prereq '(disk) keyword-list))
  (cond ((setf memory-only (not (car (get-keyword-value '(disk) keyword-list))))
 (if *provide-status-messages*
     (format *standard-output* "~%The relation ~s will be deleted from memory" relation)))
(t
 (if *provide-status-messages*
     (format *standard-output* "~%Destroying the ~s relation" (read-from-string relation)))))
  ;;
  ;;Delete the system-relation tuple.
  ;;
    (delete-or-modify 'system-relation nil (list 'string-equal 'relation-name relation))
  ;;Since the following modification is not done for system-relation in delete-or-modify we have to make an explicit call.
    (delete-or-modify 'system-relation t (list 'string-equal 'relation-name "SYSTEM-RELATION")
      '("CARDINALITY" "MODIFIEDP") (list '(- cardinality 1) t))
  ;;
  ;;Delete the system-attribute tuples.
  ;;
  (mapcar (function
    (lambda (attr)
      (delete-or-modify 'system-attribute nil
 (list 'and (list 'string-equal 'relation-name (string-upcase (string relation)))
       (list 'string-equal 'attribute-name  (string-upcase attr))))))
  (car qtrieve-var))
  (delete-or-modify 'system-relation t (list 'string-equal 'relation-name "SYSTEM-ATTRIBUTE")
    '("CARDINALITY" "MODIFIEDP")  (list (list '- 'cardinality (length (car qtrieve-var))) t))
  ;;
  ;;  Determine if there are any indices defined for this relation, if so they must also be removed.
  ;;
  (cond ((and (not (member relation *system-relations* :test 'string-equal))
      (setf indices (qtrieve 'system-index *system-index-attributes* '("INDEX-NAME")
      *system-index-key* `(string-equal relation-name ,relation))))
 (mapc (function (lambda (index-name)
   (putp (car index-name) nil 'entry-point)))
       indices)
 (delete-or-modify 'system-index nil `(string-equal relation-name ,relation))
 (delete-or-modify 'system-relation t '(string-equal relation-name "SYSTEM-INDEX")
   '("CARDINALITY" "MODIFIEDP") (list (list '- 'cardinality (length indices)) t)))
(t
   ;;
  ;; Later, if the relation is defined but not restored yet, delete the disk file. If the relation is restored and also on disk, delete the file.
  ;; And if the optional parameter is T, then expunge the file. We can expunge the whole directory, but that is not what we want.
  ;; It will be nice if we can undefine flavor-definition, get rid of array etc.
  ;;
 (putp relation nil 'entry-point)))
  ;;
  ;;Delete all the files on the disk.
  ;;
  (cond ((not memory-only)
 (setf pathname (string-append (fourth qtrieve-var) *active-db* "-" relation "."))
 (cond-every ((probe-file (setf path (string-append pathname "XLD")))
      (delete-file (string-append path "#*")))
     ((probe-file (setf path (string-append pathname "XFASL")))
      (delete-file (string-append path "#*")))
     ((probe-file (setf path (string-append pathname "QFASL")))
      (delete-file (string-append path "#*")))
     ((probe-file (setf path (string-append pathname "LISP")))
      (delete-file (string-append path "#*"))))))
  (return-from destroy-relation relation)))

(defun destroy-domain (domain-name &aux status?)
  "Destroys the domain definition but keeps the domain predicate to handle previously defined data."
  (block destroy-domain
(cond (*parameter-checking*
       (if (not (active-database domain-name))
   (return-from destroy-domain nil))
       (if (not (setf domain-name (validate-sym domain-name t)))
   (return-from destroy-domain nil)))
      (t
       (setf domain-name (string domain-name))))
(if (not (member domain-name
 (mapcar (function (lambda (dom)
       (car dom)))
   (qtrieve 'system-domain *system-domain-attributes* '(domain-name)
    *system-domain-key* t)) :test 'string-equal))
    (progn
      (if *provide-error-messages*
  (format *standard-output* "~%ERROR - The domain ~s does not exist." domain-name))
      (return-from destroy-domain nil)))
(if (not (equal user-id
(caar (qtrieve 'system-domain *system-domain-attributes* '("OWNER-ID")
    *system-domain-key* `(string-equal domain-name ,domain-name)))))
    (progn
      (if *provide-error-messages*
  (format *standard-output*
  "~%ERROR - The owner of the domain ~s is not the same as the current user-id ~S."
  domain-name user-id))
      (return-from destroy-domain nil)))
(setf status? *provide-status-messages*
      *provide-status-messages* nil)
(delete-tuples 'system-domain 'where `(string-equal domain-name ',domain-name))
(setf *provide-status-messages* status?)
(setf *domain-list* (remove domain-name *domain-list* :test 'string-equal))
(if *provide-status-messages*
    (format *standard-output* "~%Destruction of domain ~s completed." domain-name))
(return-from destroy-domain domain-name)))

(defun destroy-implementation (implementation-name &aux temp-status)
  "Destroys implementation type definition but keeps the accessor functions to handle previously defined relations using this implementation."
  (block destroy-implementation
(cond (*parameter-checking*
       (if (not (active-database implementation-name))
   (return-from destroy-implementation nil))
       (if (not (setf implementation-name (validate-sym implementation-name t)))
   (return-from destroy-implementation nil)))
      (t
       (setf implementation-name (string implementation-name))))
(if (not (member implementation-name
 (mapcar (function (lambda (dom) (car dom)))
  (qtrieve 'system-implementation *system-implementation-attributes*
    '(implementation-name) *system-implementation-key* t))
 :test 'string-equal))
    (progn
      (if *provide-error-messages*
  (format *standard-output* "~%ERROR - The implementation ~s does not exist."
  implementation-name))
      (return-from destroy-implementation nil)))
(if (not (equal user-id
(caar (qtrieve 'system-implementation *system-implementation-attributes*
    '("OWNER-ID") *system-implementation-key*
    `(string-equal implementation-name ,implementation-name)))))
    (progn
      (if *provide-error-messages*
  (format *standard-output*
  "~%ERROR - The owner of the implementation ~s is not the same as the current user-id ~S."
  implementation-name user-id))
      (return-from destroy-implementation nil)))
(setf temp-status *provide-status-messages*
      *provide-status-messages* nil)
(delete-tuples 'system-implementation 'where `(string-equal implementation-name ',implementation-name))
(setf *provide-status-messages* temp-status)
(if *provide-status-messages*
    (format *standard-output* "~%The ~s implementation has been removed from the ~s database"
    implementation-name *active-db*))
(return-from destroy-implementation implementation-name)))

(defun destroy-storage-structure (storage-structure-name &aux temp-status)
  "Destroys storage structure definition but keeps the accessor functions to handle previously defined relations using this structure."
  (block destroy-storage-structure
(cond (*parameter-checking*
       (if (not (active-database storage-structure-name))
   (return-from destroy-storage-structure nil))
       (if (not (setf storage-structure-name (validate-sym storage-structure-name t)))
   (return-from destroy-storage-structure nil)))
      (t
       (setf storage-structure-name (string storage-structure-name))))
(if (not (member storage-structure-name
 (mapcar (function (lambda (dom)
       (car dom)))
  (qtrieve 'system-storage-structure *system-storage-structure-attributes*
    '(storage-structure-name) *system-storage-structure-key* t))
 :test 'string-equal))
    (progn
      (if *provide-error-messages*
  (format *standard-output* "~%ERROR - The storage-structure ~s does not exist."
  storage-structure-name))
      (return-from destroy-storage-structure nil)))
(if (not (equal user-id
(caar (qtrieve 'system-storage-structure *system-storage-structure-attributes*
    '("OWNER-ID") *system-storage-structure-key*
    `(string-equal storage-structure-name ,storage-structure-name)))))
    (progn
      (if *provide-error-messages*
  (format *standard-output*
  "~%ERROR - The owner of the storage structure ~s is not the same as the current user-id ~S."
  storage-structure-name user-id))
      (return-from destroy-storage-structure nil)))
(setf temp-status *provide-status-messages*
      *provide-status-messages* nil)
(delete-tuples 'system-storage-structure
       'where `(string-equal storage-structure-name ',storage-structure-name))
(setf *provide-status-messages* temp-status)
(if *provide-status-messages*
    (format *standard-output* "~%The ~s storage-structure has been removed from the ~s database"
    storage-structure-name *active-db*))
(return-from destroy-storage-structure storage-structure-name)))

(defun destroy-view (view-name &aux temp-status)
  "Destroys the view from memory."
  (block destroy-view
(cond (*parameter-checking*
       (if (not (active-database view-name))
   (return-from destroy-view nil))
       (if (not (setf view-name (validate-sym view-name t)))
   (return-from destroy-view nil))))
(if (not (member view-name (mapcar (function (lambda (dom)
          (car dom)))
     (qtrieve 'system-view *system-view-attributes* '(view-name)
       *system-view-key* t)) :test 'string-equal))
    (progn
      (if *provide-error-messages*
  (format *standard-output* "~%ERROR - The view ~s does not exist." view-name))
      (return-from destroy-view nil)))
(if (not (caar (qtrieve 'system-view *system-view-attributes* '("OWNER-ID") *system-view-key*
 `(and (string-equal view-name ,view-name) (string-equal owner-id ,user-id)))))
    (progn
      (if *provide-error-messages*
  (format *standard-output*
  "~%ERROR - The owner of the view ~s is not the same as the current user-id ~S."
  view-name user-id))
      (return-from destroy-view nil)))
(setf temp-status *provide-status-messages*
      *provide-status-messages* nil)
(delete-tuples 'system-view 'where `(string-equal view-name ',view-name))
(setf *provide-status-messages* temp-status)
(if *provide-status-messages*
    (format *standard-output* "~%The ~s view has been removed from the ~s database"
    view-name *active-db*))
(return-from destroy-view view-name)))


(deff envstat 'environment-status)

(defun environment-status (&rest ignore &aux output-to-window)
  (setf output-to-window (car (errset (send  *output-window* ':exposed-p) nil)))
  (setf *environment-name* (read-from-string (string-upcase *environment-name*)))
  (cond (output-to-window
 (send *output-window* ':append-item (format nil ""))
 (send *output-window* ':append-item (format nil "Environment name --> ~s" *environment-name*))
 (send *output-window* ':append-item (format nil "Save directory --> ~s" *save-directory*))
 (send *output-window* ':append-item (format nil ""))
 (send *output-window* ':append-item
       (format nil "Provide warning messages --> ~s" *provide-warning-messages*))
 (send *output-window* ':append-item
       (format nil "Provide status messages --> ~s" *provide-status-messages*))
 (send *output-window* ':append-item
       (format nil "Provide error messages --> ~s" *provide-error-messages*))
 (send *output-window* ':append-item
       (format nil "Validity checking --> ~s" *validity-checking*))
 (send *output-window* ':append-item
       (format nil "Auto save --> ~s" *auto-save*))
 (send *output-window* ':append-item
       (format nil "Parameter-checking --> ~s" *parameter-checking*))
 (send *output-window* ':append-item (format nil ""))
 (send *output-window* ':append-item
       (format nil "Relation implementation --> ~s" *relation-implementation*))
 (send *output-window* ':append-item
       (format nil "System relation implementation --> ~s" *system-relation-base-implementation*))
 (send *output-window* ':append-item
       (format nil "System relation storage structure --> ~s" *system-relation-storage-structure*))
 (send *output-window* ':append-item (format nil "")))
(t
 (format *standard-output* "~%Environment name --> ~s" *environment-name*)
 (format *standard-output* "~%Save directory --> ~s" *save-directory*)
 (format *standard-output* "~%~%Provide warning messages --> ~s" *provide-warning-messages*)
 (format *standard-output* "~%Provide status messages --> ~s" *provide-status-messages*)
 (format *standard-output* "~%Provide error messages --> ~s" *provide-error-messages*)
 (format *standard-output* "~%Validity checking --> ~s" *validity-checking*)
 (format *standard-output* "~%Auto save --> ~s" *auto-save*)
 (format *standard-output* "~%Parameter-checking --> ~s" *parameter-checking*)
 (format *standard-output* "~%~%Relation implementation --> ~s" *relation-implementation*)
 (format *standard-output* "~%System relation implementation --> ~s"
 *system-relation-base-implementation*)
 (format *standard-output* "~%System relation storage structure --> ~s"
 *system-relation-storage-structure*)
 (format *standard-output* "~%")))
  *environment-name*)

(defun modify-domain (domain-name &rest keyword-list
      &key &optional format default doc &allow-other-keys
      &aux attribute-list default-value doc-value domain format-value value-list)
  "Modify the default format, value, and documentation of a domain.

   DOMAIN-NAME - Name of the domain to be modified.
   FORMAT      - New format, i.e the print width, for this domain.
   DEFAULT     - New default value for this domain.
   DOC         - New description of this domain."
  format default doc
  (block modify-domain
  (if (not (active-database))
      (return-from modify-domain nil))
  (if (null (setf domain-name (validate-sym domain-name t)))
      (return-from modify-domain nil))
  (setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
 ((or (null keyword-list)(not (listp (car keyword-list)))) keyword-list)))
  (setf domain (car (qtrieve 'system-domain *system-domain-attributes* '("DOMAIN-NAME") *system-domain-key*
     (list 'string-equal 'domain-name domain-name))))
  (cond ((null domain)
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - ~s is not a domain which is defined in the current database"
     domain-name))
 (return-from modify-domain nil)))
  (setf keyword-list (get-keyword-value-prereq '(format def doc) keyword-list)
attribute-list nil
value-list nil
*doc* "RTMS-NO-VALUE")
  (cond-every ((setf format-value (car (get-keyword-value '(format) keyword-list)))
       (cond ((numberp format-value)
      (cond ((validate-format format-value)
     (setf attribute-list (cons 'default-print-width attribute-list)
    value-list (cons format-value value-list)))))
     ((numberp (car format-value))
      (cond ((validate-format (car format-value))
     (setf attribute-list (cons 'default-print-width attribute-list)
    value-list (cons (car format-value) value-list)))))
     (t
      (if *provide-warning-messages*
  (format *standard-output*
   "~%WARNING - The value for the FORMAT keyword, ~s is not a positive numeric value"
   format-value)))))
      ((setf default-value (car (get-keyword-value '(def) keyword-list)))
       (cond (*validity-checking*
      (cond ((funcall (find-symbol (car domain) *pkg-string*) default-value)
     (setf attribute-list (cons 'default-value attribute-list)
    value-list (cons (list 'quote default-value) value-list)))
    (*provide-warning-messages*
     (format *standard-output* "~%WARNING - ~s is not a value of the domain ~s"
      default-value domain)
     (format *standard-output* "~%         It may not be used as the default value"))))
     (t
      (setf attribute-list (cons 'default-value attribute-list)
    value-list (cons (list 'quote default-value) value-list)))))
      ((not (string-equal "RTMS-NO-VALUE"
   (setf doc-value (string (car (get-keyword-value '(doc) keyword-list))))))
       (setf attribute-list (cons 'doc attribute-list)
     value-list (cons doc-value value-list))))
  ;;
  ;;  The value and attribute list have been formed, modify the system-domain function
  ;;
  (setf *doc* nil)
  (delete-or-modify 'system-domain t (list 'string-equal 'domain-name domain-name)
    (convert-attributes attribute-list) value-list)
  (return-from modify-domain domain-name)))

(defun validate-format (format-value)
  (cond ((<= format-value 0)
 (if *provide-warning-messages*
     (format *standard-output*
     "~%WARNING - The FORMAT value specified, ~s, is not a positive numeric value"
     format-value))
 nil)
(t
 t)))


;; fragment

erator val1 val2)
  (setf comparison-operator 'equal)
  ;;
  ;;  It is assumed that the key lists are of the same length, this should
  ;; always be the case.
  ;;
  (do ((key1 key-list-1 (cdr key1))
       (key2 key-list-2 (cdr key2))
       (domain key-doma
