
;;; -*- Mode:Common-Lisp; Package:RTMS; Fonts:(*CODE-FONT* *COMMENT-FONT* *STRING-FONT*); Base:10 -*-
;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved
;;; INDEX
;;;
;;; This file contains the following Explorer extensions to CommonLisp Standard as Indicated in the June 1985 Explorer Lisp
;;; Reference
;;;     errset
;;;
;;; The following function contains flavor references and thus are incompatable with CommonLisp. Their removal will not
;;; effect the functionality of RTMS.
;;;     index-insert-flavor-hash
;;;     index-insert-flavor-heap
;;;     index-insert-flavor-avl
;;;

(defun define-index (relation-name
     &rest keyword-list
     &key &optional name documentation storage-structure key priority
     &allow-other-keys
     &aux index-doc index-key index-name index-priority index-type relation-attributes
     relation-implementation relation-storage-structure temp-relation-name)
  "Define an index on a relation in the active database.

    RELATION-NAME - Name of the relation on which the index will be defined.
    NAME - Name of the index to be defined
    KEY - List of attributes names which form the key of the index.
    STORAGE-STRUCTURE - The name of a RTMS defined storage structure upon which will be used as the index structure.
    PRIORITY - A numerical value which determines the order in which RTMS will search multiple indices of a relation
               for a possible key. The number one receives the highest consideration, if it fails the next index in
               value is attempted.
    DOCUMENTATION - A string describing this index."
  name documentation storage-structure key priority
  (block define-index
  (cond (*parameter-checking*
 (if (not (active-database relation-name))
     (return-from define-index nil))))
  (if (not (setf relation-name (validate-sym relation-name)))
      (return-from define-index nil))
  (setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
 ((or (null keyword-list)(not (listp (car keyword-list)))) keyword-list)))
  (setf keyword-list (get-keyword-value-prereq '(name sto key doc priority) keyword-list))
  (setf index-name (get-keyword-value '(name sto key doc priority) keyword-list)
index-type (second index-name)
index-key (convert-attributes (third index-name))
index-doc (fourth index-name)
index-priority (fifth index-name)
index-name (first index-name))
  (setf temp-relation-name (get-relation relation-name '(attributes implementation-type storage-structure) nil)
relation-attributes (caadr temp-relation-name)
relation-implementation (second (second temp-relation-name))
relation-storage-structure (third (second temp-relation-name))
temp-relation-name (car temp-relation-name))
  (cond ((not relation-attributes)
 (cond (*provide-error-messages*
(format *standard-output* "~%ERROR - The relation ~s does not exist in the ~s database"
relation-name *active-db*)))
 (return-from define-index nil))
(t
 (setf relation-name temp-relation-name)))
  ;;
  ;;  Determine if there is an index on the current relation with the same name as that requested. If so, it is an error.
  ;;
  (cond ((null index-name)
 (setf index-name (string (gensym))))
(t
 (setf index-name (string-upcase index-name))
 (cond ((setf temp-relation-name (car (qtrieve 'system-index *system-index-attributes*
         '("RELATION-NAME" "INDEX-NAME") *system-index-key*
         `(string-equal index-name ,(string-upcase index-name)))))
(if *provide-error-messages*
    (format *standard-output*
    "~%ERROR - An index with the name of ~s has already been defined on the relation ~s"
    (second temp-relation-name) (first temp-relation-name)))
(return-from define-index nil)))))

  ;;
  ;;  Determine if the requested storage structure is defined in the current database
  ;;
  (cond ((null index-type)
 (setf index-type "AVL"))
(t
 (cond ((not (car (qtrieve 'system-storage-structure *system-storage-structure-attributes*
    '("STORAGE-STRUCTURE-NAME") *system-storage-structure-key*
    `(string-equal storage-structure-name ,(string-upcase index-type)))))
(if *provide-error-messages*
    (format *standard-output* "~%ERROR - ~s is an undefined storage structure in the ~s database"
    index-type *active-db*))
(return-from define-index nil)))))
  ;;
  ;;  Determine if the priority specified is within acceptable limits
  ;;
  (cond ((null index-priority)
 (setf index-priority 2))
((< index-priority 1)
 (cond (*provide-error-messages*
(format *standard-output* "~%ERROR - ~s is an illegal value for the value of priority." index-priority)
(format *standard-output* "~%        Priority must be a positive number.")))
 (return-from define-index nil)))
  (cond ((null index-key)
 (setf index-key (list (car relation-attributes)))))
  ;;
  ;;  Everything seems to be in order proceed
  ;;
  (if *provide-status-messages*
      (format *standard-output* "~%Define index ~s on relation ~s in database ~s"
      (read-from-string index-name) (read-from-string (string-upcase relation-name))
      (read-from-string *active-db*)))
  (if (null (create-index-relation relation-name index-name relation-attributes index-key
    index-type relation-implementation relation-storage-structure))
      (return-from define-index nil))
  ;;
  ;;  Insert the index tuple into the SYSTEM-INDEX relation
  ;;
  (insert 'system-index 'tuples (list (list (string-upcase relation-name) index-name
      (string-upcase index-type) index-key index-priority index-doc)))

  (if *provide-status-messages*
      (format *standard-output* "~%Index ~s has been defined on relation ~s in database ~s"
      (read-from-string index-name)
      (read-from-string (string-upcase relation-name))
      (read-from-string *active-db*)))
  ;;
  ;;  Create the actual index structure and insert the tuples into it using the new index
  ;;
  (return-from define-index index-name)))

(defun create-index-relation (relation-name index-name relation-attributes index-key index-type
      relation-implementation relation-storage-structure
      &aux tuples)
  ;;
  ;;  Create the structure of the type of the storage structure of the index and insert the tuples into the new index. The DEFREL-sto
  ;; function will validate the attributes and the key and define the appropriate structure.
  ;;
  (cond ((null (errset
 (setf index-key (funcall (find-symbol (concatenate 'string "DEFREL-"
         (string-upcase index-type)) *pkg-string*)
    index-name relation-attributes
    (list 'key (convert-attributes index-key))))
       nil))
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - ~s is an undefined storage structure" index-key))
 (setf index-name nil))
(t
 (setf tuples (funcall (find-symbol (concatenate 'string "OBTAIN-TUPLES-"
     (string-upcase relation-storage-structure))
      *pkg-string*)
       relation-name))
 (funcall (find-symbol (concatenate 'string "INDEX-INSERT-" (string-upcase relation-implementation)
      "-" (string-upcase index-type))
       *pkg-string*)
  index-name tuples relation-attributes index-key relation-name)))
  index-name)

(defun obtain-tuples-heap (relation-name)
  (getp relation-name 'entry-point))

(defun obtain-tuples-hash (relation-name &aux tuple-list)
  (maphash (function (lambda (key-val tuples)
       key-val
       (setf tuple-list (append tuples tuple-list))))
   (getp relation-name 'entry-point))
  tuple-list)

(defun obtain-tuples-avl (relation-name)
  (avl-inorder-traversal (getp relation-name 'entry-point)))

(defun index-insert-flavor-hash (index-name tuples attributes key-list relation-name
  &aux key hash-relation)
  attributes relation-name key-list
  (setf key (project-flavor tuples attributes key-list relation-name)
hash-relation (getp index-name 'entry-point))
  (mapcar (function
    (lambda (tuple keyval)
      ;;
      ;;Insert the tuple into the hash table using heap formation for collisions.
      ;;
      (puthash keyval (cons tuple (gethash keyval hash-relation)) hash-relation)))
  tuples key))


(defun index-insert-struct-hash (index-name tuples attributes key-list relation-name
  &aux key hash-relation)
  attributes relation-name key-list
  (setf key (project-struct tuples attributes key-list relation-name)
hash-relation (getp index-name 'entry-point))
  (mapcar (function
    (lambda (tuple keyval)
      ;;
      ;;Insert the tuple into the hash table using heap formation for collisions.
      ;;
      (puthash keyval (cons tuple (gethash keyval hash-relation)) hash-relation)))
  tuples key))

(defun index-insert-flavor-heap (index-name tuples attributes key-list relation-name)
  attributes key-list relation-name
  (putp index-name (nconc (getp index-name 'entry-point) tuples) 'entry-point))

(defun index-insert-struct-heap (index-name tuples attributes key-list relation-name)
  attributes key-list relation-name
  (putp index-name (nconc (getp index-name 'entry-point) tuples) 'entry-point))

(defun index-insert-flavor-avl (index-name tuples attributes key-list relation-name
       &aux domain-list domain-key-list key new-element tree)
  (setf domain-list (caar (qtrieve 'system-relation *system-relation-attributes* '(domains)
  *system-relation-key*
  (list 'string-equal 'relation-name (string-upcase relation-name))))
domain-key-list (car (project-list (list domain-list) attributes key-list))
key (project-flavor tuples attributes key-list)
tree (getp index-name 'entry-point))
  ;;
  ;; Insert one tuple at a time into the AVL tree
  ;;
  (do ((tuple% tuples (cdr tuple%))
       (key% key (cdr key%)))
      ((null tuple%) t)
    ;;
    ;;  Form the new element in a form suitable for insertion
    ;;
    (setf new-element (cons (list (car tuple%)) (append (list 0) (list nil) (list nil)))
  tree (insert-avl-flavor new-element tree (car key%) key-list attributes domain-key-list nil
   index-name)))
  (putp index-name tree 'entry-point))

(defun index-insert-struct-avl (index-name tuples attributes key-list relation-name
       &aux domain-list domain-key-list key new-element tree)
  (setf domain-list (caar (qtrieve 'system-relation *system-relation-attributes* '(domains)
    *system-relation-key*
  (list 'string-equal 'relation-name (string-upcase relation-name))))
domain-key-list (car (project-list (list domain-list) attributes key-list))
key (project-struct tuples attributes key-list relation-name)
tree (getp index-name 'entry-point))
  ;;
  ;; Insert one tuple at a time into the AVL tree
  ;;
  (do ((tuple% tuples (cdr tuple%))
       (key% key (cdr key%)))
      ((null tuple%) t)
    ;;
    ;;  Form the new element in a form suitable for insertion
    ;;
    (setf new-element (cons (list (car tuple%)) (append (list 0) (list nil) (list nil)))
  tree (insert-avl-struct new-element tree (car key%) key-list attributes domain-key-list nil
   relation-name)))
  (putp index-name tree 'entry-point))

(defun index-insert-list-heap (index-name tuples attributes key-list relation-name)
  relation-name key-list attributes
  (putp index-name (nconc (getp index-name 'entry-point) tuples) 'entry-point))

(defun index-insert-list-hash (index-name tuples attributes key-list relation-name &aux hash-relation key)
  relation-name
  (setf hash-relation (getp index-name 'entry-point)
key (project-list tuples attributes key-list))
   (mapcar
     (function (lambda (tuple keyval)
 ;;
 ;;Here the tuple (val.1 val.2 .......val.n) itself is stored in the the hash table.
 ;;
 (puthash keyval (cons tuple (gethash keyval hash-relation)) hash-relation)))
     tuples key))

(defun index-insert-list-avl (index-name tuples attributes key-list relation-name
       &aux domain-list domain-key-list key new-element tree)
  (setf domain-list (caar (qtrieve 'system-relation *system-relation-attributes* '(domains)
  *system-relation-key*
  (list 'string-equal 'relation-name (string-upcase (string relation-name)))))
domain-key-list (car (project-list (list domain-list) attributes key-list))
key (project-list tuples attributes key-list)
tree (getp index-name 'entry-point))
  ;;
  ;; Insert one tuple at a time into the AVL tree
  ;;
  (do ((tuple% tuples (cdr tuple%))
       (key% key (cdr key%)))
      ((null tuple%) t)
    ;;
    ;;  Form the new element in a form suitable for insertion
    ;;
    (setf new-element (cons (list (car tuple%)) (append (list 0) (list nil) (list nil)))
  tree (insert-avl-list new-element tree (car key%) key-list attributes domain-key-list nil
   index-name)))
  (putp index-name tree 'entry-point))


(defun extract-key (relation-name attributes key domains relation-storage-structure where-clause index-name
    &aux index-key index-list index-type (key-value nil)
    (string-relation-name (string relation-name)) package-name)
  ;;
  ;;  First attempt to obtain a key from the main relation if an index name is not passed
  ;;
  (setf package-name (package-name (or (symbol-package relation-name) *pkg-string*)))
  (cond ((null index-name)
 (setf key-value (funcall (find-symbol (concatenate 'string "EXTRACT-KEY-" relation-storage-structure)
         *pkg-string*)
   attributes key domains where-clause package-name)
       index-type relation-storage-structure
       index-key key)))
  ;;
  ;;  If no key could be obtained attempt to find an index which will do
  ;;
  (cond ((and (null key-value) (not (member string-relation-name *system-relations*
      :test 'string-equal)))
   (cond ((null index-name)
(setf index-name relation-name
      index-list (project-list (process-quick-sort (qtrieve 'system-index
          *system-index-attributes*
          *system-index-attributes*
          *system-index-key*
          `(string-equal relation-name
           ,string-relation-name
           ))
        '(priority) *system-index-attributes*)
         *system-index-attributes* '(index-name index-type key))))
       (t
(cond ((setf index-list
     (qtrieve 'system-index *system-index-attributes* '(index-name index-type key)
       *system-index-key*
       `(and (string-equal relation-name ,string-relation-name)
      (string-equal index-name ,(string-upcase index-name)))))
       (cond ((stringp index-name)
      (setf relation-name (find-symbol (string-upcase index-name) *pkg-string*))))))))
 (cond (index-list
(do ((index-list index-list (cdr index-list)))
    ((or (null index-list) key-value) key-value)
  (setf key-value (funcall (find-symbol (concatenate 'string "EXTRACT-KEY-"
            (second (car index-list))) *pkg-string*)
     attributes (third (car index-list)) domains where-clause
     package-name)
index-name (find-symbol (string-upcase (first (car index-list))) *pkg-string*)
index-type (second (car index-list))
index-key (third (car index-list))))
(cond ((and (null index-name)
    (null key-value))
       (setf index-name relation-name
     index-type relation-storage-structure
     index-key key))))
       (t
(setf key-value nil
      index-name relation-name
      index-type relation-storage-structure
      index-key key))))
((null index-name)
 (setf index-name relation-name)))
  (values index-name key-value index-type index-key))

(defun modify-index (relation-name index-name
     &rest keyword-list
     &key &optional new-name documentation storage-structure key priority
     &allow-other-keys
     &aux index-doc index-info index-key index-priority index-type new-index-name
          relation-attributes relation-implementation relation-key relation-storage-structure
  temp-relation-name tuples)
  "Modify an index on a relation in the active database.

    RELATION-NAME - Name of the relation on which the index to be modified is defined
    INDEX-NAME - Name of the index to be modified
    NEW-NAME - New name for the specified index
    KEY - List of attributes names which form the key of the index.
    STORAGE-STRUCTURE - The name of a RTMS defined storage structure upon which will be used as the index structure.
    PRIORITY - A numerical value which determines the order in which RTMS will search multiple indices of a relation
               for a possible key. The number one receives the highest consideration, if it fails the next index in
               value is attempted.
    DOCUMENTATION - A string describing this index."
  new-name documentation storage-structure key priority
  (block modify-index
  (cond (*parameter-checking*
 (if (not (active-database relation-name))
     (return-from modify-index nil))))
  (if (not (setf relation-name (validate-sym relation-name)))
      (return-from modify-index nil))
  (setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
 ((or (null keyword-list)(not (listp (car keyword-list)))) keyword-list)))
  (setf index-name (string-upcase index-name))
  (setf keyword-list (get-keyword-value-prereq '(new sto key doc priority) keyword-list))
  (setf new-index-name (get-keyword-value '(new sto key doc priority) keyword-list)
index-type (second new-index-name)
index-key (third new-index-name)
index-doc (fourth new-index-name)
index-priority (fifth new-index-name)
new-index-name (first new-index-name))
  ;;
  ;;  Must determine if the relation upon which the index is requested does indeed exist in the current database.
  ;; We must also know the attributes which are defined in the relation so that the validity of the key can be verified.
  ;;
  (setf temp-relation-name (get-relation relation-name '(attributes implementation-type storage-structure key)
   nil)
relation-attributes (caadr temp-relation-name)
relation-implementation (second (second temp-relation-name))
relation-storage-structure (third (second temp-relation-name))
relation-key (fourth (second temp-relation-name))
temp-relation-name (car temp-relation-name))
  (cond ((not relation-attributes)
 (cond (*provide-error-messages*
(format *standard-output* "~%ERROR - The relation ~s does not exist in the ~s database"
relation-name *active-db*)))
 (return-from modify-index nil))
(t
 (setf relation-name temp-relation-name)))
  ;;
  ;;  Determine if there is an index on the current relation with the same name as that requested. If not, it is an error.
  ;;
  (setf index-info (car (funcall (find-symbol (concatenate 'string "RETRIEVE-"
       *system-relation-base-implementation*
       "-" *system-relation-storage-structure*)
        *pkg-string*)
  'system-index *system-index-attributes* '("INDEX-TYPE" "PRIORITY" "DOC" "KEY")
  *system-index-key*
  `(and (string-equal index-name ,index-name)
        (string-equal relation-name ,(string-upcase relation-name)))
  nil 'system-index)))
  (cond ((null index-info)
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - An index with the name of ~s has not been defined on the relation ~s"
      index-name relation-name))
 (return-from modify-index nil)))
  ;;
  ;;  Determine if there is an index defined on this relation with the new-index-name already. If so it is an error
  ;;
  (cond ((null new-index-name)
 (setf new-index-name index-name))
(t
 (cond ((caar (funcall (find-symbol (concatenate 'string "RETRIEVE-"
     *system-relation-base-implementation* "-"
     *system-relation-storage-structure*) *pkg-string*)
       'system-index *system-index-attributes* '("INDEX-NAME") *system-index-key*
       `(and (string-equal index-name ,(string-upcase new-index-name))
      (string-equal relation-name ,(string-upcase relation-name)))
       nil 'system-index))
(if *provide-error-messages*
    (format *standard-output*
    "~%ERROR - An index with the name of ~s has already been defined on the relation ~s"
    new-index-name relation-name))
(return-from modify-index nil)))))
  ;;
  ;;  Determine if the requested storage structure is defined in the current database
  ;;
  (cond ((null index-type)
 (setf index-type (first index-info)))
(t
 (setf index-type (string-upcase index-type))
 (cond ((not (car (qtrieve 'system-storage-structure *system-storage-structure-attributes*
    '("STORAGE-STRUCTURE-NAME") *system-storage-structure-key*
    `(string-equal storage-structure-name ,index-type))))
(if *provide-error-messages*
    (format *standard-output* "~%ERROR - ~s is an undefined storage structure in the ~s database"
    index-type *active-db*))
(return-from modify-index nil)))))
  ;;
  ;;  Determine if the priority specified is within acceptable limits
  ;;
  (cond ((null index-priority)
 (setf index-priority (second index-info)))
((< index-priority 1)
 (cond (*provide-error-messages*
(format *standard-output* "~%ERROR - ~s is an illegal value for the value of priority." index-priority)
(format *standard-output* "~%        Priority must be a positive number.")))
 (return-from modify-index nil)))
  (setf index-doc (or index-doc (third index-info)))
  (setf index-key (or (convert-attributes index-key) (fourth index-info)))
;
;  Everything seems to be in order proceed
;
  (if *provide-status-messages*
      (format *standard-output* "~%Modify index ~s on relation ~s in database ~s" (read-from-string index-name)
      (read-from-string (string-upcase relation-name)) (read-from-string *active-db*)))
  ;;
  ;;  Must determine if the index relation needs to be recreated. This is the case if either the key or the
  ;; storage structure has been modified.
  ;;
  (cond ((not (and (equal (fourth index-info) index-key) (string-equal (first index-info) index-type)))
 ;;
 ;;  Create the structure of the type of the storage structure of the index and insert the tuples into the new index.
 ;; The DEFREL-sto function will validate the attributes and the key and define the appropriate structure.
 ;;
 (cond ((null (errset
(setf index-key (funcall (find-symbol (concatenate 'string "DEFREL-" index-type)
          *pkg-string*)
        index-name relation-attributes (list 'key index-key))) nil))
(if *provide-error-messages*
     (format *standard-output* "~%ERROR - ~s is an undefined storage structure" index-key))
(return-from modify-index nil)))
 (setf tuples (funcall (find-symbol (concatenate 'string "OBTAIN-TUPLES-" relation-storage-structure)
      *pkg-string*)
       relation-name))
 (funcall (find-symbol (concatenate 'string "INDEX-INSERT-" relation-implementation "-" index-type)
       *pkg-string*)
  index-name tuples relation-attributes index-key relation-name)))
  ;;
  ;;  The index has been modified now modify the system-index relation to reflect the change.
  ;;
  (delete-or-modify 'system-index t
    `(and (string-equal relation-name ,relation-name) (string-equal index-name ,index-name))
     '("INDEX-NAME" "INDEX-TYPE" "KEY" "PRIORITY" "DOC")
     (list (string-upcase new-index-name) (string-upcase index-type) `(quote ,index-key)
       index-priority index-doc))
  (if *provide-status-messages*
      (format *standard-output* "~%Index ~s has been modified on relation ~s in database ~s"
      (read-from-string index-name) (read-from-string (string-upcase relation-name))
      (read-from-string *active-db*)))
  (return-from modify-index new-index-name)))

(defun destroy-index (relation-name index-name
      &aux status?)
  "Destroy the specified index which is defined on the specified relation.

   RELATION-NAME - The name of the relation upon which the relation is defined.
   INDEX-NAME - The name of the index to be deleted."
  (block destroy-index
(cond (*parameter-checking*
       (if (not (active-database index-name))
   (return-from destroy-index nil))))
(cond ((not (setf relation-name (validate-sym relation-name)))
       (return-from destroy-index nil)))
(cond ((not (setf index-name (validate-sym index-name)))
       (return-from destroy-index nil)))
(cond ((not (member  (list (string-upcase relation-name)) (qtrieve 'system-relation
         *system-relation-attributes*
         '(relation-name)
         *system-relation-key* t)
     :test 'equal))
      (if *provide-error-messages*
  (format *standard-output* "~%ERROR - The relation ~s does not exist in the database ~s."
  relation-name *active-db*))
      (return-from destroy-index nil)))
(cond ((not (member (list (string-upcase index-name))
    (qtrieve 'system-index  *system-index-attributes* '(index-name) *system-index-key*
      `(string-equal relation-name ,(string-upcase relation-name)))
    :test 'equal))
      (if *provide-error-messages*
  (format *standard-output* "~%ERROR - The index ~s is not defined on relation ~s in database ~s"
  index-name relation-name *active-db*))
      (return-from destroy-index nil)))
(setf status? *provide-status-messages*
      *provide-status-messages* nil)
(delete-tuples 'system-index
       'where `(and (string-equal relation-name ,(string-upcase relation-name))
     (string-equal index-name ',(string-upcase index-name))))
(setf *provide-status-messages* status?)
(putp index-name nil 'entry-point)
(if *provide-status-messages*
    (format *standard-output* "~%Destruction of index ~s on relation ~s completed."
    index-name relation-name))
(return-from destroy-index index-name)))

;; fragments


   TRANSACTION - Name of the transaction to be commited.
   DIRECTORY   - Name of the directory in which this transaction can be found, if not in memory.
PATHNAME    - Name of the file in which it can be found.

    RELATION-NAME - Name of the relation on which the index will be defined.
    NAME - Name of the index to be defined
    KEY - List of attributes names which form the key of the index.
    STORAGE-STRUCTURE - The name of a RTMS defined storage structure upon which will be used as the index structure.
    PRIORITY - A numerical value which determines the order in which RTMS will search multiple indices of a relation
               for a possible key. The number one receives the highest consideration, if it fails the next index in
value is attempted.

    DOCUMENTATION - A string describing this index. *PROVIDE-STATUS-MESSAGES* *SYSTEM-STORAGE-STRUCTURE-KEY*
    RELATION-NAME - Name of the relation on which the index to be modified is defined
    INDEX-NAME - Name of the index to be modified
    NEW-NAME - New name for the specified index
    KEY - List of attributes names which form the key of the index.
    STORAGE-STRUCTURE - The name of a RTMS defined storage structure upon which will be used as the index structure.
    PRIORITY - A numerical value which determines the order in which RTMS will search multiple indices of a relation
               for a possible key. The number one receives the highest consideration, if it fails the next index in
               value is attempted.
    DOCUMENTATION - A string describing this index.
