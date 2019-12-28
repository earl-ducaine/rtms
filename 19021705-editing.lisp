
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
BÄBÄ`BÄ7BÄ	BÄ
BÄ:\Ä
BÄ3BÄ7BÄ	BÄ9BÄBÄUBÄBÄ3BÄ:BÄ:\ÄBÄ \ÄBÄéBÄãBÄëBÄ¨†Execute the database calls in a transaction.

   TRANSACTION - Name of the transaction to be commited.
   DIRECTORY   - Name of the directory in which this transaction can be found, if not in memory.
   PATHNAME    - Name of the file in which it can be found.ÄÄBÄ]—BÄí—BÄAë\ÄBÄOBÄ¿BÄ“BÄc“BÄÈ“BÄ“\ÄBÄ9BÄ¿BÄj“BÄ¿BÄó“\ÄBÄ¿BÄC“BÄÓ¿\ÄBÄ9¿BÄ*“BÄ“BÄb“√ÇRTMS-READ-INSERT-FILEÄ“BÄ“BÄ+“¨ê~%ERROR - The transaction file ~S does not exist; ~@
                              ~7T the transaction ~S has not been defined yet.Ä¿BÄæ“BÄ°“ÏÉERROR - The transaction file Ä¿BÄ£“BÄ€“,Ç does not exist.¿BÄ¿BÄ¿BÄ@í@‰@QPˇ›A—†ÄÊRÄQˇ›	íÄ¡ÊR@Q¸GSG¡‰LMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540731. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "XLD" :NAME "INDEX" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760360074. :AUTHOR "REL3" :LENGTH-IN-BYTES 6626. :LENGTH-IN-BLOCKS 13. :BYTE-SIZE 16.) pp2Ä\Ä©ÅCOMPILE-DATA\ÄÏÄSW-MFG,ÅGODZILLAÜÄá§öŒFÄFÄ\Äp¿,ÅCOMPILER,ÅVERSIONÄ\ÄFÄFÄp¿BÄ),ÇOPTIMIZE-SWITCHÄÜÄ©ÉQFASL-SOURCE-FILE-UNIQUE-IDÄ1Ä\Äp¿lÄFSÏÇMAKE-FASLOAD-PATHNAMEÄ\ÄÍÄQUOTEÄBÄ$\ÄBÄ8™ÄNILÄ\ÄBÄ8\Ä¨ÄRTMS\ÄBÄ8ÏÄINDEXÄ\ÄBÄ8¨ÄLISP\ÄBÄ8FÄ©ÄBASEFÄ
ÈÄFONTSÄ\Ä©Å*CODE-FONT*ÄÈÅ*COMMENT-FONT*ÈÅ*STRING-FONT*Ä)ÅPACKAGEÄ©ÄRTMS©ÄMODE©ÅCOMMON-LISPÄÄÉÅDEFINE-INDEXÄÎÄ8v$ÜÄ‡8@FÄÆ¿$Ä¿BÄ:p¿¨ÄTICLÏÄART-QÄ]ÄFÄÄ:BÄ:BÄ:jÄTÄFÄp¿¨ÄSYSÄlÇDEBUG-INFO-STRUCTÄBÄP\Ä√ÅRELATION-NAMEÄÍÄ&RESTÄÉÅKEYWORD-LIST™Ä&KEYjÅ&OPTIONALÄÉÄNAMEÍÅDOCUMENTATIONÄCÇSTORAGE-STRUCTUREÄÉÄKEYÄÅPRIORITYjÇ&ALLOW-OTHER-KEYSÄBÄ:\ÄBÄbBÄeBÄfBÄgBÄhBÄiCÅINDEX-DOCÄCÅINDEX-KEYÄCÅINDEX-NAME√ÅINDEX-PRIORITYCÅINDEX-TYPEÉÇRELATION-ATTRIBUTESÄÉRELATION-IMPLEMENTATIONÄCÉRELATION-STORAGE-STRUCTURECÇTEMP-RELATION-NAMEBÄb\Ä)ÇMACROS-EXPANDEDÄ\Äp¿BÄ\lÅXR-BQ-LISTÍÄFIRSTÄÍÄFIFTHÄÍÄFOURTHÍÄTHIRDÄÍÄSECOND™ÄPROG™ÄSETFÈÅDOCUMENTATIONÄÏøTDefine an index on a relation in the active database.

    RELATION-NAME - Name of the relation on which the index will be defined.
    NAME - Name of the index to be defined
    KEY - List of attributes names which form the key of the index.
    STORAGE-STRUCTURE - The name of a RTMS defined storage structure upon which will be used as the index structure.
    PRIORITY - A numerical value which determines the order in which RTMS will search multiple indices of a relation
               for a possible key. The number one receives the highest consideration, if it fails the next index in
               value is attempted.
    DOCUMENTATION - A string describing this index.Ä¿ÜÄê ÄCÉ*PROVIDE-STATUS-MESSAGES*Ä—√É*SYSTEM-STORAGE-STRUCTURE-KEY*—√Ñ*SYSTEM-STORAGE-STRUCTURE-ATTRIBUTES*Ä—CÇ*SYSTEM-INDEX-KEY*—CÉ*SYSTEM-INDEX-ATTRIBUTES*Ä—ÉÅ*ACTIVE-DB*Ä—É*PROVIDE-ERROR-MESSAGES*—ÉÇ*PARAMETER-CHECKING*ë\Ä©ÄNAMEBÄÅiÇSTORAGE-STRUCTUREÄ©ÄKEYÄ)ÅPRIORITY¿p¿BÄ\ÏÅSTORE-KEYARGSÄ“ÇACTIVE-DATABASEÄ“ÉÅVALIDATE-SYM“\ÄBÄeÉÄSTOÄBÄhÉÄDOCÄBÄi¿ÉGET-KEYWORD-VALUE-PREREQ“CÇGET-KEYWORD-VALUEÄ“CÇCONVERT-ATTRIBUTES“\ÄCÅATTRIBUTESÉÇIMPLEMENTATION-TYPEÄBÄg¿ÉÅGET-RELATION“ÍÄTERPRI“ÏÇERROR - The relation Ä¿™ÅWRITE-STRING“ÍÄPRIN1Ä“,É does not exist in the Ä¿lÅ databaseÄ¿ÍÄGENSYM“ÍÄSTRING“ÍÅSTRING-UPCASEÄ“ÉÅSYSTEM-INDEX¿\ÄÏÅRELATION-NAMEÄlÅINDEX-NAME¿™ÅSTRING-EQUAL¿BÄn¿™ÄLIST“ÅQTRIEVEÄ“lÑERROR - An index with the name of ¿lÖ has already been defined on the relation ¿¨ÄAVLÄ¿ÉSYSTEM-STORAGE-STRUCTURE¿\ÄÏÇSTORAGE-STRUCTURE-NAME¿√ÇSTORAGE-STRUCTURE-NAME¿,ÅERROR - ¿lÖ is an undefined storage structure in the ¿,Ü is an illegal value for the value of priority.Ä¿¨Ö        Priority must be a positive number.Ä¿ÏÅDefine index Ä¿*ÇREAD-FROM-STRING“ÏÅ on relation Ä¿ÏÅ in database Ä¿√ÇCREATE-INDEX-RELATIONÄ“√ÄTUPLES¿√ÄINSERT“ÏÄIndex ¿ÏÉ has been defined on relation Ä@‰@QPˇ›A—†‰ÄQàÊRÄQäÄ¡ÊR@Q¸OSO¡‰OSˇ5˙ÁOQ@¡P@Qí@¡P@QíH¡HWJ¡H[äG¡HQBF¡HUBI¡HSH¡ÄQPˇ€öN√	BK¡NQBL¡NUBM¡NSN¡KÊ
‰ÄPàÄQàPà	PàPàRNQÄ¡HÊÇäH¡¸HQäH¡PP PP!P"PHQä#ö$™BN¡‰
	‰Ä%PàNWà&PàNSàRJÊ'PJ¡¸(PP)PP!P*PJQä#ö$™Ê
‰Ä+PàJQà,Pà	PàPàRIÊJI¡¸IQ‰

‰Ä+PàIQà-PàÄ.PàRGÊKS#äG¡‰Ä/PàHQ0äà1PàÄQä0äà2Pà	P0äàÄQHQKQGQJQLQMQJ3∏ÊRP4PÄQäHQJQäGQIQFQ#≤#ä5ò‰Ä6PàHQ0äà7PàÄQä0äà2Pà	P0äàHOÄ√BÄPÄÄBÄæÄÎÄ*nÜÄA–FÄD¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄæ\ÄBÄ`BÄnBÄqBÄmBÄpBÄrBÄsBÄ:\ÄBÄøBÄ:BÄ:BÄ:\ÄBÄv\ÄÄÄp¿BÄTlÇCONDITION-BIND-IFÄp¿BÄTÏÅCONDITION-BINDp¿BÄTÏÇCATCH-CONTINUATION-IFÄp¿BÄTlÇCATCH-CONTINUATIONp¿BÄTÏÄERRSETÄp¿lÄEH¨Ç*CONDITION-HANDLERS*—BÄä—ÉÅ*PKG-STRING*ëp¿,ÄÏÄG3216Ä¿FÄ\¿ÍÄERRORÄ¿p¿BÄ\ÏÅERRSET-HANDLER¿BÄ¶¿,ÅDEFREL-Ä¿BÄß“™ÅCONCATENATEÄ“™ÅFIND-SYMBOLÄ“BÄh¿BÄö“BÄ≠“BÄü“,ÅERROR - ¿BÄ°“BÄ¢“lÑ is an undefined storage structure¿ÏÅOBTAIN-TUPLES-¿ÏÅINDEX-INSERT-Ä¿lÄ-ÄÄPPTP	PPˇ€JCA√PJCB√÷
PPÑQäöPíC¡ÅQÇQPÉQäíCõÉ√äJ!BJ!Bˇ\¸\ˇÊ‰ÄPàÉQàPàÅ€Å
PPÜQäöPíC¡ÄQCã@¡
PPÖQäPÑQä™PíC¡ÅQ@QÇQÉQÄQC©ÅOÄÏBÄæÄÄCÇOBTAIN-TUPLES-HEAPÄÎÄ	FÄ@FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÌ\ÄBÄ`BÄ:BÄ:BÄ:ÄÉÅENTRY-POINTÄ¿ÉÄGETPíÄQPîOÄ¯BÄÌÄÄCÇOBTAIN-TUPLES-HASHÄÎÄÜÄ@\FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ˘\ÄBÄ`BÄ:\ÄBÄ:BÄ:p¿BÄ\lÇLEX-PARENT-ENV-REGp¿BÄ\ÏÅLEX-ENV-B-REGÄp¿BÄ\ÏÇLEX-CURRENT-VECTOR-REGp¿BÄ\¨ÇLEX-ALL-VECTORS-REGÄCÅTUPLE-LIST\Ä©ÇINTERNAL-FEF-OFFSETS\ÄFÄiÑVARIABLES-USED-IN-LEXICAL-CLOSURES\ÄBÄÄ\ÄFÄFÄ¿\Ä)ÅINTERNALBÄ˘Ä¿BÄˆ¿BÄ˜“*ÅMAPHASHÄíPP”CÄQPíêFOÄBÄ˘ÄÄBÄÄÎÄ
ÜÄ@åFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ\ÄÅKEY-VALÄBÄøBÄ:\ÄBÄ:BÄ:BÄ\ÄBÄv\ÄBÄÄiÉLEXICAL-PARENT-DEBUG-INFOÄBÄˇÄp¿BÄ\,Å*APPENDÄíÅQ¿Pí¿¬ˇOÄ)BÄÄÄCÇOBTAIN-TUPLES-AVLÄÄÎÄ
FÄ@FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ*\ÄBÄ`BÄ:BÄ:BÄ:ÄBÄˆ¿BÄ˜“√ÇAVL-INORDER-TRAVERSALÄíÄQPíåOÄ4BÄ*ÄÄÉINDEX-INSERT-FLAVOR-HASHÄÎÄ0ÜÄA`FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ5\ÄBÄnBÄøBÄúÅKEY-LISTBÄ`BÄ:\ÄBÄh√ÅHASH-RELATIONÄBÄ:BÄ:BÄ:BÄ:√ÄTUPLEÄ√ÄKEYVAL\ÄBÄv\ÄBÄp¿¨ÄZLCÄ,ÅDO-NAMEDp¿BÄTÏÇINHIBIT-STYLE-WARNINGSBÄÄÄ√ÅPROJECT-FLAVOR“BÄˆ¿BÄ˜“*ÅGETHASHÄ“p¿BÄT,ÅPUTHASHÄíÅQÇQÉQÑQ¢@¡ÄQPíA¡B—ÅQ@QE¡D¡C¡¸CQDSESG¡F¡GQFQGQAQí
CAQöCC√¡D≈E≈D‰EÍÁBOÄNBÄ5ÄÄÉINDEX-INSERT-STRUCT-HASHÄÎÄ0ÜÄA`FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄO\ÄBÄnBÄøBÄúBÄ>BÄ`BÄ:\ÄBÄhBÄ@BÄ:BÄ:BÄ:BÄ:BÄABÄB\ÄBÄv\ÄBÄBÄGBÄIBÄÄÄ√ÅPROJECT-STRUCT“BÄˆ¿BÄ˜“BÄK“BÄMíÅQÇQÉQÑQ¢@¡ÄQPíA¡B—ÅQ@QE¡D¡C¡¸CQDSESG¡F¡GQFQGQAQí
CAQöCC√¡D≈E≈D‰EÍÁBOÄ\BÄOÄÄÉINDEX-INSERT-FLAVOR-HEAPÄÎÄÜÄ@FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ]\ÄBÄnBÄøBÄúBÄ>BÄ`BÄ:BÄ:BÄ:ÄBÄˆ¿BÄ˜“p¿BÄ\ÏÄ*NCONC“ÉÄPUTPíÄQÄQPíÅQíPúOÄiBÄ]ÄÄÉINDEX-INSERT-STRUCT-HEAPÄÎÄÜÄ@FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄj\ÄBÄnBÄøBÄúBÄ>BÄ`BÄ:BÄ:BÄ:ÄBÄˆ¿BÄ˜“BÄg“BÄhíÄQÄQPíÅQíPúOÄsBÄjÄÄÉINDEX-INSERT-FLAVOR-AVLÄÄÎÄQÜÄA\FÄ2¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄt\ÄBÄnBÄøBÄúBÄ>BÄ`BÄ:\ÄÉÅDOMAIN-LISTÄÇDOMAIN-KEY-LISTÄBÄhÉÅNEW-ELEMENTÄÉÄTREE√ÄTUPLE%ÉÄKEY%\ÄBÄv\ÄBÄBÄÄÄ√Ç*SYSTEM-RELATION-KEY*Ä—ÉÉ*SYSTEM-RELATION-ATTRIBUTES*ëÇSYSTEM-RELATIONÄ¿\ÄÅDOMAINSÄ¿BÄ¨¿BÄ`¿BÄß“BÄ≠“BÄÆ“ÉÅPROJECT-LIST“BÄJ“BÄˆ¿BÄ˜“ÍÄAPPEND“CÇINSERT-AVL-FLAVORÄ“BÄhíPPPPPPÑQ	ä
ö™B@√
äÇQÉQöBA¡ÅQÇQÉQöB¡ÄQPíD¡ÅQBQF¡E¡‰ES
äJ
äˇ€
äˇ€
äö
CC√DQFSÉQÇQAQˇ€ÄQJ∫D¡E≈F≈EÁÁÄQDQPúOÄéBÄtÄÄÉINDEX-INSERT-STRUCT-AVLÄÄÎÄQÜÄA\FÄ2¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄè\ÄBÄnBÄøBÄúBÄ>BÄ`BÄ:\ÄBÄ~BÄBÄhBÄÄBÄÅBÄÇBÄÉ\ÄBÄv\ÄBÄBÄÄÄBÄÜ—BÄáëBÄà¿\ÄBÄä¿BÄ¨¿BÄ`¿BÄß“BÄ≠“BÄÆ“BÄã“BÄ[“BÄˆ¿BÄ˜“BÄå“CÇINSERT-AVL-STRUCTÄ“BÄhíPPPPPPÑQ	ä
ö™B@√
äÇQÉQöBA¡ÅQÇQÉQÑQ¢B¡ÄQPíD¡ÅQBQF¡E¡‰ES
äJ
äˇ€
äˇ€
äö
CC√DQFSÉQÇQAQˇ€ÑQJ∫D¡E≈F≈EÁÁÄQDQPúOÄùBÄèÄÄ√ÇINDEX-INSERT-LIST-HEAPÄÎÄÜÄ@FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄû\ÄBÄnBÄøBÄúBÄ>BÄ`BÄ:BÄ:BÄ:ÄBÄˆ¿BÄ˜“BÄg“BÄhíÄQÄQPíÅQíPúOÄßBÄûÄÄ√ÇINDEX-INSERT-LIST-HASHÄÎÄ0ÜÄA`FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ®\ÄBÄnBÄøBÄúBÄ>BÄ`BÄ:\ÄBÄ@BÄhBÄ:BÄ:BÄ:BÄ:BÄABÄB\ÄBÄv\ÄBÄBÄGBÄIBÄÄÄBÄˆ¿BÄ˜“BÄã“BÄK“BÄMíÄQPí@¡ÅQÇQÉQöA¡B—ÅQAQE¡D¡C¡¸CQDSESG¡F¡GQFQGQ@Qí
C@QöCC√¡D≈E≈D‰EÍÁBOÄ¥BÄ®ÄÄ√ÇINDEX-INSERT-LIST-AVLÄÄÎÄQÜÄA\FÄ2¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄµ\ÄBÄnBÄøBÄúBÄ>BÄ`BÄ:\ÄBÄ~BÄBÄhBÄÄBÄÅBÄÇBÄÉ\ÄBÄv\ÄBÄBÄÄÄBÄÜ—BÄáëBÄà¿\ÄBÄä¿BÄ¨¿BÄ`¿BÄ¶“BÄß“BÄ≠“BÄÆ“BÄã“BÄˆ¿BÄ˜“BÄå“ÇINSERT-AVL-LISTÄ“BÄhíPPPPPPÑQ	ä
äö™B@√äÇQÉQöBA¡ÅQÇQÉQöB¡ÄQPíD¡ÅQBQF¡E¡‰ESäJäˇ€äˇ€äö
CC√DQFSÉQÇQAQˇ€ÄQJ∫D¡E≈F≈EÁÁÄQDQPúOÄ√BÄµÄÄÉÅEXTRACT-KEYÄÄÎÄG®ÜÄA‡FÄa¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄƒ\ÄBÄ`BÄúBÄhBÄäBÄsÉÅWHERE-CLAUSEBÄnBÄ:\ÄBÄmCÅINDEX-LISTBÄpCÅKEY-VALUEÄÉÇSTRING-RELATION-NAME™ÅPACKAGE-NAMEBÄ:BÄœ\ÄBÄv\ÄÄzBÄ}BÄ~BÄBÄyBÄÄÄBÄá—BÄà—CÇ*SYSTEM-RELATIONS*—BÄ‹ëBÄ¶“BÄ““BÄ¶¿¨ÅEXTRACT-KEY-¿BÄÂ“BÄÊ“BÄ¨¿p¿BÄ\¨ÅMEMBER-TESTÄ“BÄ®¿BÄ`¿BÄ≠“BÄÆ“\ÄBÄi¿CÇPROCESS-QUICK-SORT“\ÄBÄnBÄpBÄh¿BÄã“™ÄANDÄ¿BÄn¿BÄßíÄQäD¡ÄQùC‚PäE¡ÜÊ	P
PÑQöPíF¡ÅQÇQÉQÖQEQF´C¡ÑQB¡ÇQ@¡CeÊDQPPò`ÊÜÊÄQÜ¡PPPPPPDQö™PPöPPöA¡¸PPPPPPPDQöPPÜQäöö™A¡1‰Ü7‰ÜQäPíÄ¡A(‰AQG¡¸	P
PGQ
BöPíF¡ÅQGQBÉQÖQEQF´C¡GQBäPíÜ¡GQ
BB¡GQB@¡G≈‰C‡ÂÜÊCÊ¸C€ÄQÜ¡ÑQB¡ÇQ@¡¸ÜÊÄQÜ¡ÜQCQBQ@QÑOÄ›BÄƒÄÄÉÅMODIFY-INDEXÄÎÄL√“ÜÄ‡LÄFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄﬁ\ÄBÄ`BÄnBÄaBÄbBÄcBÄdÅNEW-NAMEBÄfBÄgBÄhBÄiBÄjBÄ:\ÄBÄbBÄÁBÄfBÄgBÄhBÄiBÄlCÅINDEX-INFOBÄmBÄoBÄp√ÅNEW-INDEX-NAMEBÄqBÄrÉÅRELATION-KEYBÄsBÄtBÄøBÄbBÄ:BÄ:BÄ:BÄ:\ÄBÄv\ÄBÄ–BÄ“BÄ‘BÄ÷BÄÿBÄyBÄzBÄ{BÄ|BÄ}BÄ~BÄBÄÄBÄÅÏøtModify an index on a relation in the active database.

    RELATION-NAME - Name of the relation on which the index to be modified is defined
    INDEX-NAME - Name of the index to be modified
    NEW-NAME - New name for the specified index
    KEY - List of attributes names which form the key of the index.
    STORAGE-STRUCTURE - The name of a RTMS defined storage structure upon which will be used as the index structure.
    PRIORITY - A numerical value which determines the order in which RTMS will search multiple indices of a relation
               for a possible key. The number one receives the highest consideration, if it fails the next index in
               value is attempted.
    DOCUMENTATION - A string describing this index.Ä¿ÜÄAÄBÄ€—BÄÑ—BÄÖ—BÄÜ—BÄá—BÄà—BÄ‹—ÉÑ*SYSTEM-RELATION-STORAGE-STRUCTURE*Ä—√Ñ*SYSTEM-RELATION-BASE-IMPLEMENTATION*Ä—BÄ —BÄä—BÄãë\Ä)ÅNEW-NAMEBÄÅBÄéBÄèBÄê¿BÄí“BÄì“BÄî“BÄß“\ÄÉÄNEWÄBÄñBÄhBÄóBÄi¿BÄò“BÄô“\ÄBÄúBÄùBÄgBÄh¿BÄû“BÄü“ÏÇERROR - The relation Ä¿BÄ°“BÄ¢“,É does not exist in the Ä¿lÅ databaseÄ¿BÄ¶¿lÅRETRIEVE-Ä¿lÄ-Ä¿BÄÂ“BÄÊ“BÄ®¿\ÄlÅINDEX-TYPE,ÅPRIORITY¨ÄDOCÄ¨ÄKEYÄ¿BÄ‹¿BÄ¨¿BÄn¿BÄ≠“BÄ`¿lÑERROR - An index with the name of ¿ÏÑ has not been defined on the relation ¿\ÄlÅINDEX-NAME¿lÖ has already been defined on the relation ¿BÄ≤¿\ÄÏÇSTORAGE-STRUCTURE-NAME¿BÄµ¿BÄÆ“,ÅERROR - ¿lÖ is an undefined storage structure in the ¿,Ü is an illegal value for the value of priority.Ä¿¨Ö        Priority must be a positive number.Ä¿BÄö“ÏÅModify index Ä¿BÄª“ÏÅ on relation Ä¿ÏÅ in database Ä¿p¿BÄ\ÏÅSTRING-EQUAL*Ä“p¿BÄ›ÏÄG3393Ä¿FÄÀ¿BÄ·¿BÄ„¿,ÅDEFREL-Ä¿BÄh¿lÑ is an undefined storage structure¿ÏÅOBTAIN-TUPLES-¿ÏÅINDEX-INSERT-Ä¿\ÄlÅINDEX-NAMElÅINDEX-TYPE¨ÄKEYÄ,ÅPRIORITY¨ÄDOCÄ¿BÄ8¿ÇDELETE-OR-MODIFY“ÏÄIndex ¿,Ñ has been modified on relation ÄÄ@‰@QPˇ›A—†‰ÄQàÊRÄQäÄ¡ÊR@Q¸RSR¡‰RSˇ5˙ÁRQ@¡ÅQäÅ¡P@Qí@¡P@QíK¡KWJ¡K[H¡KQBF¡KUBI¡KSK¡ÄQPˇ€öP√	BL¡PQBM¡PUBO¡PWBN¡PSP¡LÊ‰ÄPàÄQàPàPàPàRPQÄ¡ P!PP"PP#™
P$íS¡%P	P&PP'P(P)PÅQ*ö(P+PÄQä*ö*öˇ€%PJSªBG¡Ê	‰Ä,PàÅQà-PàÄQàRKÊÅQK¡+¸ P!PP"PP#™
P$íS¡%P	P.PP'P(P)PKQä*ö(P+PÄQä*ö*öˇ€%PJSª‰	‰Ä,PàKQà/PàÄQàRJÊGSJ¡¸JQäJ¡0PP1PP(P2PJQ*ö3™Ê‰Ä4PàJQà5PàPàPàRIÊGWI¡¸IQ‰
‰Ä4PàIQà6PàÄ7PàRFQ‚G[F¡HQ8ä‚GQBH¡‰Ä9PàÅQ:äà;PàÄQä:äà<PàP:äàGQBH+‰GSJQ=êLÊ>P?PT@PAP>Pˇ€JCT√PJCU√÷ PBPJQ#ö
P$íV¡ÅQLQCPHQ*íVõH√*äJ!BJ!Bˇ\¸\ˇ
Ê‰Ä4PàHQàDPàR PEPOQ#ö
P$íV¡ÄQVãQ¡ PFPMQ"PJQ#™
P$íV¡ÅQQQLQHQÄQV©%Pˇ›'P(P+PÄQ*ö(P)PÅQ*ö*öGPKQäJQäHPHQ*íIQFQ*™I®‰ÄJPàÅQ:äàKPàÄQä:äà<PàP:äàKOÄ!BÄﬁÄÄ√ÅDESTROY-INDEXÄÄÎÄ+8õÜÄ@+ÑFÄc¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ"\ÄBÄ`BÄnBÄ:\ÄÅSTATUS?Ä\ÄBÄv\ÄBÄyBÄÄBÄÅÏôDestroy the specified index which is defined on the specified relation.

   RELATION-NAME - The name of the relation upon which the relation is defined.
   INDEX-NAME - The name of the index to be deleted.ÄÄBÄÑ—BÄá—BÄà—BÄ —BÄä—BÄÜ—BÄá—BÄãëBÄì“BÄî“BÄß“BÄ≠“BÄà¿\ÄBÄ`¿BÄÆ“p¿BÄ\¨ÅMEMBER-EQUAL“BÄü“ÏÇERROR - The relation Ä¿BÄ°“BÄ¢“,Ñ does not exist in the database ¿eÄ.¿jÅWRITE-CHAR“BÄ®¿\ÄBÄn¿BÄ¨¿BÄ`¿lÇERROR - The index ¿¨É is not defined on relation ¿ÏÅ in database Ä¿√ÄWHEREÄ¿BÄ‹¿BÄn¿BÄ8¿√ÅDELETE-TUPLESÄ“BÄˆ¿BÄh“ÏÇDestruction of index Ä¿ÏÅ on relation Ä¿¨Å completed.ÄÄ
‰ÅQàÊRÄQäÄ¡ÊRÅQäÅ¡ÊRÄQääP	PPPˇ›™êÊ‰ÄPàÄQàPàPàPàRÅQääPPPPPPÄQäö™êÊ‰ÄPàÅQàPàÄQà PàPàRP@¡⁄P!P"PPPÄQäöP#P$PÅQäíöö%ò@Q¿ÅQˇ€&P'ò‰Ä(PàÅQà)PàÄQà*PàÅOÄ@BÄ"Ä1Ä\Äp¿BÄ\,ÑFASL-RECORD-FILE-MACROS-EXPANDED\ÄBÄ8\Ä\ÄÍÄDEFUNÄÜÄ'\ÄBÄIÜÄ(Ã¢\ÄBÄGÜÄ*˝j\ÄBÄÿÜÄ-i\ÄBÄ÷ÜÄ~…z\ÄBÄ‘ÜÄ<pë\ÄBÄ“ÜÄ`sN\ÄBÄ–ÜÄ|ƒÙ\ÄBÄÄÜÄ[ÊÑ\ÄBÄÜÄ=Ã#\ÄBÄ~ÜÄ{öÕ\ÄBÄ}ÜÄ:}n\ÄBÄ|ÜÄxıø\ÄBÄ{ÜÄZiÛ\ÄBÄzÜÄz(á\ÄBÄyÜÄ.ŸãÄÄ (package-name (or (symbol-package relation-name) *pkg-strinLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540734. :SYSTEM-TYPE :LOGICAL :VERSION 2. :TYPE "LISP" :NAME "INSERT" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :CHARACTERS T :NOT-BACKED-UP T :CREATION-DATE 2755196698. :AUTHOR "REL3" :LENGTH-IN-BYTES 19826. :LENGTH-IN-BLOCKS 20. :BYTE-SIZE 8.)

;;; -*- Mode:Common-Lisp; Package:RTMS; Fonts:(*CODE-FONT* *COMMENT-FONT* *STRING-FONT*); Base:10 -*-
;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved
;;; INSERT
;;;
;;; This file contains the following Explorer extensions to CommonLisp Standard as Indicated in the June 1985 Explorer Lisp
;;; Reference
;;;     firstn
;;;     deff
;;;     :string-in
;;;
;;; The following function contains flavor references and thus are incompatable with CommonLisp. Their removal will not
;;; effect the functionality of RTMS.
;;;     insert-flavor-hash
;;;     insert-flavor-heap
;;;

;;; Change History --
;;; 04.20.87 MRR  Removed &optional from INSERT lambda list.


(defun rtms-read-insert-file (pathname
      &aux (beg-index 0) end-index index value (extend-size 4096) (total-length 0))
  (setf value (make-array extend-size)
end-index extend-size)
  (with-open-file (stream1  pathname)
    (do ((eof-flag nil))
(eof-flag t)
      (multiple-value-setq (index eof-flag)
(funcall stream1 ':string-in nil value beg-index end-index))
      (setf total-length (+ total-length index))
      (cond ((not eof-flag)
     (setf value (adjust-array value (+ total-length extend-size)))
     (setf beg-index end-index
           end-index (+ total-length extend-size))))))
  (read-from-string value nil nil :start 0 :end total-length))

;************************************************************************
;           For the sake of old references to this function             *
;************************************************************************
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
  ;;
  ;;  INSERT has been called by one of the restore operations (LOAD-RELATION) and in reference to one of the system-relations
  ;; insert the tuples without further processing and return.
  ;;
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
  ;;
  ;;  If the data is stored in a file, read it into the TUPLE.
  ;;
  (cond (path
 (if (probe-file path)
     (setf tuple (rtms-read-insert-file path))
     (if *provide-error-messages*
 (format *standard-output* "~%ERROR - File ~S does not exist." path)))))
  (cond ((null tuple)
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - No tuples provided."))
 (return-from insert nil)))
  ;;
  ;;  Obtain information about the attributes of the insert relation
  ;;
  (if (not (or (member (string relation-name) *system-relations* :test 'string-equal) *restore-operation*
       (not *validity-checking*)))
      (setf qtrieve-var
    (funcall (find-symbol (concatenate 'string "RETRIEVE-" *system-relation-base-implementation* "-"
         *system-relation-storage-structure*) *pkg-string*)
     'system-attribute *system-attribute-attributes*
     '("ATTRIBUTE-NAME" "DOMAIN-FUNCTION" "DEFAULT-VALUE") *system-attribute-key*
     (list 'string-equal 'relation-name  (string relation-name)) nil 'system-attribute)))
  ;;
  ;; Check for various possibilities of INSERT format. First see if tuple is a list of tuples. Store the attribute names
  ;;
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
E") *system-index-key*
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
 (LMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540738. :SYSTEM-TYPE :LOGICAL :VERSION 2. :TYPE "XLD" :NAME "INSERT" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760360272. :AUTHOR "REL3" :LENGTH-IN-BYTES 4353. :LENGTH-IN-BLOCKS 9. :BYTE-SIZE 16.) pp2Ä\Ä©ÅCOMPILE-DATA\ÄÏÄSW-MFG,ÅGODZILLAÜÄá§`œFÄFÄ\Äp¿,ÅCOMPILER,ÅVERSIONÄ\ÄFÄFÄp¿BÄ),ÇOPTIMIZE-SWITCHÄÜÄ©ÉQFASL-SOURCE-FILE-UNIQUE-IDÄ1Ä\Äp¿lÄFSÏÇMAKE-FASLOAD-PATHNAMEÄ\ÄÍÄQUOTEÄBÄ$\ÄBÄ8™ÄNILÄ\ÄBÄ8\Ä¨ÄRTMS\ÄBÄ8ÏÄINSERT\ÄBÄ8¨ÄLISP\ÄBÄ8FÄ©ÄBASEFÄ
ÈÄFONTSÄ\Ä©Å*CODE-FONT*ÄÈÅ*COMMENT-FONT*ÈÅ*STRING-FONT*Ä)ÅPACKAGEÄ©ÄRTMS©ÄMODE©ÅCOMMON-LISPÄÄ√ÇRTMS-READ-INSERT-FILEÄÄÎÄ"TÜÄ@`FÄ2¿$Ä¿BÄ:p¿¨ÄTICLÏÄART-QÄ]ÄFÄÄ:BÄ:BÄ:jÄTÄFÄp¿¨ÄSYSÄlÇDEBUG-INFO-STRUCTÄBÄP\Ä*ÅPATHNAMEBÄ:\ÄCÅBEG-INDEXÄCÅEND-INDEXÄ√ÄINDEXÄ√ÄVALUEÄÉÅTOTAL-LENGTHBÄ:p¿BÄ\¨Ç.FILE-ABORTED-FLAG.ÄÅEOF-FLAG\Ä)ÇMACROS-EXPANDEDÄ\Ä™ÄPROG*ÇWITH-OPEN-STREAMÍÅWITH-OPEN-FILE™ÄSETFÄFÄ¿p¿BÄ\lÇSIMPLE-MAKE-ARRAYÄ“ÈÄABORTÄ¿FÄM¿™ÄOPEN“iÅSTRING-INÄ¿FÄê¿™ÅADJUST-ARRAY“p¿BÄTÏÄERRORP“ÈÄCLOSEÄ¿ÈÄSTARTÄ¿©ÄENDÄ¿*ÇREAD-FROM-STRINGí@ﬂDﬂPJíC¡PA¡PF¡ˇ›PJUÄQäE¡¸Pˇ€CQ@QAQ	PEQAG¡B√DaD¡GÊCQDQ`
íC¡AQ@¡DQ`A¡GÊÂF€]RZ	¸E‰EQàÊPFQEëCQˇ€ˇ€PJPDQJºOÄÄBÄPÄÄÅINSERT1ÄÄÎÄÜÄ$ÄFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÅ\ÄÉÄRELÄ√ÄTUPLESjÅ&OPTIONALÄ√ÅATTRIBUTE-LISTBÄ:BÄ:BÄ:ÄBÄã¿ÉÄATTR¿™ÄLIST“√ÄINSERTíÄQPÅQPÇQ¢îOÄëBÄÅÄBÄêOÄê√ÅINSERT-TUPLESÄÄBÄêÄÎÄ`FÏÜÄ‡`@FÄ¶¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄê\Ä√ÅRELATION-NAMEÄÍÄ&RESTÄÉÅKEYWORD-LIST™Ä&KEYBÄãCÅATTRIBUTESBÄ`jÇ&ALLOW-OTHER-KEYSÄBÄ:\Ä!BÄùBÄãBÄüBÄ`√ÅATTR-VAL-LISTÄÅSUB-LIST√ÄTUPLEÄÉÅPROJECT-LISTÉÄPATHBÄ
ÉÄKEYÄCÄSSÉÄIMPÄÉÄCARDÉÅQTRIEVE-VARÄCÅMOD-TUPLESÉÅQTRIEVE-VAR1ÉÅQTRIEVE-VAR2ÅINDICESÄBÄ:BÄ:BÄ:BÄéCÅSUB-TUPLEÄCÅACTUAL-P-LBÄ:ÉÄTESTBÄ:BÄ:ÉÄFUNÄÉÄDOMÄBÄãÉÄKEY%\ÄBÄk\Ä™ÄPUSHBÄmp¿¨ÄZLCÄ,ÅDO-NAMEDp¿BÄTÏÇINHIBIT-STYLE-WARNINGSÍÄFIRSTÄÍÄFIFTHÄÍÄFOURTHÍÄTHIRDÄÍÄSECONDp¿BÄ\lÅXR-BQ-LISTBÄpÈÅDOCUMENTATIONÄ¨ΩInsert a list of tuples or data from a file.

   RELATION-NAME   - Name of the relation into which the data is to be inserted.
   TUPLES     - List of tuples to be inserted. Tuples are expected to be in the list-of-values format.
   ATTRIBUTES - If the values in the tuples do not correspond to the attribute-list specified during
                relation-defintion, specify a list of attributes to determine the order.
   PATHNAME   - If the data is in a file, specify the name of the file.Ä¿ÜÄ° ÄCÉ*PROVIDE-STATUS-MESSAGES*Ä—CÉ*PROVIDE-WARNING-MESSAGES*—√Ç*SYSTEM-ATTRIBUTE-KEY*—√É*SYSTEM-ATTRIBUTE-ATTRIBUTES*Ä—ÉÇ*VALIDITY-CHECKING*Ä—ÉÅ*ACTIVE-DB*Ä—CÇ*SYSTEM-INDEX-KEY*—CÉ*SYSTEM-INDEX-ATTRIBUTES*Ä—CÅ*PKG-NAME*—ÉÅ*PKG-STRING*—ÉÑ*SYSTEM-RELATION-STORAGE-STRUCTURE*Ä—√Ñ*SYSTEM-RELATION-BASE-IMPLEMENTATION*Ä—CÇ*SYSTEM-RELATIONS*—ÉÇ*RESTORE-OPERATION*Ä—É*PROVIDE-ERROR-MESSAGES*—ÉÇ*PARAMETER-CHECKING*ë\ÄÈÄTUPLESiÅATTRIBUTES)ÅPATHNAME¿p¿BÄ\ÏÅSTORE-KEYARGSÄ“ÉÇDE-NEST-KEYWORD-LIST“ÇACTIVE-DATABASEÄ“ÉÅVALIDATE-SYM“\ÄBÄ§BÄéBÄ¶¿ÉGET-KEYWORD-VALUE-PREREQ“\ÄBÄ§¿CÇGET-KEYWORD-VALUEÄ“\ÄBÄé¿\ÄBÄ¶¿BÄè“ÍÄTERPRI“ÏÜERROR - List of tuples as well as a pathname provided.¿™ÅWRITE-STRING“ÍÅSTRING-UPCASEÄ“™ÅSTRING-EQUAL¿p¿BÄ\¨ÅMEMBER-TESTÄ“ÍÄSTRING¿,ÅINSERT-Ä¿lÄ-Ä¿™ÅCONCATENATEÄ“™ÅFIND-SYMBOLÄ“lÄ*Ä¿BÄÓ“¨Å-ATTRIBUTES*¿BÄ“™ÄEVAL“ÏÄ-KEY*Ä¿ÉÅSYSTEM-INDEX¿\ÄlÅINDEX-NAMElÅINDEX-TYPE¨ÄKEYÄ¿BÄõ¿ÅQTRIEVEÄ“\ÄlÅATTRIBUTES¨ÇIMPLEMENTATION-TYPEÄlÇSTORAGE-STRUCTUREÄ¨ÄKEYÄ¨ÅCARDINALITYÄ¿ÉÅGET-RELATION“lÇERROR - Relation Ä¿ÍÄPRIN1Ä“,Ñ is not defined in the database ¿CÇCONVERT-ATTRIBUTES“jÅPROBE-FILE“BÄP“ÏÅERROR - File Ä¿,Ç does not exist.¿¨ÉERROR - No tuples provided.Ä¿lÅRETRIEVE-Ä¿ÇSYSTEM-ATTRIBUTE¿\ÄÏÅATTRIBUTE-NAME,ÇDOMAIN-FUNCTIONÄÏÅDEFAULT-VALUEÄ¿¨ÑERROR - List of tuples not provided.¿,ÅERROR - ¿ÏÑ is not an attribute in the relation Ä¿¨ÇWARNING - Attribute ¿lá has been specified more than once in the attribute list.Ä¿p¿BÄ\,Å*APPENDÄ“p¿BÄTÏÄFIRSTN“¨ÇWARNING - The tuple ¿,é is smaller in length than the attribute-list. The extra attributes will get the default values for this tuple.Ä¿*ÅREVERSEÄ“BÄ•“p¿BÄ\lÅASSOC-TEST“,Ç is not a list.Ä¿lÑ          It will not be inserted.¿ÉÅDOMAIN-CHECK“lÅWARNING - ¿ÏÇ is not a valid tuple.¿,ÖERROR - No valid tuples to be inserted.Ä¿ÏÅINDEX-INSERT-Ä¿ÇSYSTEM-RELATIONÄ¿\ÄlÅMODIFIEDPÄ¨ÅCARDINALITYÄ¿ÇDELETE-OR-MODIFY“,ÇSYSTEM-RELATIONÄ¿\ÄlÅMODIFIEDPÄ¿ÏÄ tuple¿eÄs¿jÅWRITE-CHAR“¨Ç inserted into the Ä¿lÅ relationÄÄ@‰@QPˇ›A—†@Qä@¡‰Ä‰ÄQäÄ¡ÊRP@Qí@¡P@QíBF¡P@QíBG¡P@QíBH¡‰G‰G5ÊGQäG¡
‰F‰H‰‰ Ä!P"àR%‰ÄQ#äP$P%ò‰&P'PP(PP)™P*íS¡ÄQ&PP+PÄQ,ä-P)™.ä/äFQ&PP+PÄQ,ä0P)™.ä/äÄQS≠ÄQ,äP$P%òÊ1PP2P
P$P3PÄQ,äö4™R¡ÄQ5Pˇ›6öN¡NÊ	‰ Ä7P"àÄQ8à9P"à	P8àRNSÄ¡NWN¡NWL¡N[K¡NQBJ¡NUBM¡NS:äI¡‰G‰S€S—GQU¡T¡¸TQUSV√ˇ›êÊRVQˇ›íCT√¡U≈UÁSQ¸GQ:äG¡H‰HQ;à‰HQ<äF¡
¸‰ Ä=P"àHQ8à>P"àFÊ‰ Ä?P"àRÄQ,äP$P%òÊÊ‰&P@PP(PP)™P*íU¡APPBPP$P3PÄQ,äöˇ€APJUªN¡ÄQ#äP$P%òÏÊÍÊF5Ê‰ ÄCP"àRGô‰&‰GQT¡#‰V€TSV√IQ$P%òÊ	‰ ÄDP"àVQ8àEP"àÄQ8àRVQTU$P%ò	‰‰ ÄFP"àVQ8àGP"àT≈›ÁFQT¡∑‰TSW¡GQX¡W5Y‰1‰GQäCWQäCx‰IQY¡‰YSV√XQ$P%òÊXQäCWQäC|ÊXQVQäHíX¡Y≈ÌÁ‰XQWQäCô‰WQäCXQIíX¡‰ ÄJP"àWQ8àKP"àE€IQLäS¡‰SSV¡Z€VQXQ$P%ò‰WQäXQVQäMöBZ¡‰ZQ¸VQ#äNQ$PNöBE]E¡S≈„ÁEQD]D¡¸
‰ ÄJP"àWQ8àOP"à ÄPP"àT≈ìÁI¸E‰FQU¡D‰USW¡W5Ê9‰ ÄJP"àWQ8àOP"à ÄPP"à.¸WQäCIQäCx‰WQT€T—WQäCIQ
C\¡[¡¸[Q\SV√#äNQ$PNöBC[√¡\≈\ÚÁTQHí¸WQIQäCô‰IQäCWQIí¸WQD]D¡U≈øÁ¸FQD¡C‰ÄQ#äP$P%ò=Ê;ÊDQO¡D€P€Q€IQLä[¡‰[SV¡]€^€&PPVQNQ$PNöB^√)ö.ä]√P]P¡^QQ]Q¡[≈ÍÁOQ_¡‰IQPQQQ_SQ†‰_SD]D¡¸
‰ ÄRP"à_S8àSP"à ÄPP"à_≈ËÁ¸DQLäD¡DÊ ÄTP"àR&P'PLQ(PKQ)™P*í\¡ÄQ,äIQDQJQÄQ,ä\´A¡R‰RQ[¡‰[S`¡&PUPLQ(P`W)™P*íY¡`SAQIQ`[ÄQY©[≈ÌÁ,ÊVPˇ›$P3PÄQ#äöWPˇ›DQäCMaíX®VPˇ›$P3PYPöZPˇ›äX®‰ ÄDQäC8à[P"àDQäCÊ\P]à^P"àÄQ8à_P"àÄOÄ4BÄêÄÄCÇINSERT-FLAVOR-HASHÄÎÄ"TÜÄAhFÄ2¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ5\ÄBÄõCÅATTR-LISTÄBÄãBÄßCÅINDEX-NAMEBÄ:\Ä
√ÅHASH-RELATIONÄBÄ:BÄ:BÄ:BÄ:BÄ§√ÄKEYVAL√Ä%TUPLEBÄ§BÄ>\ÄBÄk\ÄBÄmBÄªBÄΩBÄpÄBÄ–ëBÄ•“ÉÅENTRY-POINTÄ¿ÉÄGETP“ÉÇUNCONVERT-ATTRIBUTES“BÄÓ¿BÄÍ“BÄÒ“BÄ“p¿BÄTÏÅMAKE-INSTANCEÄ“p¿BÄT,ÇSET-IN-INSTANCEÄ“*ÅGETHASHÄ“p¿BÄT,ÅPUTHASHÄíÇQÅQÉQöÉ¡ÑQPí@¡ÅQäÅ¡PPÄQ	ä
öäÄ¡A—ÇQÉQD¡C¡B¡$¸BQCSDSF¡E¡G€ÄQäG¡EQH¡ÅQI¡¸GQISHSòH≈I≈H¯ÁFQGQFQ@Qí
C@QòGQCB√¡C≈D≈C‰DÿÁAOÄPBÄ5ÄÄCÇINSERT-FLAVOR-HEAPÄÎÄ3uÜÄAlFÄB¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄQ\ÄBÄõBÄ>BÄãBÄßBÄ?BÄ:\Ä√ÅFLAVOR-TUPLESÄÇRELATION-TUPLESÄ√ÅTUPLES-LENGTHÄBÄ:BÄ:BÄ:BÄ§BÄCBÄ§BÄ>BÄ:\ÄBÄk\ÄBÄmBÄªBÄΩBÄpÄBÄ–ëBÄH“BÄÓ¿BÄÍ“BÄÒ“BÄ“BÄF¿BÄG“BÄJ“BÄL“BÄ“ÉÄPUTPíÅQäÅ¡PPÄQäöäÄ¡ÑQ	P
íÇQäCB¡A¡ÑQBQAQå&‰C—ÇQE¡D¡¸DQESF¡G€ÄQäG¡FQH¡ÅQI¡¸GQISHSòH≈I≈H¯ÁGQCD√¡E≈EÂÁCQ@√ÑQ	P
í&¸ÑQ	P
íE€E—ÇQC¡J¡¸JQCSH¡G€ÄQäG¡HQF¡ÅQI¡¸GQISFSòF≈I≈F¯ÁGQCJ√¡C≈CÂÁEQ@√í	Pò@OÄaBÄQÄÄÇINSERT-LIST-HASHÄÎÄ*ÜÄATFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄb\ÄÅRELATIONBÄ>BÄãBÄßBÄ?BÄ:\ÄBÄABÄ:BÄ:BÄ§BÄB\ÄBÄk\ÄBÄmBÄªBÄpÄBÄ•“BÄF¿BÄG“BÄM“BÄOíÇQÅQÉQöÉ¡ÑQPí@¡ÇQÉQB¡A¡¸ASBSD¡C¡DQCQDQ@Qí
C@QòA≈B≈A‰BÓÁÇOÄoBÄbÄÄÇINSERT-LIST-HEAPÄÎÄÜÄAHFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄp\ÄBÄkBÄ>BÄãBÄßBÄ?BÄ:\ÄBÄ\BÄ]BÄ:ÄBÄF¿BÄG“BÄ“BÄ`íÑQPíÇQäCA¡@¡ÑQAQ@Qå‰ÇQ@Q¸@QÇQíPòÇOÄzBÄpÄÄCÇINSERT-STRUCT-HASHÄÎÄ0rÜÄA|FÄB¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ{\ÄBÄõBÄ>BÄãBÄßBÄ?BÄ:\ÄBÄA√ÅRELATION-MACROÉÇSTRING-RELATION-NAMEBÄ:BÄ:BÄ:BÄéBÄ:BÄ:BÄ§BÄBBÄCÅATTR-VALBÄ§BÄ>\ÄBÄk\Äp¿BÄ\lÅXR-BQ-CONSBÄƒBÄ∏BÄmBÄªBÄΩBÄpÄBÄ–ëBÄÓ“BÄ•“BÄF¿BÄG“BÄÓ¿ÏÄMAKE-Ä¿BÄÒ“BÄ“lÄ:Ä¿BÄ8¿BÄè“BÄı“BÄM“BÄOíÄQäB¡ÇQÅQÉQöÉ¡ÑQPí@¡PP	PBQ
¢äA¡C—ÅQE¡D¡¸DQESF¡PPBQFQ
¢äCD√¡E≈EÒÁCQÅ¡E€E—ÇQÉQC¡H¡G¡*¸GQHSCSJ¡I¡K€L€IQM¡ÅQN¡
¸PMSíL]L¡NSL]L¡M≈N≈MÙÁAQL]äK¡JQKQJQ@Qí
C@QòKQCG√¡H≈C≈H‰C“ÁEOÄéBÄ{ÄÄCÇINSERT-STRUCT-HEAPÄÎÄ.mÜÄAxFÄ?¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄè\ÄBÄõBÄ>BÄãBÄßBÄ?BÄ:\ÄBÄÖ√ÅSTRUCT-TUPLESÄBÄÜBÄ:BÄ:BÄ:BÄéBÄ:BÄ§BÄáBÄ§BÄ>BÄ\BÄ]\ÄBÄk\ÄBÄãBÄƒBÄ∏BÄmBÄªBÄΩBÄpÄBÄ–ëBÄÓ“BÄÓ¿ÏÄMAKE-Ä¿BÄÒ“BÄ“lÄ:Ä¿BÄ8¿BÄè“BÄı“BÄF¿BÄG“BÄ“BÄ`íÄQäB¡PPPBQ¢ä@¡C—ÅQE¡D¡¸DQESF¡P	PBQFQ¢äCD√¡E≈EÒÁCQÅ¡E€E—ÇQC¡G¡¸GQCSH¡I€HQJ¡ÅQK¡
¸
PJSíI]I¡KSI]I¡J≈K≈JÙÁ@QI]äCG√¡C≈C‚ÁEQA¡ÑQPíAQäCM¡L¡ÑQMQLQå‰AQLQ¸LQAQíPòAOÄûBÄèÄ1Ä\Äp¿BÄ\,ÑFASL-RECORD-FILE-MACROS-EXPANDED\ÄBÄ8\Ä\ÄÍÄDEFUNÄÜÄ'\Äp¿BÄT¨ÄDEFFÜÄb\ÄBÄãÜÄñΩ\ÄBÄƒÜÄ.Ÿã\ÄBÄ¬ÜÄ{öÕ\ÄBÄ¡ÜÄ:}n\ÄBÄ¿ÜÄxıø\ÄBÄøÜÄZiÛ\ÄBÄæÜÄz(á\ÄBÄΩÜÄ(Ã¢\ÄBÄªÜÄ*˝j\ÄBÄ∏ÜÄ•ò\ÄBÄpÜÄ[ÊÑ\ÄBÄoÜÄFö≤\ÄBÄnÜÄ)»‰\ÄBÄmÜÄ=Ã#ÄÄ'(lambda (attr)
  (if (not (or (member attr actual-p-l :test 'string-equal)
        (equal (length actual-p-l) (length sub-tuple))))
      (setf actual-p-l (append actual-p-l (list attr)))))
     attribute-list))
  (if (and *parameter-checking* (> (length actual-p-l)(length sub-tuple)))
      (progn
 (setf actual-p-l (firstn (length sub-tuple) actual-p-l))
 (if *provide-warning-messages*
     (format *standard-output*
      "~%WARNING - The tuple ~S is smaller in length than LMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540741. :SYSTEM-TYPE :LOGICAL :VERSION 3. :TYPE "LISP" :NAME "INTERFACE" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :CHARACTERS T :NOT-BACKED-UP T :CREATION-DATE 2757594269. :AUTHOR "REL3" :LENGTH-IN-BYTES 131948. :LENGTH-IN-BLOCKS 129. :BYTE-SIZE 8.)

;;; -*- Mode:Common-Lisp; Package:RTMS; Fonts:(WIDER-MEDFNT MEDFNB MEDFNB HL7); Base:10 -*-
;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved
;**************************************************************************
;                             USER INTERFACE *
;      *
;      *
; 1. Issues to be considered later.          *
;   a) Output-window  ----> a specified file.                                      *
;   b) Parts of output (ex. a relation) to a ZMACS window.                         *
;   c) Use line area scrolling for interactive maintenance of database.            *
;      *
;      *
;      *
;      *
;  AUTHOR                         *
; CSL                  *
; Texas Instruments                 *
; .....                  *
; Version 0.0                 *
;**************************************************************************
;;;Change History
;;;  03.31.87  MRR  Changed DBMS-RC defflavor to make scroll-bar always appear.
;;;                 Changed references to XFASL files to XLD for Save-relation command.
;;;  04.01.87  MRR  Changed DBMS-RC defflavor to prevent pixel overlap of scroll-bar.
;;;  04.06.87  MRR  Fixed HELP-LINE-AREA-DEL to delete tuples using the display.
;;;                 Fixed mouse documentation strings for various windows.
;;;                 Fixed method (DBMS-RC :handle-unknown-input) to call Relation help functions
;;;                 correctly.
;;;  04.07.87  MRR  Fixed HELP-LINE-AREA. Made references to w:*remove-typeout-standard-message*
;;;                 for typeout windows. Fixed HELP-LINE-AREA-MOD for the case when the current
;;;                 package is not RTMS. (SPR #4197)
;;;  04.09.87  MRR  Added :sensitive-item-types initialization option to DBMS-RC defflavor so
;;;                 that only valid types are made mouse-sensitive. (see SPR #1858)
;;;                 Fixed command for sending display output to file.

;**************************************************************************
;                          INTERFACE GLOBAL VARIABLES                           *
;     These global variables are used to hold the latest user-values for the       *
;     variables in the choose-variables windows associated with various commands.  *
;**************************************************************************
(PUTPROP 'display nil 'ucl:items)
(PUTPROP 'display nil 'ucl:commands-wanting-on)
(PUTPROP 'command-menu nil 'ucl:items)
(PUTPROP 'command-menu nil 'ucl:commands-wanting-on)
(PUTPROP 'system-menu nil 'ucl:items)
(PUTPROP 'system-menu nil 'ucl:commands-wanting-on)
;;
(SETQ rtms:*default-pkg* *PACKAGE*)
(PKG-GOTO *pkg-string*)
(UCL:MAKE-SYNONYM '*ui-relation* nil)
(UCL:MAKE-SYNONYM '*ui-tuples* nil)
(UCL:MAKE-SYNONYM '*ui-transaction* nil)
(UCL:MAKE-SYNONYM '*ui-function* nil)
(UCL:MAKE-SYNONYM '*ui-attributes* nil)
(UCL:MAKE-SYNONYM '*ui-format* nil)
(UCL:MAKE-SYNONYM '*ui-file* nil)
(UCL:MAKE-SYNONYM '*ui-database* *active-db*)
(UCL:MAKE-SYNONYM '*ui-directory* (STRING-APPEND "SYS:" user-id ";"))  ;mrr 03.31.87
(UCL:MAKE-SYNONYM '*ui-type* 'xld)     ;mrr 03.31.87
(UCL:MAKE-SYNONYM '*ui-attr-desc* nil)
(UCL:MAKE-SYNONYM '*ui-doc* ".....")
(UCL:MAKE-SYNONYM '*ui-key* nil)
(UCL:MAKE-SYNONYM '*ui-imp* *system-relation-base-implementation*)
(UCL:MAKE-SYNONYM '*ui-ss* *system-relation-storage-structure*)
(UCL:MAKE-SYNONYM '*ui-viewdef* nil)
(UCL:MAKE-SYNONYM '*ui-where* T)
(UCL:MAKE-SYNONYM '*ui-values* nil)
(UCL:MAKE-SYNONYM '*ui-join-into* nil)
(UCL:MAKE-SYNONYM '*ui-over* T)
(UCL:MAKE-SYNONYM '*ui-into* nil)
(UCL:MAKE-SYNONYM '*ui-from* nil)
(UCL:MAKE-SYNONYM '*ui-wide* nil)
(UCL:MAKE-SYNONYM '*ui-num* -1)
(UCL:MAKE-SYNONYM '*ui-sort* nil)
(UCL:MAKE-SYNONYM '*ui-object* nil)
(UCL:MAKE-SYNONYM '*ui-rel2* nil)

(defparameter *line-area-documentation*
      '(:documentation ""
:mouse-L-1 "To see the entire line."
:mouse-M-2 "To delete the tuple."
:mouse-R-1 "To modify the tuple.")
  "The wholine documentation string when a line is selected.")

(defparameter *dbms-window-wholine-documentation*
      '(:documentation "Window for database output. Some items are made mouse-sensitive for inspection."
:mouse-R-1 "RTMS Command Menu"
:mouse-R-2 "System Menu")
      "The wholine documentation string when in the RTMS interface output window.")

(defparameter *interaction-wholine-documentation*
      '(:documentation "This window accepts user input. Input can also be provided through the command menu."
:mouse-R-1 "RTMS Command Menu"
:mouse-R-2 "System Menu"))
(defparameter *attribute-wholine-documentation*
      '(:mouse-any "To see this ATTRIBUTE's definition." ))
(defparameter  *dbms-object-wholine-documentation*    ;mrr 04.06.87
      '(:mouse-any "To see this object's definition." ))
(defparameter *relation-wholine-documentation* ;mrr 04.06.87
      '(:documentation ""
:mouse-L-1 "To see the RELATION definition."
:mouse-M-1 "To modify the RELATION features."
:mouse-R-1 "To retrieve this RELATION."))
(defparameter *database-wholine-documentation*
      '(:mouse-any "List the relations in this DATABASE, if it is active."))

;**************************************************************************
;                      FLAVORS AND METHODS   *
;      *
;     MENU-PANE  ... Used for the main menu that appears in the interface.         *
;     DBMS-WINDOW .. The output-window in the interface .. text-scrolling, mouse-  *
;                    sensitive and line-area-scrolling window.                     *
;     DBMS-WINDOW-WITH-TYPEOUT .. The actual flavor used for output-window. It is  *
;                                 the above flavor with typeout-mixin added to it  *
;                                 such that temporary, unimportant and informatory *
;                                 messages can be printed on the typeout-window and*
;                                 it disappears when the user hits any character.  *
;     INTERACTION-PANE .. The flavor used for interaction. It is basically the     *
;                         universal command loop typein flavor.                    *
;     DBMS-RC  ..  Flavor for the entire interface screen. Inclusion of the command*
;                  loop mixin makes the database interface to run under the        *
;                  UCL package.              *
;**************************************************************************
(DEFFLAVOR MENU-PANE ()
   (w:menu)
  (:default-init-plist :command-menu t
                       :dynamic t))
(DEFFLAVOR DBMS-WINDOW ()
   (W:LINE-AREA-TEXT-SCROLL-MIXIN
    W:FUNCTION-TEXT-SCROLL-WINDOW
    W:MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW
    W:MARGIN-REGION-MIXIN
    W:SCROLL-BAR-MIXIN
    W:ANY-TYI-MIXIN
    W:WINDOW))
(DEFMETHOD (DBMS-WINDOW :line-area-mouse-documentation) ()
   *line-area-documentation*)

(DEFFLAVOR DBMS-WINDOW-WITH-TYPEOUT ()
   (W:TEXT-SCROLL-WINDOW-TYPEOUT-MIXIN DBMS-WINDOW)
  (:DEFAULT-INIT-PLIST :typeout-window '(W:typeout-window
    :Deexposed-typeout-action
    (:expose-for-typeout))))

(defmethod (DBMS-WINDOW-WITH-TYPEOUT :who-line-documentation-string) ()        ;mrr 04.06.87
  (multiple-value-bind
    (ignore m-s-i-type)
      (send *output-window* :mouse-sensitive-item w:mouse-x w:mouse-y)
    (case m-s-i-type
      (attribute  *attribute-wholine-documentation*)
      (relation   *relation-wholine-documentation*)
      (database   *database-wholine-documentation*)
      (dbms-object *dbms-object-wholine-documentation*)
      (t *dbms-window-wholine-documentation*))))

(DEFFLAVOR INTERACTION-PANE () (UCL:COMMAND-AND-LISP-TYPEIN-WINDOW
 W:PREEMPTABLE-READ-ANY-TYI-MIXIN))
(defmethod (INTERACTION-PANE  :who-line-documentation-string) ()
   *interaction-wholine-documentation*)        ;mrr 04.06.87

(DEFMETHOD (INTERACTION-PANE :before :SELECT) (&rest ignore)
    (SEND dbms-frame1 :expose))
(DEFMETHOD (INTERACTION-PANE :after :SELECT) (&rest ignore)
;  (PKG-GOTO "RTMS")
 )


(DEFFLAVOR DBMS-RC () (UCL:COMMAND-LOOP-MIXIN W:STREAM-MIXIN
       W:INFERIORS-NOT-IN-SELECT-MENU-MIXIN
       W:BORDERED-CONSTRAINT-FRAME-WITH-SHARED-IO-BUFFER)
  (:DEFAULT-INIT-PLIST :menu-panes '((s-m-pane system-menu))
                       :active-command-tables '(dbms-comtab)
       :all-command-tables '(dbms-comtab)
       :typein-handler :handle-typein-input
;The following change is being made to prevent the first character going
;into the interface buffer.
;         :io-buffer W:kbd-io-buffer
       :minimum-width (SEND W:default-screen :width)
       :minimum-height (SEND W:default-screen :height)
       :basic-help '(help)
       :print-function 'new-print
       :print-results? #'(LAMBDA () T)
       :panes
  `((o-pane dbms-window-with-typeout
     :blinker-p NIL              ;:blink
     :print-function DBMS-PRINTER
     :print-function-arg NIL
     :scroll-bar-side :right
     :scroll-bar-mode :maximum  ;mrr 03.31.87
     :borders nil       ;mrr 04.01.87
     :label ,(LIST :bottom :string "OUTPUT"
     :font fonts:cptfont)
     :font-map ,(LIST fonts:cptfontb)
     :sensitive-item-types ,(list 'relation 'attribute ;mrr 04.09.87
      'database 'dbms-object))
    (i-pane interaction-pane
     :save-bits T
     :blinker-p :OFF            ;:blink
     :label ,(LIST :bottom :string "Rtms Interface"
     :font fonts:medfnt)
     :borders 1
     :font-map ,(LIST fonts:medfnb))
    (s-m-pane menu-pane
     :font-map ,(LIST fonts:hl12b)
     :rows 1.
              :label NIL))
               :constraints  '((main . ((o-pane i-pane s-m-pane)
       ((s-m-pane 1 :lines))
       ((o-pane .8))
       ((i-pane :even))))))
  (:INIT-KEYWORDS :TYPEIN-HANDLER :handle-typein-input))

(DEFMETHOD (DBMS-RC :handle-unknown-input) (&AUX item)
  (case UCL:input-mechanism
    (UCL:menu (beep))
    (UCL:key-or-button (BEEP))
    (UCL:typein (SEND *terminal-io* :send-if-handles :fresh-line)
    (BEEP)
    (FORMAT *STANDARD-OUTPUT* " ** ~a"
    (OR UCL:error-message "Unrecognized input")))
    (OTHERWISE (IF (LISTP ucl:kbd-input)
      (CASE (FIRST ucl:kbd-input)
(:line-area (CASE (FOURTH ucl:kbd-input)
      (#\mouse-l-1 (HELP-LINE-AREA (CADR ucl:kbd-input)))
      (#\mouse-r-1 (HELP-LINE-AREA-MOD (CADR ucl:kbd-input)))
      (#\mouse-m-2 (HELP-LINE-AREA-DEL (CADR ucl:kbd-input)))))
;I think this help can be made lot faster now that we can recognize the type of
;the object right away.
(attribute (HELP-OBJECT (STRING
   (IF (LISTP (SETQ item (CADR ucl:kbd-input)))
       (CADR item)
     item))))
(database (HELP-OBJECT (STRING
   (IF (LISTP (SETQ item (CADR ucl:kbd-input)))
       (CADR item)
     item))))
(dbms-object (HELP-OBJECT (STRING
     (IF (LISTP (SETQ item (CADR ucl:kbd-input)))
  (CADR item)
       item))))
(relation (CASE (FOURTH ucl:kbd-input)
    (#\mouse-r-1 (retrieve
    (if (stringp (setq item (CADR ucl:kbd-input)))
        (read-from-string item) ;mrr 04.06.87
        item)))
    (#\mouse-m-1 (HELP-MODIFY
    (if (stringp (setq item (CADR ucl:kbd-input)))
        (read-from-string item) ;mrr 04.06.87
        item)))
    (otherwise (HELP-OBJECT (STRING
   (IF (LISTP (SETQ item (CADR ucl:kbd-input)))
       (CADR item)
     item))))))
(OTHERWISE (BEEP)))))))


;**************************************************************************
;                          DEFCOMMANDS FOR ALL DATABASE COMMANDS                   *
;      *
;     Each defcommand definition enables individual database commands and a few    *
;     help commands to become part of the database command table. If the reader    *
;     is familiar with UCL, the following DEFCOMMAND definitions will be           *
;     self-explanatory.                      *
;**************************************************************************
;**************************************************************************
;            DEFCOMMAND FOR ACTIVE DATABASE  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC active-database)()
            `(:description "Returns the name of the active database. (ACTIVE-DATABASE)"
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Returns the name of the active database."
      :keys ((#\SUPER-F #\SUPER-A)))
  (SEND *output-window* :append-item (FORMAT nil "~S" '(ACTIVE-DATABASE)))
  (SEND *output-window* :append-item (FORMAT nil "~S" (ACTIVE-DATABASE))))
;**************************************************************************
;            DEFCOMMAND FOR ABORT TRANSACTION                                      *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC abort-transaction)()
            `(:description "Terminates the special transaction processing. (ABORT-TRANSACTION)"
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Terminates the special transaction processing."
      :keys ((#\SUPER-T #\SUPER-A)))
  (SEND *output-window* :append-item (FORMAT nil "~S" '(ABORT-TRANSACTION)))
  (SEND *output-window* :append-item (FORMAT nil "~S" (ABORT-TRANSACTION))))
;**************************************************************************
;            DEFCOMMAND FOR BEGIN TRANSACTION                                      *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC begin-transaction)()
            `(:description "Begins the special transaction processing. (BEGIN-TRANSACTION)"
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Begins the special transaction processing."
      :keys ((#\SUPER-T #\SUPER-B)))
  (SEND *output-window* :append-item (FORMAT nil "~S" '(BEGIN-TRANSACTION)))
  (SEND *output-window* :append-item (FORMAT nil "~S" (BEGIN-TRANSACTION))))
;**************************************************************************
;            DEFCOMMAND FOR END TRANSACTION  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC end-transaction)()
            `(:description "Executes the database calls postponed due to special transaction processing and terminates the transaction.  (END-TRANSACTION)"
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Executes the database calls postponed due to special transaction processing and terminates the transaction."
      :keys ((#\SUPER-T #\SUPER-E)))
  (SEND *output-window* :append-item (FORMAT nil "~S" '(END-TRANSACTION)))
  (SEND *output-window* :append-item (FORMAT nil "~S" (END-TRANSACTION))))
;**************************************************************************
;            DEFCOMMAND FOR ENVIRONMENT STATUS                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC environment-status)()
            `(:description "Returns the values of the environment variables. (ENVIRONMENT-STATUS)"
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Returns the values of the environment variables."
      :keys ((#\SUPER-F #\SUPER-E)))
  (SEND *output-window* :append-item (FORMAT nil "~S" '(ENVIRONMENT-STATUS)))
  (ENVIRONMENT-STATUS))
;**************************************************************************
;            DEFCOMMAND FOR ATTACH RELATION  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC attach-relation) (relation att path tup dir doc key
          imp ss mem &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'attach-relation)
     (FORMAT NIL "  ~S"
      (CONS
        'attach-relation
        (ARGLIST 'attach-relation))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default nil
    :type (:documentation
     "Name of the relation to be attached."
     :sexp))
   ,*ucl-attr-desc*
   ,*ucl-pathname*
   ,*ucl-format*
   ,*ucl-dir*
   ,*ucl-doci*
   ,*ucl-key*
    ,*ucl-imp*
   ,*ucl-sto*
   (:label "Memory:"
    :default nil
    :type (:documentation
     "If the data is stored in the memory, then give the name of the variable that contains the data."
     :sexp))
     :label "Give parameters for ATTACH RELATION:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "used to attach a relation."
      :keys (#\SUPER-A))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(LIST 'ATTACH-RELATION
      relation
      (SETQ keywords
    (LIST 'format tup 'dir dir 'doc doc 'path path
  'key key 'imp imp 'sto ss 'att att 'mem mem)))))
  (ATTACH-RELATION relation keywords))
;**************************************************************************
;            DEFCOMMAND FOR RENAME ATTRIBUTE *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC rename-attribute) (relation old-new)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'rename-attribute)
     (FORMAT NIL "  ~S"
      (CONS
        'rename-attribute
        (ARGLIST 'rename-attribute))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default *ui-relation*
    :type (:documentation
     "Name of the relation whose attributes are to be renamed."
     :sexp))
   (:label "Attributes and their new names:"
    :default nil
    :type (:documentation
     "Specify a list of the attributes and their new names. For ex. (a1 new-a1 a2 new-a2...)"
     :sexp))
     :label "Give parameters for RENAME ATTRIBUTE:")
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "used to rename attributes in a relation."
      :keys ((#\SUPER-R #\SUPER-A)))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(APPEND (LIST 'RENAME-ATTRIBUTE
      relation) old-new)))
  (EVAL `(RENAME-ATTRIBUTE* ,relation ,@old-new)))
;**************************************************************************
;            DEFCOMMAND FOR RENAME RELATION  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC rename-relation) (old-new)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'rename-relation)
     (FORMAT NIL "  ~S"
      (CONS
        'rename-relation
        (ARGLIST 'rename-relation))))
      :arguments (:user-supplied (:label "Relations and their new names:"
    :default nil
    :type (:documentation
     "Specify a list of the relations and their new names. For ex. (rel-1 new-rel-1 rel-2 new-rel-2...)"
     :sexp))
     :label "Give parameters for RENAME RELATION:")
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "used to rename relations in the current database."
      :keys ((#\SUPER-R #\SUPER-R)))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(CONS 'RENAME-RELATION
      old-new)))
  (EVAL `(RENAME-RELATION* ,@old-new)))
;**************************************************************************
;            DEFCOMMAND FOR RENAME DATABASE  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC rename-database) (old-new)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'rename-database)
     (FORMAT NIL "  ~S"
      (CONS
        'rename-database
        (ARGLIST 'rename-database))))
      :arguments (:user-supplied (:label "Databases and their new names:"
    :default nil
    :type (:documentation
     "Specify a list of the databases and their new names. For ex. (db-1 new-db-1 db-2 new-db-2...)"
     :sexp))
     :label "Give parameters for RENAME DATABASE:")
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "used to rename databases."
      :keys ((#\SUPER-R #\HYPER-D)))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(CONS 'RENAME-DATABASE
      old-new)))
  (EVAL `(RENAME-DATABASE* ,@old-new)))
;**************************************************************************
;            DEFCOMMAND FOR DETACH RELATION  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC detach-relation) (relation path mem disk &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'detach-relation)
     (FORMAT NIL "  ~S"
      (CONS
        'detach-relation
        (ARGLIST 'detach-relation))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default nil
    :type (:documentation
     "Name of the relation to be Detached."
     :sexp))
   (:label "Pathname:"
    :default *ui-file*
    :type (:documentation
     "Specify the name of the file where the data is to be stored."
     :SEXP))
   (:label "Memory:"
    :default nil
    :type (:documentation
     "If the data is to be in the memory and not save it on the disk, give the name of a variable."
     :sexp))
   (:label "Disk:"
    :default nil
    :type (:documentation
     "Indicate if files corresponding to the relation are to be deleted from the disk."
     :boolean))
     :label "Give parameters for DETACH RELATION:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "used to detach a relation."
      :keys (#\SUPER-D))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(LIST 'DETACH-RELATION
      relation
      (SETQ keywords
    (LIST 'path path 'mem mem 'disk disk)))))
  (DETACH-RELATION relation keywords))
;**************************************************************************
;            DEFCOMMAND FOR INSERT TUPLES    *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC insert-tuples) (relation-name list-of-tuples attributes
   pathname &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'insert)
     (FORMAT NIL "  ~S"
      (CONS
        'insert
        (ARGLIST 'insert))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default *ui-relation*
    :type (
      :documentation "Specify the relation into which the tuples are to be inserted."
      :sexp))
   (:label "List of tuples:"
    :default *ui-tuples*
       :type (:documentation "Give a list of tuples to be inserted." :SEXP))
   (:label "Attributes:"
    :default nil
    :type (:documentation "If a list of attributes is provided, then values in the tuples are assumed to be in the same order."
:SEXP))
   (:label "Pathname:"
    :default *ui-file*
    :type (:documentation "If a list of tuples is not provided, then specify the file which contains the data."
     :SEXP))
 :label "Give parameters for INSERTING TUPLES:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to insert a list of tuples in a given relation."
      :keys (#\SUPER-I))
  (SEND *output-window* :append-item (FORMAT nil "~S"
        (LIST 'INSERT relation-name (SETQ keywords
    (LIST 'tuples list-of-tuples
   'attr attributes
   'path pathname)))))
  (INSERT relation-name keywords))

;**************************************************************************
;                DEFCOMMAND FOR MAPON ALLTUPLES                                    *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC maptuple) (relation dbfunction)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'maptuple)
     (FORMAT NIL "  ~S"
      (CONS
        'maptuple
        (ARGLIST
          'maptuple))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default *ui-relation*
    :type (:documentation
         "Give the relation to be mapped." :sexp))
   (:label "Function Definition"
    :default *ui-function*
    :type (:documentation
      "Specify a function definition."
      :sexp))
  :label "Map a function on all tuples using MAPCAR:")
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Maps a given function on all the tuples in a relation using MAPCAR."
      :keys ((#\SUPER-F #\SUPER-M)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'MAPTUPLE dbfunction relation)))
  (MAPTUPLE (EVAL dbfunction) relation))
;**************************************************************************
;                DEFCOMMAND FOR MAPON ALLTUPLES                                    *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC mapt) (relation dbfunction)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'mapt)
     (FORMAT NIL "  ~S"
      (CONS
        'mapt
        (ARGLIST
          'mapt))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default *ui-relation*
    :type (:documentation
         "Give the relation to be mapped." :sexp))
   (:label "Function Definition"
    :default *ui-function*
    :type (:documentation
      "Specify a function definition."
      :sexp))
  :label "Map a function on all tuples using MAPC:")
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Maps a given function on all the tuples in a relation using MAPC."
      :keys (#\SUPER-HYPER-F))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'MAPT dbfunction relation)))
  (MAPT (EVAL dbfunction) relation))
;**************************************************************************
;                    DEFCOMMAND FOR PRINT RELATION                                 *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC print-relation) (relation
    into dir doc key imp sto
    qprint to-file sort
    format wide number print
    tuples qsort stream unique
    &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'print-relation)
     (FORMAT NIL "  ~S"
      (CONS
        'print-relation
        (ARGLIST
          'print-relation))))
      :arguments (:user-supplied ,*ucl-retrieve-rel*
   ,*ucl-into*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-qprint*
   ,*ucl-out*
   ,*ucl-sort*
   ,*ucl-format*
   ,*ucl-wide*
     ,*ucl-num*
   ,*ucl-print*
   ,*ucl-tuples*
   ,*ucl-quick-sort*
   ,*ucl-stream*
   ,*ucl-unique*
 :label "Give parameters for PRINT RELATION ==>")
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Used to print tuples in a relation."
      :keys ((#\SUPER-F #\SUPER-P)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'RETRIEVE
      relation
      (SETQ keywords
    (LIST 'dir dir
   'doc doc
   'into into
   'qprint (NOT qprint) 'output-to-file to-file
   'sort sort 'format format
   'wide wide 'num number 'key key
   'print print 'tuples tuples
   'quick-sort qsort 'stream stream
   'unique unique 'imp imp 'sto sto)))))
  (RETRIEVE relation keywords))
;**************************************************************************
;                DEFCOMMAND  FOR RESTORE DATABASE                                  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC load-database) (database directory)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'load-database)
     (FORMAT NIL "  ~S"
      (CONS
        'load-database
        (ARGLIST
          'load-database))))
      :arguments (:user-supplied (:label "Database Name:"
    :default *ui-database*
    :type (
      :documentation "Name of the database to be loaded."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (
      :documentation "Name of the directory in which it is stored."
      :sexp))
 :label "Give parameters for LOAD DATABASE:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to load database from a given directory."
      :keys ((#\SUPER-L #\SUPER-D)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'LOAD-DATABASE database (LIST 'dir directory))))
  (LOAD-DATABASE database (LIST 'dir directory)))
;**************************************************************************
;                DEFCOMMAND  FOR RESTORE ENVIRONMENT                               *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC load-environment) (environment directory)
`(:description ,(STRING-APPEND (DOCUMENTATION 'load-environment)
     (FORMAT NIL "  ~S"
      (CONS
        'load-environment
        (ARGLIST
          'load-environment))))
      :arguments (:user-supplied (:label "Environment Name:"
    :default *ui-database*
    :type (
      :documentation "Name of the environment to be loaded."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (
      :documentation "Name of the directory in which it is stored."
      :sexp))
 :label "Give parameters for LOAD ENVIRONMENT:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to load environment from a given directory."
      :keys ((#\SUPER-L #\SUPER-E)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'LOAD-ENVIRONMENT environment (LIST 'dir directory))))
  (LOAD-ENVIRONMENT environment (LIST 'dir directory)))
;**************************************************************************
;                DEFCOMMAND  FOR RESTORE RELATION                                  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC load-relation) (relation directory)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'load-relation)
     (FORMAT NIL "  ~S"
      (CONS
        'load-relation
        (ARGLIST
          'load-relation))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default *ui-relation*
    :type (
      :documentation "Name of the relation to be loaded."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (
      :documentation "Name of the directory in which it is saved."
                :sexp))
  :label "Give parameters for LOAD RELATION:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to load a relation from a given directory."
      :keys ((#\SUPER-L #\SUPER-R)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'LOAD-RELATION relation (LIST 'dir directory))))
  (LOAD-RELATION relation (LIST 'dir directory)))
;**************************************************************************
;                DEFCOMMAND  FOR SAVE DATABASE                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC save-database) (database directory)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'save-database)
     (FORMAT NIL "  ~S"
      (CONS
        'save-database
        (ARGLIST
          'save-database))))
      :arguments (:user-supplied (:label "Database Name:"
    :default *ui-database*
    :type (:documentation
       "Name of the database to be saved."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (:documentation
      "Name of the directory to write to."
      :sexp))
  :label "Give parameters for SAVE DATABASE:")
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Used to save a database on a given directory."
      :keys ((#\SUPER-S #\HYPER-D)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'SAVE-DATABASE database (LIST 'dir directory))))
  (SAVE-DATABASE database (LIST 'dir directory)))
;**************************************************************************
;                DEFCOMMAND  FOR SAVE ENVIRONMENT                                  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC save-environment) (environment directory)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'save-environment)
     (FORMAT NIL "  ~S"
      (CONS
        'save-environment
        (ARGLIST
          'save-environment))))
      :arguments (:user-supplied (:label "Environment Name:"
    :default nil
    :type (:documentation
       "Name of the environment to be saved."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (:documentation
      "Name of the directory to write to."
      :sexp))
  :label "Give parameters for SAVE environment:")
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Used to save an environment on a given directory."
      :keys ((#\SUPER-S #\SUPER-E)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'SAVE-ENVIRONMENT environment (LIST 'dir directory))))
  (SAVE-ENVIRONMENT environment (LIST 'dir directory)))
;**************************************************************************
;                DEFCOMMAND  FOR SAVE RELATION                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC save-relation) (relation directory type save
     &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'save-relation)
     (FORMAT NIL "  ~S"
      (CONS
        'save-relation
        (ARGLIST
          'save-relation))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default *ui-relation*
    :type (
      :documentation "Name of the relation to be saved."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (
      :documentation "Name of the directory to write to."
      :sexp))
   (:label "Type of SAVE:"
    :default *ui-type*
    :type (:documentation "Save type. It can be either XLD or COMMAND." ;mrr 03.31.87
     :sexp))
   (:label "Must Save:"
    :default nil
    :type (:documentation "Save the relation even if the relation has not been modified." :BOOLEAN))
 :label "Give parameters for SAVE RELATION:")
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Used to save a relation on a given directory."
      :keys ((#\SUPER-S #\SUPER-R)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'SAVE-RELATION relation
      (SETQ keywords (LIST 'type type 'dir directory
     'save save)))))
  (SAVE-RELATION relation keywords))
;**************************************************************************
;                DEFCOMMAND  FOR SAVE TRANSACTION                                  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC save-transaction) (transaction directory pathname
     &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'save-transaction)
     (FORMAT NIL "  ~S"
      (CONS
        'save-transaction
        (ARGLIST
          'save-transaction))))
      :arguments (:user-supplied (:label "Transaction Name:"
    :default *ui-transaction*
    :type (
      :documentation "Name of the transaction to be saved."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (
      :documentation "Name of the directory to write to."
      :sexp))
   (:label "Pathname:"
    :default *ui-file*
    :type (:documentation
     "The name of the file into which the transaction forms will be stored. It defaults to <transaction>.lisp"
     :SEXP))
 :label "Give parameters for SAVE TRANSACTION:")
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Used to save a transaction on a given directory."
      :keys ((#\SUPER-S #\SUPER-T)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'SAVE-TRANSACTION transaction
      (SETQ keywords (LIST 'path pathname 'dir directory)))))
  (SAVE-TRANSACTION transaction keywords))

;**************************************************************************
;                DEFCOMMAND  FOR DEFINE IMPLEMENTATION                             *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-implementation) (implementation doc
   &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'define-implementation)
     (FORMAT NIL "  ~S"
      (CONS
        'define-implementation
        (ARGLIST 'define-implementation))))
      :arguments (:user-supplied (:label "Implementation Name:"
    :default nil
    :type (:documentation
      "Name of the implementation. Implementation-dependent routines are expected to be defined by the user."
      :sexp))
   (:label "Documentation:"
    :default *ui-doc*
    :type (:documentation
      "Documentation for the implementation."
      :string))
  :label "Give parameters for DEFINE IMPLEMENTATION:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define an implementation."
      :keys ((#\SUPER-D #\SUPER-I)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'DEFINE-IMPLEMENTATION implementation
      (SETQ keywords (LIST 'doc doc
     )))))
  (DEFINE-IMPLEMENTATION implementation keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE INDEX                                      *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-index) (relation-name index-name key-attributes storage-structure priority
  doc &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'define-index)
     (FORMAT NIL "  ~S"
      (CONS
        'define-index
        (ARGLIST 'define-index))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default nil
    :type (:documentation
      "Name of the relation upon which the index will be defined."
      :sexp))
   (:label "Index Name:"
    :default nil
    :type (:documentation
      "Name of the index to be defined."
      :string))
   (:label "Key Attributes:"
    :default nil
    :type (:documentation
      "List of attribute names which form the key for this index."
      :sexp))
   (:label "Storage Structure:"
    :default "AVL"
    :type (:documentation
      "The storage structure used to define the index."
      :string))
   (:label "Priority:"
    :default 10
    :type (:documentation
      "A numerical value which indicates the priority given to this index. 1 is the highest priority."
      :number))
   (:label "Documentation:"
    :default *ui-doc*
    :type (:documentation
      "Documentation for the index."
      :string))
  :label "Give parameters for DEFINE INDEX:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define a secondary index on a relation."
      :keys ((#\SUPER-D #\HYPER-I)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'DEFINE-INDEX relation-name
      (SETQ keywords (LIST 'name index-name 'key key-attributes 'sto storage-structure
     'priority priority 'doc doc
     )))))
  (DEFINE-INDEX relation-name keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY INDEX                                      *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-index) (relation-name index-name new-index-name
  key-attributes storage-structure priority
  doc &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-index)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-index
        (ARGLIST 'modify-index))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default nil
    :type (:documentation
      "Name of the relation upon which the index to be modified is defined."
      :sexp))
   (:label "Index Name:"
    :default nil
    :type (:documentation
      "Name of the index to be modified."
      :string))
   (:label "New Index Name:"
    :default nil
    :type (:documentation
      "New name of the index."
      :string))
   (:label "Key Attributes:"
    :default nil
    :type (:documentation
      "List of attribute names which form the key for this index."
      :sexp))
   (:label "Storage Structure:"
    :default nil
    :type (:documentation
      "The storage structure used to define the index."
      :string))
   (:label "Priority:"
      :default 10
    :type (:documentation
      "A numerical value which indicates the priority given to this index. 1 is the highest priority."
      :number))
   (:label "Documentation:"
    :default *ui-doc*
    :type (:documentation
      "Documentation for the index."
      :string))
  :label "Give parameters for DEFINE INDEX:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to define a secondary index on a relation."
      :keys ((#\SUPER-M #\HYPER-I)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'MODIFY-INDEX relation-name index-name
      (SETQ keywords (LIST 'new-name new-index-name 'key key-attributes 'sto storage-structure
     'priority priority 'doc doc
     )))))
  (MODIFY-INDEX relation-name index-name keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE STORAGE-STRUCTURE                          *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-storage-structure) (storage-structure doc
   &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'define-storage-structure)
     (FORMAT NIL "  ~S"
      (CONS
        'define-storage-structure
        (ARGLIST 'define-storage-structure))))
      :arguments (:user-supplied (:label "Storage structure name:"
    :default nil
    :type (:documentation
      "Name of the storage structure. Storage-structure-dependent routines are expected to be defined by the user."
      :sexp))
   (:label "Documentation:"
    :default nil
    :type (:documentation
      "Documentation for the storage structure."
      :string))
  :label "Give parameters for DEFINE STORAGE STRUCTURE:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define a storagestructure."
      :keys ((#\SUPER-D #\SUPER-S)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'DEFINE-STORAGE-STRUCTURE storage-structure
      (SETQ keywords (LIST 'doc doc
     )))))
  (DEFINE-STORAGE-STRUCTURE storage-structure keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE DOMAIN                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-domain) (domain def doc format
   &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'define-domain)
     (FORMAT NIL "  ~S"
      (CONS
        'define-domain
        (ARGLIST 'define-domain))))
      :arguments (:user-supplied (:label "Domain Name:"
    :default nil
    :type (:documentation
      "Name of the domain. Domain predicate is expected to be defined prior to this."
      :sexp))
   (:label "Default value:"
    :default nil
    :type (:documentation
     "Default value for this domain."
     :sexp))
   (:label "Documentation:"
    :default nil
    :type (:documentation
      "Documentation for the domain."
      :string))
   (:label "Default width :"
    :default nil
    :type (:documentation
      "The default width to be used for this domain."
      :sexp))
  :label "Give parameters for DEFINE DOMAIN:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define a domain."
      :keys (#\SUPER-HYPER-D))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'DEFINE-DOMAIN domain
      (SETQ keywords (LIST 'default def
     'doc doc
     'format format)))))
  (DEFINE-DOMAIN domain keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY DOMAIN                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-domain) (domain def doc format
   &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-domain)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-domain
        (ARGLIST 'modify-domain))))
      :arguments (:user-supplied (:label "Domain Name:"
    :default nil
    :type (:documentation
      "Name of the domain to be modified."
      :sexp))
   (:label "Default value:"
    :default nil
    :type (:documentation
     "New default value for this domain."
     :sexp))
   (:label "Documentation:"
    :default nil
    :type (:documentation
      "New documentation for the domain."
      :string))
   (:label "Default width :"
    :default nil
    :type (:documentation
      "The new default width to be used for this domain."
      :sexp))
  :label "Give parameters for MODIFY DOMAIN:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to modify a domain."
      :keys ((#\SUPER-M #\SUPER-D)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
  (LIST 'MODIFY-DOMAIN domain
      (SETQ keywords (LIST 'default def
     'doc doc
     'format format)))))
  (MODIFY-DOMAIN domain keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE TRANSACTION                                *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-transaction) (transaction forms dir path
      &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'define-transaction)
     (FORMAT NIL "  ~S"
      (CONS
        'define-transaction
        (ARGLIST 'define-transaction))))
      :arguments (:user-supplied (:label "Transaction Name:"
    :default *ui-transaction*
    :type (:documentation
      "Name of the transaction."
      :sexp))
   (:label "Database calls:"
    :default nil
    :type (:documentation
     "A list of database calls."
     :sexp))
   ,*ucl-dir*
   (:label "Pathname :"
    :default *ui-file*
    :type (:documentation
      "The default file in which it will be saved."
      :SEXP))
  :label "Give parameters for DEFINE TRANSACTION:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define a transaction."
      :keys ((#\SUPER-D #\SUPER-T)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'DEFINE-TRANSACTION transaction forms
      (SETQ keywords (LIST 'dir dir
     'path path)))))
  (DEFINE-TRANSACTION transaction forms keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY TRANSACTION                                *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-transaction) (transaction dir path
      &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-transaction)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-transaction
        (ARGLIST 'modify-transaction))))
      :arguments (:user-supplied (:label "Transaction Name:"
    :default *ui-transaction*
    :type (:documentation
      "Name of the transaction to be modified."
      :sexp))
   (:label "Directory:"
    :default *ui-directory*
    :type (:documentation
      "Default directory in which it can be found, if not in memory."
      :SEXP))
   (:label "Pathname :"
    :default *ui-file*
    :type (:documentation
      "The default file in which it can be found, if not in memory."
      :SEXP))
  :label "Give parameters for MODIFY TRANSACTION:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to modify a transaction."
      :keys ((#\SUPER-M #\SUPER-T)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'MODIFY-TRANSACTION transaction
      (SETQ keywords (LIST 'dir dir
     'path path)))))
  (MODIFY-TRANSACTION transaction keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE DATABASE                                   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-database) (database directory doc env
     &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'define-database)
     (FORMAT NIL "  ~S"
      (CONS
        'define-database
        (ARGLIST 'define-database))))
      :arguments (:user-supplied (:label "Database Name:"
    :default nil
    :type (:documentation
      "Name of the database."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (:documentation
     "Name of the save directory for this database."
     :sexp))
   (:label "Documentation:"
    :default *ui-doc*
    :type (:documentation
      "Documentation for the database."
      :string))
   (:label "Environment:"
    :default nil
    :type (:documentation
      "Name of the environment to be used to replace the default settings."
      :sexp))
  :label "Give parameters for DEFINE DATABASE:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define a database in a given directory."
      :keys ((#\SUPER-D #\SUPER-D)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'DEFDB database
      (SETQ keywords (LIST 'dir directory
     'doc doc
     'environment env)))))
  (DEFDB database keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY DATABASE                                   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-database) (database new-database directory doc
     &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-database)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-database
        (ARGLIST 'modify-database))))
      :arguments (:user-supplied (:label "Database Name:"
    :default nil
    :type (:documentation
      "Name of the database."
      :sexp))
   (:label "New Database Name:"
    :default nil
    :type (:documentation
      "If the database is to be renamed specify the new name."
      :sexp))
   (:label "Directory Name:"
    :default NIL
    :type (:documentation
     "To change the save directory for this database specify a new directory."
     :sexp))
   (:label "Documentation:"
    :default NIL
    :type (:documentation
      "New documentation for the database."
      :string))
  :label "Give parameters for MODIFY DATABASE:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to modify the features a database."
      :keys ((#\SUPER-M #\HYPER-D)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'MODIFY-DATABASE database
      (SETQ keywords (LIST 'database-name new-database
      'dir directory
     'doc doc
     )))))
  (MODIFY-DATABASE database keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY ATTRIBUTE                                  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-attribute) (relation attr new-attr def doc format
     &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-attribute)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-attribute
        (ARGLIST 'modify-attribute))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default *ui-relation*
    :type (:documentation
      "Name of the relation."
      :sexp))
   (:label "Attribute Name:"
    :default nil
    :type (:documentation
      "Name of the attribute."
      :sexp))
   (:label "New Attribute Name:"
    :default nil
    :type (:documentation
      "If the attribute is to be renamed specify the new name."
      :sexp))
   (:label "Default Value:"
    :default NIL
    :type (:documentation
     "To change the default value of this attribute specify a new value."
     :sexp))
   (:label "Documentation:"
    :default NIL
    :type (:documentation
      "New documentation for the attribute."
      :string))
   (:label "Default width :"
    :default nil
    :type (:documentation
      "The new default width to be used for this attribute."
      :sexp))
  :label "Give parameters for MODIFY ATTRIBUTE:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to modify the features a attribute."
      :keys ((#\SUPER-M #\SUPER-A)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'MODIFY-ATTRIBUTE relation attr
      (SETQ keywords (LIST 'attribute-name new-attr
     'def def
     'doc doc 'format format
     )))))
  (MODIFY-ATTRIBUTE relation attr keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY VIEW *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-view) (view def doc
     &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-view)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-view
        (ARGLIST 'modify-view))))
      :arguments (:user-supplied (:label "View Name:"
    :default NIL
    :type (:documentation
      "Name of the view."
      :sexp))
   (:label "View Definition:"
    :default nil
    :type (:documentation
      "New definition of the view."
      :sexp))
   (:label "Documentation:"
    :default NIL
    :type (:documentation
      "New documentation for the view."
      :string))
  :label "Give parameters for MODIFY VIEW:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to modify the features a view."
      :keys ((#\SUPER-M #\SUPER-V)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'MODIFY-VIEW view
      (SETQ keywords (LIST
     'view-def def
     'view-doc doc
     )))))
  (MODIFY-VIEW view keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY RELATION                                   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-relation) (rel new-rel add-att del-att ren-att
     imp sto format key dir doc
     &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-relation)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-relation
        (ARGLIST 'modify-relation))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default *ui-relation*
    :type (:documentation
      "Name of the Relation."
      :sexp))
   (:label "New Relation Name:"
    :default nil
    :type (:documentation
      "If the relation is to be renamed specify the new name."
      :sexp))
   (:label "Add attributes:"
    :default NIL
    :type (:documentation
     "Specify a list of attribute-descriptor pairs for attributes to be added to this relation."
     :sexp))
   (:label "Delete attributes:"
    :default NIL
    :type (:documentation
     "Specify a list of attributes in this relation which are to be deleted."
     :sexp))
   (:label "Rename attributes:"
    :default NIL
    :type (:documentation
     "To rename some of the attributes provide a list of the form (<old-attribute new-attribute>)."
     :sexp))
   (:label "Implementation Type:"
    :default NIL
    :type (:documentation
     "To change the implementation type of this relation specify a new value."
     :sexp))
   (:label "Storage structure:"
    :default NIL
    :type (:documentation
     "To change the storage structure of this relation specify a new value."
     :sexp))
   (:label "Format:"
    :default NIL
    :type (:documentation
     "To change the format for this relation specify a new format as a list of values."
     :sexp))
   (:label "Key:"
    :default NIL
    :type (:documentation
     "To change the key for this relation specify a new key as a list of attributes."
     :sexp))
   (:label "Directory Name:"
    :default NIL
    :type (:documentation
        "To change the save directory for this relation specify a new directory."
     :sexp))
   (:label "Documentation:"
    :default NIL
    :type (:documentation
      "New documentation for the relation."
      :string))
  :label "Give parameters for MODIFY RELATION:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to modify the features a relation."
      :keys ((#\SUPER-M #\SUPER-R)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'MODIFY-RELATION rel
      (SETQ keywords (LIST 'relation new-rel
     'add-attributes add-att
     'delete-attributes del-att
     'rename-attributes ren-att
     'imp imp
     'sto sto
     'format format
     'key key
     'doc doc
     'dir dir
     )))))
  (MODIFY-RELATION rel keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE ENVIRONMENT                                *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-environment) (environment save dir err par-check
        rel-imp rel-sto status sys-imp
        sys-sto val-check warn
        &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'define-environment)
     (FORMAT NIL "  ~S"
      (CONS
        'define-environment
        (ARGLIST 'define-environment))))
      :arguments (:user-supplied (:label "Environment Name:"
    :default nil
    :type (:documentation
      "Name of the environment."
      :sexp))
   (:label "Auto save:"
    :default nil
    :type (:documentation
     "Automatically saves all the modified relations after each function." :boolean))
   ,*ucl-dir*
   (:label "Errors:"
    :default T
    :type (:documentation
      "Controls the printing of the error messages."
      :boolean))
   (:label "Parameter Checking:"
    :default T
    :type (:documentation
      "Controls the checking of the parameters."
      :boolean))
   (:label "Relation Implementation:"
    :default *ui-imp*
    :type (:documentation
      "Default implementation of the user relations."
      :sexp))
   (:label "Relation storage structure:"
    :default *ui-ss*
    :type (:documentation
      "Default storage structure for the user relations."
      :sexp))
   (:label "Status:"
    :default T
    :type (:documentation
      "Controls the printing of the status messages."
      :boolean))
   (:label "System Implementation:"
    :default nil
    :type (:documentation
      "Default implementation of the system relations. Can not change this when a database is active."
      :sexp))
   (:label "System storage structure:"
    :default nil
    :type (:documentation
      "Default storage structure for the system relations. Can not change this when a database is active."
      :sexp))
   (:label "Validity Checking:"
    :default T
    :type (:documentation
      "Controls the checking of the values during insertion and modification for validity."
      :boolean))
   (:label "Warnings:"
    :default T
     :type (:documentation
      "Controls the printing of the warning messages."
      :boolean))
  :label "Give parameters for DEFINE ENVIRONMENT:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define an environment in a given directory."
      :keys ((#\SUPER-D #\SUPER-E)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'DEFENV environment
      (SETQ keywords (IF *active-db*
   (LIST 'auto-save save 'para par-check
     'dir dir 'rel-imp rel-imp 'rel-sto
     rel-sto 'errors err 'status status
     'validity val-check 'warnings warn)
        (LIST 'auto-save save 'para par-check
     'dir dir 'rel-imp rel-imp 'rel-sto
     rel-sto 'errors err 'status status
     'sys-imp sys-imp 'sys-sto sys-sto
     'validity val-check 'warnings warn))))))
  (DEFENV environment keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE RELATION                                   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-relation) (relation attr-des tup
     dir doc key imp ss &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'define-relation)
     (FORMAT NIL "  ~S"
      (CONS
        'define-relation
        (ARGLIST 'define-relation))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default nil
    :type (:documentation
     "Name of the relation to be defined."
     :sexp))
   ,*ucl-attr-desc*
   ,*ucl-format*
   ,*ucl-dir*
   ,*ucl-doci*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
     :label "Give parameters for DEFINE RELATION:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "used to define a relation."
      :keys ((#\SUPER-D #\SUPER-R)))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(LIST 'DEFREL
      relation attr-des
      (SETQ keywords
    (LIST 'tuple-format tup 'dir dir 'doc doc
  'key key 'imp imp 'sto ss)))))
  (DEFREL relation attr-des keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE VIEW *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-view) (viewname view-definition doc)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'define-view)
     (FORMAT NIL "  ~S"
      (CONS
        'define-view
        (ARGLIST 'define-view))))
      :arguments (:user-supplied (:label "View Name:"
    :default nil
    :type (:documentation
       "Specify a name for the view."
     :sexp))
   (:label "View Definition:"
    :default *ui-viewdef*
    :type (:documentation
       "Specify a definition for the view."
     :sexp))
   (:label "View Documentation:"
    :default nil
    :type (:documentation
       "Specify documentation for the view."
     :sexp))
 :label "Give parameters for DEFINE VIEW:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define a view."
      :keys ((#\SUPER-D #\SUPER-V)))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(LIST 'DEFVIEW viewname view-definition doc)))
  (DEFVIEW viewname view-definition doc))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE ATTRIBUTE                                  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-attribute) (relation-name attr-des key
      &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'define-attribute)
     (FORMAT NIL "  ~S"
      (CONS
        'define-attribute
        (ARGLIST 'define-attribute))))
      :arguments (:user-supplied (:label "Relation name: "
    :default *ui-relation*
    :type (:documentation
       "The name of the relation to which new attributes are to be added." :SEXP))
   ,*ucl-attr-desc*
   (:label "Key: "
    :default nil
    :type (:documentation
       "New key for the relation if it is to be different from the previous value. Specify a list of attributes."
       :SEXP))
 :label "Give parameters for DEFINE ATTRIBUTE:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to add attributes to relations."
      :keys ((#\SUPER-D #\SUPER-A)))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(LIST 'DEFINE-ATTRIBUTE relation-name attr-des
      (SETQ keywords (LIST 'key key)))))
  (DEFINE-ATTRIBUTE relation-name attr-des keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY TUPLES                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-tuples) (relation where-clause attributes values
     &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-tuples)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-tuples
        (ARGLIST 'modify-tuples))))
      :arguments (:user-supplied (:label "Relation: "
    :default *ui-relation*
    :type (:documentation
       "Specify the relation whose tuples are to be modified."
     :sexp))
   ,*ucl-where*
   (:label "Attributes: "
    :default *ui-attributes*
    :type (:documentation
       "Specify a list of attributes in the above relation to be modified." :sexp))
   (:label "Values: "
    :default *ui-values*
    :type (:documentation
       "Specify a corresponding list of values to modify the above attributes." :sexp))
 :label "Give parameters for MODIFY TUPLES ==>")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to modify tuples in a relation."
      :keys ((#\SUPER-M #\HYPER-M)))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(LIST 'MODIFY relation (SETQ keywords (LIST 'where where-clause
       'attr attributes
       'values values)))))
  (MODIFY relation keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DELETE TUPLES                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC delete-tuples) (relation where-clause)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'delete-tuples)
     (FORMAT NIL "  ~S"
      (CONS
        'delete-tuples
        (ARGLIST 'delete-tuples))))
      :arguments (:user-supplied (:label "Relation: "
    :default *ui-relation*
    :type (:documentation
       "Specify a relation whose tuples are to be deleted."
     :sexp))
   (:label "Where clause: "
    :default nil
    :type (:documentation
       "Deletes the tuples which satisfy this condition."
     :sexp))
 :label "Give parameters for DELETE TUPLES ==>")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to delete tuples in a relation."
      :keys (#\HYPER-D))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(LIST 'DELETE-TUPLES relation (LIST 'where where-clause))))
  (DELETE-TUPLES  relation (LIST 'where where-clause)))
;**************************************************************************
;                DEFCOMMAND  FOR RETRIEVE TUPLES                                   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC retrieve-tuples) (relation attributes where-clause
     into dir doc key imp sto
     qprint to-file sort
     format wide number print
     tuples qsort stream unique index-name
     &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'retrieve)
       (FORMAT NIL "  ~S"
      (CONS
        'retrieve
        (ARGLIST 'retrieve))))
      :arguments (:user-supplied ,*ucl-retrieve-rel*
   ,*ucl-attributes*
   ,*ucl-where*
   ,*ucl-into*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-qprint*
   ,*ucl-out*
   ,*ucl-sort*
   ,*ucl-format*
   ,*ucl-wide*
   ,*ucl-num*
   ,*ucl-print*
   ,*ucl-tuples*
   ,*ucl-quick-sort*
   ,*ucl-stream*
   ,*ucl-unique*
    ,*ucl-index-name*
 :label "Give parameters for RETRIEVE TUPLES ==>")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to Retrieve tuples in a relation."
      :keys (#\HYPER-R))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'RETRIEVE
      relation
      (SETQ keywords
    (LIST 'project
   (IF (EQUAL attributes T)
       NIL
     attributes)
   'where where-clause 'into into
   'dir dir 'doc doc 'key key 'imp imp 'sto sto
    'qprint (NOT qprint) 'output-to-file to-file
   'sort sort 'format format
   'wide wide 'num number
   'print print 'tuples tuples
   'quick-sort qsort 'stream stream
   'unique unique 'index-name index-name)))))
  (RETRIEVE relation keywords))
;**************************************************************************
;                DEFCOMMAND  FOR SELECT TUPLES                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC select) (relation where-clause
     into dir doc key imp sto
     qprint to-file sort
     format wide number print
     tuples qsort stream unique index-name
     &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'select-tuples)
     (FORMAT NIL "  ~S"
      (CONS
        'select-tuples
        (ARGLIST 'select-tuples))))
      :arguments (:user-supplied ,*ucl-retrieve-rel*
   ,*ucl-where*
   ,*ucl-into*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-qprint*
   ,*ucl-out*
   ,*ucl-sort*
   ,*ucl-format*
   ,*ucl-wide*
   ,*ucl-num*
   ,*ucl-print*
   ,*ucl-tuples*
   ,*ucl-quick-sort*
   ,*ucl-stream*
   ,*ucl-unique*
   ,*ucl-index-name*
 :label "Give parameters for SELECT TUPLES ==>")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to Select tuples in a relation."
      :keys ((#\SUPER-R #\SUPER-S)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'SELECT-TUPLES
      relation
      (SETQ keywords
    (LIST
   'where where-clause 'into into
   'dir dir 'doc doc 'key key 'imp imp 'sto sto
   'qprint (NOT qprint) 'output-to-file to-file
   'sort sort 'format format
   'wide wide 'num number
   'print print 'tuples tuples
   'quick-sort qsort 'stream stream
   'unique unique 'index-name index-name)))))
  (RETRIEVE relation (APPEND (LIST 'project nil) keywords)))
;**************************************************************************
;                DEFCOMMAND  FOR PROJECT TUPLES                                    *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC Project) (relation attributes
      into dir doc key imp sto
      qprint to-file sort
      format wide number print tuples
      qsort stream unique
      &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'project)
     (FORMAT NIL "  ~S"
      (CONS
        'project
        (ARGLIST
          'project))))
      :arguments (:user-supplied ,*ucl-retrieve-rel*
   ,*ucl-attributes*
   ,*ucl-into*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-qprint*
   ,*ucl-out*
   ,*ucl-sort*
   ,*ucl-format*
   ,*ucl-wide*
   ,*ucl-num*
   ,*ucl-print*
   ,*ucl-tuples*
   ,*ucl-quick-sort*
   ,*ucl-stream*
   ,*ucl-unique*
 :label "Give parameters for PROJECT TUPLES ==>")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to Project tuples in a relation."
      :keys ((#\SUPER-R #\SUPER-P)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'PROJECT
      relation
      (SETQ keywords
    (LIST 'project (IF (EQUAL attributes T)
   nil
        attributes)
    'into into 'dir dir 'doc doc 'key key 'imp imp 'sto sto
    'qprint (NOT qprint) 'output-to-file to-file
    'sort sort 'format format
    'wide wide 'num number 'print print 'tuples tuples
    'quick-sort qsort 'stream stream 'unique unique)))))
  (RETRIEVE relation (APPEND (LIST 'where t) keywords)))
;**************************************************************************
;                DEFCOMMAND  FOR COMMIT TRANSACTION                                *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC commit-transaction) (trans dir path &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'commit-transaction)
     (FORMAT NIL "  ~S"
      (CONS
        'commit-transaction
        (ARGLIST
          'commit-transaction))))
      :arguments (:user-supplied (:label "Name of the transaction :"
    :default *ui-transaction*
    :type (:documentation
       "The name of an existing transaction." :SEXP))
   (:label "Name of the directory:"
    :default *ui-directory*
    :type (:documentation
       "Name of the directory which contains the transaction file, if the transaction is not in the memory." :SEXP))
   (:label "Pathname:"
    :default *ui-file*
    :type (:documentation
    "If the transaction is not in memory, provide the pathname for the transaction file. It defaults to <transaction>.lisp." :SEXP))
 :label "Give parameters for COMMIT TRANSACTION")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Commit a transaction - execute all the database calls in it."
      :keys ((#\SUPER-T #\SUPER-C)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'COMMIT-TRANSACTION trans (SETQ keywords
         (LIST 'dir dir
        'path path)))))
  (COMMIT-TRANSACTION trans keywords)
)
;**************************************************************************
;                DEFCOMMAND  FOR JOIN        *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC join) (into from project where
      tuples format dir doc key imp sto
             print unique &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'join)
     (FORMAT NIL "  ~S"
      (CONS
        'join
        (ARGLIST
          'join))))
      :arguments (:user-supplied (:label "Output relation :"
    :default *ui-join-into*
    :type (:documentation
       "If not provided, the result of JOIN is stored in a temporary relation unless only the resultant tuples are requested." :SEXP))
   (:LABEL "FROM :"
    :DEFAULT *ui-from*
    :TYPE (:DOCUMENTATION
     "Specify a list of two relations to be joined." :SEXP))
   (:label "Project :"
    :default NIL
    :type (:documentation
       "This gives the attributes in the output relation. Example: (rel1.* a3 (rel2.a1 a4)) ==> All the attributes in rel1, attribute A3 of rel2 and atribute A1 of rel2 renamed as A4." :SEXP))
   (:label "Where :"
    :default *ui-over*
    :type (:documentation
     "The join clause using the theta-operators. It is a where clause consisting of attributes from the relations being joined." :SEXP))
   (:label "Tuples?"
    :default NIL
    :type (:documentation
     "Specify if the resultant tuples be returned rather than inserted in a relation. The following parameters can be ignored if this is true."
     :boolean))
   ,*ucl-format*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-print*
   ,*ucl-unique*
 :label "Give parameters for JOIN")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to join relations."
      :keys (#\SUPER-J))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'JOIN 'from from
      (SETQ keywords (LIST 'project project
     'into into
     'tuples tuples
     'format format
     'dir dir
     'doc doc
     'key key
     'imp imp
     'sto sto
     'print print
     'where where 'unique unique)))))
  (JOIN-INTERNAL (APPEND (LIST 'from from) keywords))
)
;**************************************************************************
;                DEFCOMMAND  FOR DESTROY DATABASE                                 *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC destroy-database) (database disk &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-database)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-database
        (ARGLIST
          'destroy-database))))
      :arguments (:user-supplied (:label "Database Name:"
    :default nil
    :type (:documentation
       "Name of the database to be destroyed." :SEXP))
   (:label "Delete from the DISK:"
    :default NIL
    :type (:documentation
     "IF YES all the files pertaining to this database are deleted but NOT EXPUNGED." :BOOLEAN))
 :label "Give parameters for DESTROY DATABASE:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy databases"
      :keys ((#\SUPER-K #\SUPER-D)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-DATABASE database
      (SETQ keywords (LIST 'disk disk)))))
  (DESTROY-DATABASE database keywords)
)
;**************************************************************************
;                DEFCOMMAND  FOR DESTROY DOMAIN                                    *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC destroy-domain) (domain)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-domain)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-domain
        (ARGLIST
          'destroy-domain))))
      :arguments (:user-supplied (:label "Domain Name:"
    :default nil
    :type (:documentation
       "Name of the domain to be destroyed." :SEXP))
 :label "Give parameters for DESTROY DOMAIN:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy domains."
      :keys (#\SUPER-HYPER-K))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-DOMAIN domain)))
  (DESTROY-DOMAIN domain))
;**************************************************************************
;                DEFCOMMAND  FOR DESTROY IMPLEMENTATION                            *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC destroy-implementation) (implementation)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-implementation)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-implementation
        (ARGLIST
          'destroy-implementation))))
      :arguments (:user-supplied (:label "Implementation Name:"
    :default nil
    :type (:documentation
       "Name of the implementation to be destroyed." :SEXP))
 :label "Give parameters for DESTROY IMPLEMENTATION:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy implementations."
      :keys ((#\SUPER-K #\SUPER-I)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-IMPLEMENTATION implementation)))
  (DESTROY-IMPLEMENTATION implementation))
;**************************************************************************
;                DEFCOMMAND  FOR DESTROY INDEX                            *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC DESTROY-INDEX) (relation-name index-name)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-index)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-index
        (ARGLIST
          'destroy-index))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default nil
    :type (:documentation
       "Name of the relation on which the index to be destroyed is defined." :SEXP))
   (:label "Index Name:"
    :default nil
    :type (:documentation
       "Name of the index to be destroyed." :SEXP))
     :label "Give parameters for DESTROY INDEX:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy indices."
      :keys ((#\SUPER-K #\HYPER-I)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-INDEX relation-name index-name)))
  (DESTROY-INDEX relation-name index-name))
;**************************************************************************
;                DEFCOMMAND  FOR DESTROY STORAGE STRUCTURE                         *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC destroy-storage-structure) (storage-structure)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-storage-structure)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-storage-structure
        (ARGLIST
          'destroy-storage-structure))))
      :arguments (:user-supplied (:label "Storage structure name:"
    :default nil
    :type (:documentation
       "Name of the storage structure to be destroyed." :SEXP))
 :label "Give parameters for DESTROY STORAGE STRUCTURE:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy storage structures."
      :keys ((#\SUPER-K #\SUPER-S)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-STORAGE-STRUCTURE storage-structure)))
  (DESTROY-STORAGE-STRUCTURE storage-structure))
;**************************************************************************
;                DEFCOMMAND  FOR DESTROY VIEW                                      *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC destroy-view) (view)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-view)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-view
        (ARGLIST
          'destroy-view))))
      :arguments (:user-supplied (:label "View name:"
    :default nil
    :type (:documentation
       "Name of the view to be destroyed."
       :SEXP))
 :label "Give parameters for DESTROY VIEW:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy views."
      :keys ((#\SUPER-K #\SUPER-V)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-VIEW view)))
  (DESTROY-VIEW view))
;**************************************************************************
;                DEFCOMMAND  FOR DESTROYREL   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC destroy-relation) (relation disk &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-relation)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-relation
        (ARGLIST
          'destroy-relation))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default nil
    :type (:documentation
       "Name of the relation to be destroyed." :SEXP))
   (:label "Delete from the DISK:"
    :default NIL
    :type (:documentation
     "IF YES the file corresponding to this relation is deleted but NOT EXPUNGED." :BOOLEAN))
 :label "Give parameters for DESTROY RELATION:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy relations"
      :keys ((#\SUPER-K #\SUPER-R)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-RELATION
      relation (SETQ keywords (LIST 'disk disk)))))
  (DESTROY-RELATION relation keywords)
)
;**************************************************************************
;                DEFCOMMAND  FOR DESTROY ATTRIBUTE                                 *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC destroy-attribute) (relation attr key &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-attribute)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-attribute
        (ARGLIST
          'destroy-attribute))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default nil
    :type (:documentation
       "Name of the relation from which attributes are to be destroyed." :SEXP))
   (:label "Attributes:"
    :default nil
    :type (:documentation
       "List of attributes to destroy." :SEXP))
   (:label "Key:"
    :default NIL
    :type (:documentation
     "New key for the relation if it is to be different from the previous value or if any of the key attributes are destroyed." :SEXP))
 :label "Give parameters for DESTROY ATTRIBUTE:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy attributes from relations"
      :keys ((#\SUPER-K #\SUPER-A)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-ATTRIBUTE relation (SETQ keywords (LIST 'attr attr
      'key key)))))
  (DESTROY-ATTRIBUTE relation keywords)
)
;**************************************************************************
;                DEFCOMMAND  FOR SET UNION   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC union) (from into tuples format
       dir doc key imp sto print unique
       &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'relation-union)
     (FORMAT NIL "  ~S"
       (CONS
        'relation-union
        (ARGLIST
          'relation-union))))
      :arguments (:user-supplied (:label "List of two relations:"
    :default NIL
    :type (:documentation
     "List of the names of two relations which will take part in the relation union operation. The attributes to be projected and a where clause can be specified for each relation using keywords. For instance, (REL1 (PROJECT <attr> WHERE <where-claue>) REL2 (WHERE <where-clause> PROJECT <attr>))." :SEXP))
   ,*ucl-into*
   (:label "Tuples?"
    :default NIL
    :type (:documentation
     "Specify if the resultant tuples be returned rather than inserted in a relation. The following parameters can be ignored if this is true."
     :boolean))
   ,*ucl-format*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-print*
   ,*ucl-unique*
  :LABEL "Parameters for the set-union of two relations")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to form union of two compatible relations"
      :keys ((#\SUPER-O #\SUPER-U)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'RELATION-UNION
      (SETQ keywords (LIST 'into into
     'from from 'tuples tuples
     'format format 'dir dir 'doc doc
     'key key 'imp imp 'sto sto
     'print print 'unique unique)))))
  (RELATION-UNION keywords))
;**************************************************************************
;                DEFCOMMAND  FOR SET DIFFERENCE                                    *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC difference) (from into tuples format
       dir doc key imp sto print unique
       &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'relation-difference)
     (FORMAT NIL "  ~S"
      (CONS
        'relation-difference
        (ARGLIST
          'relation-difference))))
      :arguments (:user-supplied (:label "List of two relations:"
    :default NIL
    :type (:documentation
     "List of the names of two relations which will take part in the relation difference operation. The attributes to be projected and a where clause can be specified for each relation using keywords. For instance, (REL1 (PROJECT <attr> WHERE <where-claue>) REL2 (WHERE <where-clause> PROJECT <attr>))." :SEXP))
   ,*ucl-into*
   (:label "Tuples?"
    :default NIL
    :type (:documentation
     "Specify if the resultant tuples be returned rather than inserted in a relation. The following parameters can be ignored if this is true."
     :boolean))
   ,*ucl-format*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-print*
   ,*ucl-unique*
  :LABEL "Parameters for the set-difference of two relations")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to form difference of two compatible relations"
      :keys ((#\SUPER-O #\SUPER-D)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'RELATION-DIFFERENCE
      (SETQ keywords (LIST 'into into
     'from from 'tuples tuples
     'format format 'dir dir 'doc doc
     'key key 'imp imp 'sto sto
     'print print 'unique unique)))))
  (RELATION-DIFFERENCE keywords))
;**************************************************************************
;                DEFCOMMAND  FOR SET INTERSECTION                                  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC intersection) (from into tuples format
       dir doc key imp sto print unique
       &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'relation-intersection)
     (FORMAT NIL "  ~S"
      (CONS
        'relation-intersection
        (ARGLIST
          'relation-intersection))))
      :arguments (:user-supplied (:label "List of two relations:"
    :default NIL
    :type (:documentation
     "List of the names of two relations which will take part in the relation intersection operation. The attributes to be projected and a where clause can be specified for each relation using keywords. For instance, (REL1 (PROJECT <attr> WHERE <where-claue>) REL2 (WHERE <where-clause> PROJECT <attr>))." :SEXP))
   ,*ucl-into*
   (:label "Tuples?"
    :default NIL
    :type (:documentation
     "Specify if the resultant tuples be returned rather than inserted in a relation. The following parameters can be ignored if this is true."
     :boolean))
   ,*ucl-format*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-print*
   ,*ucl-unique*
  :LABEL "Parameters for the set-intersection of two relations")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to form intersection of two compatible relations"
      :keys ((#\SUPER-O #\SUPER-I)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'RELATION-INTERSECTION
      (SETQ keywords (LIST 'into into
     'from from 'tuples tuples
     'format format 'dir dir 'doc doc
     'key key 'imp imp 'sto sto
     'print print 'unique unique)))))
  (RELATION-INTERSECTION keywords))
;**************************************************************************
;                DEFCOMMAND  FOR AVERAGE     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC average) (relation attribute unique where by tuples
      &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'average)
     (FORMAT NIL "  ~S"
      (CONS
        'average
        (ARGLIST
          'average))))
      :arguments (:user-supplied (:label "Relation name:"
    :default *ui-relation*
    :type (:documentation
     "Name of the relation which contains the attribute to be averaged." :SEXP))
   ,*ucl-count-attr*
   ,*ucl-count-unique*
      ,*ucl-where*
   ,*ucl-by*
   ,*ucl-tuples*
  :LABEL "Parameters for average:")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to compute the average of the attribute values in a relation."
      :keys ((#\SUPER-O #\SUPER-A)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'AVERAGE relation attribute
      (SETQ keywords (LIST 'unique unique
     'where where 'by by 'tuples tuples)))))
  (AVERAGE relation attribute keywords))
;**************************************************************************
;                DEFCOMMAND  FOR SUM         *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC sum) (relation attribute unique where by tuples
  &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'sum)
     (FORMAT NIL "  ~S"
      (CONS
        'sum
        (ARGLIST
          'sum))))
      :arguments (:user-supplied (:label "Relation name:"
    :default *ui-relation*
    :type (:documentation
     "Name of the relation which contains the attribute to be summed." :SEXP))
   ,*ucl-count-attr*
   ,*ucl-count-unique*
   ,*ucl-where*
   ,*ucl-by*
   ,*ucl-tuples*
  :LABEL "Parameters for sum:")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to compute the sum of the attribute values in a relation."
      :keys ((#\SUPER-O #\SUPER-S)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'SUM relation attribute
      (SETQ keywords (LIST 'unique unique 'by by 'tuples tuples
     'where where)))))
  (SUM relation attribute keywords))
;**************************************************************************
;                DEFCOMMAND  FOR SIZE        *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC size) (relation unique where by tuples &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'size)
     (FORMAT NIL "  ~S"
      (CONS
        'size
        (ARGLIST
          'size))))
      :arguments (:user-supplied (:label "Relation name:"
    :default *ui-relation*
    :type (:documentation
     "Name of the relation whose size is required." :SEXP))
   ,*ucl-count-unique*
   ,*ucl-where*
   ,*ucl-by*
   ,*ucl-tuples*
  :LABEL "Parameters for size:")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to compute the size of the relation."
      :keys (#\SUPER-HYPER-S))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'SIZE relation
      (SETQ keywords (LIST 'unique unique 'by by 'tuples tuples
     'where where)))))
  (SIZE relation keywords))
;**************************************************************************
;                DEFCOMMAND  FOR COUNT-RTMS     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC count) (relation attribute unique where by tuples
         &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'count-rtms)
     (FORMAT NIL "  ~S"
      (CONS
        'count-rtms
        (ARGLIST
          'count-rtms))))
      :arguments (:user-supplied (:label "Relation name:"
    :default *ui-relation*
    :type (:documentation
     "Name of the relation which contains the attribute to be used to find the number of tuples." :SEXP))
   ,*ucl-count-attr*
   ,*ucl-count-unique*
   ,*ucl-where*
   ,*ucl-by*
   ,*ucl-tuples*
  :LABEL "Parameters for count:")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to compute the count of the attribute values in a relation."
      :keys ((#\SUPER-O #\SUPER-C)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'COUNT-RTMS relation attribute
      (SETQ keywords (LIST 'unique unique 'by by 'tuples tuples
     'where where)))))
  (COUNT-RTMS relation attribute keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MAXIMUM     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC maximum) (relation attribute where by tuples
      &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'maximum)
     (FORMAT NIL "  ~S"
      (CONS
        'maximum
        (ARGLIST
          'maximum))))
      :arguments (:user-supplied (:label "Relation name:"
    :default *ui-relation*
    :type (:documentation
     "Name of the relation which contains the attribute to be maximumd." :SEXP))
   ,*ucl-count-attr*
   ,*ucl-where*
   ,*ucl-by*
   ,*ucl-tuples*
  :LABEL "Parameters for maximum:")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to compute the maximum of the attribute values in a relation."
      :keys ((#\SUPER-O #\SUPER-M)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'MAXIMUM relation attribute
      (SETQ keywords (LIST 'where where 'by by 'tuples tuples)))))
  (MAXIMUM relation attribute keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MINIMUM     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC minimum) (relation attribute where by tuples
      &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'minimum)
     (FORMAT NIL "  ~S"
      (CONS
        'minimum
        (ARGLIST
          'minimum))))
      :arguments (:user-supplied (:label "Relation name:"
    :default *ui-relation*
    :type (:documentation
     "Name of the relation which contains the attribute to be minimumd." :SEXP))
   ,*ucl-count-attr*
   ,*ucl-where*
   ,*ucl-by*
   ,*ucl-tuples*
  :LABEL "Parameters for minimum:")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to compute the minimum of the attribute values in a relation."
      :keys (#\SUPER-HYPER-M))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'MINIMUM relation attribute
      (SETQ keywords (LIST 'where where 'by by 'tuples tuples)))))
  (MINIMUM relation attribute keywords))
;**************************************************************************
;                DEFCOMMAND  FOR HELP DBMS OBJECT                                  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC inspect-dbms-object) (object)
            `(:description "Information on any database object"
      :arguments (:user-supplied (:label "Database Object:"
    :default *ui-object*
    :type (:documentation
     "Specify a database object (COMMAND / RELATION / ATTRIBUTE)."
     :sexp))
  :LABEL "Help on the database object ->")
      :menus help
      :documentation "Used to inspect any database object."
      :keys (#\CONTROL-HELP))
  (SEND *output-window* :append-item
(FORMAT nil "(INSPECT-DBMS-OBJECT '~S)" object))
  (HELP-OBJECT object))
;**************************************************************************
;                DEFCOMMAND  FOR REFRESH OUTPUT WINDOW                             *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC clear-output-window) ()
    `(:description "Clear the entire output window"
      :menus display
      :keys (#\CLEAR-SCREEN))
  (SEND *output-window* :set-items nil)
  (FUNCALL *OUTPUT-WINDOW* :SCROLL-TO
   (- 2 (W:SHEET-NUMBER-OF-INSIDE-LINES *OUTPUT-WINDOW*))
   :RELATIVE))
;**************************************************************************
;                DEFCOMMAND  FOR SCROLL DOWN *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC scroll-forward) ()
   `(:description "scrolling forward in the output-window"
     :menus display
     :keys (#\CONTROL-V))
  (FUNCALL *OUTPUT-WINDOW* :SCROLL-TO
   (- (W:SHEET-NUMBER-OF-INSIDE-LINES *OUTPUT-WINDOW*) 2)
   :RELATIVE))
;**************************************************************************
;                DEFCOMMAND  FOR SCROLL UP   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC scroll-backward) ()
   `(:description "scrolling backward in the output-window"
     :menus display
     :keys (#\META-V))
  (FUNCALL *OUTPUT-WINDOW* :SCROLL-TO
   (- 2 (W:SHEET-NUMBER-OF-INSIDE-LINES *OUTPUT-WINDOW*))
   :RELATIVE))
;**************************************************************************
;                DEFCOMMAND  FOR SCROLL TO THE TOP                                 *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC scroll-to-top) ()
   `(:description "scrolling to the top in the output-window"
     :menus display
     :keys (#\META-<))
  (SEND *OUTPUT-WINDOW* :put-item-in-window
(SEND *OUTPUT-WINDOW* :item-of-number 0)))
;**************************************************************************
;                DEFCOMMAND  FOR SCROLL TO THE BOTTOM                              *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC scroll-to-bottom) ()
   `(:description "scrolling to the bottom in the output-window"
     :menus display
     :keys (#\META->))
  (SEND *OUTPUT-WINDOW* :put-last-item-in-window))
;**************************************************************************
;                DEFCOMMAND  FOR SCROLL TO A RELATION                              *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC scroll-to-a-relation) (relation &aux index)
     `(:description "Scroll to a particular relation"
       :arguments (:user-supplied (:label "Relation Name:"
    :default *ui-relation*
    :type (:documentation
     "Name of the relation to scroll to:"
     :sexp))
   :label "Scroll to the relation ==>")
       :menus display
       :keys (#\CONTROL-R))
  (IF (AND (SETQ index (GETP relation :index))
   (< index (LENGTH (SEND *output-window* :items))))
      (SEND *output-window* :put-item-in-window
    (SEND *output-window* :item-of-number index))
    (FORMAT *typeout-window* "~%The relation ~S is not in the output-window"
    relation)))
;**************************************************************************
;                DEFCOMMAND  FOR SEND OUTPUT TO A FILE                             *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC send-output-to-file) (file &AUX pathname)
     `(:description "Send the contents of the output window to a file."
       :arguments (:user-supplied (:label "File name:"
       :default *ui-file*
    :type (:documentation
     "Name of the file to send the output to:" :sexp))
   :label "Send the output window contents to:")
       :menus display
       :keys (#\HYPER-F))
  (UNWIND-PROTECT
      (SETQ pathname (CAR (ERRSET
  (OPEN (SETQ pathname file) :characters t
                     :direction :output        ;mrr 04.09.87
                     :if-does-not-exist :create) nil)))
    (IF (null pathname)
(FORMAT *typeout-window* "~S is a bad file." file)
      (MAPCAR (FUNCTION (LAMBDA (line &AUX item)
       (COND ((OR (STRINGP line)
  (NUMBERP line)
  (SYMBOLP line))
      (PRINC line pathname))
     ((LISTP line)
      (DOLIST (element line)
(COND ((OR (STRINGP element)
    (NUMBERP element)
    (SYMBOLP element))
       (PRINC element pathname))
      ((NULL (LISTP element)) nil)
      ((NULL (EQUAL (CAR element) :item1))
       (PRINC (CAR element) pathname))
      (T (SETQ item (CADR element))
  (PRINC
    (IF (LISTP item)
        (CAR item)
      item)
    pathname)
  )))))
       (TERPRI pathname)))
      (LISTARRAY (SEND *output-window* :items)))))
  (IF pathname
      (CLOSE pathname)))
;**************************************************************************
;                DEFCOMMAND  FOR INTRODUCTION                                      *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC introduction) ()
    `(:description "Introduction to this interface."
      :menus help
      :keys (#\META-HELP))
  (HELP))

;**************************************************************************
;                DEFCOMMAND  FOR SUB-MENU DBMS HELP                                *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC help) ()
    `(:description "Introduction to the interface. Help on any database object (COMMAND / RELATION / ATTRIBUTE)."
      :documentation "Introduction to the interface. Help on any database object (COMMAND/RELATION/ATTRIBUTE)."
      :menus system-menu)
  (LET ((command (SEND SELF :submenu-choose *help-submenu*)))
    (IF command (SEND command :execute SELF))))
;**************************************************************************
;                DEFCOMMAND  FOR SUB-MENU DBMS COMMANDS                            *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC command-menu) ()
    `(:description "Select a database command from a menu. A choose-variable-values window will be presented to get the arguments for that command."
      :documentation "Select a database command from a menu. A choose-variable-values window will be presented to get the arguments for that command."
      :menus system-menu
      :keys (#\mouse-r-1))
  (LET ((command (SEND SELF :submenu-choose *command-submenu*)))
    (IF command (SEND command :execute SELF))))
;**************************************************************************
;                DEFCOMMAND  FOR SUB-MENU DISPLAY COMMANDS                         *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC display) ()
    `(:description "Select an item from a menu to scroll in the output window."
      :documentation "Select an item from a menu to scroll in the output window."
      :menus system-menu)
  (LET ((command (SEND SELF :submenu-choose *display-submenu*)))
    (IF command (SEND command :execute SELF))))
;**************************************************************************
;                DEFCOMMAND  FOR EXIT        *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC kill) ()
     `(:description "To exit the interface by killing the process."
      :documentation "To exit the interface by killing the process."
      :menus system-menu
      :keys (#\SUPER-END))
  (SEND dbms-frame1 :kill)
  (SETQ dbms-frame1 nil))
;**************************************************************************
;                DEFCOMMAND  FOR QUIT        *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC exit) ()
     `(:description "To exit the interface by burying it."
      :documentation "To exit the interface by burying it."
      :menus system-menu
      :keys (#\END))
  (SEND dbms-frame1 :bury))
;**************************************************************************
;              Build the command table       *
;**************************************************************************
(SETQ dbms-comtab (MAKE-INSTANCE 'UCL:COMMAND-TABLE
     :name "Database command table"
     :documentation "database help"))
(UCL:BUILD-COMMAND-TABLE 'dbms-comtab 'dbms-rc
 '(help command-menu display kill exit
   delete-tuples destroy-attribute
                 destroy-database destroy-relation
  destroy-domain destroy-implementation destroy-index
  destroy-storage-structure destroy-view
  modify-database modify-transaction
  modify-domain modify-relation
  modify-attribute modify-index modify-view modify-tuples
   union intersection difference join
         retrieve-tuples select project
 commit-transaction average sum count size maximum
 minimum
   define-view define-database define-relation
         define-attribute define-environment
 define-implementation define-storage-structure
 define-domain define-transaction define-index
 attach-relation detach-relation insert-tuples
         load-database load-relation load-environment
   maptuple print-relation save-database save-relation
         save-environment save-transaction
         active-database environment-status
 rename-attribute rename-relation
 rename-database mapt abort-transaction
 begin-transaction  end-transaction
   inspect-dbms-object introduction
   scroll-forward clear-output-window
   scroll-to-top scroll-to-bottom
   scroll-backward scroll-to-a-relation send-output-to-file))
;**************************************************************************
;            Init method to define the submenus COMMAND-MENU HELP                 *
;            DISPLAY as part of the system menu.                                  *
;**************************************************************************

(DEFMETHOD (dbms-rc :after :init) (&rest ignore)
  (declare (special command-menu))
  (SETQ
   *help-submenu*  (MAKE-INSTANCE 'W:menu
   :pop-up t
   :dynamic t
   :superior W:mouse-sheet
                                  :item-list-pointer 'help)
   *command-submenu*  (MAKE-INSTANCE 'W:menu
      :pop-up t
      :dynamic t
      :multicolumn t
                                     :superior W:mouse-sheet
                                     :column-spec-list command-menu)
   *display-submenu* (MAKE-INSTANCE 'W:menu
        :pop-up t
     :dynamic t
                                    :superior W:mouse-sheet
                                    :item-list-pointer 'display))
  (SETQ *menupane* (SEND SELF :get-pane 's-m-pane))
  (SEND *menupane* :set-item-list-pointer 'system-menu)
  (SEND *menupane* :update-item-list)
  (SETQ *interaction* (FUNCALL self :get-pane 'i-pane)
      *output-window* (FUNCALL self :get-pane 'o-pane))
  (SEND self :set-selection-substitute rtms:*interaction*)
  )
;**************************************************************************
;  Build the submenus.                       *
;**************************************************************************
(UCL:BUILD-MENU 'system-menu 'dbms-rc
:item-list-order '(help kill command-menu exit display))
(UCL:BUILD-MENU 'help 'dbms-rc :item-list-order
'(introduction inspect-dbms-object))
(UCL:BUILD-MENU 'display 'dbms-rc :item-list-order
'(scroll-to-top scroll-backward clear-output-window
  send-output-to-file
  scroll-to-a-relation ucl:display-command-tables
  ucl:edit-command-tables scroll-forward scroll-to-bottom))
(PUTPROP 'command-menu '(dbms-comtab) 'ucl:items)
(UCL:BUILD-MENU 'command-menu 'dbms-rc
:item-list-order
'(define-database define-relation define-view define-attribute
  define-index define-environment define-domain define-transaction
  define-implementation define-storage-structure
  attach-relation detach-relation
  load-database load-relation load-environment
  insert-tuples delete-tuples modify-tuples
  modify-database modify-relation modify-attribute
  modify-index modify-domain modify-transaction modify-view
                  destroy-database destroy-relation destroy-attribute
  destroy-domain destroy-implementation destroy-index
  destroy-storage-structure destroy-view
  retrieve-tuples join union intersection difference
  select project commit-transaction
  average sum size count maximum minimum
  print-relation save-database save-relation
  save-environment save-transaction maptuple mapt
  active-database environment-status
  rename-attribute rename-relation
  rename-database abort-transaction
  begin-transaction end-transaction)
:column-list-order
'(("Definition" :FONT FONTS:hl12bi)
  ("Manipulation" :FONT FONTS:hl12bi)
  ("Operators" :FONT FONTS:hl12bi)
  ("Other Features" :FONT FONTS:hl12bi)))
;**************************************************************************
;         Define the variable to hold the instance of the application flavor.      *
;**************************************************************************
(SETQ dbms-frame1 nil)
;**************************************************************************
;         Method used to get input from submenus.                                  *
;**************************************************************************
(DEFMETHOD (dbms-rc :submenu-choose) (submenu)
  (LET ((sup (SEND submenu :superior)))
    (UNWIND-PROTECT
      (PROGN
(SEND (CAR (SEND *interaction* :blinker-list)) :set-visibility NIL)
(SEND submenu :set-superior W:mouse-sheet)
(SEND submenu :choose))
      (SEND submenu :set-superior sup)
      (SEND (CAR (SEND *interaction* :blinker-list)) :set-visibility :blink)
      )))
;**************************************************************************
;                              Some initializations                                *
;**************************************************************************
(DEFMETHOD (dbms-rc :before :command-loop) ()
    (SETQ *typeout-window* (FUNCALL *output-window* :typeout-window))
    (SEND *interaction* :clear-screen)
    (SEND *output-window* :clear-screen)
    (SEND *typeout-window* :set-io-buffer
  (SEND *interaction* :io-buffer)))
;**************************************************************************
;    Method to be executed before each time it enters the command-loop. Used       *
;    to refresh the output window if its typeout window is exposed.                *
;**************************************************************************

(DEFMETHOD (dbms-rc :before :fetch-and-execute) (&rest ignore)
  (DECLARE (SPECIAL ch))
  (IF (SEND *typeout-window* :active-p)
      (PROGN
(FORMAT *typeout-window* "~%")
(FORMAT *typeout-window* "~%")
(FORMAT *typeout-window*
w:*remove-typeout-standard-message*)   ;mrr 04.07.87
(SETQ ch (FUNCALL dbms-frame1 :any-tyi))
(SEND *output-window* :flush-typeout))))
;(SEND dbms-frame1 :set-basic-help '(help))
;(SEND dbms-frame1 :set-print-function 'NEW-PRINT)
(DEFUN NEW-PRINT (x &AUX ch)
  (IF (SEND *typeout-window* :active-p)
      (PROGN
(FORMAT *typeout-window* "~%")
(FORMAT *typeout-window* "~%")
(FORMAT *typeout-window*
w:*remove-typeout-standard-message*)   ;mrr 04.07.87
(SETQ ch (FUNCALL dbms-frame1 :any-tyi))
(SEND *output-window* :flush-typeout)))
  (SEND *output-window* :append-item (FORMAT nil "~S" x)))
(DEFMETHOD (dbms-rc :before :execute-command) (&rest ignore)
;  (setq ucl:inhibit-results-print? T)
  (IF (EQ ucl:input-mechanism 'ucl:typein)
      (SEND *output-window* :append-item (FORMAT nil "~S" -))))
(DEFMETHOD (dbms-rc :after :execute-command) (&rest ignore &AUX ch)
  (IF (SEND *typeout-window* :active-p)
      (PROGN
(FORMAT *typeout-window* "~%")
(FORMAT *typeout-window* "~%")
(FORMAT *typeout-window*
w:*remove-typeout-standard-message*)   ;mrr
(SETQ ch (FUNCALL dbms-frame1 :any-tyi))
(SEND *output-window* :flush-typeout)))
  '(MAPC #'(LAMBDA (val)
    (IF val
(PROGN
  (SEND *output-window* :append-item (FORMAT NIL "~S" val))
  (SEND *output-window* :put-last-item-in-window))))
//)
  )

;**************************************************************************
;      Sets the I/O streams the appropriate panes in the interface.                *
;**************************************************************************
(DEFMETHOD (dbms-rc :designate-io-streams) ()
  (DECLARE (special *standard-output* error-output debug-io
    *terminal-io*))
  (SETQ *terminal-io* *interaction*
*standard-output* *interaction*
error-output *typeout-window*
debug-io *typeout-window*))
;**************************************************************************
;      The function to be called from lisp-listener to get use the interface.      *
;**************************************************************************
(COMPILE-FLAVOR-METHODS dbms-rc)
(DEFUN Interface (&rest ignore)
  (IF (W:FIND-WINDOW-OF-FLAVOR 'RTMS:dbms-rc)
      dbms-frame1
    (SETQ dbms-frame1 (W:MAKE-WINDOW 'RTMS:dbms-rc)))
  (SEND dbms-frame1 :expose)
  (SEND *interaction* :select))
;**************************************************************************
;             Add the database interface to the system keys and system menu      *
;**************************************************************************
(DEFUN CREATE-KEYS ()
  (W:ADD-SYSTEM-KEY #\D 'RTMS:dbms-rc
     "Rtms Interface"
     '(RTMS:interface))
  (W:ADD-TO-SYSTEM-MENU-COLUMN :PROGRAMS
    "RTMS" '(RTMS:interface) "Rtms interface"))
(CREATE-KEYS)
;**************************************************************************
;              Function used to scroll down in the output window.                  *
;**************************************************************************
(DEFUN scroll-to-bottom ()
  (SEND *output-window* :append-item " ")
  (SEND *output-window* :put-last-item-in-window)
  (FUNCALL *OUTPUT-WINDOW* :SCROLL-TO
   (- (W:SHEET-NUMBER-OF-INSIDE-LINES *OUTPUT-WINDOW*) 2)
   :RELATIVE))
;**************************************************************************
;           Function used to print items in the output window.                     *
;**************************************************************************
(DEFUN DBMS-PRINTER (line arg stream item-no)
  (LET (item)  ;item was declared special locally in Rel 2 -mrr
  arg
  item-no
  (COND ((STRINGP line) (PRINC line stream))
((NUMBERP line) (PRINC line stream))
((SYMBOLP line) (PRINC line stream))
((LISTP line)
   (DOLIST (element line)
     (COND ((STRINGP element) (PRINC element stream))
   ((SYMBOLP element) (PRINC element stream))
   ((NUMBERP element) (PRINC element stream))
   ((NULL (LISTP element)) nil)
   ((NULL (EQUAL (CAR element) :item1))
    (IF (STRINGP (CAR element))
(PRINC (CAR element) stream)
(PRIN1 (CAR element) stream)))
   (T (SETQ item (CADR element))
      (FUNCALL stream :item1 item (CADDR element)
       #'(LAMBDA (item stream)
    (PRINC
      (IF (LISTP item)
   (CAR item)
        item)
      stream))))))))))
;**************************************************************************
;               Functions used to provide help on line-area scrolling.            *
;**************************************************************************
(DEFUN HELP-LINE-AREA (line &AUX item)
  (COND ((OR (STRINGP line)
     (NUMBERP line)
     (SYMBOLP line))
 (PRINC line *TYPEOUT-WINDOW*))
((LISTP line)
   (DOLIST (element line)
     (COND ((OR (STRINGP element)
(NUMBERP element)
(SYMBOLP element))
    (PRINC element *TYPEOUT-WINDOW*))
   ((NULL (LISTP element)) nil)
   ((NULL (EQUAL (CAR element) :item1))
    (IF (STRINGP (CAR element))
(PRINC (CADR element) *typeout-window*)
(PRIN1 (CADR element) *typeout-window*)))
   (T (SETQ item (CADR element))
      (PRINC
(IF (LISTP item)
    (CADR item)
  item)
*TYPEOUT-WINDOW*)
      )))
   (FORMAT *typeout-window* "~%")
   (FORMAT *typeout-window* "~%")
   (FORMAT *typeout-window*
w:*remove-typeout-standard-message*)   ;mrr 04.07.87
   (SEND dbms-frame1 :any-tyi)
   (SEND *output-window* :flush-typeout))))
(DEFUN HELP-LINE-AREA-DEL (line &AUX items item-number mod-relation
             mod-attributes num)
  (SETQ item-number (SEND *output-window* :number-of-item line))
  (MAPC (FUNCTION (LAMBDA (rel &AUX numbers)
  (IF (AND
(SETQ numbers (GETP (READ-FROM-STRING
      (STRING-APPEND *pkg-name*
       (CAR rel)))
    'items))
(>= item-number (CAR numbers))
(<= item-number (CADR numbers))
)
      (PROGN
(SETQ num numbers)
(SETQ mod-relation (READ-FROM-STRING
      (STRING-APPEND *pkg-name* (CAR rel))) ;mrr 04.06.87
      mod-attributes (CADR rel))
))))
(QTRIEVE 'system-relation
 *system-relation-attributes*
 '(relation-name attributes)
 *system-relation-key*
 t))
  (IF mod-relation
      (PROGN
(IF (W:MOUSE-CONFIRM "Delete the indicated tuple?")
    (PROGN
      (DOLIST (element line)
(IF (LISTP element)
    (SETQ items (APPEND items (CDR element)))))
      (IF (>
    (CADR
      (MULTIPLE-VALUE-LIST
      (DELETE-TUPLES mod-relation
      'where (CONS 'AND
    (MAPCAR (FUNCTION (LAMBDA (attr val)
       (LIST 'EQUAL (READ-FROM-STRING (STRING attr))
      `(QUOTE
         ,(READ-FROM-STRING val)))))
     mod-attributes
     items)))))
    0)
  (PROGN
    (SEND *output-window* :delete-item item-number)
    (PUTP mod-relation
     (LIST (CAR num) (- (CADR num) 1))
     'items)))
      )))))
(DEFUN HELP-LINE-AREA-MOD (line
   &AUX items item-number attribute-vars mod-tuple
   mod-relation mod-attributes blanks tuple-format tuple)
  (BLOCK nil
  (SETQ item-number (SEND *output-window* :number-of-item line))
  (MAPC (FUNCTION (LAMBDA (rel &AUX numbers)
  (IF (AND
(SETQ numbers (GETP (READ-FROM-STRING
      (STRING-APPEND *pkg-name*
       (CAR rel)))
    'items))
(>= item-number (CAR numbers))
(<= item-number (CADR numbers))
)
      (SETQ mod-relation (READ-FROM-STRING     ;mrr 04.06.87
      (STRING-APPEND *pkg-name* (CAR rel)))
    mod-attributes (CADR rel)))))
(QTRIEVE 'system-relation
 *system-relation-attributes*
 '(relation-name attributes)
 *system-relation-key*
 t))
  (IF mod-relation
      (PROGN
(DOLIST (element line)
  (IF (LISTP element)
      (PROGN
(SETQ tuple-format (APPEND tuple-format
     (LIST (LENGTH (CAR element)))))
(SETQ items (APPEND items (CDR element))))))
(SETQ blanks
      (MAKE-ARRAY
(+ 1 (LENGTH mod-attributes)
   (APPLY (FUNCTION +) tuple-format)) :type 'art-string
:initial-value 32))
(SETQ attribute-vars
      (MAPCAR (FUNCTION (LAMBDA (attr)
       (READ-FROM-STRING (STRING-APPEND "MOD" attr))))
      mod-attributes))
(MAPC (FUNCTION (LAMBDA (attr val)
                  (SET attr (READ-FROM-STRING val))))
      attribute-vars
      items)
(SETQ *line-area-values-modifiedp* nil)
(IF (CATCH 'abort
      (W:CHOOSE-VARIABLE-VALUES
(MAPCAR (FUNCTION (LAMBDA (var attr)
     (LIST var (STRING attr))))
attribute-vars
mod-attributes)
:label (FORMAT nil "Modify the relation: ~S" mod-relation)
:function 'line-area-domain-check
:margin-choices '("Do It" ("Abort" (THROW 'abort T)))))        ;mrr 04.06.87
    (setq  *line-area-values-modifiedp* NIL))
(IF *line-area-values-modifiedp*
    (PROGN
      (SETQ tuple (MAPCAR (FUNCTION (LAMBDA (x)
        `(QUOTE ,(SYMBOL-VALUE x))))
   attribute-vars))
      (SETQ mod-tuple
    (CAR (PRINT-TUPLE (LIST
  (MAPCAR (FUNCTION (LAMBDA (x)
        (eval `,x)))
   tuple))
       tuple-format nil T blanks nil)))
      (IF (>
    (CADR
      (MULTIPLE-VALUE-LIST
      (MODIFY mod-relation 'attributes mod-attributes
'values tuple
'where (CONS 'AND
        (MAPCAR (FUNCTION (LAMBDA (attr val)
      (LIST 'EQUAL (READ-FROM-STRING
       (STRING-APPEND *pkg-name*    ;mrr
        (STRING attr)))
     `(QUOTE
        ,(READ-FROM-STRING
           val)))))
         mod-attributes
         items)))))
    0)
  (PROGN
     (SEND *output-window* :delete-item item-number)
    (SEND *output-window* :insert-item item-number mod-tuple)))))))))
(DEFUN line-area-domain-check (&rest ignore)
  (BLOCK nil
;Later on, we will have to take the arguments window, variable, old-value, and
;new-value (see page 195. bottom) inorder to do the domain check for this attribute
;But we probably should not do the domain checking here because it will be done
;anyway in the modify routine.
(SETQ *line-area-values-modifiedp* T)
(RETURN nil)))


(DEFUN HELP-MODIFY (relation &rest ignore
    &AUX qtrieve)
  (DECLARE (SPECIAL new-rel relation
    old-att  new-add new-del new-ren old-add old-del old-ren
    old-imp  new-imp
    old-sto  new-sto
    old-key  new-key
    old-dir  new-dir
    old-doc  new-doc
    old-tup  new-tup))
  (BLOCK nil
    (SETQ qtrieve (CADR (GET-RELATION
relation
'(attributes save-directory doc tuple-format
  implementation-type storage-structure key)
T)))
    (COND ((NULL (CADR qtrieve))
   (IF *provide-error-messages*
       (FORMAT *STANDARD-OUTPUT*
     "~%ERROR - Relation ~s does not exist in the database ~s"
     relation *active-db*))
   (RETURN NIL)))
  (SETQ old-att (FIRST qtrieve)
old-dir (SECOND qtrieve)
old-doc (THIRD qtrieve)
old-tup (FOURTH qtrieve)
old-imp (FIFTH qtrieve)
old-sto (SIXTH qtrieve)
old-key (SEVENTH qtrieve) old-add NIL old-del NIL old-ren NIL)
  (SETQ new-dir old-dir new-doc old-doc new-rel relation
new-tup old-tup new-imp old-imp new-sto old-sto new-key old-key
new-add old-add new-del old-del new-ren old-ren)
  (IF (NOT (CATCH 'abort
     (W:CHOOSE-VARIABLE-VALUES
       `(
 (new-rel "Relation Name"
  :documentation "To change the relation name." :SEXP)
 ,(FORMAT nil "     Attributes: ~S" old-att)
 (new-add "Add attributes"
  :documentation "To add attributes specify attribute descriptor pair." :SEXP)
 (new-del "Delete attributes"
  :documentation "To delete attributes, specify a list of the attributes." :SEXP)
 (new-ren "Rename attributes"
  :documentation "To rename attributes, specify a list of the type <(old new)>." :SEXP)
 " "
 (new-imp "Implementation-type"
  :documentation "To change the type of implementation."
  :SEXP)
 (new-sto "Storage Structure"
  :documentation
  "To change the type of storage structure." :SEXP)
 (new-key "Key"
  :documentation "To change the key attributes."
  :SEXP)
 (new-doc "Documentation"
  :documentation "To change the relation documentation."
  :SEXP)
 (new-dir "Save Directory"
  :documentation
  "To change the directory in which this relation can be saved."
  :SEXP)
 (new-tup "Tuple format"
  :documentation
  "To change the format in printing the relation."
  :SEXP))
       :label (FORMAT nil "Change the features of ~S" relation)
       :margin-choices '("Do It" ("Abort" (THROW 'abort T))))))       ;mrr 04.06.87
      (PROGN
(SETQ qtrieve NIL)
(MAPC #'(LAMBDA (old new key)
  (IF (NOT (EVAL `(*EQUALP ,old ,new)))
      (SETQ qtrieve (APPEND qtrieve (LIST key (eval `,new))))))
      '(relation old-add old-del old-ren
 old-dir old-doc old-tup old-imp old-sto old-key)
      '(new-rel new-add new-del new-ren
new-dir new-doc new-tup new-imp new-sto new-key)
      '(rel add-attr delete-attr rename-attr dir doc format imp sto key))
(IF qtrieve
    (MODIFY-RELATION relation qtrieve)))))
 )

me of the database to be loaded."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (
      :documentation "Name of the directory in which it is stored."
      :sexp))
 :label "Give parameters for LOAD DATABASE:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to load database from a given directory."
      :keys ((#\SUPER-L #\SUPER-D)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'LOAD-DATABASE database (LIST 'dir directory))))
  (LOAD-DATABASE database (LIST 'dir directory)))
;**************************************************************************
;                DEFCOMMAND  FOR RESTORE ENVIRONMENT                               *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC load-environment) (environment directory)
`(:description ,(STRING-APPEND (DOCUMENTATION 'load-environment)
     (FORMAT NIL "  ~S"
      (CONS
        'load-environment
        (ARGLIST
          'load-environment))))
      :arguments (:user-supplied (:label "Environment Name:"
    :default *ui-database*
    :type (
      :documentation "Name of the environment to be loaded."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (
      :documentation "Name of the directory in which it is stored."
      :sexp))
 :label "Give parameters for LOAD ENVIRONMENT:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to load environment from a given directory."
      :keys ((#\SUPER-L #\SUPER-E)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'LOAD-ENVIRONMENT environment (LIST 'dir directory))))
  (LOAD-ENVIRONMENT environment (LIST 'dir directory)))
;**************************************************************************
;                DEFCOMMAND  FOR RESTORE RELATION                                  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC load-relation) (relation directory)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'load-relation)
     (FORMAT NIL "  ~S"
      (CONS
        'load-relation
        (ARGLIST
          'load-relation))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default *ui-relation*
    :type (
      :documentation "Name of the relation to be loaded."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (
      :documentation "Name of the directory in which it is saved."
                :sexp))
  :label "Give parameters for LOAD RELATION:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to load a relation from a given directory."
      :keys ((#\SUPER-L #\SUPER-R)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'LOAD-RELATION relation (LIST 'dir directory))))
  (LOAD-RELATION relation (LIST 'dir directory)))
;**************************************************************************
;                DEFCOMMAND  FOR SAVE DATABASE                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC save-database) (database directory)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'save-database)
     (FORMAT NIL "  ~S"
      (CONS
        'save-database
        (ARGLIST
          'save-database))))
      :arguments (:user-supplied (:label "Database Name:"
    :default *ui-database*
    :type (:documentation
       "Name of the database to be saved."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (:documentation
      "Name of the directory to write to."
      :sexp))
  :label "Give parameters for SAVE DATABASE:")
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Used to save a database on a given directory."
      :keys ((#\SUPER-S #\HYPER-D)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'SAVE-DATABASE database (LIST 'dir directory))))
  (SAVE-DATABASE database (LIST 'dir directory)))
;**************************************************************************
;                DEFCOMMAND  FOR SAVE ENVIRONMENT                                  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC save-environment) (environment directory)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'save-environment)
     (FORMAT NIL "  ~S"
      (CONS
        'save-environment
        (ARGLIST
          'save-environment))))
      :arguments (:user-supplied (:label "Environment Name:"
    :default nil
    :type (:documentation
       "Name of the environment to be saved."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (:documentation
      "Name of the directory to write to."
      :sexp))
  :label "Give parameters for SAVE environment:")
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Used to save an environment on a given directory."
      :keys ((#\SUPER-S #\SUPER-E)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'SAVE-ENVIRONMENT environment (LIST 'dir directory))))
  (SAVE-ENVIRONMENT environment (LIST 'dir directory)))
;**************************************************************************
;                DEFCOMMAND  FOR SAVE RELATION                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC save-relation) (relation directory type save
     &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'save-relation)
     (FORMAT NIL "  ~S"
      (CONS
        'save-relation
        (ARGLIST
          'save-relation))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default *ui-relation*
    :type (
      :documentation "Name of the relation to be saved."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (
      :documentation "Name of the directory to write to."
      :sexp))
   (:label "Type of SAVE:"
    :default *ui-type*
    :type (:documentation "Save type. It can be either XLD or COMMAND." ;mrr 03.31.87
     :sexp))
   (:label "Must Save:"
    :default nil
    :type (:documentation "Save the relation even if the relation has not been modified." :BOOLEAN))
 :label "Give parameters for SAVE RELATION:")
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Used to save a relation on a given directory."
      :keys ((#\SUPER-S #\SUPER-R)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'SAVE-RELATION relation
      (SETQ keywords (LIST 'type type 'dir directory
     'save save)))))
  (SAVE-RELATION relation keywords))
;**************************************************************************
;                DEFCOMMAND  FOR SAVE TRANSACTION                                  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC save-transaction) (transaction directory pathname
     &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'save-transaction)
     (FORMAT NIL "  ~S"
      (CONS
        'save-transaction
        (ARGLIST
          'save-transaction))))
      :arguments (:user-supplied (:label "Transaction Name:"
    :default *ui-transaction*
    :type (
      :documentation "Name of the transaction to be saved."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (
      :documentation "Name of the directory to write to."
      :sexp))
   (:label "Pathname:"
    :default *ui-file*
    :type (:documentation
     "The name of the file into which the transaction forms will be stored. It defaults to <transaction>.lisp"
     :SEXP))
 :label "Give parameters for SAVE TRANSACTION:")
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Used to save a transaction on a given directory."
      :keys ((#\SUPER-S #\SUPER-T)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'SAVE-TRANSACTION transaction
      (SETQ keywords (LIST 'path pathname 'dir directory)))))
  (SAVE-TRANSACTION transaction keywords))

;**************************************************************************
;                DEFCOMMAND  FOR DEFINE IMPLEMENTATION                             *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-implementation) (implementation doc
   &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'define-implementation)
     (FORMAT NIL "  ~S"
      (CONS
        'define-implementation
        (ARGLIST 'define-implementation))))
      :arguments (:user-supplied (:label "Implementation Name:"
    :default nil
    :type (:documentation
      "Name of the implementation. Implementation-dependent routines are expected to be defined by the user."
      :sexp))
   (:label "Documentation:"
    :default *ui-doc*
    :type (:documentation
      "Documentation for the implementation."
      :string))
  :label "Give parameters for DEFINE IMPLEMENTATION:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define an implementation."
      :keys ((#\SUPER-D #\SUPER-I)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'DEFINE-IMPLEMENTATION implementation
      (SETQ keywords (LIST 'doc doc
     )))))
  (DEFINE-IMPLEMENTATION implementation keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE INDEX                                      *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-index) (relation-name index-name key-attributes storage-structure priority
  doc &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'define-index)
     (FORMAT NIL "  ~S"
      (CONS
        'define-index
        (ARGLIST 'define-index))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default nil
    :type (:documentation
      "Name of the relation upon which the index will be defined."
      :sexp))
   (:label "Index Name:"
    :default nil
    :type (:documentation
      "Name of the index to be defined."
      :string))
   (:label "Key Attributes:"
    :default nil
    :type (:documentation
      "List of attribute names which form the key for this index."
      :sexp))
   (:label "Storage Structure:"
    :default "AVL"
    :type (:documentation
      "The storage structure used to define the index."
      :string))
   (:label "Priority:"
    :default 10
    :type (:documentation
      "A numerical value which indicates the priority given to this index. 1 is the highest priority."
      :number))
   (:label "Documentation:"
    :default *ui-doc*
    :type (:documentation
      "Documentation for the index."
      :string))
  :label "Give parameters for DEFINE INDEX:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define a secondary index on a relation."
      :keys ((#\SUPER-D #\HYPER-I)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'DEFINE-INDEX relation-name
      (SETQ keywords (LIST 'name index-name 'key key-attributes 'sto storage-structure
     'priority priority 'doc doc
     )))))
  (DEFINE-INDEX relation-name keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY INDEX                                      *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-index) (relation-name index-name new-index-name
  key-attributes storage-structure priority
  doc &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-index)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-index
        (ARGLIST 'modify-index))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default nil
    :type (:documentation
      "Name of the relation upon which the index to be modified is defined."
      :sexp))
   (:label "Index Name:"
    :default nil
    :type (:documentation
      "Name of the index to be modified."
      :string))
   (:label "New Index Name:"
    :default nil
    :type (:documentation
      "New name of the index."
      :string))
   (:label "Key Attributes:"
    :default nil
    :type (:documentation
      "List of attribute names which form the key for this index."
      :sexp))
   (:label "Storage Structure:"
    :default nil
    :type (:documentation
      "The storage structure used to define the index."
      :string))
   (:label "Priority:"
      :default 10
    :type (:documentation
      "A numerical value which indicates the priority given to this index. 1 is the highest priority."
      :number))
   (:label "Documentation:"
    :default *ui-doc*
    :type (:documentation
      "Documentation for the index."
      :string))
  :label "Give parameters for DEFINE INDEX:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to define a secondary index on a relation."
      :keys ((#\SUPER-M #\HYPER-I)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'MODIFY-INDEX relation-name index-name
      (SETQ keywords (LIST 'new-name new-index-name 'key key-attributes 'sto storage-structure
     'priority priority 'doc doc
     )))))
  (MODIFY-INDEX relation-name index-name keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE STORAGE-STRUCTURE                          *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-storage-structure) (storage-structure doc
   &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'define-storage-structure)
     (FORMAT NIL "  ~S"
      (CONS
        'define-storage-structure
        (ARGLIST 'define-storage-structure))))
      :arguments (:user-supplied (:label "Storage structure name:"
    :default nil
    :type (:documentation
      "Name of the storage structure. Storage-structure-dependent routines are expected to be defined by the user."
      :sexp))
   (:label "Documentation:"
    :default nil
    :type (:documentation
      "Documentation for the storage structure."
      :string))
  :label "Give parameters for DEFINE STORAGE STRUCTURE:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define a storagestructure."
      :keys ((#\SUPER-D #\SUPER-S)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'DEFINE-STORAGE-STRUCTURE storage-structure
      (SETQ keywords (LIST 'doc doc
     )))))
  (DEFINE-STORAGE-STRUCTURE storage-structure keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE DOMAIN                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-domain) (domain def doc format
   &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'define-domain)
     (FORMAT NIL "  ~S"
      (CONS
        'define-domain
        (ARGLIST 'define-domain))))
      :arguments (:user-supplied (:label "Domain Name:"
    :default nil
    :type (:documentation
      "Name of the domain. Domain predicate is expected to be defined prior to this."
      :sexp))
   (:label "Default value:"
    :default nil
    :type (:documentation
     "Default value for this domain."
     :sexp))
   (:label "Documentation:"
    :default nil
    :type (:documentation
      "Documentation for the domain."
      :string))
   (:label "Default width :"
    :default nil
    :type (:documentation
      "The default width to be used for this domain."
      :sexp))
  :label "Give parameters for DEFINE DOMAIN:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define a domain."
      :keys (#\SUPER-HYPER-D))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'DEFINE-DOMAIN domain
      (SETQ keywords (LIST 'default def
     'doc doc
     'format format)))))
  (DEFINE-DOMAIN domain keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY DOMAIN                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-domain) (domain def doc format
   &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-domain)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-domain
        (ARGLIST 'modify-domain))))
      :arguments (:user-supplied (:label "Domain Name:"
    :default nil
    :type (:documentation
      "Name of the domain to be modified."
      :sexp))
   (:label "Default value:"
    :default nil
    :type (:documentation
     "New default value for this domain."
     :sexp))
   (:label "Documentation:"
    :default nil
    :type (:documentation
      "New documentation for the domain."
      :string))
   (:label "Default width :"
    :default nil
    :type (:documentation
      "The new default width to be used for this domain."
      :sexp))
  :label "Give parameters for MODIFY DOMAIN:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to modify a domain."
      :keys ((#\SUPER-M #\SUPER-D)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
  (LIST 'MODIFY-DOMAIN domain
      (SETQ keywords (LIST 'default def
     'doc doc
     'format format)))))
  (MODIFY-DOMAIN domain keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE TRANSACTION                                *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-transaction) (transaction forms dir path
      &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'define-transaction)
     (FORMAT NIL "  ~S"
      (CONS
        'define-transaction
        (ARGLIST 'define-transaction))))
      :arguments (:user-supplied (:label "Transaction Name:"
    :default *ui-transaction*
    :type (:documentation
      "Name of the transaction."
      :sexp))
   (:label "Database calls:"
    :default nil
    :type (:documentation
     "A list of database calls."
     :sexp))
   ,*ucl-dir*
   (:label "Pathname :"
    :default *ui-file*
    :type (:documentation
      "The default file in which it will be saved."
      :SEXP))
  :label "Give parameters for DEFINE TRANSACTION:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define a transaction."
      :keys ((#\SUPER-D #\SUPER-T)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'DEFINE-TRANSACTION transaction forms
      (SETQ keywords (LIST 'dir dir
     'path path)))))
  (DEFINE-TRANSACTION transaction forms keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY TRANSACTION                                *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-transaction) (transaction dir path
      &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-transaction)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-transaction
        (ARGLIST 'modify-transaction))))
      :arguments (:user-supplied (:label "Transaction Name:"
    :default *ui-transaction*
    :type (:documentation
      "Name of the transaction to be modified."
      :sexp))
   (:label "Directory:"
    :default *ui-directory*
    :type (:documentation
      "Default directory in which it can be found, if not in memory."
      :SEXP))
   (:label "Pathname :"
    :default *ui-file*
    :type (:documentation
      "The default file in which it can be found, if not in memory."
      :SEXP))
  :label "Give parameters for MODIFY TRANSACTION:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to modify a transaction."
      :keys ((#\SUPER-M #\SUPER-T)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'MODIFY-TRANSACTION transaction
      (SETQ keywords (LIST 'dir dir
     'path path)))))
  (MODIFY-TRANSACTION transaction keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE DATABASE                                   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-database) (database directory doc env
     &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'define-database)
     (FORMAT NIL "  ~S"
      (CONS
        'define-database
        (ARGLIST 'define-database))))
      :arguments (:user-supplied (:label "Database Name:"
    :default nil
    :type (:documentation
      "Name of the database."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (:documentation
     "Name of the save directory for this database."
     :sexp))
   (:label "Documentation:"
    :default *ui-doc*
    :type (:documentation
      "Documentation for the database."
      :string))
   (:label "Environment:"
    :default nil
    :type (:documentation
      "Name of the environment to be used to replace the default settings."
      :sexp))
  :label "Give parameters for DEFINE DATABASE:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define a database in a given directory."
      :keys ((#\SUPER-D #\SUPER-D)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'DEFDB database
      (SETQ keywords (LIST 'dir directory
     'doc doc
     'environment env)))))
  (DEFDB database keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY DATABASE                                   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-database) (database new-database directory doc
     &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-database)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-database
        (ARGLIST 'modify-database))))
      :arguments (:user-supplied (:label "Database Name:"
    :default nil
    :type (:documentation
      "Name of the database."
      :sexp))
   (:label "New Database Name:"
    :default nil
    :type (:documentation
      "If the database is to be renamed specify the new name."
      :sexp))
   (:label "Directory Name:"
    :default NIL
    :type (:documentation
     "To change the save directory for this database specify a new directory."
     :sexp))
   (:label "Documentation:"
    :default NIL
    :type (:documentation
      "New documentation for the database."
      :string))
  :label "Give parameters for MODIFY DATABASE:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to modify the features a database."
      :keys ((#\SUPER-M #\HYPER-D)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'MODIFY-DATABASE database
      (SETQ keywords (LIST 'database-name new-database
      'dir directory
     'doc doc
     )))))
  (MODIFY-DATABASE database keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY ATTRIBUTE                                  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-attribute) (relation attr new-attr def doc format
     &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-attribute)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-attribute
        (ARGLIST 'modify-attribute))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default *ui-relation*
    :type (:documentation
      "Name of the relation."
      :sexp))
   (:label "Attribute Name:"
    :default nil
    :type (:documentation
      "Name of the attribute."
      :sexp))
   (:label "New Attribute Name:"
    :default nil
    :type (:documentation
      "If the attribute is to be renamed specify the new name."
      :sexp))
   (:label "Default Value:"
    :default NIL
    :type (:documentation
     "To change the default value of this attribute specify a new value."
     :sexp))
   (:label "Documentation:"
    :default NIL
    :type (:documentation
      "New documentation for the attribute."
      :string))
   (:label "Default width :"
    :default nil
    :type (:documentation
      "The new default width to be used for this attribute."
      :sexp))
  :label "Give parameters for MODIFY ATTRIBUTE:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to modify the features a attribute."
      :keys ((#\SUPER-M #\SUPER-A)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'MODIFY-ATTRIBUTE relation attr
      (SETQ keywords (LIST 'attribute-name new-attr
     'def def
     'doc doc 'format format
     )))))
  (MODIFY-ATTRIBUTE relation attr keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY VIEW *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-view) (view def doc
     &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-view)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-view
        (ARGLIST 'modify-view))))
      :arguments (:user-supplied (:label "View Name:"
    :default NIL
    :type (:documentation
      "Name of the view."
      :sexp))
   (:label "View Definition:"
    :default nil
    :type (:documentation
      "New definition of the view."
      :sexp))
   (:label "Documentation:"
    :default NIL
    :type (:documentation
      "New documentation for the view."
      :string))
  :label "Give parameters for MODIFY VIEW:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to modify the features a view."
      :keys ((#\SUPER-M #\SUPER-V)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'MODIFY-VIEW view
      (SETQ keywords (LIST
     'view-def def
     'view-doc doc
     )))))
  (MODIFY-VIEW view keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY RELATION                                   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-relation) (rel new-rel add-att del-att ren-att
     imp sto format key dir doc
     &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-relation)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-relation
        (ARGLIST 'modify-relation))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default *ui-relation*
    :type (:documentation
      "Name of the Relation."
      :sexp))
   (:label "New Relation Name:"
    :default nil
    :type (:documentation
      "If the relation is to be renamed specify the new name."
      :sexp))
   (:label "Add attributes:"
    :default NIL
    :type (:documentation
     "Specify a list of attribute-descriptor pairs for attributes to be added to this relation."
     :sexp))
   (:label "Delete attributes:"
    :default NIL
    :type (:documentation
     "Specify a list of attributes in this relation which are to be deleted."
     :sexp))
   (:label "Rename attributes:"
    :default NIL
    :type (:documentation
     "To rename some of the attributes provide a list of the form (<old-attribute new-attribute>)."
     :sexp))
   (:label "Implementation Type:"
    :default NIL
    :type (:documentation
     "To change the implementation type of this relation specify a new value."
     :sexp))
   (:label "Storage structure:"
    :default NIL
    :type (:documentation
     "To change the storage structure of this relation specify a new value."
     :sexp))
   (:label "Format:"
    :default NIL
    :type (:documentation
     "To change the format for this relation specify a new format as a list of values."
     :sexp))
   (:label "Key:"
    :default NIL
    :type (:documentation
     "To change the key for this relation specify a new key as a list of attributes."
     :sexp))
   (:label "Directory Name:"
    :default NIL
    :type (:documentation
        "To change the save directory for this relation specify a new directory."
     :sexp))
   (:label "Documentation:"
    :default NIL
    :type (:documentation
      "New documentation for the relation."
      :string))
  :label "Give parameters for MODIFY RELATION:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to modify the features a relation."
      :keys ((#\SUPER-M #\SUPER-R)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'MODIFY-RELATION rel
      (SETQ keywords (LIST 'relation new-rel
     'add-attributes add-att
     'delete-attributes del-att
     'rename-attributes ren-att
     'imp imp
     'sto sto
     'format format
     'key key
     'doc doc
     'dir dir
     )))))
  (MODIFY-RELATION rel keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE ENVIRONMENT                                *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-environment) (environment save dir err par-check
        rel-imp rel-sto status sys-imp
        sys-sto val-check warn
        &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'define-environment)
     (FORMAT NIL "  ~S"
      (CONS
        'define-environment
        (ARGLIST 'define-environment))))
      :arguments (:user-supplied (:label "Environment Name:"
    :default nil
    :type (:documentation
      "Name of the environment."
      :sexp))
   (:label "Auto save:"
    :default nil
    :type (:documentation
     "Automatically saves all the modified relations after each function." :boolean))
   ,*ucl-dir*
   (:label "Errors:"
    :default T
    :type (:documentation
      "Controls the printing of the error messages."
      :boolean))
   (:label "Parameter Checking:"
    :default T
    :type (:documentation
      "Controls the checking of the parameters."
      :boolean))
   (:label "Relation Implementation:"
    :default *ui-imp*
    :type (:documentation
      "Default implementation of the user relations."
      :sexp))
   (:label "Relation storage structure:"
    :default *ui-ss*
    :type (:documentation
      "Default storage structure for the user relations."
      :sexp))
   (:label "Status:"
    :default T
    :type (:documentation
      "Controls the printing of the status messages."
      :boolean))
   (:label "System Implementation:"
    :default nil
    :type (:documentation
      "Default implementation of the system relations. Can not change this when a database is active."
      :sexp))
   (:label "System storage structure:"
    :default nil
    :type (:documentation
      "Default storage structure for the system relations. Can not change this when a database is active."
      :sexp))
   (:label "Validity Checking:"
    :default T
    :type (:documentation
      "Controls the checking of the values during insertion and modification for validity."
      :boolean))
   (:label "Warnings:"
    :default T
     :type (:documentation
      "Controls the printing of the warning messages."
      :boolean))
  :label "Give parameters for DEFINE ENVIRONMENT:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define an environment in a given directory."
      :keys ((#\SUPER-D #\SUPER-E)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'DEFENV environment
      (SETQ keywords (IF *active-db*
   (LIST 'auto-save save 'para par-check
     'dir dir 'rel-imp rel-imp 'rel-sto
     rel-sto 'errors err 'status status
     'validity val-check 'warnings warn)
        (LIST 'auto-save save 'para par-check
     'dir dir 'rel-imp rel-imp 'rel-sto
     rel-sto 'errors err 'status status
     'sys-imp sys-imp 'sys-sto sys-sto
     'validity val-check 'warnings warn))))))
  (DEFENV environment keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE RELATION                                   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-relation) (relation attr-des tup
     dir doc key imp ss &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'define-relation)
     (FORMAT NIL "  ~S"
      (CONS
        'define-relation
        (ARGLIST 'define-relation))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default nil
    :type (:documentation
     "Name of the relation to be defined."
     :sexp))
   ,*ucl-attr-desc*
   ,*ucl-format*
   ,*ucl-dir*
   ,*ucl-doci*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
     :label "Give parameters for DEFINE RELATION:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "used to define a relation."
      :keys ((#\SUPER-D #\SUPER-R)))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(LIST 'DEFREL
      relation attr-des
      (SETQ keywords
    (LIST 'tuple-format tup 'dir dir 'doc doc
  'key key 'imp imp 'sto ss)))))
  (DEFREL relation attr-des keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE VIEW *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-view) (viewname view-definition doc)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'define-view)
     (FORMAT NIL "  ~S"
      (CONS
        'define-view
        (ARGLIST 'define-view))))
      :arguments (:user-supplied (:label "View Name:"
    :default nil
    :type (:documentation
       "Specify a name for the view."
     :sexp))
   (:label "View Definition:"
    :default *ui-viewdef*
    :type (:documentation
       "Specify a definition for the view."
     :sexp))
   (:label "View Documentation:"
    :default nil
    :type (:documentation
       "Specify documentation for the view."
     :sexp))
 :label "Give parameters for DEFINE VIEW:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define a view."
      :keys ((#\SUPER-D #\SUPER-V)))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(LIST 'DEFVIEW viewname view-definition doc)))
  (DEFVIEW viewname view-definition doc))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE ATTRIBUTE                                  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-attribute) (relation-name attr-des key
      &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'define-attribute)
     (FORMAT NIL "  ~S"
      (CONS
        'define-attribute
        (ARGLIST 'define-attribute))))
      :arguments (:user-supplied (:label "Relation name: "
    :default *ui-relation*
    :type (:documentation
       "The name of the relation to which new attributes are to be added." :SEXP))
   ,*ucl-attr-desc*
   (:label "Key: "
    :default nil
    :type (:documentation
       "New key for the relation if it is to be different from the previous value. Specify a list of attributes."
       :SEXP))
 :label "Give parameters for DEFINE ATTRIBUTE:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to add attributes to relations."
      :keys ((#\SUPER-D #\SUPER-A)))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(LIST 'DEFINE-ATTRIBUTE relation-name attr-des
      (SETQ keywords (LIST 'key key)))))
  (DEFINE-ATTRIBUTE relation-name attr-des keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY TUPLES                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-tuples) (relation where-clause attributes values
     &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-tuples)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-tuples
        (ARGLIST 'modify-tuples))))
      :arguments (:user-supplied (:label "Relation: "
    :default *ui-relation*
    :type (:documentation
       "Specify the relation whose tuples are to be modified."
     :sexp))
   ,*ucl-where*
   (:label "Attributes: "
    :default *ui-attributes*
    :type (:documentation
       "Specify a list of attributes in the above relation to be modified." :sexp))
   (:label "Values: "
    :default *ui-values*
    :type (:documentation
       "Specify a corresponding list of values to modify the above attributes." :sexp))
 :label "Give parameters for MODIFY TUPLES ==>")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to modify tuples in a relation."
      :keys ((#\SUPER-M #\HYPER-M)))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(LIST 'MODIFY relation (SETQ keywords (LIST 'where where-clause
       'attr attributes
       'values values)))))
  (MODIFY relation keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DELETE TUPLES                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC delete-tuples) (relation where-clause)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'delete-tuples)
     (FORMAT NIL "  ~S"
      (CONS
        'delete-tuples
        (ARGLIST 'delete-tuples))))
      :arguments (:user-supplied (:label "Relation: "
    :default *ui-relation*
    :type (:documentation
       "Specify a relation whose tuples are to be deleted."
     :sexp))
   (:label "Where clause: "
    :default nil
    :type (:documentation
       "Deletes the tuples which satisfy this condition."
     :sexp))
 :label "Give parameters for DELETE TUPLES ==>")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to delete tuples in a relation."
      :keys (#\HYPER-D))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(LIST 'DELETE-TUPLES relation (LIST 'where where-clause))))
  (DELETE-TUPLES  relation (LIST 'where where-clause)))
;**************************************************************************
;                DEFCOMMAND  FOR RETRIEVE TUPLES                                   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC retrieve-tuples) (relation attributes where-clause
     into dir doc key imp sto
     qprint to-file sort
     format wide number print
     tuples qsort stream unique index-name
     &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'retrieve)
       (FORMAT NIL "  ~S"
      (CONS
        'retrieve
        (ARGLIST 'retrieve))))
      :arguments (:user-supplied ,*ucl-retrieve-rel*
   ,*ucl-attributes*
   ,*ucl-where*
   ,*ucl-into*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-qprint*
   ,*ucl-out*
   ,*ucl-sort*
   ,*ucl-format*
   ,*ucl-wide*
   ,*ucl-num*
   ,*ucl-print*
   ,*ucl-tuples*
   ,*ucl-quick-sort*
   ,*ucl-stream*
   ,*ucl-unique*
    ,*ucl-index-name*
 :label "Give parameters for RETRIEVE TUPLES ==>")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to Retrieve tuples in a relation."
      :keys (#\HYPER-R))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'RETRIEVE
      relation
      (SETQ keywords
    (LIST 'project
   (IF (EQUAL attributes T)
       NIL
     attributes)
   'where where-clause 'into into
   'dir dir 'doc doc 'key key 'imp imp 'sto sto
    'qprint (NOT qprint) 'output-to-file to-file
   'sort sort 'format format
   'wide wide 'num number
   'print print 'tuples tuples
   'quick-sort qsort 'stream stream
   'unique unique 'index-name index-name)))))
  (RETRIEVE relation keywords))
;**************************************************************************
;                DEFCOMMAND  FOR SELECT TUPLES                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC select) (relation where-clause
     into dir doc key imp sto
     qprint to-file sort
     format wide number print
     tuples qsort stream unique index-name
     &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'select-tuples)
     (FORMAT NIL "  ~S"
      (CONS
        'select-tuples
        (ARGLIST 'select-tuples))))
      :arguments (:user-supplied ,*ucl-retrieve-rel*
   ,*ucl-where*
   ,*ucl-into*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-qprint*
   ,*ucl-out*
   ,*ucl-sort*
   ,*ucl-format*
   ,*ucl-wide*
   ,*ucl-num*
   ,*ucl-print*
   ,*ucl-tuples*
   ,*ucl-quick-sort*
   ,*ucl-stream*
   ,*ucl-unique*
   ,*ucl-index-name*
 :label "Give parameters for SELECT TUPLES ==>")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to Select tuples in a relation."
      :keys ((#\SUPER-R #\SUPER-S)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'SELECT-TUPLES
      relation
      (SETQ keywords
    (LIST
   'where where-clause 'into into
   'dir dir 'doc doc 'key key 'imp imp 'sto sto
   'qprint (NOT qprint) 'output-to-file to-file
   'sort sort 'format format
   'wide wide 'num number
   'print print 'tuples tuples
   'quick-sort qsort 'stream stream
   'unique unique 'index-name index-name)))))
  (RETRIEVE relation (APPEND (LIST 'project nil) keywords)))
;**************************************************************************
;                DEFCOMMAND  FOR PROJECT TUPLES                                    *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC Project) (relation attributes
      into dir doc key imp sto
      qprint to-file sort
      format wide number print tuples
      qsort stream unique
      &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'project)
     (FORMAT NIL "  ~S"
      (CONS
        'project
        (ARGLIST
          'project))))
      :arguments (:user-supplied ,*ucl-retrieve-rel*
   ,*ucl-attributes*
   ,*ucl-into*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-qprint*
   ,*ucl-out*
   ,*ucl-sort*
   ,*ucl-format*
   ,*ucl-wide*
   ,*ucl-num*
   ,*ucl-print*
   ,*ucl-tuples*
   ,*ucl-quick-sort*
   ,*ucl-stream*
   ,*ucl-unique*
 :label "Give parameters for PROJECT TUPLES ==>")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to Project tuples in a relation."
      :keys ((#\SUPER-R #\SUPER-P)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'PROJECT
      relation
      (SETQ keywords
    (LIST 'project (IF (EQUAL attributes T)
   nil
        attributes)
    'into into 'dir dir 'doc doc 'key key 'imp imp 'sto sto
    'qprint (NOT qprint) 'output-to-file to-file
    'sort sort 'format format
    'wide wide 'num number 'print print 'tuples tuples
    'quick-sort qsort 'stream stream 'unique unique)))))
  (RETRIEVE relation (APPEND (LIST 'where t) keywords)))
;**************************************************************************
;                DEFCOMMAND  FOR COMMIT TRANSACTION                                *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC commit-transaction) (trans dir path &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'commit-transaction)
     (FORMAT NIL "  ~S"
      (CONS
        'commit-transaction
        (ARGLIST
          'commit-transaction))))
      :arguments (:user-supplied (:label "Name of the transaction :"
    :default *ui-transaction*
    :type (:documentation
       "The name of an existing transaction." :SEXP))
   (:label "Name of the directory:"
    :default *ui-directory*
    :type (:documentation
       "Name of the directory which contains the transaction file, if the transaction is not in the memory." :SEXP))
   (:label "Pathname:"
    :default *ui-file*
    :type (:documentation
    "If the transaction is not in memory, provide the pathname for the transaction file. It defaults to <transaction>.lisp." :SEXP))
 :label "Give parameters for COMMIT TRANSACTION")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Commit a transaction - execute all the database calls in it."
      :keys ((#\SUPER-T #\SUPER-C)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'COMMIT-TRANSACTION trans (SETQ keywords
         (LIST 'dir dir
        'path path)))))
  (COMMIT-TRANSACTION trans keywords)
)
;**************************************************************************
;                DEFCOMMAND  FOR JOIN        *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC join) (into from project where
      tuples format dir doc key imp sto
             print unique &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'join)
     (FORMAT NIL "  ~S"
      (CONS
        'join
        (ARGLIST
          'join))))
      :arguments (:user-supplied (:label "Output relation :"
    :default *ui-join-into*
    :type (:documentation
       "If not provided, the result of JOIN is stored in a temporary relation unless only the resultant tuples are requested." :SEXP))
   (:LABEL "FROM :"
    :DEFAULT *ui-from*
    :TYPE (:DOCUMENTATION
     "Specify a list of two relations to be joined." :SEXP))
   (:label "Project :"
    :default NIL
    :type (:documentation
       "This gives the attributes in the output relation. Example: (rel1.* a3 (rel2.a1 a4)) ==> All the attributes in rel1, attribute A3 of rel2 and atribute A1 of rel2 renamed as A4." :SEXP))
   (:label "Where :"
    :default *ui-over*
    :type (:documentation
     "The join clause using the theta-operators. It is a where clause consisting of attributes from the relations being joined." :SEXP))
   (:label "Tuples?"
    :default NIL
    :type (:documentation
     "Specify if the resultant tuples be returned rather than inserted in a relation. The following parameters can be ignored if this is true."
     :boolean))
   ,*ucl-format*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-print*
   ,*ucl-unique*
 :label "Give parameters for JOIN")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to join relations."
      :keys (#\SUPER-J))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'JOIN 'from from
      (SETQ keywords (LIST 'project project
     'into into
     'tuples tuples
     'format format
     'dir dir
     'doc doc
     'key key
     'imp imp
     'sto sto
     'print print
     'where where 'unique unique)))))
  (JOIN-INTERNAL (APPEND (LIST 'from from) keywords))
)
;**************************************************************************
;                DEFCOMMAND  FOR DESTROY DATABASE                                 *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC destroy-database) (database disk &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-database)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-database
        (ARGLIST
          'destroy-database))))
      :arguments (:user-supplied (:label "Database Name:"
    :default nil
    :type (:documentation
       "Name of the database to be destroyed." :SEXP))
   (:label "Delete from the DISK:"
    :default NIL
    :type (:documentation
     "IF YES all the files pertaining to this database are deleted but NOT EXPUNGED." :BOOLEAN))
 :label "Give parameters for DESTROY DATABASE:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy databases"
      :keys ((#\SUPER-K #\SUPER-D)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-DATABASE database
      (SETQ keywords (LIST 'disk disk)))))
  (DESTROY-DATABASE database keywords)
)
;**************************************************************************
;                DEFCOMMAND  FOR DESTROY DOMAIN                                    *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC destroy-domain) (domain)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-domain)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-domain
        (ARGLIST
          'destroy-domain))))
      :arguments (:user-supplied (:label "Domain Name:"
    :default nil
    :type (:documentation
       "Name of the domain to be destroyed." :SEXP))
 :label "Give parameters for DESTROY DOMAIN:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy domains."
      :keys (#\SUPER-HYPER-K))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-DOMAIN domain)))
  (DESTROY-DOMAIN domain))
;**************************************************************************
;                DEFCOMMAND  FOR DESTROY IMPLEMENTATION                            *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC destroy-implementation) (implementation)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-implementation)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-implementation
        (ARGLIST
          'destroy-implementation))))
      :arguments (:user-supplied (:label "Implementation Name:"
    :default nil
    :type (:documentation
       "Name of the implementation to be destroyed." :SEXP))
 :label "Give parameters for DESTROY IMPLEMENTATION:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy implementations."
      :keys ((#\SUPER-K #\SUPER-I)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-IMPLEMENTATION implementation)))
  (DESTROY-IMPLEMENTATION implementation))
;**************************************************************************
;                DEFCOMMAND  FOR DESTROY INDEX                            *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC DESTROY-INDEX) (relation-name index-name)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-index)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-index
        (ARGLIST
          'destroy-index))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default nil
    :type (:documentation
       "Name of the relation on which the index to be destroyed is defined." :SEXP))
   (:label "Index Name:"
    :default nil
    :type (:documentation
       "Name of the index to be destroyed." :SEXP))
     :label "Give parameters for DESTROY INDEX:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy indices."
      :keys ((#\SUPER-K #\HYPER-I)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-INDEX relation-name index-name)))
  (DESTROY-INDEX relation-name index-name))
;**************************************************************************
;                DEFCOMMAND  FOR DESTROY STORAGE STRUCTURE                         *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC destroy-storage-structure) (storage-structure)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-storage-structure)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-storage-structure
        (ARGLIST
          'destroy-storage-structure))))
      :arguments (:user-supplied (:label "Storage structure name:"
    :default nil
    :type (:documentation
       "Name of the storage structure to be destroyed." :SEXP))
 :label "Give parameters for DESTROY STORAGE STRUCTURE:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy storage structures."
      :keys ((#\SUPER-K #\SUPER-S)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-STORAGE-STRUCTURE storage-structure)))
  (DESTROY-STORAGE-STRUCTURE storage-structure))
;**************************************************************************
;                DEFCOMMAND  FOR DESTROY VIEW                                      *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC destroy-view) (view)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-view)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-view
        (ARGLIST
          'destroy-view))))
      :arguments (:user-supplied (:label "View name:"
    :default nil
    :type (:documentation
       "Name of the view to be destroyed."
       :SEXP))
 :label "Give parameters for DESTROY VIEW:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy views."
      :keys ((#\SUPER-K #\SUPER-V)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-VIEW view)))
  (DESTROY-VIEW view))
;**************************************************************************
;                DEFCOMMAND  FOR DESTROYREL   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC destroy-relation) (relation disk &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-relation)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-relation
        (ARGLIST
          'destroy-relation))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default nil
    :type (:documentation
       "Name of the relation to be destroyed." :SEXP))
   (:label "Delete from the DISK:"
    :default NIL
    :type (:documentation
     "IF YES the file corresponding to this relation is deleted but NOT EXPUNGED." :BOOLEAN))
 :label "Give parameters for DESTROY RELATION:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy relations"
      :keys ((#\SUPER-K #\SUPER-R)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-RELATION
      relation (SETQ keywords (LIST 'disk disk)))))
  (DESTROY-RELATION relation keywords)
)
;**************************************************************************
;                DEFCOMMAND  FOR DESTROY ATTRIBUTE                                 *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC destroy-attribute) (relation attr key &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-attribute)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-attribute
        (ARGLIST
          'destroy-attribute))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default nil
    :type (:documentation
       "Name of the relation from which attributes are to be destroyed." :SEXP))
   (:label "Attributes:"
    :default nil
    :type (:documentation
       "List of attributes to destroy." :SEXP))
   (:label "Key:"
    :default NIL
    :type (:documentation
     "New key for the relation if it is to be different from the previous value or if any of the key attributes are destroyed." :SEXP))
 :label "Give parameters for DESTROY ATTRIBUTE:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy attributes from relations"
      :keys ((#\SUPER-K #\SUPER-A)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-ATTRIBUTE relation (SETQ keywords (LIST 'attr attr
      'key key)))))
  (DESTROY-ATTRIBUTE relation keywords)
)
;**************************************************************************
;                DEFCOMMAND  FOR SET UNION   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC union) (from into tuples format
       dir doc key imp sto print unique
       &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'relation-union)
     (FORMAT NIL "  ~S"
       (CONS
        'relation-union
        (ARGLIST
          'relation-union))))
      :arguments (:user-supplied (:label "List of two relations:"
    :default NIL
    :type (:documentation
     "List of the names of two relations which will take part in the relation union operation. The attributes to be projected and a where clause can be specified for each relation using keywords. For instance, (REL1 (PROJECT <attr> WHERE <where-claue>) REL2 (WHERE <where-clause> PROJECT <attr>))." :SEXP))
   ,*ucl-into*
   (:label "Tuples?"
    :default NIL
    :type (:documentation
     "Specify if the resultant tuples be returned rather than inserted in a relation. The following parameters can be ignored if this is true."
     :boolean))
   ,*ucl-format*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-print*
   ,*ucl-unique*
  :LABEL "Parameters for the set-union of two relations")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to form union of two compatible relations"
      :keys ((#\SUPER-O #\SUPER-U)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'RELATION-UNION
      (SETQ keywords (LIST 'into into
     'from from 'tuples tuples
     'format format 'dir dir 'doc doc
     'key key 'imp imp 'sto sto
     'print print 'unique unique)))))
  (RELATION-UNION keywords))
;**************************************************************************
;                DEFCOMMAND  FOR SET DIFFERENCE                                    *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC difference) (from into tuples format
       dir doc key imp sto print unique
       &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'relation-difference)
     (FORMAT NIL "  ~S"
      (CONS
        'relation-difference
        (ARGLIST
          'relation-difference))))
      :arguments (:user-supplied (:label "List of two relations:"
    :default NIL
    :type (:documentation
     "List of the names of two relations which will take part in the relation difference operation. The attributes to be projected and a where clause can be specified for each relation using keywords. For instance, (REL1 (PROJECT <attr> WHERE <where-claue>) REL2 (WHERE <where-clause> PROJECT <attr>))." :SEXP))
   ,*ucl-into*
   (:label "Tuples?"
    :default NIL
    :type (:documentation
     "Specify if the resultant tuples be returned rather than inserted in a relation. The following parameters can be ignored if this is true."
     :boolean))
   ,*ucl-format*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-print*
   ,*ucl-unique*
  :LABEL "Parameters for the set-difference of two relations")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to form difference of two compatible relations"
      :keys ((#\SUPER-O #\SUPER-D)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'RELATION-DIFFERENCE
      (SETQ keywords (LIST 'into into
     'from from 'tuples tuples
     'format format 'dir dir 'doc doc
     'key key 'imp imp 'sto sto
     'print print 'unique unique)))))
  (RELATION-DIFFERENCE keywords))
;**************************************************************************
;                DEFCOMMAND  FOR SET INTERSECTION                                  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC intersection) (from into tuples format
       dir doc key imp sto print unique
       &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'relation-intersection)
     (FORMAT NIL "  ~S"
      (CONS
        'relation-intersection
        (ARGLIST
          'relation-intersection))))
      :arguments (:user-supplied (:label "List of two relations:"
    :default NIL
    :type (:documentation
     "List of the names of two relations which will take part in the relation intersection operation. The attributes to be projected and a where clause can be specified for each relation using keywords. For instance, (REL1 (PROJECT <attr> WHERE <where-claue>) REL2 (WHERE <where-clause> PROJECT <attr>))." :SEXP))
   ,*ucl-into*
   (:label "Tuples?"
    :default NIL
    :type (:documentation
     "Specify if the resultant tuples be returned rather than inserted in a relation. The following parameters can be ignored if this is true."
     :boolean))
   ,*ucl-format*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-print*
   ,*ucl-unique*
  :LABEL "Parameters for the set-intersection of two relations")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to form intersection of two compatible relations"
      :keys ((#\SUPER-O #\SUPER-I)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'RELATION-INTERSECTION
      (SETQ keywords (LIST 'into into
     'from from 'tuples tuples
     'format format 'dir dir 'doc doc
     'key key 'imp imp 'sto sto
     'print print 'unique unique)))))
  (RELATION-INTERSECTION keywords))
;**************************************************************************
;                DEFCOMMAND  FOR AVERAGE     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC average) (relation attribute unique where by tuples
      &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'average)
     (FORMAT NIL "  ~S"
      (CONS
        'average
        (ARGLIST
          'average))))
      :arguments (:user-supplied (:label "Relation name:"
    :default *ui-relation*
    :type (:documentation
     "Name of the relation which contains the attribute to be averaged." :SEXP))
   ,*ucl-count-attr*
   ,*ucl-count-unique*
      ,*ucl-where*
   ,*ucl-by*
   ,*ucl-tuples*
  :LABEL "Parameters for average:")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to compute the average of the attribute values in a relation."
      :keys ((#\SUPER-O #\SUPER-A)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'AVERAGE relation attribute
      (SETQ keywords (LIST 'unique unique
     'where where 'by by 'tuples tuples)))))
  (AVERAGE relation attribute keywords))
;**************************************************************************
;                DEFCOMMAND  FOR SUM         *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC sum) (relation attribute unique where by tuples
  &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'sum)
     (FORMAT NIL "  ~S"
      (CONS
        'sum
        (ARGLIST
          'sum))))
      :arguments (:user-supplied (:label "Relation name:"
    :default *ui-relation*
    :type (:documentation
     "Name of the relation which contains the attribute to be summed." :SEXP))
   ,*ucl-count-attr*
   ,*ucl-count-unique*
   ,*ucl-where*
   ,*ucl-by*
   ,*ucl-tuples*
  :LABEL "Parameters for sum:")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to compute the sum of the attribute values in a relation."
      :keys ((#\SUPER-O #\SUPER-S)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'SUM relation attribute
      (SETQ keywords (LIST 'unique unique 'by by 'tuples tuples
     'where where)))))
  (SUM relation attribute keywords))
;**************************************************************************
;                DEFCOMMAND  FOR SIZE        *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC size) (relation unique where by tuples &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'size)
     (FORMAT NIL "  ~S"
      (CONS
        'size
        (ARGLIST
          'size))))
      :arguments (:user-supplied (:label "Relation name:"
    :default *ui-relation*
    :type (:documentation
     "Name of the relation whose size is required." :SEXP))
   ,*ucl-count-unique*
   ,*ucl-where*
   ,*ucl-by*
   ,*ucl-tuples*
  :LABEL "Parameters for size:")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to compute the size of the relation."
      :keys (#\SUPER-HYPER-S))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'SIZE relation
      (SETQ keywords (LIST 'unique unique 'by by 'tuples tuples
     'where where)))))
  (SIZE relation keywords))
;**************************************************************************
;                DEFCOMMAND  FOR COUNT-RTMS     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC count) (relation attribute unique where by tuples
         &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'count-rtms)
     (FORMAT NIL "  ~S"
      (CONS
        'count-rtms
        (ARGLIST
          'count-rtms))))
      :arguments (:user-supplied (:label "Relation name:"
    :default *ui-relation*
    :type (:documentation
     "Name of the relation which contains the attribute to be used to find the number of tuples." :SEXP))
   ,*ucl-count-attr*
   ,*ucl-count-unique*
   ,*ucl-where*
   ,*ucl-by*
   ,*ucl-tuples*
  :LABEL "Parameters for count:")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to compute the count of the attribute values in a relation."
      :keys ((#\SUPER-O #\SUPER-C)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'COUNT-RTMS relation attribute
      (SETQ keywords (LIST 'unique unique 'by by 'tuples tuples
     'where where)))))
  (COUNT-RTMS relation attribute keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MAXIMUM     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC maximum) (relation attribute where by tuples
      &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'maximum)
     (FORMAT NIL "  ~S"
      (CONS
        'maximum
        (ARGLIST
          'maximum))))
      :arguments (:user-supplied (:label "Relation name:"
    :default *ui-relation*
    :type (:documentation
     "Name of the relation which contains the attribute to be maximumd." :SEXP))
   ,*ucl-count-attr*
   ,*ucl-where*
   ,*ucl-by*
   ,*ucl-tuples*
  :LABEL "Parameters for maximum:")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to compute the maximum of the attribute values in a relation."
      :keys ((#\SUPER-O #\SUPER-M)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'MAXIMUM relation attribute
      (SETQ keywords (LIST 'where where 'by by 'tuples tuples)))))
  (MAXIMUM relation attribute keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MINIMUM     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC minimum) (relation attribute where by tuples
      &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'minimum)
     (FORMAT NIL "  ~S"
      (CONS
        'minimum
        (ARGLIST
          'minimum))))
      :arguments (:user-supplied (:label "Relation name:"
    :default *ui-relation*
    :type (:documentation
     "Name of the relation which contains the attribute to be minimumd." :SEXP))
   ,*ucl-count-attr*
   ,*ucl-where*
 LMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540749. :SYSTEM-TYPE :LOGICAL :VERSION 3. :TYPE "XLD" :NAME "INTERFACE" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760360973. :AUTHOR "REL3" :LENGTH-IN-BYTES 68086. :LENGTH-IN-BLOCKS 133. :BYTE-SIZE 16.)                                 pp2Ä\Ä©ÅCOMPILE-DATA\ÄÏÄSW-MFG,ÅGODZILLAÜÄá§“FÄFÄ\Äp¿,ÅCOMPILER,ÅVERSIONÄ\ÄFÄFÄp¿BÄ),ÇOPTIMIZE-SWITCHÄÜÄ©ÉQFASL-SOURCE-FILE-UNIQUE-IDÄ1Ä\Äp¿lÄFSÏÇMAKE-FASLOAD-PATHNAMEÄ\ÄÍÄQUOTEÄBÄ$\ÄBÄ8™ÄNILÄ\ÄBÄ8\Ä¨ÄRTMS\ÄBÄ8lÅINTERFACEÄ\ÄBÄ8¨ÄLISP\ÄBÄ8FÄ©ÄBASEFÄ
ÈÄFONTSÄ\Ä©ÅWIDER-MEDFNTÈÄMEDFNBBÄJ©ÄHL7Ä)ÅPACKAGEÄ©ÄRTMS©ÄMODE©ÅCOMMON-LISPÄ1Ä\Ä*ÅPUTPROPÄ\ÄBÄ8ÅDISPLAYÄBÄ:\ÄBÄ8p¿¨ÄUCLÄÏÄITEMSÄ1Ä\ÄBÄQ\ÄBÄ8BÄSBÄ:\ÄBÄ8p¿BÄU¨ÇCOMMANDS-WANTING-ONÄ1Ä\ÄBÄQ\ÄBÄ8ÉÅCOMMAND-MENUBÄ:\ÄBÄ8BÄW1Ä\ÄBÄQ\ÄBÄ8BÄaBÄ:\ÄBÄ8BÄ]1Ä\ÄBÄQ\ÄBÄ8ÉÅSYSTEM-MENUÄBÄ:\ÄBÄ8BÄW1Ä\ÄBÄQ\ÄBÄ8BÄjBÄ:\ÄBÄ8BÄ]ÄjÅ*PACKAGE*ÄNÄr√Å*DEFAULT-PKG*Ä1Ä\Äp¿¨ÄTICL,ÅPKG-GOTOÉÅ*PKG-STRING*1Ä\Äp¿BÄU¨ÅMAKE-SYNONYM\ÄBÄ8√Å*UI-RELATION*ÄBÄ:1Ä\ÄBÄ|\ÄBÄ8ÉÅ*UI-TUPLES*ÄBÄ:1Ä\ÄBÄ|\ÄBÄ8Ç*UI-TRANSACTION*BÄ:1Ä\ÄBÄ|\ÄBÄ8√Å*UI-FUNCTION*ÄBÄ:1Ä\ÄBÄ|\ÄBÄ8Ç*UI-ATTRIBUTES*ÄBÄ:1Ä\ÄBÄ|\ÄBÄ8ÉÅ*UI-FORMAT*ÄBÄ:1Ä\ÄBÄ|\ÄBÄ8CÅ*UI-FILE*ÄBÄ:1Ä\ÄBÄ|\ÄBÄ8√Å*UI-DATABASE*ÄÉÅ*ACTIVE-DB*Ä1Ä\ÄBÄ|\ÄBÄ8√Å*UI-DIRECTORY*\Äp¿BÄuÏÅSTRING-APPENDÄ¨ÄSYS:p¿BÄu,ÅUSER-IDÄlÄ;Ä1Ä\ÄBÄ|\ÄBÄ8CÅ*UI-TYPE*Ä\ÄBÄ8ÉÄXLDÄ1Ä\ÄBÄ|\ÄBÄ8√Å*UI-ATTR-DESC*BÄ:1Ä\ÄBÄ|\ÄBÄ8Å*UI-DOC*ÏÄ.....Ä1Ä\ÄBÄ|\ÄBÄ8Å*UI-KEY*BÄ:1Ä\ÄBÄ|\ÄBÄ8Å*UI-IMP*√Ñ*SYSTEM-RELATION-BASE-IMPLEMENTATION*Ä1Ä\ÄBÄ|\ÄBÄ8Å*UI-SS*ÄÉÑ*SYSTEM-RELATION-STORAGE-STRUCTURE*Ä1Ä\ÄBÄ|\ÄBÄ8ÉÅ*UI-VIEWDEF*BÄ:1Ä\ÄBÄ|\ÄBÄ8CÅ*UI-WHERE*jÄTÄ1Ä\ÄBÄ|\ÄBÄ8ÉÅ*UI-VALUES*ÄBÄ:1Ä\ÄBÄ|\ÄBÄ8√Å*UI-JOIN-INTO*BÄ:1Ä\ÄBÄ|\ÄBÄ8CÅ*UI-OVER*ÄBÄÃ1Ä\ÄBÄ|\ÄBÄ8CÅ*UI-INTO*ÄBÄ:1Ä\ÄBÄ|\ÄBÄ8CÅ*UI-FROM*ÄBÄ:1Ä\ÄBÄ|\ÄBÄ8CÅ*UI-WIDE*ÄBÄ:1Ä\ÄBÄ|\ÄBÄ8Å*UI-NUM*F¿1Ä\ÄBÄ|\ÄBÄ8CÅ*UI-SORT*ÄBÄ:1Ä\ÄBÄ|\ÄBÄ8ÉÅ*UI-OBJECT*ÄBÄ:1Ä\ÄBÄ|\ÄBÄ8CÅ*UI-REL2*ÄBÄ:1Ä\Äp¿¨ÄSYSÄlÅDEFCONST-1CÉ*LINE-AREA-DOCUMENTATION*Ä\ÄBÄ8\ÄÈÅDOCUMENTATIONÄ,ÄiÅMOUSE-L-1Ä,ÉTo see the entire line.ÄiÅMOUSE-M-2Ä¨ÇTo delete the tuple.iÅMOUSE-R-1Ä¨ÇTo modify the tuple.láThe wholine documentation string when a line is selected.Ä1Ä\ÄBÄ˙ÉÑ*DBMS-WINDOW-WHOLINE-DOCUMENTATION*Ä\ÄBÄ8\ÄÄ˛,äWindow for database output. Some items are made mouse-sensitive for inspection.ÄBÄlÇRTMS Command MenuÄiÅMOUSE-R-2Ä¨ÅSystem MenuÄl The wholine documentation string when in the RTMS interface output window.1Ä\ÄBÄ˙ÉÑ*INTERACTION-WHOLINE-DOCUMENTATION*Ä\ÄBÄ8\ÄÄ˛¨äThis window accepts user input. Input can also be provided through the command menu.BÄlÇRTMS Command MenuÄBÄ¨ÅSystem MenuÄ1Ä\ÄBÄ˙CÑ*ATTRIBUTE-WHOLINE-DOCUMENTATION*Ä\ÄBÄ8\ÄiÅMOUSE-ANYÄ¨ÑTo see this ATTRIBUTE's definition.Ä1Ä\ÄBÄ˙ÉÑ*DBMS-OBJECT-WHOLINE-DOCUMENTATION*Ä\ÄBÄ8\ÄBÄ,ÑTo see this object's definition.1Ä\ÄBÄ˙Ñ*RELATION-WHOLINE-DOCUMENTATION*\ÄBÄ8\ÄBÄ˛,ÄBÄ,ÑTo see the RELATION definition.ÄiÅMOUSE-M-1Ä,ÑTo modify the RELATION features.BÄlÉTo retrieve this RELATION.1Ä\ÄBÄ˙Ñ*DATABASE-WHOLINE-DOCUMENTATION*\ÄBÄ8\ÄBÄÏÜList the relations in this DATABASE, if it is active.Ä1Ä\Äp¿BÄ¯lÅDEFFLAVOR2\ÄBÄ8CÅMENU-PANEÄBÄ:\ÄBÄ8\Äp¿lÄWÄ¨ÄMENU\ÄBÄ8\Ä\ÄiÇDEFAULT-INIT-PLIST©ÅCOMMAND-MENUBÄÃ)ÅDYNAMICÄBÄÃ1Ä\ÄBÄ9\ÄBÄ8ÉÅDBMS-WINDOWÄBÄ:\ÄBÄ8\Äp¿lÄTV¨ÉLINE-AREA-TEXT-SCROLL-MIXINÄp¿BÄM¨ÉFUNCTION-TEXT-SCROLL-WINDOWÄp¿BÄMlÑMOUSE-SENSITIVE-TEXT-SCROLL-WINDOWp¿BÄM¨ÇMARGIN-REGION-MIXINÄp¿BÄM,ÇSCROLL-BAR-MIXINp¿BÄMÏÅANY-TYI-MIXINÄp¿BÄ>ÏÄWINDOWBÄ:Ä\ÄÈÄMETHODBÄJÈÉLINE-AREA-MOUSE-DOCUMENTATIONÄÄÎÄFÄ@FÄ¿$Ä¿BÄ:p¿BÄuÏÄART-QÄ]ÄFÄÄ:BÄ:BÄ:BÄÃFÄp¿BÄ¯lÇDEBUG-INFO-STRUCTÄBÄ]\Äp¿BÄ¯¨Å.OPERATION.ÄBÄ:BÄ:\Ä©ÅSELF-FLAVORÄBÄJÄBÄ˚ëOÄpBÄ]Ä1Ä\ÄBÄ9\ÄBÄ8ÉDBMS-WINDOW-WITH-TYPEOUTBÄ:\ÄBÄ8\Äp¿BÄM,ÑTEXT-SCROLL-WINDOW-TYPEOUT-MIXINBÄJ\ÄBÄ8\Ä\ÄBÄDÈÅTYPEOUT-WINDOW\ÄBÄ8\Äp¿BÄMÏÅTYPEOUT-WINDOW)ÉDEEXPOSED-TYPEOUT-ACTION\ÄiÇEXPOSE-FOR-TYPEOUTÄ\ÄBÄ^BÄsÈÉWHO-LINE-DOCUMENTATION-STRINGÄÄÎÄ(ÜÄ@HFÄ ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄÑ\ÄBÄmBÄ:\ÄÍÄIGNORECÅM-S-I-TYPE\Ä)ÇMACROS-EXPANDEDÄ\Äp¿BÄu¨ÄSEND™ÄCASEBÄoBÄsÄBÄ	—BÄ"—BÄ2—BÄ(—BÄ—p¿BÄ¯,ÅMOUSE-YÄ—p¿BÄ¯,ÅMOUSE-XÄ—Ç*OUTPUT-WINDOW*Äë©ÇMOUSE-SENSITIVE-ITEM¿FÄêÄFÄ¿CÅATTRIBUTEÄ¿ÅRELATION¿ÅDATABASE¿ÉÅDBMS-OBJECTÄ¿FÄ¿FÄ>¿FÄ:¿FÄ;¿FÄ<¿FÄ=ÄP	PPP
PAA¡@¡AQrOÄ©BÄÑÄ1Ä\ÄBÄ9\ÄBÄ8ÇINTERACTION-PANEBÄ:\ÄBÄ8\Äp¿BÄUÏÉCOMMAND-AND-LISP-TYPEIN-WINDOWp¿BÄMÏÉPREEMPTABLE-READ-ANY-TYI-MIXINBÄ:Ä\ÄBÄ^BÄ¨BÄÖÄÎÄFÄ@FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ¥\ÄBÄmBÄ:BÄ:\ÄBÄoBÄ¨ÄBÄëOÄæBÄ¥ÄÄ\ÄBÄ^BÄ¨ÈÄBEFOREÈÄSELECTÄÎÄÜÄ`DFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄø\ÄBÄmÍÄ&RESTÄBÄèBÄ:\ÄBÄè\ÄBÄí\ÄBÄïBÄoBÄ¨ÄÉÅDBMS-FRAME1ÄëÈÄEXPOSEÄPåOÄ–BÄøÄÄ\ÄBÄ^BÄ¨ÈÄAFTERÄBÄ¡ÄÎÄÜÄ`DFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ—\ÄBÄmBÄ BÄèBÄ:\ÄBÄè\ÄBÄoBÄ¨ÄROÄ›BÄ—Ä1Ä\ÄBÄ9\ÄBÄ8ÅDBMS-RCÄBÄ:\ÄBÄ8\Äp¿BÄUlÇCOMMAND-LOOP-MIXINp¿BÄM¨ÅSTREAM-MIXINp¿BÄMlÑINFERIORS-NOT-IN-SELECT-MENU-MIXINp¿BÄM,ÜBORDERED-CONSTRAINT-FRAME-WITH-SHARED-IO-BUFFERÄ\ÄBÄ8\Ä\ÄBÄDiÅMENU-PANES\ÄBÄ8\Ä\ÄÅS-M-PANEBÄjÈÇACTIVE-COMMAND-TABLESÄ\ÄBÄ8\ÄÉÅDBMS-COMTABÄiÇALL-COMMAND-TABLES\ÄBÄ8\ÄBÄˆÈÅTYPEIN-HANDLER©ÇHANDLE-TYPEIN-INPUTÄÈÅMINIMUM-WIDTHÄ\ÄBÄïp¿BÄMÏÅDEFAULT-SCREENÈÄWIDTHÄÈÅMINIMUM-HEIGHT\ÄBÄïBÄˇÈÄHEIGHTiÅBASIC-HELP\ÄBÄ8\ÄÉÄHELPÈÅPRINT-FUNCTION\ÄBÄ8CÅNEW-PRINTÄÈÅPRINT-RESULTS?\Ä*ÅFUNCTION\ÄÍÄLAMBDABÄ:BÄÃÈÄPANESÄ\Äp¿BÄ¯lÅXR-BQ-LIST\ÄBÄ\ÄBÄ8√ÄO-PANE\ÄBÄ8BÄs\ÄBÄ8iÅBLINKER-PÄBÄ:\ÄBÄ8BÄ\ÄBÄ8ÉÅDBMS-PRINTER\ÄBÄ8iÇPRINT-FUNCTION-ARGBÄ:\ÄBÄ8)ÇSCROLL-BAR-SIDEÄ\ÄBÄ8ÈÄRIGHTÄ\ÄBÄ8)ÇSCROLL-BAR-MODEÄ\ÄBÄ8)ÅMAXIMUMÄ\ÄBÄ8)ÅBORDERSÄBÄ:\ÄBÄ8ÈÄLABELÄ\ÄÄÈÄBOTTOMÈÄSTRINGÏÄOUTPUT©ÄFONTp¿ÏÄFONTSÄ,ÅCPTFONTÄ\ÄBÄ8)ÅFONT-MAP\ÄBÄp¿BÄ0,ÅCPTFONTB\ÄBÄ8©ÇSENSITIVE-ITEM-TYPES\ÄBÄ\ÄBÄ8BÄ†\ÄBÄ8BÄü\ÄBÄ8BÄ°\ÄBÄ8BÄ¢\ÄBÄ\ÄBÄ8√ÄI-PANE\ÄBÄ8BÄ¨\ÄBÄ8iÅSAVE-BITSÄBÄÃ\ÄBÄ8BÄ\ÄBÄ8©ÄOFFÄ\ÄBÄ8BÄ*\ÄÄBÄ,BÄ-ÏÅRtms InterfaceBÄ/p¿BÄ0ÏÄMEDFNT\ÄBÄ8BÄ(FÄ\ÄBÄ8BÄ4\ÄBÄp¿BÄ0ÏÄMEDFNB\Äp¿BÄ¯¨ÅXR-BQ-LIST*Ä\ÄBÄ8BÄÚ\ÄBÄ8BÄ;\ÄBÄ8BÄ4\ÄBÄp¿BÄ0ÏÄHL12BÄ\ÄBÄ8\Ä©ÄROWSFÄBÄ*BÄ:©ÅCONSTRAINTSÄ\ÄBÄ8\Ä\ÄÉÄMAIN\ÄBÄBÄABÄÚ\Ä\ÄBÄÚFÄÈÄLINESÄ\Ä\ÄBÄ®ÄL?ÕÃ\Ä\ÄBÄA©ÄEVEN\ÄÈÅINIT-KEYWORDSÄBÄ˙BÄ˚Ä\ÄBÄ^BÄ‡©ÇHANDLE-UNKNOWN-INPUTÄÎÄ=.ôÜ¿ø√≥FÄk¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄs\ÄBÄmBÄ:\ÄÉÄITEMp¿BÄ¯¨Å.CASE.ITEM.ÄBÄÄ\ÄBÄí\ÄÍÄFOURTHÍÄFIRSTÄBÄïBÄñÄBÄ‡ÄÍÅ*TERMINAL-IO*Äë1Ä]Äp¿BÄ¯lÉFLAVOR-VAR-SELF-REF-INDEXÄ]ÄBÄ8]ÄBÄ‡p¿BÄU,ÇINPUT-MECHANISMÄ¿p¿BÄu¨ÄBEEP“)ÇSEND-IF-HANDLESÄ¿iÅFRESH-LINE¿¨Ä ** ¿™ÅWRITE-STRING“1Ä]ÄBÄà]ÄBÄ8]ÄBÄ‡p¿BÄUÏÅERROR-MESSAGEÄ¿lÇUnrecognized input¿ÍÄPRINCÄ“1Ä]ÄBÄà]ÄBÄ8]ÄBÄ‡p¿BÄUlÅKBD-INPUTÄ¿√ÅHELP-LINE-AREA“CÇHELP-LINE-AREA-MOD“CÇHELP-LINE-AREA-DEL“ÍÄSTRING“ÉÅHELP-OBJECTÄ“*ÇREAD-FROM-STRING“ÅRETRIEVE“ÉÅHELP-MODIFYÄíFÄ¿p¿BÄU¨ÄMENU¿p¿BÄUÏÅKEY-OR-BUTTONÄ¿p¿BÄUÏÄTYPEIN¿FÄ¿FÄ ¿FÄ|¿FÄ}¿FÄ~ÄFÄ¿iÅLINE-AREAÄ¿BÄü¿BÄ°¿BÄ¢¿BÄ†¿FÄ¿FÄ”¿FÄé¿FÄô¿FÄ¢¿FÄ´¿FÄ¥ÄFÄ¿•ÄÄ¿•ÄÄ¿•ÄÄ	¿FÄ¿FÄò¿FÄí¿FÄî¿FÄñÄFÄ¿BÄ¡¿•ÄÄ¿FÄ¿FÄ ¿FÄ∏¿FÄ¡ÄPrÑÑPPêÄ	P
àP‚PàR4I‰RA√ rPBB√-rVåVåVåRV@√ˇ5‰@W¸@QäåV@√ˇ5‰@W¸@QäåV@√ˇ5‰@W¸@QäåPBB√6rV@√Å‰@Qäå@QåV@√Å‰@Qäå@QåV@√ˇ5‰@W¸@QäåÑROÄŒBÄsÄÄ\ÄBÄ^BÄ‡ÇACTIVE-DATABASEÄÄÎÄ	ÜÄ@	DFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄœ\ÄBÄmBÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛láReturns the name of the active database. (ACTIVE-DATABASE)ÄBÄõë©ÅAPPEND-ITEMÄ¿lÄ~S¿\ÄBÄ–¿ÍÄFORMAT“BÄ–íP@¡Pˇ€PPö@ëP@¡Pˇ€PÇö@ïOÄ‡BÄœÄ1Ä\ÄjÄOR\Äp¿BÄU,ÉRE-USE-COMMAND-INSTANCE?\ÄBÄ8\ÄBÄ^BÄ‡BÄ–\ÄBÄ8\ÄÈÄNAMESÄ,ÇActive DatabaseÄiÅDEFINITIONBÄÁ)ÅDEFNAMEÄBÄÁ©ÄKEYS\Ä\Ä•Ä F•Ä ABÄ˛,ÖReturns the name of the active database.ÈÄMENUSÄ\Ä\ÄBÄaÈÄCOLUMNÏÅOther Features©ÅDESCRIPTIONÄBÄ€\ÄBÄ8p¿BÄU,ÅCOMMANDÄ\Äp¿BÄulÇINSTANTIATE-FLAVOR\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄÎBÄÏBÄÁBÄÌBÄÁBÄÓBÄÔBÄ˛BÄÛBÄÙBÄıBÄ˘BÄ€BÄÃÄ\ÄBÄ^BÄ‡CÇABORT-TRANSACTIONÄÄÎÄ	ÜÄ@	DFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ\ÄBÄmBÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛làTerminates the special transaction processing. (ABORT-TRANSACTION)ÄBÄõëBÄ‹¿lÄ~S¿\ÄBÄ¿BÄﬂ“BÄíP@¡Pˇ€PPö@ëP@¡Pˇ€PÇö@ïOÄBÄÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ\ÄBÄ8\ÄBÄÍlÇAbort TransactionÄBÄÏBÄBÄÌBÄBÄÓ\Ä\Ä•Ä TBÄÚBÄ˛ÏÖTerminates the special transaction processing.BÄÙ\Ä\ÄBÄaBÄ˜ÏÅOther FeaturesBÄ˘BÄ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄBÄÏBÄBÄÌBÄBÄÓBÄBÄ˛BÄBÄÙBÄBÄ˘BÄBÄÃÄ\ÄBÄ^BÄ‡CÇBEGIN-TRANSACTIONÄÄÎÄ	ÜÄ@	DFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ(\ÄBÄmBÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏáBegins the special transaction processing. (BEGIN-TRANSACTION)ÄBÄõëBÄ‹¿lÄ~S¿\ÄBÄ)¿BÄﬂ“BÄ)íP@¡Pˇ€PPö@ëP@¡Pˇ€PÇö@ïOÄ7BÄ(Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ)\ÄBÄ8\ÄBÄÍlÇBegin TransactionÄBÄÏBÄ;BÄÌBÄ;BÄÓ\Ä\ÄBÄ•Ä BBÄ˛lÖBegins the special transaction processing.BÄÙ\Ä\ÄBÄaBÄ˜ÏÅOther FeaturesBÄ˘BÄ4\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ>BÄÏBÄ;BÄÌBÄ;BÄÓBÄ?BÄ˛BÄBBÄÙBÄCBÄ˘BÄ4BÄÃÄ\ÄBÄ^BÄ‡ÇEND-TRANSACTIONÄÄÎÄ	ÜÄ@	DFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄL\ÄBÄmBÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏèExecutes the database calls postponed due to special transaction processing and terminates the transaction.  (END-TRANSACTION)ÄBÄõëBÄ‹¿lÄ~S¿\ÄBÄM¿BÄﬂ“BÄMíP@¡Pˇ€PPö@ëP@¡Pˇ€PÇö@ïOÄ[BÄLÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄM\ÄBÄ8\ÄBÄÍ,ÇEnd TransactionÄBÄÏBÄ_BÄÌBÄ_BÄÓ\Ä\ÄBÄ•Ä EBÄ˛¨
Executes the database calls postponed due to special transaction processing and terminates the transaction.ÄBÄÙ\Ä\ÄBÄaBÄ˜ÏÅOther FeaturesBÄ˘BÄX\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄbBÄÏBÄ_BÄÌBÄ_BÄÓBÄcBÄ˛BÄfBÄÙBÄgBÄ˘BÄXBÄÃÄ\ÄBÄ^BÄ‡CÇENVIRONMENT-STATUSÄÎÄ	ÜÄ@	DFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄp\ÄBÄmBÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏàReturns the values of the environment variables. (ENVIRONMENT-STATUS)ÄÄBÄõëBÄ‹¿lÄ~S¿\ÄBÄq¿BÄﬂ“BÄqíP@¡Pˇ€PPö@ëÑOÄBÄpÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄq\ÄBÄ8\ÄBÄÍlÇEnvironment StatusBÄÏBÄÉBÄÌBÄÉBÄÓ\Ä\ÄBÄÒBÄeBÄ˛,ÜReturns the values of the environment variables.BÄÙ\Ä\ÄBÄaBÄ˜ÏÅOther FeaturesBÄ˘BÄ|\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄÜBÄÏBÄÉBÄÌBÄÉBÄÓBÄáBÄ˛BÄ BÄÙBÄäBÄ˘BÄ|BÄÃÄ\ÄBÄ^BÄ‡ÇATTACH-RELATIONÄÄÎÄ5ÜÄB»FÄ$¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄì\ÄBÄmBÄ†ÉÄATTÄÉÄPATHÉÄTUPÄÉÄDIRÄÉÄDOCÄÉÄKEYÄÉÄIMPÄCÄSSÉÄMEMÄBÄ:\ÄÅKEYWORDSBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏøŸAttach some existing data to relation.

   RELATION-NAME - The name of the relation to which the data is to be attached.
   ATTRIBUTES    - A list that describes the attributes in this relation.
   DIRECTORY     - The directory in which RTMS saves the attached data.
   DOCUMENTATION - A string that describes the specified relation.
   FORMAT        - A list corresponding to the ATTRIBUTES specifying their print width.
   IMPLEMENTATION-TYPE - Name of the implementation type.
   KEY           - A list of attributes that are to form the key.
   MEMORY        - Specifies a variable where the data to be attached is stored.
   PATHNAME      - If the data is stored in a file, specify its name here.
   STORAGE-STRUCTURE - Name of the storage-structure type.  (ATTACH-RELATION RELATION-NAME &REST KEYWORD-LIST &KEY &OPTIONAL ATTRIBUTES DIRECTORY DOCUMENTATION FORMAT IMPLEMENTATION-TYPE KEY MEMORY PATHNAME STORAGE-STRUCTURE &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄî¿BÄﬂ¿BÄ†¿BÄ°¿BÄû¿BÄ¢¿BÄ£¿ÉÄSTOÄ¿BÄù¿BÄ•¿™ÄLIST“BÄﬂ“BÄîíPA¡Pˇ€PPÅQPÑQPÖQ	PÜQ
PÉQPáQPàQP QPÇQPäQJ∫@√ööAëÅQ@QîOÄÆBÄìÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄî\ÄBÄ8\ÄBÄÍ,ÇAttach RelationÄBÄÏBÄ≤BÄÌBÄ≤BÄÓ\ÄBÄÚBÄ˛lÉused to attach a relation.BÄÙ\Ä\ÄBÄaBÄ˜lÅDefinitioniÅARGUMENTSÄ\ÄÈÅUSER-SUPPLIEDÄ\ÄÄ*ÏÅRelation Name:)ÅDEFAULTÄBÄ:©ÄTYPE\ÄBÄ˛¨ÑName of the relation to be attached.©ÄSEXP\ÄÄ*lÉAttribute descriptor pair:BÄ¿BÄ∞BÄ¡\ÄBÄ˛,ùList of attributes and their domains default, and documentation. EX. (a1 (dom <something> def <something>) a2) . If any values are not given there is a default for everything. So, the minimum necessary input is a list of attributes.BÄƒ\ÄÄ*lÅPathname:ÄBÄ¿BÄñBÄ¡\ÄBÄ˛¨ÑSpecify the name of the input file.ÄBÄƒ\ÄÄ*ÏÅTuple Format :BÄ¿BÄíBÄ¡\ÄBÄ˛,îSpecify the tuple format as a list of numbers representing the column width for each attribute. If not specified, the default format for this relation is used.ÄBÄƒ\ÄÄ*lÅDirectory:BÄ¿BÄüBÄ¡\ÄBÄ˛¨ÖSpecify the save directory for the relation.BÄƒ\ÄÄ*ÏÅDocumentation:BÄ¿lÄ..BÄ¡\ÄBÄ˛¨ÖSpecify the documentation for this relation.BÄ-\ÄÄ*¨ÄKey:BÄ¿BÄπBÄ¡\ÄBÄ˛,ÖSpecify the key as a list of attributes.BÄƒ\ÄÄ*¨ÇImplementation Type:BÄ¿BÄΩBÄ¡\ÄBÄ˛,ÑSpecify the implementation type.BÄƒ\ÄÄ*lÇStorage Structure:BÄ¿BÄ¬BÄ¡\ÄBÄ˛¨ÑSpecify the storage structure type.ÄBÄƒ\ÄÄ*,ÅMemory:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛,åIf the data is stored in the memory, then give the name of the variable that contains the data.ÄBÄƒBÄ*¨ÑGive parameters for ATTACH RELATION:BÄ˘BÄ™\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄµBÄÏBÄ≤BÄÌBÄ≤BÄÓBÄ∂BÄ˛BÄ∑BÄÙBÄ∏BÄªBÄºBÄ˘BÄ™BÄÃÄ\ÄBÄ^BÄ‡ÇRENAME-ATTRIBUTEÄÎÄÜÄ@ƒFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄÒ\ÄBÄmBÄ†ÅOLD-NEWÄBÄ:BÄ:\ÄÄí\ÄBÄUBÄïBÄoBÄ‡BÄ˛,®Use this function to rename attributes in a relation.

   RELATION-NAME  - Name of the relation whose attributes are to be renamed.
   ATTRIBUTES     - Specify old-attribute and new-attribute names.

   Example: (RENAME-ATTRIBUTE 'parts 'number 'id 'name 'description).  (RENAME-ATTRIBUTE RELATION-NAME &REST ATTRIBUTES)ÄBÄõëBÄ‹¿lÄ~S¿BÄÚ¿ÍÄLIST*Ä“BÄﬂ“CÇRENAME-ATTRIBUTE*Ä¿™ÄEVALíP@¡Pˇ€PPÅQÇQöö@ë	PÅQÇQö
åOÄBÄÒÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄÚ\ÄBÄ8\ÄBÄÍ,ÇRename AttributeBÄÏBÄBÄÌBÄBÄÓ\Ä\Ä•Ä RBÄÚBÄ˛,Öused to rename attributes in a relation.BÄÙ\Ä\ÄBÄaBÄ˜ÏÅOther FeaturesBÄª\ÄBÄΩ\ÄÄ*ÏÅRelation Name:BÄ¿BÄ~BÄ¡\ÄBÄ˛,áName of the relation whose attributes are to be renamed.BÄƒ\ÄÄ*,ÑAttributes and their new names:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛ÏäSpecify a list of the attributes and their new names. For ex. (a1 new-a1 a2 new-a2...)BÄƒBÄ*ÏÑGive parameters for RENAME ATTRIBUTE:ÄBÄ˘BÄ˛\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ
BÄÏBÄBÄÌBÄBÄÓBÄBÄ˛BÄBÄÙBÄBÄªBÄBÄ˘BÄ˛BÄÃÄ\ÄBÄ^BÄ‡ÇRENAME-RELATIONÄÄÎÄ
ÜÄ@
ÑFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ"\ÄBÄmBÄ˚BÄ:BÄ:\ÄÄí\Äp¿BÄ¯lÅXR-BQ-CONSBÄïBÄoBÄ‡BÄ˛lóRename relations in the active database.

   RELATIONS - Specify <old-rel-name new-rel-name>

   Example: (RENAME-RELATION rel1 new-rel1 rel2 new-rel2)  (RENAME-RELATION &REST RELATIONS)ÄBÄõëBÄ‹¿lÄ~S¿BÄ#¿BÄﬂ“ÇRENAME-RELATION*¿BÄíP@¡Pˇ€PPÅ]ö@ëPÅ]	åOÄ3BÄ"Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ#\ÄBÄ8\ÄBÄÍ,ÇRename RelationÄBÄÏBÄ7BÄÌBÄ7BÄÓ\Ä\ÄBÄBÄBÄ˛lÜused to rename relations in the current database.ÄBÄÙ\Ä\ÄBÄaBÄ˜ÏÅOther FeaturesBÄª\ÄBÄΩ\ÄÄ*ÏÉRelations and their new names:BÄ¿BÄ:BÄ¡\ÄBÄ˛låSpecify a list of the relations and their new names. For ex. (rel-1 new-rel-1 rel-2 new-rel-2...)ÄBÄƒBÄ*¨ÑGive parameters for RENAME RELATION:BÄ˘BÄ0\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ:BÄÏBÄ7BÄÌBÄ7BÄÓBÄ;BÄ˛BÄ=BÄÙBÄ>BÄªBÄABÄ˘BÄ0BÄÃÄ\ÄBÄ^BÄ‡ÇRENAME-DATABASEÄÄÎÄ
ÜÄ@
ÑFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄM\ÄBÄmBÄ˚BÄ:BÄ:\ÄÄí\ÄBÄ/BÄïBÄoBÄ‡BÄ˛lôUsed to rename a database.

   DATABASES - Specify old-database-name and new-database-name.

   Example: (RENAME-DATABASE parts suppliers micro-parts micro-suppliers).  (RENAME-DATABASE &REST DATABASES)ÄBÄõëBÄ‹¿lÄ~S¿BÄN¿BÄﬂ“ÇRENAME-DATABASE*¿BÄíP@¡Pˇ€PPÅ]ö@ëPÅ]	åOÄ\BÄMÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄN\ÄBÄ8\ÄBÄÍ,ÇRename DatabaseÄBÄÏBÄ`BÄÌBÄ`BÄÓ\Ä\ÄBÄ•Ä@DBÄ˛lÉused to rename databases.ÄBÄÙ\Ä\ÄBÄaBÄ˜ÏÅOther FeaturesBÄª\ÄBÄΩ\ÄÄ*ÏÉDatabases and their new names:BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏãSpecify a list of the databases and their new names. For ex. (db-1 new-db-1 db-2 new-db-2...)ÄBÄƒBÄ*¨ÑGive parameters for RENAME DATABASE:BÄ˘BÄY\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄcBÄÏBÄ`BÄÌBÄ`BÄÓBÄdBÄ˛BÄgBÄÙBÄhBÄªBÄkBÄ˘BÄYBÄÃÄ\ÄBÄ^BÄ‡ÇDETACH-RELATIONÄÄÎÄ#ÜÄAHFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄw\ÄBÄmBÄ†BÄûBÄ•ÉÄDISKBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛Ïø1Detach data in an existing relation into a variable or onto the disk.

   RELATION-NAME - The name of the relation from which the data is to be detached.
   DISK          - If T, RTMS stores the data in the file specified in the PATHNAME.
   MEMORY        - If set to T, the detached data is stored in the variable rtms:*attach-detach-data*.
                   If any variable name is supplied, the data will be stored in it.
   PATHNAME      - Name of the file in which the detached data is to be saved.  (DETACH-RELATION RELATION-NAME &REST KEYWORD-LIST &KEY &OPTIONAL DISK MEMORY PATHNAME &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿BÄx¿BÄû¿BÄ•¿BÄÅ¿BÄ≠“BÄﬂ“BÄxíPA¡Pˇ€PPÅQPÇQPÉQ	PÑQ
≤@√
ööAëÅQ@QîOÄáBÄwÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄx\ÄBÄ8\ÄBÄÍ,ÇDetach RelationÄBÄÏBÄãBÄÌBÄãBÄÓ\Ä•Ä DBÄ˛lÉused to detach a relation.BÄÙ\Ä\ÄBÄaBÄ˜lÅDefinitionBÄª\ÄBÄΩ\ÄÄ*ÏÅRelation Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨ÑName of the relation to be Detached.BÄƒ\ÄÄ*lÅPathname:ÄBÄ¿BÄñBÄ¡\ÄBÄ˛¨áSpecify the name of the file where the data is to be stored.BÄƒ\ÄÄ*,ÅMemory:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛¨ãIf the data is to be in the memory and not save it on the disk, give the name of a variable.BÄƒ\ÄÄ*ÏÄDisk:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛,äIndicate if files corresponding to the relation are to be deleted from the disk.)ÅBOOLEANÄBÄ*¨ÑGive parameters for DETACH RELATION:BÄ˘BÄÖ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄéBÄÏBÄãBÄÌBÄãBÄÓBÄèBÄ˛BÄëBÄÙBÄíBÄªBÄïBÄ˘BÄÖBÄÃÄ\ÄBÄ^BÄ‡√ÅINSERT-TUPLESÄÄÎÄ#ÜÄAHFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄÆ\ÄBÄm√ÅRELATION-NAMEÄ√ÅLIST-OF-TUPLESCÅATTRIBUTES*ÅPATHNAMEBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛Ïø$Insert a list of tuples or data from a file.

   RELATION-NAME   - Name of the relation into which the data is to be inserted.
   TUPLES     - List of tuples to be inserted. Tuples are expected to be in the list-of-values format.
   ATTRIBUTES - If the values in the tuples do not correspond to the attribute-list specified during
                relation-defintion, specify a list of attributes to determine the order.
   PATHNAME   - If the data is in a file, specify the name of the file.  (INSERT RELATION-NAME &REST KEYWORD-LIST &KEY TUPLES ATTRIBUTES PATHNAME &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿√ÄINSERT¿√ÄTUPLES¿ÉÄATTR¿BÄû¿BÄ≠“BÄﬂ“BÄ¡íPA¡Pˇ€PPÅQPÇQPÉQ	PÑQ
≤@√
ööAëÅQ@QîOÄƒBÄÆÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄØ\ÄBÄ8\ÄBÄÍÏÅInsert TuplesÄBÄÏBÄ»BÄÌBÄ»BÄÓ\Ä•Ä IBÄ˛¨ÜUsed to insert a list of tuples in a given relation.BÄÙ\Ä\ÄBÄaBÄ˜lÅDefinitionBÄª\ÄBÄΩ\ÄÄ*ÏÅRelation Name:BÄ¿BÄ~BÄ¡\ÄBÄ˛ÏáSpecify the relation into which the tuples are to be inserted.BÄƒ\ÄÄ*,ÇList of tuples:ÄBÄ¿BÄÇBÄ¡\ÄBÄ˛ÏÑGive a list of tuples to be inserted.ÄBÄƒ\ÄÄ*¨ÅAttributes:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛¨åIf a list of attributes is provided, then values in the tuples are assumed to be in the same order.ÄBÄƒ\ÄÄ*lÅPathname:ÄBÄ¿BÄñBÄ¡\ÄBÄ˛¨äIf a list of tuples is not provided, then specify the file which contains the data.ÄBÄƒBÄ*ÏÑGive parameters for INSERTING TUPLES:ÄBÄ˘BÄø\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄÀBÄÏBÄ»BÄÌBÄ»BÄÓBÄÃBÄ˛BÄŒBÄÙBÄœBÄªBÄ“BÄ˘BÄøBÄÃÄ\ÄBÄ^BÄ‡ÅMAPTUPLEÄÎÄÜÄ@ƒFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄÍ\ÄBÄmBÄ†CÅDBFUNCTIONBÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏòMap a function on all the tuples in a relation using MAPCAR.

   DBFUNCTION  - Function to be applied to each and every tuple.
   RELATION    - Name of the relation.  (MAPTUPLE DBFUNCTION RELATION)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄÎ¿BÄ≠“BÄﬂ“BÄ“BÄÎíP@¡Pˇ€PPÇQÅQöö@ëÇQ	äÅQ
îOÄ˘BÄÍÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄÎ\ÄBÄ8\ÄBÄÍ,ÅMaptupleBÄÏBÄ˝BÄÌBÄ˝BÄÓ\Ä\ÄBÄÒ•Ä MBÄ˛¨àMaps a given function on all the tuples in a relation using MAPCAR.ÄBÄÙ\Ä\ÄBÄaBÄ˜ÏÅOther FeaturesBÄª\ÄBÄΩ\ÄÄ*ÏÅRelation Name:BÄ¿BÄ~BÄ¡\ÄBÄ˛,ÑGive the relation to be mapped.ÄBÄƒ\ÄÄ*¨ÇFunction DefinitionÄBÄ¿BÄäBÄ¡\ÄBÄ˛ÏÉSpecify a function definition.BÄƒBÄ*lÖMap a function on all tuples using MAPCAR:BÄ˘BÄ˜\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄBÄÏBÄ˝BÄÌBÄ˝BÄÓBÄBÄ˛BÄBÄÙBÄBÄªBÄBÄ˘BÄ˜BÄÃÄ\ÄBÄ^BÄ‡ÉÄMAPTÄÎÄÜÄ@ƒFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ\ÄBÄmBÄ†BÄÙBÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛,òMap a function on all the tuples in a relation using MAPC.

   DBFUNCTION  - Function to be applied to each and every tuple.
   RELATION    - Name of the relation.  (MAPT DBFUNCTION RELATION)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄ¿BÄ≠“BÄﬂ“BÄ“BÄíP@¡Pˇ€PPÇQÅQöö@ëÇQ	äÅQ
îOÄ&BÄÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ\ÄBÄ8\ÄBÄÍ¨ÄMaptBÄÏBÄ*BÄÌBÄ*BÄÓ\Ä•Ä`FBÄ˛làMaps a given function on all the tuples in a relation using MAPC.ÄBÄÙ\Ä\ÄBÄaBÄ˜ÏÅOther FeaturesBÄª\ÄBÄΩ\ÄÄ*ÏÅRelation Name:BÄ¿BÄ~BÄ¡\ÄBÄ˛,ÑGive the relation to be mapped.ÄBÄƒ\ÄÄ*¨ÇFunction DefinitionÄBÄ¿BÄäBÄ¡\ÄBÄ˛ÏÉSpecify a function definition.BÄƒBÄ*,ÖMap a function on all tuples using MAPC:BÄ˘BÄ$\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ-BÄÏBÄ*BÄÌBÄ*BÄÓBÄ.BÄ˛BÄ0BÄÙBÄ1BÄªBÄ4BÄ˘BÄ$BÄÃÄ\ÄBÄ^BÄ‡√ÅPRINT-RELATIONÄÎÄPÜÄ‡FÄ6¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄD\ÄBÄmBÄ†ÉÄINTOBÄ†BÄ°BÄ¢BÄ£BÄ¨√ÄQPRINTÅTO-FILEÄ™ÄSORTBÄﬂÉÄWIDEÍÄNUMBERÍÄPRINTÄBÄ¬√ÄQSORTÄÍÄSTREAM√ÄUNIQUEBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏøSame as Retrieve without a where clause and all attributes are retrieved.

   RELATION-NAME        - Name of the relation whose tuples are to be retrieved.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   INDEX-NAME           - Name of the index to use in the retrieval.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   WIDE                 - If T, result is printed out in Attribute: value format rather than as a table.
   NUMBER               - If WIDE is T, this keyword specifies the number of attributes per line.
   OUTPUT               - If the result is to be sent to a file, specify the file-name.
   PRINT                - If NIL, the result is not printed.
   QPRINT               - If T, the result is printed without formatting.
   QUICK-SORT           - Specifies the attributes to sort the result on.
   SORT                 - If any domain-specific, user-defined sort mechanism is to be used, this keyword
                          can be used.
   STREAM               - Specify the window to which the output is to be sent, if it is different than the
                          the *standard-output* or RTMS-interface.
   TUPLES               - If T, the resultant tuples are returned.
   UNIQUE               - If T, only unique tuples are retrieved.  (PRINT-RELATION RELATION &REST KEYWORD-LIST &KEY &OPTIONAL DIRECTORY DOCUMENTATION FORMAT IMPLEMENTATION-TYPE INDEX-NAME INTO KEY NUMBER OUTPUT PRINT QPRINT QUICK-SORT SORT STREAM STORAGE-STRUCTURE TUPLES UNIQUE WIDE &ALLOW-OTHER-KEYS)Ä¿ÜÄÇiÄBÄõëBÄ‹¿lÄ~S¿BÄ®¿BÄ†¿BÄ°¿BÄN¿BÄO¿√ÅOUTPUT-TO-FILE¿BÄQ¿BÄﬂ¿BÄR¿ÉÄNUMÄ¿BÄ¢¿BÄT¿BÄ¬¿CÅQUICK-SORT¿BÄV¿BÄW¿BÄ£¿BÄ¨¿BÄ≠“BÄﬂ“BÄ®íPA¡Pˇ€PPÅQPÉQ	PÑQ
PÇQPà?BP QPäQPãQPåQP
QPÖQPéQPèQPêQPëQPíQPÜQPáQ"J∫@√ööAëÅQ@QîOÄaBÄDÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄE\ÄBÄ8\ÄBÄÍÏÅPrint RelationBÄÏBÄeBÄÌBÄeBÄÓ\Ä\ÄBÄÒ•Ä PBÄ˛¨ÑUsed to print tuples in a relation.ÄBÄÙ\Ä\ÄBÄaBÄ˜ÏÅOther FeaturesBÄª\ÄBÄΩ\ÄÄ*lÅRelation: BÄ¿BÄ~BÄ¡\ÄBÄ˛¨ÜSpecify a relation whose tuples are to be retrieved.BÄƒ\ÄÄ*ÏÄINTO :BÄ¿BÄ‹BÄ¡\ÄBÄ˛,
Specify the relation to insert the resultant tuples into. If none specified, they are just printed out.ÄBÄƒBÄ—\ÄÄ*ÏÅDocumentation:BÄ¿BÄ¥BÄ¡\ÄBÄ˛lÜSpecify the documentation for the output relation.BÄ-BÄ⁄BÄﬁBÄ‚\ÄÄ*lÇFormatted Output: BÄ¿BÄÃBÄ¡\ÄBÄ˛¨ãShould the tuples returned be formatted?. If no, tuples are printed in the interaction pane.BÄ¶\ÄÄ*¨ÅOutput File:BÄ¿BÄñBÄ¡\ÄBÄ˛láIf the output is to be sent to a file, specify a pathname.BÄƒ\ÄÄ*ÏÄSort: BÄ¿BÄÌBÄ¡\ÄBÄ˛¨åShould the output be sorted? Legal values are: (<attribute-name order>) - order could be ASC or DES.BÄƒBÄÕ\ÄÄ*ÏÅWide-Format :ÄBÄ¿BÄ‰BÄ¡\ÄBÄ˛¨èShould the tuples be printed in wide format instead of tabular format? - Wide format will be of the type <attribute: value>.BÄ¶\ÄÄ*ÏÉNumber of attributes per line:BÄ¿BÄËBÄ¡\ÄBÄ˛,ëHow many attributes per line if the tuples are printed using wide format?. Default is -1 indicating as many tuples per line as possible.ÈÄNUMBER\ÄÄ*,ÅPrint?:ÄBÄ¿BÄÃBÄ¡\ÄBÄ˛ÏÑShould the results be printed or not?ÄBÄ¶\ÄÄ*,ÅTuples:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛¨ÜShould the results be returned as a list of tuples?ÄBÄ¶\ÄÄ*¨ÅQuick Sort:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛ÏäSimilar to sort except that it does not take user defined domains into consideration.ÄBÄƒ\ÄÄ*,ÅStream:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛ÏãIf the output is to be sent to a stream other than the output window, specify the stream name.BÄƒ\ÄÄ*,ÅUnique?:BÄ¿BÄ:BÄ¡\ÄBÄ˛láIf only unique tuples are desired, then this must be true.BÄ¶BÄ*ÏÑGive parameters for PRINT RELATION ==>BÄ˘BÄ[\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄhBÄÏBÄeBÄÌBÄeBÄÓBÄiBÄ˛BÄlBÄÙBÄmBÄªBÄpBÄ˘BÄ[BÄÃÄ\ÄBÄ^BÄ‡√ÅLOAD-DATABASEÄÄÎÄ	ÜÄ@ƒFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ≠\ÄBÄmBÄ°jÅDIRECTORYÄBÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛l°A database saved on the disk can be loaded using this function.

   DBNAME    - Name of the database to be restored.
   DIRECTORY - Name of the directory in which it can be found.  (LOAD-DATABASE DBNAME &REST KEYWORD-LIST &KEY &OPTIONAL DIRECTORY &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄÆ¿BÄ†¿BÄ≠“BÄﬂ“BÄÆíP@¡Pˇ€PPÅQPÇQíö	ö@ëÅQPÇQí
îOÄºBÄ≠Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄÆ\ÄBÄ8\ÄBÄÍÏÅLoad DatabaseÄBÄÏBÄ¿BÄÌBÄ¿BÄÓ\Ä\Ä•Ä LBÄêBÄ˛ÏÖUsed to load database from a given directory.ÄBÄÙ\Ä\ÄBÄaBÄ˜lÅDefinitionBÄª\ÄBÄΩ\ÄÄ*ÏÅDatabase Name:BÄ¿BÄöBÄ¡\ÄBÄ˛lÑName of the database to be loaded.BÄƒ\ÄÄ*,ÇDirectory Name:ÄBÄ¿BÄüBÄ¡\ÄBÄ˛¨ÖName of the directory in which it is stored.BÄƒBÄ*lÑGive parameters for LOAD DATABASE:BÄ˘BÄ∫\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ√BÄÏBÄ¿BÄÌBÄ¿BÄÓBÄƒBÄ˛BÄ«BÄÙBÄ»BÄªBÄÀBÄ˘BÄ∫BÄÃÄ\ÄBÄ^BÄ‡ÇLOAD-ENVIRONMENTÄÎÄ	ÜÄ@ƒFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ€\ÄBÄmÉÅENVIRONMENTÄBÄ∑BÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛lùLoad a saved environment.

   ENVNAME   - Name of the environment to be restored.
   DIRECTORY - Name of the directory in which it can be found.  (LOAD-ENVIRONMENT ENVNAME &REST KEYWORD-LIST &KEY &OPTIONAL DIRECTORY &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿BÄ‹¿BÄ†¿BÄ≠“BÄﬂ“BÄ‹íP@¡Pˇ€PPÅQPÇQíö	ö@ëÅQPÇQí
îOÄÍBÄ€Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ‹\ÄBÄ8\ÄBÄÍ,ÇLoad EnvironmentBÄÏBÄÓBÄÌBÄÓBÄÓ\Ä\ÄBÄ∆BÄeBÄ˛,ÜUsed to load environment from a given directory.BÄÙ\Ä\ÄBÄaBÄ˜lÅDefinitionBÄª\ÄBÄΩ\ÄÄ*lÇEnvironment Name:ÄBÄ¿BÄöBÄ¡\ÄBÄ˛ÏÑName of the environment to be loaded.ÄBÄƒ\ÄÄ*,ÇDirectory Name:ÄBÄ¿BÄüBÄ¡\ÄBÄ˛¨ÖName of the directory in which it is stored.BÄƒBÄ*ÏÑGive parameters for LOAD ENVIRONMENT:ÄBÄ˘BÄË\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄÒBÄÏBÄÓBÄÌBÄÓBÄÓBÄÚBÄ˛BÄÙBÄÙBÄıBÄªBÄ¯BÄ˘BÄËBÄÃÄ\ÄBÄ^BÄ‡√ÅLOAD-RELATIONÄÄÎÄ	ÜÄ@ƒFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ\ÄBÄmBÄ†BÄ∑BÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏûLoad a saved relation.

   RELATION-NAME    - Name of the relation to be restored.
   DIRECTORY        - Name of the directory in which it can be found.  (LOAD-RELATION RELATION-NAME &REST KEYWORD-LIST &KEY &OPTIONAL DIRECTORY &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄ	¿BÄ†¿BÄ≠“BÄﬂ“BÄ	íP@¡Pˇ€PPÅQPÇQíö	ö@ëÅQPÇQí
îOÄÄÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ	\ÄBÄ8\ÄBÄÍÏÅLoad RelationÄBÄÏBÄÄÌBÄÄÓ\Ä\ÄBÄ∆BÄBÄ˛,ÜUsed to load a relation from a given directory.ÄBÄÙ\Ä\ÄBÄaBÄ˜lÅDefinitionBÄª\ÄBÄΩ\ÄÄ*ÏÅRelation Name:BÄ¿BÄ~BÄ¡\ÄBÄ˛lÑName of the relation to be loaded.BÄƒ\ÄÄ*,ÇDirectory Name:ÄBÄ¿BÄüBÄ¡\ÄBÄ˛¨ÖName of the directory in which it is saved.ÄBÄƒBÄ*lÑGive parameters for LOAD RELATION:BÄ˘BÄ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄÄÏBÄÄÌBÄÄÓBÄÄ˛BÄ ÄÙBÄ!ÄªBÄ$Ä˘BÄÄÃÄ\ÄBÄ^BÄ‡√ÅSAVE-DATABASEÄÄÎÄ	ÜÄ@ƒFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ4\ÄBÄmBÄ°BÄ∑BÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛l§Save all system relations and the user-defined, modified relations.

   DATABASE-NAME    - Name of the database to be saved.
   DIRECTORY         - Name of the directory in which it is to be saved.  (SAVE-DATABASE DATABASE-NAME &REST KEYWORD-LIST &KEY &OPTIONAL DIRECTORY &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿BÄ5¿BÄ†¿BÄ≠“BÄﬂ“BÄ5íP@¡Pˇ€PPÅQPÇQíö	ö@ëÅQPÇQí
îOÄBÄ4Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ5\ÄBÄ8\ÄBÄÍÏÅSave DatabaseÄBÄÏBÄFÄÌBÄFÄÓ\Ä\Ä•Ä SBÄfBÄ˛ÏÖUsed to save a database on a given directory.ÄBÄÙ\Ä\ÄBÄaBÄ˜ÏÅOther FeaturesBÄª\ÄBÄΩ\ÄÄ*ÏÅDatabase Name:BÄ¿BÄöBÄ¡\ÄBÄ˛lÑName of the database to be saved.ÄBÄƒ\ÄÄ*,ÇDirectory Name:ÄBÄ¿BÄüBÄ¡\ÄBÄ˛lÑName of the directory to write to.BÄƒBÄ*lÑGive parameters for SAVE DATABASE:BÄ˘BÄ@\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄIÄÏBÄFÄÌBÄFÄÓBÄJÄ˛BÄMÄÙBÄNÄªBÄQÄ˘BÄ@ÄÃÄ\ÄBÄ^BÄ‡ÇSAVE-ENVIRONMENTÄÎÄ	ÜÄ@ƒFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄa\ÄBÄmBÄÂBÄ∑BÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛¨úSave an environment.

   ENVNAME   - Name of the environment to be saved.
   DIRECTORY - Name of the directory in which it is to be saved.  (SAVE-ENVIRONMENT ENVNAME &REST KEYWORD-LIST &KEY &OPTIONAL DIRECTORY &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿BÄb¿BÄ†¿BÄ≠“BÄﬂ“BÄbíP@¡Pˇ€PPÅQPÇQíö	ö@ëÅQPÇQí
îOÄoÄaÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄb\ÄBÄ8\ÄBÄÍ,ÇSave EnvironmentBÄÏBÄsÄÌBÄsÄÓ\Ä\ÄBÄLÄeBÄ˛lÜUsed to save an environment on a given directory.ÄBÄÙ\Ä\ÄBÄaBÄ˜ÏÅOther FeaturesBÄª\ÄBÄΩ\ÄÄ*lÇEnvironment Name:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛¨ÑName of the environment to be saved.BÄƒ\ÄÄ*,ÇDirectory Name:ÄBÄ¿BÄüBÄ¡\ÄBÄ˛lÑName of the directory to write to.BÄƒBÄ*ÏÑGive parameters for SAVE environment:ÄBÄ˘BÄm\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄvÄÏBÄsÄÌBÄsÄÓBÄwÄ˛BÄyÄÙBÄzÄªBÄ}Ä˘BÄmÄÃÄ\ÄBÄ^BÄ‡√ÅSAVE-RELATIONÄÄÎÄ#ÜÄAHFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ
\ÄBÄmBÄ†BÄ∑™ÄTYPEÉÄSAVEBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛,ºSave a relation if it is modified.

    RELATION-NAME - Name of the relation to be saved.
    DIRECTORY     - Name of the directory in which it is to be saved.
    SAVE          - If T, saves the relation even if the relation is not modified.
    TYPE          - Two types of save are allowed: COMMAND and XLD. This keyword can be used to
                    specify the type.  (SAVE-RELATION RELATION-NAME &REST KEYWORD-LIST &KEY &OPTIONAL DIRECTORY SAVE TYPE &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄé¿BÄó¿BÄ†¿BÄò¿BÄ≠“BÄﬂ“BÄéíPA¡Pˇ€PPÅQPÉQPÇQ	PÑQ
≤@√
ööAëÅQ@QîOÄûÄ
Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄé\ÄBÄ8\ÄBÄÍÏÅSave RelationÄBÄÏBÄ¢ÄÌBÄ¢ÄÓ\Ä\ÄBÄLÄBÄ˛ÏÖUsed to save a relation on a given directory.ÄBÄÙ\Ä\ÄBÄaBÄ˜ÏÅOther FeaturesBÄª\ÄBÄΩ\ÄÄ*ÏÅRelation Name:BÄ¿BÄ~BÄ¡\ÄBÄ˛lÑName of the relation to be saved.ÄBÄƒ\ÄÄ*,ÇDirectory Name:ÄBÄ¿BÄüBÄ¡\ÄBÄ˛lÑName of the directory to write to.BÄƒ\ÄÄ*ÏÅType of SAVE:ÄBÄ¿BÄ™BÄ¡\ÄBÄ˛¨ÖSave type. It can be either XLD or COMMAND.ÄBÄƒ\ÄÄ*lÅMust Save:BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏáSave the relation even if the relation has not been modified.ÄBÄ¶BÄ*lÑGive parameters for SAVE RELATION:BÄ˘BÄú\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ•ÄÏBÄ¢ÄÌBÄ¢ÄÓBÄ¶Ä˛BÄ®ÄÙBÄ©ÄªBÄ¨Ä˘BÄúÄÃÄ\ÄBÄ^BÄ‡ÇSAVE-TRANSACTIONÄÎÄ
 ÜÄAFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄƒ\ÄBÄmÉÅTRANSACTIONÄBÄ∑BÄªBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏßSave a transaction on disk.

   TRANSACTION - Name of the transaction.
   DIRECTORY   - Name of the directory in which this transaction is to be stored.
   PATHNAME    - Name of the file in which it is to be stored.  (SAVE-TRANSACTION TRANSACTION &REST KEYWORD-LIST &KEY &OPTIONAL DIRECTORY PATHNAME &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿BÄ≈¿BÄû¿BÄ†¿BÄ≠“BÄﬂ“BÄ≈íPA¡Pˇ€PPÅQPÉQPÇQ	¢@√	ö
öAëÅQ@QîOÄ‘ÄƒÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ≈\ÄBÄ8\ÄBÄÍ,ÇSave TransactionBÄÏBÄÿÄÌBÄÿÄÓ\Ä\ÄBÄLÄBÄ˛,ÜUsed to save a transaction on a given directory.BÄÙ\Ä\ÄBÄaBÄ˜ÏÅOther FeaturesBÄª\ÄÄΩ\ÄÄ*lÇTransaction Name:ÄBÄ¿BÄÜBÄ¡\ÄBÄ˛¨ÑName of the transaction to be saved.BÄƒ\ÄÄ*,ÇDirectory Name:ÄBÄ¿BÄüBÄ¡\ÄBÄ˛lÑName of the directory to write to.BÄƒ\ÄÄ*lÅPathname:ÄBÄ¿BÄñBÄ¡\ÄBÄ˛,
The name of the file into which the transaction forms will be stored. It defaults to <transaction>.lispÄBÄƒBÄ*ÏÑGive parameters for SAVE TRANSACTION:ÄBÄ˘BÄ“\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ€ÄÏBÄÿÄÌBÄÿÄÓBÄ‹Ä˛BÄﬁÄÙBÄﬂÄªBÄ‚Ä˘BÄ“ÄÃÄ\ÄBÄ^BÄ‡√ÇDEFINE-IMPLEMENTATIONÄÄÎÄ	ÜÄ@»FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄˆ\ÄBÄm√ÅIMPLEMENTATIONBÄ°BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛¨ÆDefine a new implementation.

   IMPLEMENTATION-NAME - Name of the implementation to be defined. All the implementation-specific
                         accessor functions are expected to be defined.
   DOCUMENTATION       - Description of this implementation.  (DEFINE-IMPLEMENTATION IMPLEMENTATION-NAME &REST KEYWORD-LIST &KEY &OPTIONAL DOCUMENTATION &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿BÄ˜¿BÄ°¿BÄ≠“BÄﬂ“BÄ˜íPA¡Pˇ€PPÅQPÇQí@√ö	öAëÅQ@Q
îOÄBÄˆÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ˜\ÄBÄ8\ÄBÄÍÏÇDefine ImplementationÄBÄÏBÄ
BÄÌBÄ
BÄÓ\Ä\ÄBÄêBÄÕBÄ˛lÑUsed to define an implementation.ÄBÄÙ\Ä\ÄBÄaBÄ˜lÅDefinitionBÄª\ÄBÄΩ\ÄÄ*¨ÇImplementation Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏåName of the implementation. Implementation-dependent routines are expected to be defined by the user.ÄBÄƒ\ÄÄ*ÏÅDocumentation:BÄ¿BÄ¥BÄ¡\ÄBÄ˛ÏÑDocumentation for the implementation.ÄBÄ-BÄ*lÖGive parameters for DEFINE IMPLEMENTATION:BÄ˘BÄ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄBÄÏBÄ
BÄÌBÄ
BÄÓBÄBÄ˛BÄBÄÙBÄBÄªBÄBÄ˘BÄBÄÃÄ\ÄBÄ^BÄ‡ÉÅDEFINE-INDEXÄÎÄ)ÜÄA»FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ$\ÄBÄmBÄ∏CÅINDEX-NAME√ÅKEY-ATTRIBUTESCÇSTORAGE-STRUCTUREÄÅPRIORITYBÄ°BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏøñDefine an index on a relation in the active database.

    RELATION-NAME - Name of the relation on which the index will be defined.
    NAME - Name of the index to be defined
    KEY - List of attributes names which form the key of the index.
    STORAGE-STRUCTURE - The name of a RTMS defined storage structure upon which will be used as the index structure.
    PRIORITY - A numerical value which determines the order in which RTMS will search multiple indices of a relation
               for a possible key. The number one receives the highest consideration, if it fails the next index in
               value is attempted.
    DOCUMENTATION - A string describing this index.  (DEFINE-INDEX RELATION-NAME &REST KEYWORD-LIST &KEY &OPTIONAL NAME DOCUMENTATION STORAGE-STRUCTURE KEY PRIORITY &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄ%¿ÉÄNAME¿BÄ¢¿BÄ¨¿BÄ1¿BÄ°¿BÄ≠“BÄﬂ“BÄ%íPA¡Pˇ€PPÅQPÇQPÉQ	PÑQ
PÖQPÜQ
J∫@√ööAëÅQ@QîOÄ8BÄ$Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ%\ÄBÄ8\ÄBÄÍ¨ÅDefine IndexBÄÏBÄ<BÄÌBÄ<BÄÓ\Ä\ÄBÄê•Ä@IBÄ˛,ÜUsed to define a secondary index on a relation.ÄBÄÙ\Ä\ÄBÄaBÄ˜lÅDefinitionBÄª\Ä	BÄΩ\ÄÄ*ÏÅRelation Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛láName of the relation upon which the index will be defined.BÄƒ\ÄÄ*¨ÅIndex Name:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛,ÑName of the index to be defined.BÄ-\ÄÄ*,ÇKey Attributes:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛láList of attribute names which form the key for this index.BÄƒ\ÄÄ*lÇStorage Structure:BÄ¿¨ÄAVLÄBÄ¡\ÄBÄ˛,ÜThe storage structure used to define the index.ÄBÄ-\ÄÄ*lÅPriority:ÄBÄ¿FÄ
BÄ¡\ÄBÄ˛ÏãA numerical value which indicates the priority given to this index. 1 is the highest priority.BÄë\ÄÄ*ÏÅDocumentation:BÄ¿BÄ¥BÄ¡\ÄBÄ˛¨ÉDocumentation for the index.BÄ-BÄ*lÑGive parameters for DEFINE INDEX:ÄBÄ˘BÄ5\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ?BÄÏBÄ<BÄÌBÄ<BÄÓBÄ@BÄ˛BÄCBÄÙBÄDBÄªBÄGBÄ˘BÄ5BÄÃÄ\ÄBÄ^BÄ‡ÉÅMODIFY-INDEXÄÎÄ+ÜÄBFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄi\ÄBÄmBÄ∏BÄ.√ÅNEW-INDEX-NAMEBÄ/BÄ0BÄ1BÄ°BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏøΩModify an index on a relation in the active database.

    RELATION-NAME - Name of the relation on which the index to be modified is defined
    INDEX-NAME - Name of the index to be modified
    NEW-NAME - New name for the specified index
    KEY - List of attributes names which form the key of the index.
    STORAGE-STRUCTURE - The name of a RTMS defined storage structure upon which will be used as the index structure.
    PRIORITY - A numerical value which determines the order in which RTMS will search multiple indices of a relation
               for a possible key. The number one receives the highest consideration, if it fails the next index in
               value is attempted.
    DOCUMENTATION - A string describing this index.  (MODIFY-INDEX RELATION-NAME INDEX-NAME &REST KEYWORD-LIST &KEY &OPTIONAL NEW-NAME DOCUMENTATION STORAGE-STRUCTURE KEY PRIORITY &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿BÄj¿ÅNEW-NAME¿BÄ¢¿BÄ¨¿BÄ1¿BÄ°¿BÄ≠“BÄﬂ“BÄjíPA¡Pˇ€PPÅQÇQPÉQPÑQ	PÖQ
PÜQPáQ
J∫@√¢öAëÅQÇQ@QúOÄzBÄiÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄj\ÄBÄ8\ÄBÄÍ¨ÅModify IndexBÄÏBÄ~BÄÌBÄ~BÄÓ\Ä\ÄBÄBÄBBÄ˛,ÜUsed to define a secondary index on a relation.ÄBÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\Ä
BÄΩ\ÄÄ*ÏÅRelation Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨àName of the relation upon which the index to be modified is defined.BÄƒ\ÄÄ*¨ÅIndex Name:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛lÑName of the index to be modified.ÄBÄ-\ÄÄ*,ÇNew Index Name:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛ÏÇNew name of the index.BÄ-\ÄÄ*,ÇKey Attributes:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛láList of attribute names which form the key for this index.BÄƒ\ÄÄ*lÇStorage Structure:BÄ¿BÄ:BÄ¡\ÄBÄ˛,ÜThe storage structure used to define the index.ÄBÄ-\ÄÄ*lÅPriority:ÄBÄ¿FÄ
BÄ¡\ÄBÄ˛ÏãA numerical value which indicates the priority given to this index. 1 is the highest priority.BÄë\ÄÄ*ÏÅDocumentation:BÄ¿BÄ¥BÄ¡\ÄBÄ˛¨ÉDocumentation for the index.BÄ-BÄ*lÑGive parameters for DEFINE INDEX:ÄBÄ˘BÄw\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄÅBÄÏBÄ~BÄÌBÄ~BÄÓBÄÇBÄ˛BÄÑBÄÙBÄÖBÄªBÄàBÄ˘BÄwBÄÃÄ\ÄBÄ^BÄ‡ÉDEFINE-STORAGE-STRUCTUREÄÎÄ	ÜÄ@»FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ≠\ÄBÄmBÄ0BÄ°BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛,≤Define a new storage structure.

   STORAGE-STRUCTURE-NAME - Name of the storage-structure to be defined. All the storage-structure-specific
                            accessor functions are expected to be defined.
   DOCUMENTATION          - Description of this storage-structure.  (DEFINE-STORAGE-STRUCTURE STORAGE-STRUCTURE-NAME &REST KEYWORD-LIST &KEY &OPTIONAL DOCUMENTATION &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄÆ¿BÄ°¿BÄ≠“BÄﬂ“BÄÆíPA¡Pˇ€PPÅQPÇQí@√ö	öAëÅQ@Q
îOÄºBÄ≠Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄÆ\ÄBÄ8\ÄBÄÍ,ÉDefine Storage StructureBÄÏBÄ¿BÄÌBÄ¿BÄÓ\Ä\ÄBÄêBÄLÄ˛lÑUsed to define a storagestructure.BÄÙ\Ä\ÄBÄaBÄ˜lÅDefinitionBÄª\ÄBÄΩ\ÄÄ*,ÉStorage structure name:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛¨
Name of the storage structure. Storage-structure-dependent routines are expected to be defined by the user.ÄBÄƒ\ÄÄ*ÏÅDocumentation:BÄ¿BÄ:BÄ¡\ÄBÄ˛,ÖDocumentation for the storage structure.BÄ-BÄ*ÏÖGive parameters for DEFINE STORAGE STRUCTURE:ÄBÄ˘BÄ∫\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ√BÄÏBÄ¿BÄÌBÄ¿BÄÓBÄƒBÄ˛BÄ∆BÄÙBÄ«BÄªBÄ BÄ˘BÄ∫BÄÃÄ\ÄBÄ^BÄ‡√ÅDEFINE-DOMAINÄÄÎÄ#ÜÄAHFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ⁄\ÄBÄm√ÄDOMAINp¿BÄu¨ÄDEFÄBÄ°BÄﬂBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛¨ØDefine new domain. Corresponding predicate is expected to be defined prior to this operation.

   DOMAIN-NAME     - Name of the domain to be defined.
   DOCUMENTATION   - Describes the new domain.
   FORMAT          - Print width for attributes belonging to this domain.  (DEFINE-DOMAIN DOMAIN-NAME &REST KEYWORD-LIST &KEY &OPTIONAL DEFAULT DOCUMENTATION FORMAT &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿BÄ€¿ÅDEFAULTÄ¿BÄ°¿BÄﬂ¿BÄ≠“BÄﬂ“BÄ€íPA¡Pˇ€PPÅQPÇQPÉQ	PÑQ
≤@√
ööAëÅQ@QîOÄÌBÄ⁄Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ€\ÄBÄ8\ÄBÄÍÏÅDefine DomainÄBÄÏBÄÒBÄÌBÄÒBÄÓ\Ä•Ä`DBÄ˛,ÉUsed to define a domain.BÄÙ\Ä\ÄBÄaBÄ˜lÅDefinitionBÄª\ÄBÄΩ\ÄÄ*¨ÅDomain Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛Ï Name of the domain. Domain predicate is expected to be defined prior to this.ÄBÄƒ\ÄÄ*ÏÅDefault value:BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏÉDefault value for this domain.BÄƒ\ÄÄ*ÏÅDocumentation:BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏÉDocumentation for the domain.ÄBÄ-\ÄÄ*,ÇDefault width :ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛ÏÖThe default width to be used for this domain.ÄBÄƒBÄ*lÑGive parameters for DEFINE DOMAIN:BÄ˘BÄÍ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄÙBÄÏBÄÒBÄÌBÄÒBÄÓBÄıBÄ˛BÄ˜BÄÙBÄ¯BÄªBÄ˚BÄ˘BÄÍBÄÃÄ\ÄBÄ^BÄ‡√ÅMODIFY-DOMAINÄÄÎÄ#ÜÄAHFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ\ÄBÄmBÄ‰BÄÊBÄ°BÄﬂBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛l∞Modify the default format, value, and documentation of a domain.

   DOMAIN-NAME - Name of the domain to be modified.
   FORMAT      - New format, i.e the print width, for this domain.
   DEFAULT     - New default value for this domain.
   DOC         - New description of this domain.  (MODIFY-DOMAIN DOMAIN-NAME &REST KEYWORD-LIST &KEY &OPTIONAL FORMAT DEFAULT DOC &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄ¿BÄÏ¿BÄ°¿BÄﬂ¿BÄ≠“BÄﬂ“BÄíPA¡Pˇ€PPÅQPÇQPÉQ	PÑQ
≤@√
ööAëÅQ@QîOÄ"BÄÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ\ÄBÄ8\ÄBÄÍÏÅModify DomainÄBÄÏBÄ&BÄÌBÄ&BÄÓ\Ä\ÄBÄBÄêBÄ˛,ÉUsed to modify a domain.BÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄBÄΩ\ÄÄ*¨ÅDomain Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛lÑName of the domain to be modified.BÄƒ\ÄÄ*ÏÅDefault value:BÄ¿BÄ:BÄ¡\ÄBÄ˛lÑNew default value for this domain.BÄƒ\ÄÄ*ÏÅDocumentation:BÄ¿BÄ:BÄ¡\ÄBÄ˛lÑNew documentation for the domain.ÄBÄ-\ÄÄ*,ÇDefault width :ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛lÜThe new default width to be used for this domain.ÄBÄƒBÄ*lÑGive parameters for MODIFY DOMAIN:BÄ˘BÄ \ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ)BÄÏBÄ&BÄÌBÄ&BÄÓBÄ*BÄ˛BÄ,BÄÙBÄ-BÄªBÄ0BÄ˘BÄ BÄÃÄ\ÄBÄ^BÄ‡CÇDEFINE-TRANSACTIONÄÎÄ"ÜÄAHFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄH\ÄBÄmBÄŒ√ÄFORMSÄBÄ†BÄûBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏØDefine a transaction, a list of database calls.

   TRANSACTION - Name of the transaction.
   FORMS       - List of RTMS calls.
   DIRECTORY   - Name of the directory in which this transaction will be stored.
   PATHNAME    - Name of the file in which it will be stored.  (DEFINE-TRANSACTION TRANSACTION FORMS &REST KEYWORD-LIST &KEY &OPTIONAL DIRECTORY PATHNAME &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄI¿BÄ†¿BÄû¿BÄ≠“BÄﬂ“BÄIíPA¡Pˇ€PPÅQÇQPÉQPÑQ	¢@√	¢
öAëÅQÇQ@QúOÄXBÄHÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄI\ÄBÄ8\ÄBÄÍlÇDefine TransactionBÄÏBÄ\BÄÌBÄ\BÄÓ\Ä\ÄBÄêBÄBÄ˛ÏÉUsed to define a transaction.ÄBÄÙ\Ä\ÄBÄaBÄ˜lÅDefinitionBÄª\ÄBÄΩ\ÄÄ*lÇTransaction Name:ÄBÄ¿BÄÜBÄ¡\ÄBÄ˛,ÉName of the transaction.BÄƒ\ÄÄ*,ÇDatabase calls:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛lÉA list of database calls.ÄBÄƒBÄ—\ÄÄ*lÅPathname :BÄ¿BÄñBÄ¡\ÄBÄ˛¨ÖThe default file in which it will be saved.ÄBÄƒBÄ*,ÖGive parameters for DEFINE TRANSACTION:ÄBÄ˘BÄV\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ_BÄÏBÄ\BÄÌBÄ\BÄÓBÄ`BÄ˛BÄbBÄÙBÄcBÄªBÄfBÄ˘BÄVBÄÃÄ\ÄBÄ^BÄ‡CÇMODIFY-TRANSACTIONÄÎÄ
 ÜÄAFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄz\ÄBÄmBÄŒÄ†BÄûBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛¨®Edit the database calls in a transaction.
   TRANSACTION - Name of the transaction.
   DIRECTORY   - Name of the directory in which this transaction can be found.
   PATHNAME    - Name of the file in which it is stored.  (MODIFY-TRANSACTION TRANSACTION &REST KEYWORD-LIST &KEY &OPTIONAL DIRECTORY PATHNAME &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿BÄ{¿BÄ†¿BÄû¿BÄ≠“BÄﬂ“BÄ{íPA¡Pˇ€PPÅQPÇQPÉQ	¢@√	ö
öAëÅQ@QîOÄ BÄzÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ{\ÄBÄ8\ÄBÄÍlÇModify TransactionBÄÏBÄ
BÄÌBÄ
BÄÓ\Ä\ÄBÄBÄBÄ˛ÏÉUsed to modify a transaction.ÄBÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄÄΩ\ÄÄ*lÇTransaction Name:ÄBÄ¿BÄÜBÄ¡\ÄBÄ˛,ÖName of the transaction to be modified.ÄBÄƒ\ÄÄ*lÅDirectory:BÄ¿BÄüBÄ¡\ÄBÄ˛ÏáDefault directory in which it can be found, if not in memory.ÄBÄƒ\ÄÄ*lÅPathname :BÄ¿BÄñBÄ¡\ÄBÄ˛¨áThe default file in which it can be found, if not in memory.BÄƒBÄ*,ÖGive parameters for MODIFY TRANSACTION:ÄBÄ˘BÄá\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄêBÄÏBÄ
BÄÌBÄ
BÄÓBÄëBÄ˛BÄìBÄÙBÄîBÄªBÄóBÄ˘BÄáBÄÃÄ\ÄBÄ^BÄ‡ÇDEFINE-DATABASEÄÄÎÄ#ÜÄAHFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ´\ÄBÄmBÄ°BÄ∑BÄ°ÉÄENVÄBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛¨∞Define a new database.

   DB-NAME     - Name of the database.
   DIRECTORY   - Name of the directory in which this database is to be saved.
   ENVIRONMENT - Name of the environment to be associated with this database.
   DOCUMENTATION - A string describing this database.  (DEFINE-DATABASE DB-NAME &REST KEYWORD-LIST &KEY &OPTIONAL DIRECTORY ENVIRONMENT DOCUMENTATION &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿√ÄDEFDBÄ¿BÄ†¿BÄ°¿BÄÂ¿BÄ≠“BÄﬂ“BÄªíPA¡Pˇ€PPÅQPÇQPÉQ	PÑQ
≤@√
ööAëÅQ@QîOÄºBÄ´Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ¨\ÄBÄ8\ÄBÄÍ,ÇDefine DatabaseÄBÄÏBÄ¿BÄÌBÄ¿BÄÓ\Ä\ÄBÄêBÄêBÄ˛,ÜUsed to define a database in a given directory.ÄBÄÙ\Ä\ÄBÄaBÄ˜lÅDefinitionBÄª\ÄBÄΩ\ÄÄ*ÏÅDatabase Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏÇName of the database.ÄBÄƒ\ÄÄ*,ÇDirectory Name:ÄBÄ¿BÄüBÄ¡\ÄBÄ˛ÏÖName of the save directory for this database.ÄBÄƒ\ÄÄ*ÏÅDocumentation:BÄ¿BÄ¥BÄ¡\ÄBÄ˛,ÑDocumentation for the database.ÄBÄ-\ÄÄ*¨ÅEnvironment:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨àName of the environment to be used to replace the default settings.ÄBÄƒBÄ*¨ÑGive parameters for DEFINE DATABASE:BÄ˘BÄπ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ√BÄÏBÄ¿BÄÌBÄ¿BÄÓBÄƒBÄ˛BÄ∆BÄÙBÄ«BÄªBÄ BÄ˘BÄπBÄÃÄ\ÄBÄ^BÄ‡ÇMODIFY-DATABASEÄÄÎÄ#ÜÄAHFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ‚\ÄBÄmBÄ°ÉÅNEW-DATABASEBÄ∑BÄ°BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛,±Modify various features of the active database.

  DATABASE      - Name of the database to be modified.
  DATABASE-NAME - New name for this database.
  DIRECTORY     - New directory in which this database is to be saved.
  DOCUMENTATION - New description for this database.  (MODIFY-DATABASE DATABASE &REST KEYWORD-LIST &KEY &OPTIONAL DATABASE-NAME DIRECTORY DOCUMENTATION &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄ„¿√ÅDATABASE-NAMEÄ¿BÄ†¿BÄ°¿BÄ≠“BÄﬂ“BÄ„íPA¡Pˇ€PPÅQPÇQPÉQ	PÑQ
≤@√
ööAëÅQ@QîOÄÛBÄ‚Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ„\ÄBÄ8\ÄBÄÍ,ÇModify DatabaseÄBÄÏBÄ˜BÄÌBÄ˜BÄÓ\Ä\ÄBÄBÄfBÄ˛,ÖUsed to modify the features a database.ÄBÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄBÄΩ\ÄÄ*ÏÅDatabase Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏÇName of the database.ÄBÄƒ\ÄÄ*lÇNew Database Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏÜIf the database is to be renamed specify the new name.BÄƒ\ÄÄ*,ÇDirectory Name:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛, To change the save directory for this database specify a new directory.ÄBÄƒ\ÄÄ*ÏÅDocumentation:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨ÑNew documentation for the database.ÄBÄ-BÄ*¨ÑGive parameters for MODIFY DATABASE:BÄ˘BÄ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ˙BÄÏBÄ˜BÄÌBÄ˜BÄÓBÄ˚BÄ˛BÄ˝BÄÙBÄ˛BÄªBÄ	BÄ˘BÄBÄÃÄ\ÄBÄ^BÄ‡ÇMODIFY-ATTRIBUTEÄÎÄ(ÜÄA»FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ	\ÄBÄmBÄ†BÄ√ÅNEW-ATTRBÄÊBÄ°BÄﬂBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏøModify various features of an attribute in a given relation.

  RELATION       - Name of the relation in which the attribute to be modified exists.
  ATTRIBUTE      - Name of the attribute to be modified.
  ATTRIBUTE-NAME - New name for this attribute.
  DEFAULT-VALUE  - New default value for this attribute.
  DOCUMENTATION  - New description.
  FORMAT         - New print width to be used for this attribute.  (MODIFY-ATTRIBUTE RELATION ATTRIBUTE &REST KEYWORD-LIST &KEY &OPTIONAL ATTRIBUTE-NAME DEFAULT-VALUE DOCUMENTATION FORMAT &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿BÄ	¿√ÅATTRIBUTE-NAME¿BÄÊ¿BÄ°¿BÄﬂ¿BÄ≠“BÄﬂ“BÄ	íPA¡Pˇ€PPÅQÇQPÉQPÑQ	PÖQ
PÜQJ∫@√¢öAëÅQÇQ@QúOÄ*	BÄ	Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ	\ÄBÄ8\ÄBÄÍ,ÇModify AttributeBÄÏBÄ.	BÄÌBÄ.	BÄÓ\Ä\ÄBÄBÄÚBÄ˛,ÖUsed to modify the features a attribute.BÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\Ä	BÄΩ\ÄÄ*ÏÅRelation Name:BÄ¿BÄ~BÄ¡\ÄBÄ˛ÏÇName of the relation.ÄBÄƒ\ÄÄ*,ÇAttribute Name:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛ÏÇName of the attribute.BÄƒ\ÄÄ*¨ÇNew Attribute Name:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛,áIf the attribute is to be renamed specify the new name.ÄBÄƒ\ÄÄ*ÏÅDefault Value:BÄ¿BÄ:BÄ¡\ÄBÄ˛làTo change the default value of this attribute specify a new value.BÄƒ\ÄÄ*ÏÅDocumentation:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨ÑNew documentation for the attribute.BÄ-\ÄÄ*,ÇDefault width :ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛¨ÜThe new default width to be used for this attribute.BÄƒBÄ*ÏÑGive parameters for MODIFY ATTRIBUTE:ÄBÄ˘BÄ'	\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ1	BÄÏBÄ.	BÄÌBÄ.	BÄÓBÄ2	BÄ˛BÄ4	BÄÙBÄ5	BÄªBÄ8	BÄ˘BÄ'	BÄÃÄ\ÄBÄ^BÄ‡ÉÅMODIFY-VIEWÄÄÎÄ
 ÜÄAFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄX	\ÄBÄmÉÄVIEWBÄÊBÄ°BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛¨•Modify a view definition or its documentation.

   VIEW-NAME       - Name of the view.
   VIEW-DEFINITION - New definition of the view.
   VIEW-DOCUMENTATION - New description of the view.  (MODIFY-VIEW VIEW-NAME &REST KEYWORD-LIST &KEY &OPTIONAL VIEW-DEFINITION VIEW-DOCUMENTATION &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿BÄY	¿ÅVIEW-DEF¿ÅVIEW-DOC¿BÄ≠“BÄﬂ“BÄY	íPA¡Pˇ€PPÅQPÇQPÉQ	¢@√	ö
öAëÅQ@QîOÄj	BÄX	Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄY	\ÄBÄ8\ÄBÄÍ¨ÅModify ViewÄBÄÏBÄn	BÄÌBÄn	BÄÓ\Ä\ÄBÄ•Ä VBÄ˛¨ÑUsed to modify the features a view.ÄBÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄÄΩ\ÄÄ*lÅView Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛lÇName of the view.ÄBÄƒ\ÄÄ*,ÇView Definition:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨ÉNew definition of the view.ÄBÄƒ\ÄÄ*ÏÅDocumentation:BÄ¿BÄ:BÄ¡\ÄBÄ˛,ÑNew documentation for the view.ÄBÄ-BÄ*,ÑGive parameters for MODIFY VIEW:BÄ˘BÄf	\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄq	BÄÏBÄn	BÄÌBÄn	BÄÓBÄr	BÄ˛BÄu	BÄÙBÄv	BÄªBÄy	BÄ˘BÄf	BÄÃÄ\ÄBÄ^BÄ‡ÇMODIFY-RELATIONÄÄÎÄ8ÜÄCFÄ&¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ
	\ÄBÄmÉÄRELÄÅNEW-RELÄÅADD-ATTÄÅDEL-ATTÄÅREN-ATTÄBÄ£BÄ¨BÄﬂBÄ¢BÄ†BÄ°BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏøÙModify various features of a relation.

  RELATION             - Name of the relation to be modified.
  RELATION-NAME        - New name for this relation.
  ADD-ATTRIBUTES       - List of new attributes and their description.
  DELETE-ATTRIBUTES    - List of attributes to be destroyed.
  RENAME-ATTRIBUTES    - List of list of OLD-NEW attribute names.
  IMPLEMENTATION-TYPE  - Name of the new implementation type.
  STORAGE-STRUCTURE    - Name of the new storage-structure.
  FORMAT               - List of new print-width values to be used for the attributes.
  KEY                  - List of attributes to form the new key for this relation.
  DOCUMENTATION        - New description of this relation.
  DIRECTORY            - New directory in which this relation is to be saved.  (MODIFY-RELATION RELATION &REST KEYWORD-LIST &KEY &OPTIONAL RELATION-NAME ADD-ATTRIBUTES DELETE-ATTRIBUTES RENAME-ATTRIBUTES IMPLEMENTATION-TYPE STORAGE-STRUCTURE FORMAT KEY DOCUMENTATION DIRECTORY &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄé	¿BÄ†¿√ÅADD-ATTRIBUTES¿CÇDELETE-ATTRIBUTESÄ¿CÇRENAME-ATTRIBUTESÄ¿BÄ£¿BÄ¨¿BÄﬂ¿BÄ¢¿BÄ°¿BÄ†¿BÄ≠“BÄﬂ“BÄé	íPA¡Pˇ€PPÅQPÇQPÉQ	PÑQ
PÖQPÜQPáQPàQP QPãQPäQJ∫@√ööAëÅQ@QîOÄ§	BÄ
	Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄé	\ÄBÄ8\ÄBÄÍ,ÇModify RelationÄBÄÏBÄ®	BÄÌBÄ®	BÄÓ\Ä\ÄBÄBÄBÄ˛,ÖUsed to modify the features a relation.ÄBÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄBÄΩ\ÄÄ*ÏÅRelation Name:BÄ¿BÄ~BÄ¡\ÄBÄ˛ÏÇName of the Relation.ÄBÄƒ\ÄÄ*lÇNew Relation Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏÜIf the relation is to be renamed specify the new name.BÄƒ\ÄÄ*,ÇAdd attributes:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛lãSpecify a list of attribute-descriptor pairs for attributes to be added to this relation.ÄBÄƒ\ÄÄ*lÇDelete attributes:BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏàSpecify a list of attributes in this relation which are to be deleted.BÄƒ\ÄÄ*lÇRename attributes:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨ãTo rename some of the attributes provide a list of the form (<old-attribute new-attribute>).BÄƒ\ÄÄ*¨ÇImplementation Type:BÄ¿BÄ:BÄ¡\ÄBÄ˛, To change the implementation type of this relation specify a new value.ÄBÄƒ\ÄÄ*lÇStorage structure:BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏàTo change the storage structure of this relation specify a new value.ÄBÄƒ\ÄÄ*,ÅFormat:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛,äTo change the format for this relation specify a new format as a list of values.BÄƒ\ÄÄ*¨ÄKey:BÄ¿BÄ:BÄ¡\ÄBÄ˛Ï To change the key for this relation specify a new key as a list of attributes.BÄƒ\ÄÄ*,ÇDirectory Name:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛, To change the save directory for this relation specify a new directory.ÄBÄƒ\ÄÄ*ÏÅDocumentation:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨ÑNew documentation for the relation.ÄBÄ-BÄ*¨ÑGive parameters for MODIFY RELATION:BÄ˘BÄü	\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ´	BÄÏBÄ®	BÄÌBÄ®	BÄÓBÄ¨	BÄ˛BÄÆ	BÄÙBÄØ	BÄªBÄ≤	BÄ˘BÄü	BÄÃÄ\ÄBÄ^BÄ‡CÇDEFINE-ENVIRONMENTÄÎÄRÜÄCHFÄ4¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄÊ	\ÄBÄmBÄÂBÄòÄ†ÉÄERRÄCÅPAR-CHECKÄÅREL-IMPÄÅREL-STOÄ√ÄSTATUSÅSYS-IMPÄÅSYS-STOÄCÅVAL-CHECKÄ™ÄWARNBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛Ïø∂Global variables defining an environment can be set using this function.

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
   WARNINGS     - If T, warning messages are generated.  (DEFINE-ENVIRONMENT ENVIRONMENT &REST KEYWORD-LIST &KEY &OPTIONAL AUTO-SAVE DIRECTORY ERRORS PARAMETER-CHECKING RELATION-IMPLEMENTATION RELATION-STORAGE-STRUCTURE STATUS SYSTEM-IMPLEMENTATION SYSTEM-STORAGE-STRUCTURE VALIDITY WARNINGS &ALLOW-OTHER-KEYS)ÄBÄõ—BÄõëBÄ‹¿lÄ~S¿√ÄDEFENV¿CÅAUTO-SAVEÄ¿ÉÄPARA¿BÄ†¿BÄÚ	¿BÄÛ	¿√ÄERRORS¿BÄÙ	¿ÅVALIDITY¿ÅWARNINGS¿BÄı	¿BÄˆ	¿BÄ≠“BÄﬂ“BÄ˛	íPA¡Pˇ€PPÅQ‰PÇQ	PÖQ
PÉQPÜQPáQPÑQPàQPãQPåQJ¸PÇQ	PÖQ
PÉQPÜQPáQPÑQPàQP QPäQPãQPåQJ∫@√ööAëÅQ@QîOÄ
BÄÊ	Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄÁ	\ÄBÄ8\ÄBÄÍlÇDefine EnvironmentBÄÏBÄ
BÄÌBÄ
BÄÓ\Ä\ÄBÄêBÄeBÄ˛¨ÜUsed to define an environment in a given directory.ÄBÄÙ\Ä\ÄBÄaBÄ˜lÅDefinitionBÄª\ÄBÄΩ\ÄÄ*lÇEnvironment Name:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛,ÉName of the environment.BÄƒ\ÄÄ*lÅAuto save:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨àAutomatically saves all the modified relations after each function.ÄBÄ¶BÄ—\ÄÄ*,ÅErrors:ÄBÄ¿BÄÃBÄ¡\ÄBÄ˛¨ÖControls the printing of the error messages.BÄ¶\ÄÄ*¨ÇParameter Checking:ÄBÄ¿BÄÃBÄ¡\ÄBÄ˛,ÖControls the checking of the parameters.BÄ¶\ÄÄ*,ÉRelation Implementation:BÄ¿BÄΩBÄ¡\ÄBÄ˛ÏÖDefault implementation of the user relations.ÄBÄƒ\ÄÄ*¨ÉRelation storage structure:ÄBÄ¿BÄ¬BÄ¡\ÄBÄ˛lÜDefault storage structure for the user relations.ÄBÄƒ\ÄÄ*,ÅStatus:ÄBÄ¿BÄÃBÄ¡\ÄBÄ˛ÏÖControls the printing of the status messages.ÄBÄ¶\ÄÄ*ÏÇSystem Implementation:BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏãDefault implementation of the system relations. Can not change this when a database is active.BÄƒ\ÄÄ*lÉSystem storage structure:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛låDefault storage structure for the system relations. Can not change this when a database is active.BÄƒ\ÄÄ*lÇValidity Checking:BÄ¿BÄÃBÄ¡\ÄBÄ˛¨äControls the checking of the values during insertion and modification for validity.ÄBÄ¶\ÄÄ*lÅWarnings:ÄBÄ¿BÄÃBÄ¡\ÄBÄ˛ÏÖControls the printing of the warning messages.BÄ¶BÄ*,ÖGive parameters for DEFINE ENVIRONMENT:ÄBÄ˘BÄ¸	\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ
BÄÏBÄ
BÄÌBÄ
BÄÓBÄ
BÄ˛BÄ
BÄÙBÄ
BÄªBÄ
BÄ˘BÄ¸	BÄÃÄ\ÄBÄ^BÄ‡ÇDEFINE-RELATIONÄÄÎÄ.ÜÄBHFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄF
\Ä	BÄmBÄ†ÅATTR-DESBÄüBÄ†BÄ°BÄ¢BÄ£BÄ§BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏøäDefine relations in the active database.

   RELATION-NAME - Name of the relation to be defined.
   ATTRIBUTE-DESCRIPTOR - List of attributes and their descriptions.
   DIRECTORY     - Name of the directory in which this relation is to be saved.
   DOCUMENTATION - Description of this relation.
   FORMAT        - List of print-width values correponding to the attribute-list.
   IMPLEMENTATION-TYPE - Name of the implementation for this relation.
   KEY           - List of attributes comprising the key for this relation.
   STORAGE-STRUCTURE   - Name of the storage structure to be used for this relation.  (DEFINE-RELATION RELATION-NAME ATTRIBUTE-DESCRIPTOR &REST KEYWORD-LIST &KEY &OPTIONAL DIRECTORY DOCUMENTATION FORMAT IMPLEMENTATION-TYPE KEY STORAGE-STRUCTURE &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿√ÄDEFREL¿ÉÅTUPLE-FORMAT¿BÄ†¿BÄ°¿BÄ¢¿BÄ£¿BÄ¨¿BÄ≠“BÄﬂ“BÄV
íPA¡Pˇ€PPÅQÇQPÉQPÑQ	PÖQ
PÜQPáQPàQJ∫@√¢öAëÅQÇQ@QúOÄX
BÄF
Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄG
\ÄBÄ8\ÄBÄÍ,ÇDefine RelationÄBÄÏBÄ\
BÄÌBÄ\
BÄÓ\Ä\ÄBÄêBÄBÄ˛lÉused to define a relation.BÄÙ\Ä\ÄBÄaBÄ˜lÅDefinitionBÄª\ÄBÄΩ\ÄÄ*ÏÅRelation Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨ÑName of the relation to be defined.ÄBÄƒBÄ≈BÄÕBÄ—BÄ’BÄ⁄BÄﬁBÄ‚BÄ*¨ÑGive parameters for DEFINE RELATION:BÄ˘BÄT
\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ_
BÄÏBÄ\
BÄÌBÄ\
BÄÓBÄ`
BÄ˛BÄb
BÄÙBÄc
BÄªBÄf
BÄ˘BÄT
BÄÃÄ\ÄBÄ^BÄ‡ÉÅDEFINE-VIEWÄÄÎÄ
ÜÄA
FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄr
\ÄBÄmÅVIEWNAMEÇVIEW-DEFINITIONÄBÄ°BÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛lûDefine views on the relations.

   VIEW-NAME - Name of the view.
   VIEW-DEF  - Definition of the view.
   DOCUMENTATION - Describes the view.  (DEFINE-VIEW VIEWNAME VIEW-DEF &REST KEYWORD-LIST &KEY &OPTIONAL DOCUMENTATION &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿ÅDEFVIEWÄ¿BÄ≠“BÄﬂ“BÄÇ
íP@¡Pˇ€PPÅQÇQÉQ¢ö@ëÅQÇQÉQ	úOÄÉ
BÄr
Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄs
\ÄBÄ8\ÄBÄÍ¨ÅDefine ViewÄBÄÏBÄá
BÄÌBÄá
BÄÓ\Ä\ÄBÄêBÄt	BÄ˛ÏÇUsed to define a view.BÄÙ\Ä\ÄBÄaBÄ˜lÅDefinitionBÄª\ÄÄΩ\ÄÄ*lÅView Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨ÉSpecify a name for the view.BÄƒ\ÄÄ*,ÇView Definition:BÄ¿BÄ«BÄ¡\ÄBÄ˛lÑSpecify a definition for the view.BÄƒ\ÄÄ*¨ÇView Documentation:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛¨ÑSpecify documentation for the view.ÄBÄƒBÄ*,ÑGive parameters for DEFINE VIEW:BÄ˘BÄÄ
\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄä
BÄÏBÄá
BÄÌBÄá
BÄÓBÄã
BÄ˛BÄ

BÄÙBÄé
BÄªBÄë
BÄ˘BÄÄ
BÄÃÄ\ÄBÄ^BÄ‡ÇDEFINE-ATTRIBUTEÄÎÄ
ÜÄAFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ•
\ÄBÄmBÄ∏BÄP
BÄ¢BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛Ï¥Add a new attribute to a relation.
    All its tuples will get the default value of the attribute for the attribute value.

   RELATION-NAME - Name of the relation.
   ATTRIBUTE-DESCRIPTOR - List of attributes and their descriptions.
   KEY           - If the key for this relation is to be changed, specify it.  (DEFINE-ATTRIBUTE RELATION-NAME ATTRIBUTE-DESCRIPTOR &REST KEYWORD-LIST &KEY &OPTIONAL KEY &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿BÄ¶
¿BÄ¢¿BÄ≠“BÄﬂ“BÄ¶
íPA¡Pˇ€PPÅQÇQPÉQí@√¢	öAëÅQÇQ@Q
úOÄ¥
BÄ•
Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ¶
\ÄBÄ8\ÄBÄÍ,ÇDefine AttributeBÄÏBÄ∏
BÄÌBÄ∏
BÄÓ\Ä\ÄBÄêBÄÚBÄ˛¨ÑUsed to add attributes to relations.BÄÙ\Ä\ÄBÄaBÄ˜lÅDefinitionBÄª\ÄÄΩ\ÄÄ*,ÇRelation name: ÄBÄ¿BÄ~BÄ¡\ÄBÄ˛làThe name of the relation to which new attributes are to be added.ÄBÄƒBÄ≈\ÄÄ*ÏÄKey: ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛,
New key for the relation if it is to be different from the previous value. Specify a list of attributes.BÄƒBÄ*ÏÑGive parameters for DEFINE ATTRIBUTE:ÄBÄ˘BÄ≤
\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄª
BÄÏBÄ∏
BÄÌBÄ∏
BÄÓBÄº
BÄ˛BÄæ
BÄÙBÄø
BÄªBÄ¬
BÄ˘BÄ≤
BÄÃÄ\ÄBÄ^BÄ‡√ÅMODIFY-TUPLESÄÄÎÄ#ÜÄAHFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ“
\ÄBÄmBÄ†ÉÅWHERE-CLAUSEBÄ∫ÍÄVALUESBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛l∑The values of the tuples in a relation can be modified using this function.

   RELATION  - Name of the relation whose tuples are to be modified.
   ATTRIBUTE - List of attributes which are to be modified.
   VALUE     - Corresponding list of values to be used in modifying the above attributes.
   WHERE     - Selection criterion to be used.  (MODIFY-TUPLES RELATION &REST KEYWORD-LIST &KEY &OPTIONAL ATTRIBUTE VALUE WHERE &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿√ÄMODIFY¿√ÄWHEREÄ¿BÄ√¿BÄ›
¿BÄ≠“BÄﬂ“BÄ„
íPA¡Pˇ€PPÅQPÇQPÉQ	PÑQ
≤@√
ööAëÅQ@QîOÄÂ
BÄ“
Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ”
\ÄBÄ8\ÄBÄÍÏÅModify TuplesÄBÄÏBÄÈ
BÄÌBÄÈ
BÄÓ\Ä\ÄBÄ•Ä@MBÄ˛¨ÑUsed to modify tuples in a relation.BÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄBÄΩ\ÄÄ*lÅRelation: BÄ¿BÄ~BÄ¡\ÄBÄ˛ÏÜSpecify the relation whose tuples are to be modified.ÄBÄƒ\ÄÄ*ÏÅWhere clause: BÄ¿BÄÀBÄ¡\ÄBÄ˛ÏÉProvide a selection criteria.ÄBÄƒ\ÄÄ*¨ÅAttributes: BÄ¿BÄéBÄ¡\ÄBÄ˛làSpecify a list of attributes in the above relation to be modified.BÄƒ\ÄÄ*,ÅValues: BÄ¿BÄ–BÄ¡\ÄBÄ˛ÏàSpecify a corresponding list of values to modify the above attributes.BÄƒBÄ*ÏÑGive parameters for MODIFY TUPLES ==>ÄBÄ˘BÄ·
\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄÏ
BÄÏBÄÈ
BÄÌBÄÈ
BÄÓBÄÌ
BÄ˛BÄ
BÄÙBÄÒ
BÄªBÄÙ
BÄ˘BÄ·
BÄÃÄ\ÄBÄ^BÄ‡√ÅDELETE-TUPLESÄÄÎÄ	ÜÄ@ƒFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ\ÄBÄmBÄ†BÄ‹
BÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛Ï£Deletes the tuples which satisfy the WHERE clause from the specified relation.

   RELATION - Name of the relation from which the tuples are to be deleted.
   WHERE    - Selection criterion to be used.  (DELETE-TUPLES RELATION &REST KEYWORD-LIST &KEY &OPTIONAL WHERE &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄ¿BÄ‰
¿BÄ≠“BÄﬂ“BÄíP@¡Pˇ€PPÅQPÇQíö	ö@ëÅQPÇQí
îOÄBÄÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ\ÄBÄ8\ÄBÄÍÏÅDelete TuplesÄBÄÏBÄBÄÌBÄBÄÓ\ÄBÄfBÄ˛¨ÑUsed to delete tuples in a relation.BÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄBÄΩ\ÄÄ*lÅRelation: BÄ¿BÄ~BÄ¡\ÄBÄ˛lÜSpecify a relation whose tuples are to be deleted.BÄƒ\ÄÄ*ÏÅWhere clause: BÄ¿BÄ:BÄ¡\ÄBÄ˛,ÜDeletes the tuples which satisfy this condition.BÄƒBÄ*ÏÑGive parameters for DELETE TUPLES ==>ÄBÄ˘BÄ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ!BÄÏBÄBÄÌBÄBÄÓBÄ"BÄ˛BÄ#BÄÙBÄ$BÄªBÄ'BÄ˘BÄBÄÃÄ\ÄBÄ^BÄ‡ÇRETRIEVE-TUPLESÄÄÎÄ]ÜÄ‡FÄ>¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ7\ÄBÄmBÄ†BÄ∫BÄ‹
BÄNBÄ†BÄ°BÄ¢BÄ£BÄ¨BÄOBÄPBÄQBÄﬂBÄRBÄSBÄTBÄ¬BÄUBÄVBÄWBÄ.BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏøORetrieve some tuples from a relation satisying a where clause.

   RELATION-NAME        - Name of the relation whose tuples are to be retrieved.
   WHERE                - Criterion to be used in selecting the tuples.
   PROJECT              - List of attributes to be projected in the result.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   INDEX-NAME           - Name of the index to use in the retrieval.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   WIDE                 - If T, result is printed out in Attribute: value format rather than as a table.
   NUMBER               - If WIDE is T, this keyword specifies the number of attributes per line.
   OUTPUT               - If the result is to be sent to a file, specify the file-name.
   PRINT                - If NIL, the result is not printed.
   QPRINT               - If T, the result is printed without formatting.
   QUICK-SORT           - Specifies the attributes to sort the result on.
   SORT                 - If any domain-specific, user-defined sort mechanism is to be used, this keyword
                          can be used.
   STREAM               - Specify the window to which the output is to be sent, if it is different than the
                          the *standard-output* or RTMS-interface.
   TUPLES               - If T, the resultant tuples are returned.
   UNIQUE               - If T, only unique tuples are retrieved.   (RETRIEVE RELATION-NAME &REST KEYWORD-LIST &KEY &OPTIONAL DIRECTORY DOCUMENTATION FORMAT IMPLEMENTATION-TYPE INDEX-NAME INTO KEY NUMBER OUTPUT PRINT PROJECT QPRINT QUICK-SORT SORT STREAM STORAGE-STRUCTURE TUPLES UNIQUE WHERE WIDE &ALLOW-OTHER-KEYS)¿ÜÄÀÄBÄõëBÄ‹¿lÄ~S¿BÄ®¿ÅPROJECTÄ¿BÄ‰
¿BÄN¿BÄ†¿BÄ°¿BÄ¢¿BÄ£¿BÄ¨¿BÄO¿BÄ^¿BÄQ¿BÄﬂ¿BÄR¿BÄ_¿BÄT¿BÄ¬¿BÄ`¿BÄV¿BÄW¿BÄ.¿BÄ≠“BÄﬂ“BÄ®íPA¡Pˇ€PPÅQPÇQ±‰ˇ€¸ÇQ	PÉQ
PÑQPÖQPÜQPáQPàQP QPä?BPãQPåQP
QPéQPèQPêQPëQPíQPìQPîQPïQ(J∫@√ööAëÅQ@QîOÄHBÄ7Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ8\ÄBÄ8\ÄBÄÍ,ÇRetrieve TuplesÄBÄÏBÄLBÄÌBÄLBÄÓ\Ä•Ä@RBÄ˛ÏÑUsed to Retrieve tuples in a relation.BÄÙ\Ä\ÄBÄaBÄ˜lÅOperatorsÄBÄª\ÄBÄΩBÄq\ÄÄ*¨ÅAttributes: BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏáProvide a list of attributes. If not all attributes all used.ÄBÄƒBÄ˘
BÄuBÄ—BÄyBÄ⁄BÄﬁBÄ‚BÄ}BÄÅBÄÖBÄÕBÄ BÄ
BÄíBÄñBÄöBÄûBÄ¢\ÄÄ*¨ÅIndex-name:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛làIf the data is to come from an index instead of the base relation.BÄƒBÄ*,ÖGive parameters for RETRIEVE TUPLES ==>ÄBÄ˘BÄD\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄOBÄÏBÄLBÄÌBÄLBÄÓBÄPBÄ˛BÄRBÄÙBÄSBÄªBÄVBÄ˘BÄDBÄÃÄ\ÄBÄ^BÄ‡p¿BÄuÏÄSELECTÄÎÄ ZÜÄ‡ FÄ=¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄf\ÄBÄmBÄ†BÄ‹
BÄNBÄ†BÄ°BÄ¢BÄ£BÄ¨BÄOBÄPBÄQBÄﬂBÄRBÄSBÄTBÄ¬BÄUBÄVBÄWBÄ.BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛Ïø˝Same as Retrieve except that all attributes are retrieved.

   RELATION-NAME        - Name of the relation whose tuples are to be retrieved.
   WHERE                - Criterion to be used in selecting the tuples.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   WIDE                 - If T, result is printed out in Attribute: value format rather than as a table.
   NUMBER               - If WIDE is T, this keyword specifies the number of attributes per line.
   OUTPUT               - If the result is to be sent to a file, specify the file-name.
   PRINT                - If NIL, the result is not printed.
   QPRINT               - If T, the result is printed without formatting.
   QUICK-SORT           - Specifies the attributes to sort the result on.
   SORT                 - If any domain-specific, user-defined sort mechanism is to be used, this keyword
                          can be used.
   STREAM               - Specify the window to which the output is to be sent, if it is different than the
                          the *standard-output* or RTMS-interface.
   TUPLES               - If T, the resultant tuples are returned.
   UNIQUE               - If T, only unique tuples are retrieved.  (SELECT-TUPLES RELATION-NAME &REST KEYWORD-LIST &KEY &OPTIONAL DIRECTORY DOCUMENTATION FORMAT IMPLEMENTATION-TYPE INTO KEY NUMBER OUTPUT PRINT QPRINT QUICK-SORT SORT STREAM STORAGE-STRUCTURE TUPLES UNIQUE WHERE WIDE &ALLOW-OTHER-KEYS)¿ÜÄÇ™ÄBÄõëBÄ‹¿lÄ~S¿√ÅSELECT-TUPLESÄ¿BÄ‰
¿BÄN¿BÄ†¿BÄ°¿BÄ¢¿BÄ£¿BÄ¨¿BÄO¿BÄ^¿BÄQ¿BÄﬂ¿BÄR¿BÄ_¿BÄT¿BÄ¬¿BÄ`¿BÄV¿BÄW¿BÄ.¿BÄ≠“BÄﬂ“BÄG¿BÄ“BÄ®íPA¡Pˇ€PPÅQPÇQ	PÉQ
PÑQPÖQPÜQPáQPàQP ?BPäQPãQPåQP
QPéQPèQPêQPëQPíQPìQPîQ&J∫@√ööAëÅQPˇ€@QöîOÄxBÄfÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄh\ÄBÄ8\ÄBÄÍÏÄSelectBÄÏBÄ|BÄÌBÄ|BÄÓ\Ä\ÄBÄBÄLÄ˛¨ÑUsed to Select tuples in a relation.BÄÙ\Ä\ÄBÄaBÄ˜lÅOperatorsÄBÄª\ÄBÄΩBÄqBÄ˘
BÄuBÄ—BÄyBÄ⁄BÄﬁBÄ‚BÄ}BÄÅBÄÖBÄÕBÄ BÄ
BÄíBÄñBÄöBÄûBÄ¢BÄ[BÄ*ÏÑGive parameters for SELECT TUPLES ==>ÄBÄ˘BÄt\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄBÄÏBÄ|BÄÌBÄ|BÄÓBÄÄBÄ˛BÄÇBÄÙBÄÉBÄªBÄÜBÄ˘BÄtBÄÃÄ\ÄBÄ^BÄ‡BÄGÄÎÄ\ÜÄ‡FÄ=¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄé\ÄBÄmBÄ†BÄ∫BÄNBÄ†BÄ°BÄ¢BÄ£BÄ¨BÄOBÄPBÄQBÄﬂBÄRBÄSBÄTBÄ¬BÄUBÄVBÄWBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛Ïø#Same as Retrieve except that all tuples are retrieved.

   RELATION-NAME        - Name of the relation whose tuples are to be retrieved.
   PROJECT              - List of attributes to be projected in the result.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   INDEX-NAME           - Name of the index to use in the retrieval.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   WIDE                 - If T, result is printed out in Attribute: value format rather than as a table.
   NUMBER               - If WIDE is T, this keyword specifies the number of attributes per line.
   OUTPUT               - If the result is to be sent to a file, specify the file-name.
   PRINT                - If NIL, the result is not printed.
   QPRINT               - If T, the result is printed without formatting.
   QUICK-SORT           - Specifies the attributes to sort the result on.
   SORT                 - If any domain-specific, user-defined sort mechanism is to be used, this keyword
                          can be used.
   STREAM               - Specify the window to which the output is to be sent, if it is different than the
                          the *standard-output* or RTMS-interface.
   TUPLES               - If T, the resultant tuples are returned.
   UNIQUE               - If T, only unique tuples are retrieved.  (PROJECT RELATION-NAME &REST KEYWORD-LIST &KEY &OPTIONAL DIRECTORY DOCUMENTATION FORMAT IMPLEMENTATION-TYPE INDEX-NAME INTO KEY NUMBER OUTPUT PRINT PROJECT QPRINT QUICK-SORT SORT STREAM STORAGE-STRUCTURE TUPLES UNIQUE WIDE &ALLOW-OTHER-KEYS)¿ÜÄäÄBÄõëBÄ‹¿lÄ~S¿BÄG¿BÄN¿BÄ†¿BÄ°¿BÄ¢¿BÄ£¿BÄ¨¿BÄO¿BÄ^¿BÄQ¿BÄﬂ¿BÄR¿BÄ_¿BÄT¿BÄ¬¿BÄ`¿BÄV¿BÄW¿BÄ≠“BÄﬂ“BÄ‰
¿BÄ“BÄ®íPA¡Pˇ€PPÅQPÇQ±‰ˇ€¸ÇQPÉQ	PÑQ
PÖQPÜQPáQPàQP ?BPäQPãQPåQP
QPéQPèQPêQPëQPíQPìQ$J∫@√ööAëÅQPˇ›@QöîOÄùBÄéÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄG\ÄBÄ8\ÄBÄÍ,ÅProjectÄBÄÏBÄ°BÄÌBÄ°BÄÓ\Ä\ÄBÄBÄkBÄ˛ÏÑUsed to Project tuples in a relation.ÄBÄÙ\Ä\ÄBÄaBÄ˜lÅOperatorsÄBÄª\ÄBÄΩBÄqBÄWBÄuBÄ—BÄyBÄ⁄BÄﬁBÄ‚BÄ}BÄÅBÄÖBÄÕBÄ BÄ
BÄíBÄñBÄöBÄûBÄ¢BÄ*ÏÑGive parameters for PROJECT TUPLES ==>BÄ˘BÄö\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ§BÄÏBÄ°BÄÌBÄ°BÄÓBÄ•BÄ˛BÄßBÄÙBÄ®BÄªBÄ´BÄ˘BÄöBÄÃÄ\ÄBÄ^BÄ‡CÇCOMMIT-TRANSACTIONÄÎÄ
 ÜÄAFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ≥\ÄBÄm√ÄTRANSÄBÄ†BÄûBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛¨≠Execute the database calls in a transaction.

   TRANSACTION - Name of the transaction to be commited.
   DIRECTORY   - Name of the directory in which this transaction can be found, if not in memory.
   PATHNAME    - Name of the file in which it can be found.  (COMMIT-TRANSACTION TRANSACTION &REST KEYWORD-LIST &KEY &OPTIONAL DIRECTORY PATHNAME &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿BÄ¥¿BÄ†¿BÄû¿BÄ≠“BÄﬂ“BÄ¥íPA¡Pˇ€PPÅQPÇQPÉQ	¢@√	ö
öAëÅQ@QîOÄ√BÄ≥Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ¥\ÄBÄ8\ÄBÄÍlÇCommit TransactionBÄÏBÄ«BÄÌBÄ«BÄÓ\Ä\ÄBÄ•Ä CBÄ˛¨áCommit a transaction - execute all the database calls in it.BÄÙ\Ä\ÄBÄaBÄ˜lÅOperatorsÄBÄª\ÄÄΩ\ÄÄ*lÉName of the transaction :ÄBÄ¿BÄÜBÄ¡\ÄBÄ˛¨ÑThe name of an existing transaction.BÄƒ\ÄÄ*ÏÇName of the directory:BÄ¿BÄüBÄ¡\ÄBÄ˛¨åName of the directory which contains the transaction file, if the transaction is not in the memory.ÄBÄƒ\ÄÄ*lÅPathname:ÄBÄ¿BÄñBÄ¡\ÄBÄ˛ÏéIf the transaction is not in memory, provide the pathname for the transaction file. It defaults to <transaction>.lisp.BÄƒBÄ*ÏÑGive parameters for COMMIT TRANSACTIONBÄ˘BÄ¡\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ BÄÏBÄ«BÄÌBÄ«BÄÓBÄÀBÄ˛BÄŒBÄÙBÄœBÄªBÄ“BÄ˘BÄ¡BÄÃÄ\ÄBÄ^BÄ‡ÉÄJOINÄÎÄDÜÄCàFÄ.¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄÊ\ÄBÄmBÄNÉÄFROMBÄGBÄ‰
BÄ¬BÄﬂBÄ†BÄ°BÄ¢BÄ£BÄ¨BÄTBÄWBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛Ïø3This function provides the capability to combine two relations into a new relation
   in which the tuples which are to participate in the operation are selected
   by a where clause.

   FROM                 - A list consisting of the relations to be joined.
   PROJECT              - This clause specifies the attributes that are to be in the resultant relation
                          and their associated names in that new relation. It should be of the form
                          (<[relation-name.]attribute-name>). The optional part relation-name can be
                          skipped if the attribute is unique in one of the two relations being joined.
                          If the keyword FROM is not specified, this clause should contain the names
                          of the relations to be joined. Also, if * is given instead of the attribute-name
                          it indicates that RTMS should use all the attributes in that relation.
   WHERE                - Can be used to perform theta-joins. It is a condition used in joining the relations.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   PRINT                - If NIL, the resultant relation will not be printed out.
   TUPLES               - If T, the resultant tuples will be returned.
   UNIQUE               - If T, only unique tuples will be part of the resultant relation.  (JOIN &REST KEYWORD-LIST &KEY FROM &KEY &OPTIONAL PROJECT WHERE INTO DIRECTORY DOCUMENTATION FORMAT IMPLEMENTATION-TYPE KEY STORAGE-STRUCTURE PRINT TUPLES UNIQUE &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄÁ¿BÄ¿BÄG¿BÄN¿BÄ¬¿BÄﬂ¿BÄ†¿BÄ°¿BÄ¢¿BÄ£¿BÄ¨¿BÄT¿BÄ‰
¿BÄW¿BÄ≠“BÄﬂ“BÄ“√ÅJOIN-INTERNALÄíPA¡Pˇ€PPPÇQPÉQ	PÅQ
PÖQPÜQPáQPàQP QPäQPãQPåQPÑQP
QJ∫@√¢öAëPÇQ@QöåOÄ˜BÄÊÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄÁ\ÄBÄ8\ÄBÄÍ¨ÄJoinBÄÏBÄ˚BÄÌBÄ˚BÄÓ\Ä•Ä JBÄ˛,ÉUsed to join relations.ÄBÄÙ\Ä\ÄBÄaBÄ˜lÅOperatorsÄBÄª\ÄBÄΩ\ÄÄ*lÇOutput relation :ÄBÄ¿BÄ‘BÄ¡\ÄBÄ˛ÏéIf not provided, the result of JOIN is stored in a temporary relation unless only the resultant tuples are requested.ÄBÄƒ\ÄÄ*ÏÄFROM :BÄ¿BÄ‡BÄ¡\ÄBÄ˛ÏÖSpecify a list of two relations to be joined.ÄBÄƒ\ÄÄ*lÅProject :ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛,ñThis gives the attributes in the output relation. Example: (rel1.* a3 (rel2.a1 a4)) ==> All the attributes in rel1, attribute A3 of rel2 and atribute A1 of rel2 renamed as A4.ÄBÄƒ\ÄÄ*,ÅWhere :ÄBÄ¿BÄÿBÄ¡\ÄBÄ˛lèThe join clause using the theta-operators. It is a where clause consisting of attributes from the relations being joined.ÄBÄƒ\ÄÄ*,ÅTuples?ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛,ëSpecify if the resultant tuples be returned rather than inserted in a relation. The following parameters can be ignored if this is true.BÄ¶BÄÕBÄ—BÄyBÄ⁄BÄﬁBÄ‚BÄíBÄ¢BÄ*,ÉGive parameters for JOINBÄ˘BÄÙ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ˛BÄÏBÄ˚BÄÌBÄ˚BÄÓBÄˇBÄ˛BÄBÄÙBÄBÄªBÄBÄ˘BÄÙBÄÃÄ\ÄBÄ^BÄ‡ÇDESTROY-DATABASEÄÎÄ	ÜÄ@»FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ!\ÄBÄmBÄ°BÄÅBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛,ßDelete the specified database from memory and all the corresponding files from
   disk if the keyword DISK is T.

   DATABASE - Name of the database to be destroyed.
   DISK     - If T, all the relevant files will be deleted.  (DESTROY-DATABASE DATABASE &REST KEYWORD-LIST &KEY &OPTIONAL DISK &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄ"¿BÄÅ¿BÄ≠“BÄﬂ“BÄ"íPA¡Pˇ€PPÅQPÇQí@√ö	öAëÅQ@Q
îOÄ0BÄ!Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ"\ÄBÄ8\ÄBÄÍ,ÇDestroy DatabaseBÄÏBÄ4BÄÌBÄ4BÄÓ\Ä\Ä•Ä KBÄêBÄ˛lÉUsed to destroy databasesÄBÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄBÄΩ\ÄÄ*ÏÅDatabase Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏÑName of the database to be destroyed.ÄBÄƒ\ÄÄ*ÏÇDelete from the DISK:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛Ï IF YES all the files pertaining to this database are deleted but NOT EXPUNGED.BÄ¶BÄ*ÏÑGive parameters for DESTROY DATABASE:ÄBÄ˘BÄ.\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ7BÄÏBÄ4BÄÌBÄ4BÄÓBÄ8BÄ˛BÄ;BÄÙBÄ<BÄªBÄ?BÄ˘BÄ.BÄÃÄ\ÄBÄ^BÄ‡√ÅDESTROY-DOMAINÄÎÄ
ÜÄ@
ÑFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄO\ÄBÄmBÄ‰BÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏèDestroys the domain definition but keeps the domain predicate to handle previously defined data.  (DESTROY-DOMAIN DOMAIN-NAME)ÄBÄõëBÄ‹¿lÄ~S¿BÄP¿BÄ≠“BÄﬂ“BÄPíP@¡Pˇ€PPÅQíö@ëÅQ	åOÄ]BÄOÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄP\ÄBÄ8\ÄBÄÍÏÅDestroy DomainBÄÏBÄaBÄÌBÄaBÄÓ\Ä•Ä`KBÄ˛,ÉUsed to destroy domains.BÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄBÄΩ\ÄÄ*¨ÅDomain Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨ÑName of the domain to be destroyed.ÄBÄƒBÄ*¨ÑGive parameters for DESTROY DOMAIN:ÄBÄ˘BÄ[\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄdBÄÏBÄaBÄÌBÄaBÄÓBÄeBÄ˛BÄgBÄÙBÄhBÄªBÄkBÄ˘BÄ[BÄÃÄ\ÄBÄ^BÄ‡√ÇDESTROY-IMPLEMENTATIONÄÎÄ
ÜÄ@
ÑFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄw\ÄBÄmBÄBÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛,óDestroys implementation type definition but keeps the accessor functions to handle previously defined relations using this implementation.  (DESTROY-IMPLEMENTATION IMPLEMENTATION-NAME)ÄBÄõëBÄ‹¿lÄ~S¿BÄx¿BÄ≠“BÄﬂ“BÄxíP@¡Pˇ€PPÅQíö@ëÅQ	åOÄÖBÄwÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄx\ÄBÄ8\ÄBÄÍÏÇDestroy ImplementationBÄÏBÄ BÄÌBÄ BÄÓ\Ä\ÄBÄ:BÄÕBÄ˛,ÑUsed to destroy implementations.BÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄBÄΩ\ÄÄ*¨ÇImplementation Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨ÖName of the implementation to be destroyed.ÄBÄƒBÄ*¨ÖGive parameters for DESTROY IMPLEMENTATION:ÄBÄ˘BÄÉ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄåBÄÏBÄ BÄÌBÄ BÄÓBÄ
BÄ˛BÄèBÄÙBÄêBÄªBÄìBÄ˘BÄÉBÄÃÄ\ÄBÄ^BÄ‡√ÅDESTROY-INDEXÄÄÎÄ
ÜÄ@
ƒFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄü\ÄBÄmBÄ∏BÄ.BÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛,üDestroy the specified index which is defined on the specified relation.

   RELATION-NAME - The name of the relation upon which the relation is defined.
   INDEX-NAME - The name of the index to be deleted.  (DESTROY-INDEX RELATION-NAME INDEX-NAME)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄ†¿BÄ≠“BÄﬂ“BÄ†íP@¡Pˇ€PPÅQÇQöö@ëÅQÇQ	îOÄ≠BÄüÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ†\ÄBÄ8\ÄBÄÍÏÅDestroy IndexÄBÄÏBÄ±BÄÌBÄ±BÄÓ\Ä\ÄBÄ:BÄBBÄ˛,ÉUsed to destroy indices.BÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄBÄΩ\ÄÄ*ÏÅRelation Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨àName of the relation on which the index to be destroyed is defined.ÄBÄƒ\ÄÄ*¨ÅIndex Name:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛lÑName of the index to be destroyed.BÄƒBÄ*lÑGive parameters for DESTROY INDEX:BÄ˘BÄ´\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ¥BÄÏBÄ±BÄÌBÄ±BÄÓBÄµBÄ˛BÄ∑BÄÙBÄ∏BÄªBÄªBÄ˘BÄ´BÄÃÄ\ÄBÄ^BÄ‡CÉDESTROY-STORAGE-STRUCTUREÄÄÎÄ
ÜÄ@
ÑFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄÀ\ÄBÄmBÄ0BÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛,óDestroys storage structure definition but keeps the accessor functions to handle previously defined relations using this structure.  (DESTROY-STORAGE-STRUCTURE STORAGE-STRUCTURE-NAME)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄÃ¿BÄ≠“BÄﬂ“BÄÃíP@¡Pˇ€PPÅQíö@ëÅQ	åOÄŸBÄÀÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄÃ\ÄBÄ8\ÄBÄÍlÉDestroy Storage StructureÄBÄÏBÄ›BÄÌBÄ›BÄÓ\Ä\ÄBÄ:BÄLÄ˛¨ÑUsed to destroy storage structures.ÄBÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄBÄΩ\ÄÄ*,ÉStorage structure name:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛ÏÖName of the storage structure to be destroyed.BÄƒBÄ*ÏÖGive parameters for DESTROY STORAGE STRUCTURE:BÄ˘BÄ◊\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ‡BÄÏBÄ›BÄÌBÄ›BÄÓBÄ·BÄ˛BÄ„BÄÙBÄ‰BÄªBÄÁBÄ˘BÄ◊BÄÃÄ\ÄBÄ^BÄ‡ÉÅDESTROY-VIEWÄÎÄ
ÜÄ@
ÑFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄÛ\ÄBÄmBÄb	BÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛,áDestroys the view from memory.  (DESTROY-VIEW VIEW-NAME)ÄBÄõëBÄ‹¿lÄ~S¿BÄÙ¿BÄ≠“BÄﬂ“BÄÙíP@¡Pˇ€PPÅQíö@ëÅQ	åOÄBÄÛÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄÙ\ÄBÄ8\ÄBÄÍ¨ÅDestroy ViewBÄÏBÄBÄÌBÄBÄÓ\Ä\ÄBÄ:BÄt	BÄ˛ÏÇUsed to destroy views.BÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄBÄΩ\ÄÄ*lÅView name:BÄ¿BÄ:BÄ¡\ÄBÄ˛lÑName of the view to be destroyed.ÄBÄƒBÄ*lÑGive parameters for DESTROY VIEW:ÄBÄ˘BÄˇ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄBÄÏBÄBÄÌBÄBÄÓBÄ	BÄ˛BÄBÄÙBÄBÄªBÄBÄ˘BÄˇBÄÃÄ\ÄBÄ^BÄ‡ÇDESTROY-RELATIONÄÎÄ	ÜÄ@»FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ\ÄBÄmBÄ†BÄÅBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛l¶Deletes the specified relation from the active database.
   Deletes all the files on disk if keyword DISK is t.

   RELATION - Name of the relation to be destroyed.
   DISK     - If T, the relevant files will be deleted.  (DESTROY-RELATION RELATION &REST KEYWORD-LIST &KEY &OPTIONAL DISK &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿BÄ¿BÄÅ¿BÄ≠“BÄﬂ“BÄíPA¡Pˇ€PPÅQPÇQí@√ö	öAëÅQ@Q
îOÄ*BÄÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ\ÄBÄ8\ÄBÄÍ,ÇDestroy RelationBÄÏBÄ.BÄÌBÄ.BÄÓ\Ä\ÄBÄ:BÄBÄ˛lÉUsed to destroy relationsÄBÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄBÄΩ\ÄÄ*ÏÅRelation Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏÑName of the relation to be destroyed.ÄBÄƒ\ÄÄ*ÏÇDelete from the DISK:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛¨ IF YES the file corresponding to this relation is deleted but NOT EXPUNGED.ÄBÄ¶BÄ*ÏÑGive parameters for DESTROY RELATION:ÄBÄ˘BÄ(\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ1BÄÏBÄ.BÄÌBÄ.BÄÓBÄ2BÄ˛BÄ4BÄÙBÄ5BÄªBÄ8BÄ˘BÄ(BÄÃÄ\ÄBÄ^BÄ‡CÇDESTROY-ATTRIBUTEÄÄÎÄ
 ÜÄAFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄH\ÄBÄmBÄ†BÄ√BÄ¢BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛,ØAttributes in a relation can be deleted using this function.

   RELATION-NAME - Name of the relation from which the attributes are to be deleted.
   ATTRIBUTE     - List of attributes to be destroyed.
   KEY           - List of attributes to form the new key, if so desired.  (DESTROY-ATTRIBUTE RELATION-NAME &REST KEYWORD-LIST &KEY &OPTIONAL ATTRIBUTE KEY &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿BÄI¿BÄ√¿BÄ¢¿BÄ≠“BÄﬂ“BÄIíPA¡Pˇ€PPÅQPÇQPÉQ	¢@√	ö
öAëÅQ@QîOÄWBÄHÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄI\ÄBÄ8\ÄBÄÍlÇDestroy AttributeÄBÄÏBÄ[BÄÌBÄ[BÄÓ\Ä\ÄBÄ:BÄÚBÄ˛lÖUsed to destroy attributes from relationsÄBÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄÄΩ\ÄÄ*ÏÅRelation Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛,àName of the relation from which attributes are to be destroyed.ÄBÄƒ\ÄÄ*¨ÅAttributes:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛ÏÉList of attributes to destroy.BÄƒ\ÄÄ*¨ÄKey:BÄ¿BÄ:BÄ¡\ÄBÄ˛,èNew key for the relation if it is to be different from the previous value or if any of the key attributes are destroyed.BÄƒBÄ*ÏÑGive parameters for DESTROY ATTRIBUTE:BÄ˘BÄU\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ^BÄÏBÄ[BÄÌBÄ[BÄÓBÄ_BÄ˛BÄaBÄÙBÄbBÄªBÄeBÄ˘BÄUBÄÃÄ\ÄBÄ^BÄ‡ÍÄUNIONÄÄÎÄ9ÜÄCFÄ'¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄy\ÄBÄmBÄBÄNBÄ¬BÄﬂBÄ†BÄ°BÄ¢BÄ£BÄ¨BÄTBÄWBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏøsUnion of tuples in two relations.

   FROM                 - This clause specifies the relations to participate in the UNION operation.
                          In addition, RTMS allows users to specify the attributes in these relations to
                          participate in the operation as well as a where-clause to specify the tuples.
                          It should be of the format: (RelA [(PROJECT <attrA> WHERE where-clause-A)] RelB
                          [(PROJECT <attrB> WHERE where-clause-B)]) where attrA indicates attributes in
                          relation relA and where-clause-A indicates a where-clause involving the attributes
                          in relation relA.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   PRINT                - If NIL, the resultant relation will not be printed out.
   TUPLES               - If T, the resultant tuples will be returned.
   UNIQUE               - If T, only unique tuples will be part of the resultant relation.  (RELATION-UNION &REST KEYWORD-LIST &KEY &OPTIONAL FROM INTO DIRECTORY DOCUMENTATION FORMAT IMPLEMENTATION-TYPE STORAGE-STRUCTURE KEY PRINT TUPLES UNIQUE &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿√ÅRELATION-UNION¿BÄN¿BÄ¿BÄ¬¿BÄﬂ¿BÄ†¿BÄ°¿BÄ¢¿BÄ£¿BÄ¨¿BÄT¿BÄW¿BÄ≠“BÄﬂ“BÄàíPA¡Pˇ€PPPÇQPÅQ	PÉQ
PÑQPÖQPÜQPáQPàQP QPäQPãQJ∫@√íöAë@QåOÄ BÄyÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄz\ÄBÄ8\ÄBÄÍÏÄUnionÄBÄÏBÄ
BÄÌBÄ
BÄÓ\Ä\Ä•Ä O•Ä UBÄ˛ÏÖUsed to form union of two compatible relationsBÄÙ\Ä\ÄBÄaBÄ˜lÅOperatorsÄBÄª\ÄBÄΩ\ÄÄ*ÏÇList of two relations:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨§List of the names of two relations which will take part in the relation union operation. The attributes to be projected and a where clause can be specified for each relation using keywords. For instance, (REL1 (PROJECT <attr> WHERE <where-claue>) REL2 (WHERE <where-clause> PROJECT <attr>)).ÄBÄƒBÄu\ÄÄ*,ÅTuples?ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛,ëSpecify if the resultant tuples be returned rather than inserted in a relation. The following parameters can be ignored if this is true.BÄ¶BÄÕBÄ—BÄyBÄ⁄BÄﬁBÄ‚BÄíBÄ¢BÄ*ÏÖParameters for the set-union of two relationsÄBÄ˘BÄÜ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄêBÄÏBÄ
BÄÌBÄ
BÄÓBÄëBÄ˛BÄïBÄÙBÄñBÄªBÄôBÄ˘BÄÜBÄÃÄ\ÄBÄ^BÄ‡CÅDIFFERENCEÄÎÄ9ÜÄCFÄ'¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ©\ÄBÄmBÄBÄNBÄ¬BÄﬂBÄ†BÄ°BÄ¢BÄ£BÄ¨BÄTBÄWBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛Ïø|Difference of the tuples in two relations.

   FROM                 - This clause specifies the relations to participate in the DIFFERENCE operation.
                          In addition, RTMS allows users to specify the attributes in these relations to
                          participate in the operation as well as a where-clause to specify the tuples.
                          It should be of the format: (RelA [(PROJECT <attrA> WHERE where-clause-A)] RelB
                          [(PROJECT <attrB> WHERE where-clause-B)]) where attrA indicates attributes in
                          relation relA and where-clause-A indicates a where-clause involving the attributes
                          in relation relA.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   PRINT                - If NIL, the resultant relation will not be printed out.
   TUPLES               - If T, the resultant tuples will be returned.
   UNIQUE               - If T, only unique tuples will be part of the resultant relation.  (RELATION-DIFFERENCE &REST KEYWORD-LIST &KEY &OPTIONAL FROM INTO DIRECTORY DOCUMENTATION FORMAT IMPLEMENTATION-TYPE STORAGE-STRUCTURE KEY PRINT TUPLES UNIQUE &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿ÉÇRELATION-DIFFERENCEÄ¿BÄN¿BÄ¿BÄ¬¿BÄﬂ¿BÄ†¿BÄ°¿BÄ¢¿BÄ£¿BÄ¨¿BÄT¿BÄW¿BÄ≠“BÄﬂ“BÄ∏íPA¡Pˇ€PPPÇQPÅQ	PÉQ
PÑQPÖQPÜQPáQPàQP QPäQPãQJ∫@√íöAë@QåOÄπBÄ©Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ™\ÄBÄ8\ÄBÄÍlÅDifferenceBÄÏBÄΩBÄÌBÄΩBÄÓ\Ä\ÄBÄìBÄêBÄ˛¨ÜUsed to form difference of two compatible relationsÄBÄÙ\Ä\ÄBÄaBÄ˜lÅOperatorsÄBÄª\ÄBÄΩ\ÄÄ*ÏÇList of two relations:BÄ¿BÄ:BÄ¡\ÄBÄ˛,•List of the names of two relations which will take part in the relation difference operation. The attributes to be projected and a where clause can be specified for each relation using keywords. For instance, (REL1 (PROJECT <attr> WHERE <where-claue>) REL2 (WHERE <where-clause> PROJECT <attr>)).BÄƒBÄu\ÄÄ*,ÅTuples?ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛,ëSpecify if the resultant tuples be returned rather than inserted in a relation. The following parameters can be ignored if this is true.BÄ¶BÄÕBÄ—BÄyBÄ⁄BÄﬁBÄ‚BÄíBÄ¢BÄ*lÜParameters for the set-difference of two relationsBÄ˘BÄ∂\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ¿BÄÏBÄΩBÄÌBÄΩBÄÓBÄ¡BÄ˛BÄ√BÄÙBÄƒBÄªBÄ«BÄ˘BÄ∂BÄÃÄ\ÄBÄ^BÄ‡™ÅINTERSECTIONÄÎÄ9ÜÄCFÄ'¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ◊\ÄBÄmBÄBÄNBÄ¬BÄﬂBÄ†BÄ°BÄ¢BÄ£BÄ¨BÄTBÄWBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛Ïø}Intersection of tuples in two relations.

   FROM                 - This clause specifies the relations to participate in the INTERSECTION operation.
                          In addition, RTMS allows users to specify the attributes in these relations to
                          participate in the operation as well as a where-clause to specify the tuples.
                          It should be of the format: (RelA [(PROJECT <attrA> WHERE where-clause-A)] RelB
                          [(PROJECT <attrB> WHERE where-clause-B)]) where attrA indicates attributes in
                          relation relA and where-clause-A indicates a where-clause involving the attributes
                          in relation relA.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   PRINT                - If NIL, the resultant relation will not be printed out.
   TUPLES               - If T, the resultant tuples will be returned.
   UNIQUE               - If T, only unique tuples will be part of the resultant relation.  (RELATION-INTERSECTION &REST KEYWORD-LIST &KEY &OPTIONAL FROM INTO DIRECTORY DOCUMENTATION FORMAT IMPLEMENTATION-TYPE STORAGE-STRUCTURE KEY PRINT TUPLES UNIQUE &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿√ÇRELATION-INTERSECTIONÄ¿BÄN¿BÄ¿BÄ¬¿BÄﬂ¿BÄ†¿BÄ°¿BÄ¢¿BÄ£¿BÄ¨¿BÄT¿BÄW¿BÄ≠“BÄﬂ“BÄÊíPA¡Pˇ€PPPÇQPÅQ	PÉQ
PÑQPÖQPÜQPáQPàQP QPäQPãQJ∫@√íöAë@QåOÄÁBÄ◊Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄÿ\ÄBÄ8\ÄBÄÍ¨ÅIntersectionBÄÏBÄÎBÄÌBÄÎBÄÓ\Ä\ÄBÄìBÄÕBÄ˛ÏÜUsed to form intersection of two compatible relationsÄBÄÙ\Ä\ÄBÄaBÄ˜lÅOperatorsÄBÄª\ÄBÄΩ\ÄÄ*ÏÇList of two relations:BÄ¿BÄ:BÄ¡\ÄBÄ˛l•List of the names of two relations which will take part in the relation intersection operation. The attributes to be projected and a where clause can be specified for each relation using keywords. For instance, (REL1 (PROJECT <attr> WHERE <where-claue>) REL2 (WHERE <where-clause> PROJECT <attr>)).BÄƒBÄu\ÄÄ*,ÅTuples?ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛,ëSpecify if the resultant tuples be returned rather than inserted in a relation. The following parameters can be ignored if this is true.BÄ¶BÄÕBÄ—BÄyBÄ⁄BÄﬁBÄ‚BÄíBÄ¢BÄ*¨ÜParameters for the set-intersection of two relationsBÄ˘BÄ‰\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄÓBÄÏBÄÎBÄÌBÄÎBÄÓBÄÔBÄ˛BÄÒBÄÙBÄÚBÄªBÄıBÄ˘BÄ‰BÄÃÄ\ÄBÄ^BÄ‡ÅAVERAGEÄÄÎÄ(ÜÄA»FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ\ÄBÄmBÄ†BÄüBÄWBÄ‰
CÄBYBÄ¬BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏøIAverage of the values of a given attribute in a relation satisfying a where clause.

   RELATION-NAME  - Name of the relation.
   ATTRIBUTE-NAME - Name of the attribute whose average is to be found.
   UNIQUE         - If T, only unique values will be used.
   WHERE          - If a selection criterion is provided, only the satisfying values will be used.
   BY             - Name of the attribute to group average of the above attribute by.
   TUPLES         - If T, the resultant values will be returned rather than printed out as a table.  (AVERAGE RELATION-NAME ATTRIBUTE-NAME &REST KEYWORD-LIST &KEY &OPTIONAL UNIQUE WHERE BY TUPLES &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄ¿BÄW¿BÄ‰
¿BÄ¿BÄ¬¿BÄ≠“BÄﬂ“BÄíPA¡Pˇ€PPÅQÇQPÉQPÑQ	PÖQ
PÜQJ∫@√¢öAëÅQÇQ@QúOÄBÄÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ\ÄBÄ8\ÄBÄÍ,ÅAverageÄBÄÏBÄBÄÌBÄBÄÓ\Ä\ÄBÄìBÄÚBÄ˛làUsed to compute the average of the attribute values in a relation.BÄÙ\Ä\ÄBÄaBÄ˜lÅOperatorsÄBÄª\Ä	BÄΩ\ÄÄ*ÏÅRelation name:BÄ¿BÄ~BÄ¡\ÄBÄ˛làName of the relation which contains the attribute to be averaged.ÄBÄƒ\ÄÄ*,ÇAttribute name:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛¨ÖName of the attribute in the above relation.BÄƒ\ÄÄ*,ÅUnique?ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛läIf true, only the unique values of the attribute will be used in the calculations.BÄ¶BÄ˘
\ÄÄ*lÄByBÄ¿BÄ:BÄ¡\ÄBÄ˛ÏàSpecify the attribute to be used in grouping the data into categories.BÄƒBÄñBÄ*,ÉParameters for average:ÄBÄ˘BÄ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄBÄÏBÄBÄÌBÄBÄÓBÄBÄ˛BÄBÄÙBÄ BÄªBÄ#BÄ˘BÄBÄÃÄ\ÄBÄ^BÄ‡ÉÄSUMÄÄÎÄ(ÜÄA»FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ;\ÄBÄmBÄ†BÄüBÄWBÄ‰
BÄBÄ¬BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏøASum of the values of a given attribute in a relation satisfying a where clause.

   RELATION-NAME  - Name of the relation.
   ATTRIBUTE-NAME - Name of the attribute whose sum is to be found.
   UNIQUE         - If T, only unique values will be used.
   WHERE          - If a selection criterion is provided, only the satisfying values will be used.
   BY             - Name of the attribute to group sum of the above attribute by.
   TUPLES         - If T, the resultant values will be returned rather than printed out as a table.  (SUM RELATION-NAME ATTRIBUTE-NAME &REST KEYWORD-LIST &KEY &OPTIONAL UNIQUE WHERE BY TUPLES &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄ<¿BÄW¿BÄ¿BÄ¬¿BÄ‰
¿BÄ≠“BÄﬂ“BÄ<íPA¡Pˇ€PPÅQÇQPÉQPÖQ	PÜQ
PÑQJ∫@√¢öAëÅQÇQ@QúOÄJBÄ;Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ<\ÄBÄ8\ÄBÄÍ¨ÄSumÄBÄÏBÄNBÄÌBÄNBÄÓ\Ä\ÄBÄìBÄLÄ˛ÏáUsed to compute the sum of the attribute values in a relation.BÄÙ\Ä\ÄBÄaBÄ˜lÅOperatorsÄBÄª\Ä	BÄΩ\ÄÄ*ÏÅRelation name:BÄ¿BÄ~BÄ¡\ÄBÄ˛,àName of the relation which contains the attribute to be summed.ÄBÄƒBÄ(BÄ,BÄ˘
BÄ0BÄñBÄ*¨ÇParameters for sum:ÄBÄ˘BÄH\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄQBÄÏBÄNBÄÌBÄNBÄÓBÄRBÄ˛BÄTBÄÙBÄUBÄªBÄXBÄ˘BÄHBÄÃÄ\ÄBÄ^BÄ‡ÉÄSIZEÄÎÄ&ÜÄAàFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄd\ÄÄmBÄ†BÄWBÄ‰
BÄBÄ¬BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛¨ÆNumber of tuples in a relation satisfying a where clause.

   RELATION-NAME  - Name of the relation whose size is to be found.
   UNIQUE         - If T, only unique values will be used.
   WHERE          - If a selection criterion is provided, only the satisfying tuples will be used.  (SIZE RELATION-NAME &REST KEYWORD-LIST &KEY &OPTIONAL UNIQUE WHERE &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄe¿BÄW¿BÄ¿BÄ¬¿BÄ‰
¿BÄ≠“BÄﬂ“BÄeíPA¡Pˇ€PPÅQPÇQPÑQ	PÖQ
PÉQJ∫@√ööAëÅQ@QîOÄsBÄdÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄe\ÄBÄ8\ÄBÄÍ¨ÄSizeBÄÏBÄwBÄÌBÄwBÄÓ\Ä•Ä`SBÄ˛lÖUsed to compute the size of the relation.ÄBÄÙ\Ä\ÄBÄaBÄ˜lÅOperatorsÄBÄª\ÄBÄΩ\ÄÄ*ÏÅRelation name:BÄ¿BÄ~BÄ¡\ÄBÄ˛¨ÖName of the relation whose size is required.BÄƒBÄ,BÄ˘
BÄ0BÄñBÄ*¨ÇParameters for size:BÄ˘BÄq\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄzBÄÏBÄwBÄÌBÄwBÄÓBÄ{BÄ˛BÄ}BÄÙBÄ~BÄªBÄÅBÄ˘BÄqBÄÃÄ\ÄBÄ^BÄ‡ÍÄCOUNTÄÄÎÄ(ÜÄA»FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ
\ÄBÄmBÄ†BÄüBÄWBÄ‰
BÄBÄ¬BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏøGNumber of the values of a given attribute in a relation satisfying a where clause.
   RELATION-NAME  - Name of the relation.
   ATTRIBUTE-NAME - Name of the attribute whose count is to be found.
   UNIQUE         - If T, only unique values will be used.
   WHERE          - If a selection criterion is provided, only the satisfying values will be used.
   BY             - Name of the attribute to group count of the above attribute by.
   TUPLES         - If T, the resultant values will be returned rather than printed out as a table.  (COUNT-RTMS RELATION-NAME ATTRIBUTE-NAME &REST KEYWORD-LIST &KEY &OPTIONAL UNIQUE WHERE BY TUPLES &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿CÅCOUNT-RTMS¿BÄW¿BÄ¿BÄ¬¿BÄ‰
¿BÄ≠“BÄﬂ“BÄúíPA¡Pˇ€PPÅQÇQPÉQPÖQ	PÜQ
PÑQJ∫@√¢öAëÅQÇQ@QúOÄùBÄ
Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄé\ÄBÄ8\ÄBÄÍÏÄCountÄBÄÏBÄ°BÄÌBÄ°BÄÓ\Ä\ÄBÄìBÄÕBÄ˛,àUsed to compute the count of the attribute values in a relation.BÄÙ\Ä\ÄBÄaBÄ˜lÅOperatorsÄBÄª\Ä	BÄΩ\ÄÄ*ÏÅRelation name:BÄ¿BÄ~BÄ¡\ÄBÄ˛lãName of the relation which contains the attribute to be used to find the number of tuples.BÄƒBÄ(BÄ,BÄ˘
BÄ0BÄñBÄ*ÏÇParameters for count:ÄBÄ˘BÄö\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ§BÄÏBÄ°BÄÌBÄ°BÄÓBÄ•BÄ˛BÄßBÄÙBÄ®BÄªBÄ´BÄ˘BÄöBÄÃÄ\ÄBÄ^BÄ‡ÅMAXIMUMÄÄÎÄ%ÜÄAàFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ∑\ÄÄmBÄ†BÄüBÄ‰
BÄBÄ¬BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛Ïø(Maximum of the values of a given attribute in a relation satisfying a where clause.

   RELATION-NAME  - Name of the relation.
   ATTRIBUTE-NAME - Name of the attribute whose maximum is to be found.
   WHERE          - If a selection criterion is provided, only the satisfying values will be used.
   BY             - Name of the attribute to group maximum of the above attribute by.
   TUPLES         - If T, the resultant values will be returned rather than printed out as a table.  (MAXIMUM RELATION-NAME ATTRIBUTE-NAME &REST KEYWORD-LIST &KEY &OPTIONAL WHERE BY TUPLES &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄ∏¿BÄ‰
¿BÄ¿BÄ¬¿BÄ≠“BÄﬂ“BÄ∏íPA¡Pˇ€PPÅQÇQPÉQPÑQ	PÖQ
≤@√
¢öAëÅQÇQ@QúOÄ∆BÄ∑Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ∏\ÄBÄ8\ÄBÄÍ,ÅMaximumÄBÄÏBÄ BÄÌBÄ BÄÓ\Ä\ÄBÄìBÄBÄ˛làUsed to compute the maximum of the attribute values in a relation.BÄÙ\Ä\ÄBÄaBÄ˜lÅOperatorsÄBÄª\ÄBÄΩ\ÄÄ*ÏÅRelation name:BÄ¿BÄ~BÄ¡\ÄBÄ˛làName of the relation which contains the attribute to be maximumd.ÄBÄƒBÄ(BÄ˘
BÄ0BÄñBÄ*,ÉParameters for maximum:ÄBÄ˘BÄƒ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄÕBÄÏBÄ BÄÌBÄ BÄÓBÄŒBÄ˛BÄ–BÄÙBÄ—BÄªBÄ‘BÄ˘BÄƒBÄÃÄ\ÄBÄ^BÄ‡ÅMINIMUMÄÄÎÄ%ÜÄAàFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ‡\ÄÄmBÄ†BÄüBÄ‰
BÄBÄ¬BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛Ïø(Minimum of the values of a given attribute in a relation satisfying a where clause.

   RELATION-NAME  - Name of the relation.
   ATTRIBUTE-NAME - Name of the attribute whose minimum is to be found.
   WHERE          - If a selection criterion is provided, only the satisfying values will be used.
   BY             - Name of the attribute to group minimum of the above attribute by.
   TUPLES         - If T, the resultant values will be returned rather than printed out as a table.  (MINIMUM RELATION-NAME ATTRIBUTE-NAME &REST KEYWORD-LIST &KEY &OPTIONAL WHERE BY TUPLES &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄ·¿BÄ‰
¿BÄ¿BÄ¬¿BÄ≠“BÄﬂ“BÄ·íPA¡Pˇ€PPÅQÇQPÉQPÑQ	PÖQ
≤@√
¢öAëÅQÇQ@QúOÄÔBÄ‡Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ·\ÄBÄ8\ÄBÄÍ,ÅMinimumÄBÄÏBÄÛBÄÌBÄÛBÄÓ\Ä•Ä`MBÄ˛làUsed to compute the minimum of the attribute values in a relation.BÄÙ\Ä\ÄBÄaBÄ˜lÅOperatorsÄBÄª\ÄBÄΩ\ÄÄ*ÏÅRelation name:BÄ¿BÄ~BÄ¡\ÄBÄ˛làName of the relation which contains the attribute to be minimumd.ÄBÄƒBÄ(BÄ˘
BÄ0BÄñBÄ*,ÉParameters for minimum:ÄBÄ˘BÄÌ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄˆBÄÏBÄÛBÄÌBÄÛBÄÓBÄ˜BÄ˛BÄ˘BÄÙBÄ˙BÄªBÄ˝BÄ˘BÄÌBÄÃÄ\ÄBÄ^BÄ‡ÉÇINSPECT-DBMS-OBJECTÄÄÎÄÜÄ@ÑFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ	\ÄBÄm√ÄOBJECTBÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛lÑInformation on any database objectÄBÄõëBÄ‹¿lÉ(INSPECT-DBMS-OBJECT '~S)Ä¿BÄﬂ“BÄ¶íP@¡Pˇ€PÅQö@ëÅQåOÄBÄ	Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ
\ÄBÄ8\ÄBÄÍ¨ÇInspect Dbms ObjectÄBÄÏBÄBÄÌBÄBÄÓ\Ä•ÄÜBÄ˛¨ÑUsed to inspect any database object.BÄÙBÄBÄª\ÄBÄΩ\ÄÄ*,ÇDatabase Object:BÄ¿BÄÒBÄ¡\ÄBÄ˛¨áSpecify a database object (COMMAND / RELATION / ATTRIBUTE).ÄBÄƒBÄ*ÏÉHelp on the database object ->BÄ˘BÄ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄBÄÏBÄBÄÌBÄBÄÓBÄ BÄ˛BÄ"BÄÙBÄBÄªBÄ#BÄ˘BÄBÄÃÄ\ÄBÄ^BÄ‡ÉÇCLEAR-OUTPUT-WINDOWÄÄÎÄÜÄ@DFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ/\ÄBÄmBÄ:BÄ:\ÄÄí\Äp¿BÄMlÇSHEET-LINE-HEIGHTÄp¿BÄM,ÉSHEET-BOTTOM-MARGIN-SIZEp¿BÄMÏÇSHEET-TOP-MARGIN-SIZEÄp¿BÄM¨ÅSHEET-HEIGHTp¿BÄM¨ÇSHEET-INSIDE-HEIGHTÄp¿BÄM¨ÉSHEET-NUMBER-OF-INSIDE-LINESBÄïBÄoBÄ‡BÄ˛ÏÉClear the entire output windowÄBÄõëiÅSET-ITEMSÄ¿iÅSCROLL-TOÄ¿)ÅRELATIVEÄPˇ€êP@¡PJPéNPíNˇcPìNˇcPõNhCˇcP@ùOÄKBÄ/Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ0\ÄBÄ8\ÄBÄÍ¨ÇClear Output WindowÄBÄÏBÄOBÄÌBÄOBÄÓ\ÄeÄåBÄÙBÄSBÄ˘BÄG\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄRBÄÏBÄOBÄÌBÄOBÄÓBÄSBÄÙBÄSBÄ˘BÄGBÄÃÄ\ÄBÄ^BÄ‡√ÅSCROLL-FORWARDÄÎÄ	ÜÄ@DFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ[\ÄBÄmBÄ:BÄ:\ÄÄí\ÄÄ<BÄ>BÄ@BÄBBÄDBÄFBÄoBÄ‡BÄ˛ÏÑscrolling forward in the output-windowÄBÄõëBÄI¿BÄJÄP@¡PPéNPíNˇcPìNˇcPõNhC˛GP@ùOÄhBÄ[Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ\\ÄBÄ8\ÄBÄÍÏÅScroll ForwardBÄÏBÄlBÄÌBÄlBÄÓ\Ä•ÄVBÄÙBÄSBÄ˘BÄg\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄoBÄÏBÄlBÄÌBÄlBÄÓBÄpBÄÙBÄSBÄ˘BÄgBÄÃÄ\ÄBÄ^BÄ‡ÇSCROLL-BACKWARDÄÄÎÄ	ÜÄ@DFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄx\ÄBÄmBÄ:BÄ:\ÄÄí\ÄÄ<BÄ>BÄ@BÄBBÄDBÄFBÄoBÄ‡BÄ˛,Öscrolling backward in the output-windowÄÄBÄõëBÄI¿BÄJÄP@¡PJPéNPíNˇcPìNˇcPõNhCˇcP@ùOÄÖBÄxÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄy\ÄBÄ8\ÄBÄÍ,ÇScroll BackwardÄBÄÏBÄ BÄÌBÄ BÄÓ\Ä•ÄVBÄÙBÄSBÄ˘BÄÑ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄåBÄÏBÄ BÄÌBÄ BÄÓBÄ
BÄÙBÄSBÄ˘BÄÑBÄÃÄ\ÄBÄ^BÄ‡√ÅSCROLL-TO-TOPÄÄÎÄÜÄ@DFÄ
¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄï\ÄBÄmBÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛lÖscrolling to the top in the output-windowÄÄBÄõëiÇPUT-ITEM-IN-WINDOW¿ÈÅITEM-OF-NUMBERÄP@¡PPJí@ïOÄ§BÄïÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄñ\ÄBÄ8\ÄBÄÍÏÅScroll To TopÄBÄÏBÄ®BÄÌBÄ®BÄÓ\Ä•Ä<BÄÙBÄSBÄ˘BÄ°\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ´BÄÏBÄ®BÄÌBÄ®BÄÓBÄ¨BÄÙBÄSBÄ˘BÄ°BÄÃÄ\ÄBÄ^BÄ‡ÇSCROLL-TO-BOTTOMÄÎÄFÄ@FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ¥\ÄBÄmBÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛¨Öscrolling to the bottom in the output-windowÄBÄõë)ÉPUT-LAST-ITEM-IN-WINDOWÄÄPåOÄ¬BÄ¥Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄµ\ÄBÄ8\ÄBÄÍ,ÇScroll To BottomBÄÏBÄ∆BÄÌBÄ∆BÄÓ\Ä•Ä>BÄÙBÄSBÄ˘BÄ¿\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ…BÄÏBÄ∆BÄÌBÄ∆BÄÓBÄ BÄÙBÄSBÄ˘BÄ¿BÄÃÄ\ÄBÄ^BÄ‡ÉÇSCROLL-TO-A-RELATIONÄÎÄ"ÜÄ@àFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ“\ÄBÄmBÄ†BÄ:\Ä√ÄINDEXÄBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛,ÑScroll to a particular relationÄÄÇ*TYPEOUT-WINDOW*—BÄõëÈÄINDEXÄ¿ÉÄGETP“ÈÄITEMSÄ¿BÄ¢¿BÄ£¿ÏÖ~%The relation ~S is not in the output-windowÄ¿BÄﬂíÅQPí@¡‰@QPääCx‰PA¡P	P@QíAïP
PÅQúOÄÊBÄ“Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ”\ÄBÄ8\ÄBÄÍ¨ÇScroll To A RelationBÄÏBÄÍBÄÌBÄÍBÄÓ\Ä•ÄRBÄÙBÄSBÄª\ÄBÄΩ\ÄÄ*ÏÅRelation Name:BÄ¿BÄ~BÄ¡\ÄBÄ˛lÑName of the relation to scroll to:BÄƒBÄ*lÉScroll to the relation ==>BÄ˘BÄ‡\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄÌBÄÏBÄÍBÄÌBÄÍBÄÓBÄÓBÄÙBÄSBÄªBÄBÄ˘BÄ‡BÄÃÄ\ÄBÄ^BÄ‡ÉÇSEND-OUTPUT-TO-FILEÄÄÎÄ:éÜÄ@†FÄT¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ¸\ÄBÄmÉÄFILEBÄ:\ÄBÄªBÄ:BÄ:BÄ:BÄ:ÉÄLINEBÄ~ÅELEMENTÄ\ÄÄí\Ä	BÄï™ÄPROGp¿¨ÄZLCÄ,ÅDO-NAMEDp¿BÄuÏÇINHIBIT-STYLE-WARNINGSp¿BÄulÇCONDITION-BIND-IFÄp¿BÄuÏÅCONDITION-BINDp¿BÄuÏÇCATCH-CONTINUATION-IFÄp¿BÄulÇCATCH-CONTINUATIONp¿BÄuÏÄERRSETBÄoBÄ‡BÄ˛lÜSend the contents of the output window to a file.ÄÄp¿lÄEH¨Ç*CONDITION-HANDLERS*—BÄõ—BÄ·ëFÄZ¿p¿,ÄÏÄG2586Ä¿FÄW¿ÍÄERRORÄ¿p¿BÄ¯ÏÅERRSET-HANDLER¿iÅCHARACTERS¿iÅDIRECTIONÄ¿ÈÄOUTPUT¿iÇIF-DOES-NOT-EXISTÄ¿ÈÄCREATE¿™ÄOPEN“BÄ≠“lÇ~S is a bad file.Ä¿BÄﬂ“BÄ‰¿p¿BÄulÅLISTARRAYÄ“BÄõ“ÈÄITEM1Ä¿ÍÄTERPRI“ÍÄCLOSEÄíˇ›PJUPPT	P
PPˇ€JCA√PJCB√÷ÅQ@√Pˇ›PPPPJ∫äJ!BJ!B\B@¡]_ZD¸@ÊPPÅQò<¸B€B—PääD¡C¡2¸CQDSE¡F€E7ÊE1ÊEÚEQ@Qê¸E5‰EQGœ‰G7ÊG1ÊGÚGQ¸G5ÙÂGS&ÊGS¸GWF¡F5‰FS¸FQ@QêÂ˝@QäCC√¡D≈DÃÁ@‰@QåROÄ4BÄ¸Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ˝\ÄBÄ8\ÄBÄÍ¨ÇSend Output To FileÄBÄÏBÄ8BÄÌBÄ8BÄÓ\Ä•Ä@FBÄÙBÄSBÄª\ÄBÄΩ\ÄÄ*lÅFile name:BÄ¿BÄñBÄ¡\ÄBÄ˛,ÖName of the file to send the output to:ÄBÄƒBÄ*¨ÑSend the output window contents to:ÄBÄ˘BÄ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ;BÄÏBÄ8BÄÌBÄ8BÄÓBÄ<BÄÙBÄSBÄªBÄ>BÄ˘BÄBÄÃÄ\ÄBÄ^BÄ‡ÉÅINTRODUCTIONÄÎÄFÄ@FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄJ\ÄBÄmBÄ:BÄ:\ÄBÄoBÄ‡BÄ˛,ÑIntroduction to this interface.ÄÄBÄíÑOÄVBÄJÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄK\ÄBÄ8\ÄBÄÍ¨ÅIntroductionBÄÏBÄZBÄÌBÄZBÄÓ\Ä•ÄÜBÄÙBÄBÄ˘BÄU\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ]BÄÏBÄZBÄÌBÄZBÄÓBÄ^BÄÙBÄBÄ˘BÄUBÄÃÄ\ÄBÄ^BÄ‡BÄÄÎÄÜÄ@DFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄf\ÄBÄmBÄ:\ÄÅCOMMANDÄ\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛¨ãIntroduction to the interface. Help on any database object (COMMAND / RELATION / ATTRIBUTE).Äp¿BÄu¨ÄSELF—√Å*HELP-SUBMENU*ëÈÅSUBMENU-CHOOSE¿)ÅEXECUTEÄÄPPÃCˇì@¡‰PP@ïROÄyBÄfÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ\ÄBÄ8\ÄBÄÍ¨ÄHelpBÄÏBÄ}BÄÌBÄ}BÄÙBÄjBÄ˛,ãIntroduction to the interface. Help on any database object (COMMAND/RELATION/ATTRIBUTE).BÄ˘BÄs\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄÄBÄÏBÄ}BÄÌBÄ}BÄÙBÄjBÄ˛BÄÅBÄ˘BÄsBÄÃÄ\ÄBÄ^BÄ‡BÄaÄÎÄÜÄ@DFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄà\ÄBÄmBÄ:\ÄBÄp\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛,êSelect a database command from a menu. A choose-variable-values window will be presented to get the arguments for that command.ÄÄBÄu—CÇ*COMMAND-SUBMENU*ÄëBÄw¿BÄxÄPPÃCˇì@¡‰PP@ïROÄñBÄàÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄa\ÄBÄ8\ÄBÄÍ¨ÅCommand MenuBÄÏBÄöBÄÌBÄöBÄÓ\ÄBÄ¡BÄÙBÄjBÄ˛,êSelect a database command from a menu. A choose-variable-values window will be presented to get the arguments for that command.ÄBÄ˘BÄî\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄùBÄÏBÄöBÄÌBÄöBÄÓBÄûBÄÙBÄjBÄ˛BÄüBÄ˘BÄîBÄÃÄ\ÄBÄ^BÄ‡BÄSÄÎÄÜÄ@DFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ¶\ÄBÄmBÄ:\ÄBÄp\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛láSelect an item from a menu to scroll in the output window.ÄBÄu—CÇ*DISPLAY-SUBMENU*ÄëBÄw¿BÄxÄPPÃCˇì@¡‰PP@ïROÄ¥BÄ¶Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄS\ÄBÄ8\ÄBÄÍ,ÅDisplayÄBÄÏBÄ∏BÄÌBÄ∏BÄÙBÄjBÄ˛láSelect an item from a menu to scroll in the output window.BÄ˘BÄ≤\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄªBÄÏBÄ∏BÄÌBÄ∏BÄÙBÄjBÄ˛BÄºBÄ˘BÄ≤BÄÃÄ\ÄBÄ^BÄ‡ÉÄKILLÄÎÄ	FÄ@FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ√\ÄBÄmBÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏÖTo exit the interface by killing the process.ÄÄBÄŒë©ÄKILLÄPà⁄ROÄ—BÄ√Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄƒ\ÄBÄ8\ÄBÄÍ¨ÄKillBÄÏBÄ’BÄÌBÄ’BÄÓ\Ä•Ä îBÄÙBÄjBÄ˛ÏÖTo exit the interface by killing the process.ÄBÄ˘BÄœ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄÿBÄÏBÄ’BÄÌBÄ’BÄÓBÄŸBÄÙBÄjBÄ˛BÄ€BÄ˘BÄœBÄÃÄ\ÄBÄ^BÄ‡ÉÄEXITÄÎÄFÄ@FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ‚\ÄBÄmBÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛¨ÑTo exit the interface by burying it.ÄBÄŒë©ÄBURYÄPåOÄBÄ‚Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ„\ÄBÄ8\ÄBÄÍ¨ÄExitBÄÏBÄÙBÄÌBÄÙBÄÓ\ÄeÄîBÄÙBÄjBÄ˛¨ÑTo exit the interface by burying it.BÄ˘BÄÓ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ˜BÄÏBÄÙBÄÌBÄÙBÄÓBÄ¯BÄÙBÄjBÄ˛BÄ˙BÄ˘BÄÓBÄÃ1Ä\Äp¿BÄuÏÅMAKE-INSTANCEÄ\ÄBÄ8p¿BÄUÏÅCOMMAND-TABLEÄ©ÄNAMEÏÇDatabase command tableBÄ˛ÏÅdatabase helpÄNÄ
BÄˆ1Ä\Äp¿BÄu¨ÇBUILD-COMMAND-TABLEÄ\ÄBÄ8BÄˆ\ÄBÄ8BÄ‡\ÄBÄ8\ÄLBÄBÄaBÄSBÄƒBÄ„BÄBÄIBÄ"BÄBÄPBÄxBÄ†BÄÃBÄÙBÄ„BÄ{BÄBÄé	BÄ	BÄjBÄY	BÄ”
BÄzBÄÿBÄ™BÄÁBÄ8BÄhBÄGBÄ¥BÄBÄ<BÄéBÄeBÄ∏BÄ·BÄs
BÄ¨BÄG
BÄ¶
BÄÁ	BÄ˜ÄÆBÄ€BÄIBÄ%BÄîBÄxBÄØBÄÆBÄ	Ä‹BÄÎBÄEBÄ5ÄéÄbÄ≈Ä–BÄqBÄÚBÄ#BÄNBÄBÄBÄ)BÄMBÄ
BÄKBÄ\BÄ0BÄñBÄµBÄyBÄ”BÄ˝Ä\ÄBÄ^BÄ‡BÄ“©ÄINITÄÎÄ[ÜÄ`DFÄ<¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ\ÄBÄmBÄ BÄèBÄ:\ÄBÄè\ÄBÄí\ÄBÄïBÄoBÄ‡ÄBÄõ—√Å*INTERACTION*Ä—CÅ*MENUPANE*—BÄ≥—BÄï—BÄa—BÄv—p¿BÄM¨ÅMOUSE-SHEETÄëBÄ@¿ÈÄPOP-UP¿BÄF¿)ÅSUPERIOR¿iÇITEM-LIST-POINTERÄ¿BÄ¿BÄ“©ÅMULTICOLUMNÄ¿)ÇCOLUMN-SPEC-LIST¿BÄS¿)ÅGET-PANE¿BÄÚ¿ÈÇSET-ITEM-LIST-POINTERÄ¿BÄj¿)ÇUPDATE-ITEM-LIST¿BÄA¿BÄ¿)ÉSET-SELECTION-SUBSTITUTEÄPPˇ›Pˇ›P
PPP	J∫	¿PPˇ›Pˇ›Pˇ›P
PPPJ∫¿PPˇ›Pˇ›P
PPP	J∫¿PPÃCˇì¿PPêPàPPÃCˇì¿PPÃCˇì¿PPÃCˇïOÄ-BÄÄ1Ä\Äp¿BÄulÅBUILD-MENU\ÄBÄ8BÄj\ÄBÄ8BÄ‡)ÇITEM-LIST-ORDERÄ\ÄBÄ8\ÄBÄBÄƒBÄaBÄ„BÄS1Ä\ÄBÄ0\ÄBÄ8BÄ\ÄBÄ8BÄ‡BÄ3\ÄBÄ8\ÄBÄKBÄ
1Ä\ÄBÄ0\ÄBÄ8BÄS\ÄBÄ8BÄ‡BÄ3\ÄBÄ8\Ä	BÄñBÄyBÄ0BÄ˝BÄ”p¿BÄUÏÇDISPLAY-COMMAND-TABLESp¿BÄU¨ÇEDIT-COMMAND-TABLESÄBÄ\BÄµ1Ä\ÄBÄQ\ÄBÄ8BÄa\ÄBÄ8\ÄBÄˆ\ÄBÄ8BÄW1Ä\ÄBÄ0\ÄBÄ8BÄa\ÄBÄ8BÄ‡BÄ3\ÄBÄ8\Ä>BÄ¨BÄG
BÄs
BÄ¶
BÄ%BÄÁ	BÄ€BÄIBÄ˜ÄÆBÄîBÄxBÄÆBÄ	Ä‹BÄØBÄBÄ”
BÄ„BÄé	BÄ	BÄjBÄBÄ{BÄY	BÄ"BÄBÄIBÄPBÄxBÄ†BÄÃBÄÙBÄ8BÄÁBÄzBÄÿBÄ™BÄhBÄGBÄ¥BÄBÄ<BÄeBÄéBÄ∏BÄ·BÄEBÄ5ÄéÄbÄ≈ÄÎBÄBÄ–BÄqBÄÚBÄ#BÄNBÄBÄ)BÄMiÇCOLUMN-LIST-ORDERÄ\ÄBÄ8\Ä\ÄlÅDefinitionBÄ/p¿BÄ0ÏÄHL12BI\Ä¨ÅManipulationBÄ/BÄX\ÄlÅOperatorsÄBÄ/BÄX\ÄÏÅOther FeaturesBÄ/BÄXBÄ:NÄ:BÄŒÄ\ÄBÄ^BÄ‡BÄwÄÎÄ1ÜÄ@ÑFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ`\ÄBÄmÅSUBMENUÄBÄ:\ÄÉÄSUPÄ\ÄBÄí\ÄBÄïBÄoBÄ‡ÄBÄ#—BÄ ëBÄ%¿FÄ.¿ÈÅSET-VISIBILITY¿©ÅBLINKER-LIST¿©ÅSET-SUPERIOR¿ÈÄCHOOSE¿FÄ0¿ÈÄBLINKÄÄPÅã@¡ˇ›Pˇ€UPˇ€PäBˇë	PPÅë
PPÅQA]3ZP	P@QÅëPPPäBˇëOÄuBÄ`ÄÄ\ÄBÄ^BÄ‡BÄ¿©ÅCOMMAND-LOOPÄÎÄ
ÜÄ@
DFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄv\ÄBÄmBÄ:BÄ:\ÄBÄí\ÄBÄïBÄoBÄ‡ÄBÄ —BÄ·—BÄõëBÄ{¿©ÅCLEAR-SCREEN¿ÈÅSET-IO-BUFFERÄ¿iÅIO-BUFFERÄÄPä¿PàPàP@¡P	Pä@ïOÄÖBÄvÄÄ\ÄBÄ^BÄ‡BÄ¿iÇFETCH-AND-EXECUTEÄÄÎÄ	ÜÄ`DFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄÜ\ÄBÄmBÄ BÄèBÄ:\ÄBÄè\ÄBÄí\ÄBÄïBÄoBÄ‡ÄBÄõ—CÄCH—BÄŒ—p¿BÄMlÑ*REMOVE-TYPEOUT-STANDARD-MESSAGE*Ä—BÄ·ë)ÅACTIVE-P¿lÄ~%¿BÄﬂ“)ÅANY-TYIÄ¿ÈÅFLUSH-TYPEOUTÄÄPà‰P	P
êP	P
êPP
êPä¿PåROÄöBÄÜÄÄBÄ
ÄÎÄ(ÜÄ@HFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ
\ÄCÄXÄBÄ:\ÄBÄìBÄ:\ÄBÄí\ÄBÄïÄBÄõ—BÄŒ—BÄï—BÄ·ëBÄñ¿lÄ~%¿BÄﬂ“BÄò¿BÄô¿BÄ‹¿lÄ~SÄPà‰PP	êPP	êPP	ê
Pä@¡PàPA¡Pˇ€PÄQ	öAïOÄ©BÄ
ÄÄ\ÄBÄ^BÄ‡BÄ¿)ÇEXECUTE-COMMANDÄÄÎÄÜ¿üı∑FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ™\ÄBÄmBÄ BÄèBÄ:\ÄBÄèBÄ:\ÄBÄí\ÄBÄïÄBÄ‡ÄjÄ-Ä—BÄõëBÄ
¿BÄ∞¿BÄ‹¿lÄ~S¿BÄﬂíP&‰PA¡Pˇ€	PP
öAïROÄπBÄ™ÄÄ\ÄBÄ^BÄ‡BÄ“BÄ´ÄÎÄ	ÜÄ`HFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ∫\ÄBÄmBÄ BÄèBÄ:\ÄBÄèBÄì\ÄBÄí\ÄBÄïBÄoBÄ‡ÄBÄõ—BÄŒ—BÄï—BÄ·ëBÄñ¿lÄ~%¿BÄﬂ“BÄò¿BÄô¿\Ä™ÄMAPC\ÄBÄ\ÄBÄ\ÄÉÄVALÄ\ÄjÄIFBÄÃ\ÄÍÄPROGNÄ\ÄBÄïBÄõBÄ‹\ÄBÄﬂBÄ:lÄ~SBÄÃ\ÄBÄïBÄõBÄ¡jÄ//ÄPà‰PP	êPP	êPP	ê
PäA¡PàOÄ÷BÄ∫ÄÄ\ÄBÄ^BÄ‡©ÇDESIGNATE-IO-STREAMSÄÎÄ	FÄ	@FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ◊\ÄBÄmBÄ:BÄ:\ÄBÄoBÄ‡Äp¿BÄu,ÅDEBUG-IO—ÉÅERROR-OUTPUT—BÄ·—jÇ*STANDARD-OUTPUT*Ä—BÄÖ—BÄ ëP¿P¿P¿P¬ˇOÄÊBÄ◊ÄÄ\ÄBÄ^BÄ‡p¿BÄ¯,ÇFASLOAD-COMBINEDBÄ´ÄÎÄ
	Ü¿üˆ˜FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄj\ÄBÄ^BÄ‡)ÅCOMBINEDBÄ´\ÄBÄ p¿BÄ¯¨Ç.DAEMON-CALLER-ARGS.BÄ:\ÄBÄıp¿BÄ¯ÏÇ.DAEMON-MAPPING-TABLE.\ÄÄí\Äp¿BÄ¯¨ÇMETHOD-MAPPING-TABLEp¿BÄ¯¨ÑCOMPILE-TIME-REMEMBER-MAPPING-TABLEÄ)ÇFUNCTION-PARENTÄ\ÄBÄ‡p¿BÄuÏÇCOMPILE-FLAVOR-METHODSp¿BÄ¯lÉCOMBINED-METHOD-DERIVATION\ÄÄ´BÄ:BÄ:\ÄBÄ“\ÄBÄ^BÄ‡BÄ“BÄ´\ÄBÄ¿\ÄBÄ^BÄ‡BÄ¿BÄ´\ÄBÄ:\ÄBÄ^p¿BÄUlÇBASIC-COMMAND-LOOPBÄ´ÄBÄ‡Äp¿BÄ¯lÇSELF-MAPPING-TABLEë1Ä\Äp¿BÄu¨ÇFDEFINITION-LOCATION\ÄBÄ8BÄ	–1Ä]ÄBÄà]ÄBÄ8]ÄBÄ‡BÄÃBÄ¿FÄ¡0¿1Ä\ÄBÄ\ÄBÄ8BÄ–1Ä\ÄBÄ\ÄBÄ8BÄêPA¡@QAQ¡JP@@QPPPA@QAQ¡J	P@POÄ BÄÁÄÄ\ÄBÄ^BÄ‡BÄÈBÄáÄÎÄÜ¿ü¯˜FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄj\ÄBÄ^BÄ‡BÄÚBÄáBÄÛBÄ:\ÄBÄıBÄ¯\ÄÄí\ÄBÄ¸BÄ˛BÄˇBÄBÄ\ÄBÄáBÄ:BÄ:\ÄBÄ¿\ÄBÄ^BÄ‡BÄ¿BÄá\ÄBÄ:\ÄBÄ^BÄBÄáÄBÄ‡ÄBÄë1Ä\ÄBÄ\ÄBÄ8BÄ/–BÄ¿1Ä\ÄBÄ\ÄBÄ8BÄ1êPA¡@QAQ¡JP@@QP¡JPBOÄ8BÄ!ÄÄ\ÄBÄ^BÄ‡BÄÈBÄwÄÎÄÜ¿ü¯˜FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄj\ÄBÄ^BÄ‡BÄÚBÄwBÄÛBÄ:\ÄBÄıBÄ¯\ÄÄí\ÄBÄ¸BÄ˛BÄˇBÄBÄ\ÄBÄwBÄ:BÄ:\ÄBÄ¿\ÄBÄ^BÄ‡BÄ¿BÄw\ÄBÄ:\ÄBÄ^BÄ‰BÄwÄBÄ‡ÄBÄë1Ä\ÄBÄ\ÄBÄ8BÄG–1Ä]ÄBÄà]ÄBÄ8]ÄBÄ‡BÄÃBÄ‰¿1Ä\ÄBÄ\ÄBÄ8BÄIêPA¡@QAQ¡JP@@QP¡JPBOÄTBÄ9ÄÄ\ÄBÄ^BÄ‡BÄÈBÄÄÎÄÜ¿ü¯˜FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄj\ÄBÄ^BÄ‡BÄÚBÄBÄÛBÄ:\ÄBÄıBÄ¯\Ä
BÄí\ÄBÄ¸BÄ˛©ÇINTERNAL-FEF-OFFSETS\ÄFÄiÇINTERNAL-FEF-NAMES\Äp¿BÄ¯¨ÅCONTINUATIONBÄˇBÄBÄ\ÄBÄBÄ:BÄ:\ÄBÄ“\ÄBÄ^p¿BÄM¨ÇESSENTIAL-SET-EDGESÄBÄ“BÄ\ÄBÄ^p¿BÄMÏÇBASIC-CONSTRAINT-FRAMEBÄ“BÄ\ÄBÄ^p¿BÄMÏÅPROCESS-MIXINÄBÄ“BÄ\ÄBÄ^BÄBÄ“BÄ\ÄBÄ^BÄ‰BÄ“BÄ\ÄBÄ^BÄ‡BÄ“BÄ\ÄBÄ¿\ÄBÄ^p¿BÄM,ÇESSENTIAL-WINDOWBÄ¿BÄ\ÄBÄ^BÄlBÄ¿BÄ\ÄBÄ^p¿BÄMÏÑCONSTRAINT-FRAME-WITH-SHARED-IO-BUFFERBÄ¿BÄ\ÄBÄ^BÄÊBÄ¿BÄ\ÄÈÅINVERSE-AROUND\ÄBÄ^p¿BÄMÏÄSHEETÄBÄÄBÄ\ÄBÄ:\ÄBÄ^BÄÉBÄÄBÄ‡ÄBÄë\Ä)ÅINTERNALBÄUBÄg¿1Ä]ÄBÄà]ÄBÄ8]ÄBÄ‡BÄÃBÄÉ¿1Ä\ÄBÄ\ÄBÄ8BÄÅêPA¡@SPAQ@Q@UP≈JPBOÄèBÄUÄÄBÄÜÄÎÄTÜ¿üÊ˜FÄ7¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄj\ÄBÄáBÄ]BÄgBÄÛBÄ:\ÄBÄıBÄ¯\ÄBÄí\ÄBÄ¸BÄ˛ÄBÄ‡ÄBÄë1Ä]ÄBÄà]ÄBÄ8]ÄBÄ‡BÄÃBÄÊ¿1Ä\ÄBÄ\ÄBÄ8BÄ~–1Ä]ÄBÄà]ÄBÄ8]ÄBÄ‡BÄÃBÄ}¿1Ä\ÄBÄ\ÄBÄ8BÄ{–1Ä]ÄBÄà]ÄBÄ8]ÄBÄ‡BÄÃBÄl¿1Ä\ÄBÄ\ÄBÄ8BÄz–1Ä]ÄBÄà]ÄBÄ8]ÄBÄ‡BÄÃBÄy¿1Ä\ÄBÄ\ÄBÄ8BÄw–BÄã¿FÄ¡0¿1Ä\ÄBÄ\ÄBÄ8BÄÖ–1Ä\ÄBÄ\ÄBÄ8BÄj–1Ä]ÄBÄà]ÄBÄ8]ÄBÄ‡BÄÃBÄo¿1Ä\ÄBÄ\ÄBÄ8BÄm–1Ä]ÄBÄà]ÄBÄ8]ÄBÄ‡BÄÃBÄr¿1Ä\ÄBÄ\ÄBÄ8BÄp–BÄ¿1Ä\ÄBÄ\ÄBÄ8BÄs–BÄP¿1Ä\ÄBÄ\ÄBÄ8BÄt–1Ä\ÄBÄ\ÄBÄ8BÄuêPA¡@QP¡JP@@QP¡JP@@Q	P¡J
P@@QP¡JP@@QPPPA@Q	P¡JP@@QP¡JP@@QP¡JP@@QP¡JP@@QP¡JP@@QAQ¡JP@POÄ’BÄÜÄÄ\ÄBÄ^BÄ‡BÄÈ©ÄSETÄÄÎÄvTÜ¿üä˜FÄ ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄj\ÄBÄ^BÄ‡BÄÚBÄ◊BÄÛBÄ:\ÄBÄıBÄ¯\ÄÄí\ÄBÄ¸BÄñBÄ˛BÄˇBÄBÄ\ÄBÄ◊©ÄCASE)ÇBASE-FLAVOR-LAST\Ä BÄ‰\ÄBÄ^BÄÉBÄ‰BÄ◊BÄÄ\ÄBÄ^BÄÉBÄ‰BÄ◊iÅCHAR-ALUFÄ\ÄBÄ^BÄÉBÄ‰BÄ◊iÅERASE-ALUF\ÄBÄ^BÄoBÄ‰BÄ◊ÈÅEXPOSED-PANESÄ\ÄBÄ^BÄÊBÄ‰BÄ◊ÈÅOLD-TYPEAHEADÄ\ÄBÄ^BÄBÄ‰BÄ◊BÄ\ÄBÄ^BÄBÄ‰BÄ◊)ÅTUTORIAL\ÄBÄ^BÄBÄ‰BÄ◊BÄÛ\ÄBÄ^BÄBÄ‰BÄ◊BÄ˜\ÄBÄ^BÄBÄ‰BÄ◊ÈÇSYSTEM-COMMAND-TABLESÄ\ÄBÄ^BÄBÄ‰BÄ◊ÈÇSPECIAL-COMMAND-TABLES\ÄBÄ^BÄBÄ‰BÄ◊BÄÓ\ÄBÄ^BÄBÄ‰BÄ◊iÅKBD-INPUTÄ\ÄBÄ^BÄBÄ‰BÄ◊)ÇINPUT-MECHANISMÄ\ÄBÄ^BÄBÄ‰BÄ◊ÈÅCOMMAND-ENTRYÄ\ÄBÄ^BÄBÄ‰BÄ◊)ÇCOMMAND-HISTORYÄ\ÄBÄ^BÄBÄ‰BÄ◊©ÇMAX-COMMAND-HISTORYÄ\ÄBÄ^BÄBÄ‰BÄ◊)ÉCOMMAND-EXECUTION-QUEUEÄ\ÄBÄ^BÄBÄ‰BÄ◊)ÇNUMERIC-ARGUMENT\ÄBÄ^BÄBÄ‰BÄ◊iÅBLIP-ALIST\ÄBÄ^BÄBÄ‰BÄ◊BÄ˙\ÄBÄ^BÄBÄ‰BÄ◊©ÅTYPEIN-MODES\ÄBÄ^BÄBÄ‰BÄ◊ÈÅREAD-FUNCTIONÄ\ÄBÄ^BÄBÄ‰BÄ◊ÈÄPROMPT\ÄBÄ^BÄBÄ‰BÄ◊iÅREAD-TYPEÄ\ÄBÄ^BÄBÄ‰BÄ◊ÈÅERROR-MESSAGEÄ\ÄBÄ^BÄBÄ‰BÄ◊BÄ\ÄBÄ^BÄBÄ‰BÄ◊BÄ\ÄBÄ^BÄBÄ‰BÄ◊ÈÇINHIBIT-RESULTS-PRINT?\ÄBÄ^BÄBÄ‰BÄ◊ÈÅOUTPUT-HISTORY\ÄBÄ^BÄBÄ‰BÄ◊iÇMAX-OUTPUT-HISTORYÄBÄ‡ÄBÄëBÄ¿1Ä\ÄBÄ\ÄBÄ8BÄ–1Ä\ÄBÄ\ÄBÄ8BÄ–1Ä\ÄBÄ\ÄBÄ8BÄ–1Ä\ÄBÄ\ÄBÄ8BÄ–1Ä\ÄBÄ\ÄBÄ8BÄ–1Ä\ÄBÄ\ÄBÄ8BÄ–1Ä\ÄBÄ\ÄBÄ8BÄ–1Ä\ÄBÄ\ÄBÄ8BÄ–1Ä\ÄBÄ\ÄBÄ8BÄ–1Ä\ÄBÄ\ÄBÄ8BÄ–1Ä\ÄBÄ\ÄBÄ8BÄ
–1Ä\ÄBÄ\ÄBÄ8BÄ–1Ä\ÄBÄ\ÄBÄ8BÄ–1Ä\ÄBÄ\ÄBÄ8BÄ–1Ä\ÄBÄ\ÄBÄ8BÄ–1Ä\ÄBÄ\ÄBÄ8BÄ–1Ä\ÄBÄ\ÄBÄ8BÄ˛–1Ä\ÄBÄ\ÄBÄ8BÄ¸–1Ä\ÄBÄ\ÄBÄ8BÄ˙–1Ä\ÄBÄ\ÄBÄ8BÄ˘–1Ä\ÄBÄ\ÄBÄ8BÄ˜–1Ä\ÄBÄ\ÄBÄ8BÄı–1Ä\ÄBÄ\ÄBÄ8BÄÙ–1Ä\ÄBÄ\ÄBÄ8BÄÛ–1Ä\ÄBÄ\ÄBÄ8BÄÒ–1Ä\ÄBÄ\ÄBÄ8BÄ–BÄû¿1Ä\ÄBÄ\ÄBÄ8BÄÓ–BÄ¡¿1Ä\ÄBÄ\ÄBÄ8BÄÏ–BÄã¿1Ä\ÄBÄ\ÄBÄ8BÄÍ–1Ä\ÄBÄ\ÄBÄ8BÄË–1Ä\ÄBÄ\ÄBÄ8BÄÁ–BÄ‡¿BÄ◊¿\ÄBÄBÄBÄBÄBÄBÄBÄBÄBÄBÄBÄ
BÄBÄBÄBÄBÄBÄ˛BÄ¸BÄ˙BÄ˘BÄ˜BÄıBÄÙBÄÛBÄÒBÄBÄÓBÄÏBÄÍBÄËBÄÁ¿p¿BÄ¯¨ÉCASE-METHOD-DEFAULT-HANDLERÄ“\ÄBÄBÄBÄBÄBÄBÄBÄBÄBÄBÄBÄ˙BÄ	BÄBÄBÄBÄBÄˇBÄ˝BÄ˚BÄÓBÄ¯BÄˆBÄ˜BÄÛBÄÚBÄBÄÔBÄÌBÄÎBÄÈBÄÄÄFÄ#¿BÄ¿BÄ¿BÄ¿BÄ¿BÄ¿BÄ¿BÄ¿BÄ¿BÄ¿BÄ¿BÄ˙¿BÄ	¿BÄ¿BÄ¿BÄ¿BÄ¿BÄˇ¿BÄ˝¿BÄ˚¿BÄÓ¿BÄ¯¿BÄˆ¿BÄ˜¿BÄÛ¿BÄÚ¿BÄ¿BÄÔ¿BÄÌ¿BÄÎ¿BÄÈ¿BÄÄ¿)ÇGET-HANDLER-FORÄ¿©ÇOPERATION-HANDLED-PÄ¿iÇCASE-DOCUMENTATION¿)ÇWHICH-OPERATIONS¿FÄ"¿FÄí¿FÄ¿FÄı¿FÄ˙¿FÄˇ¿FÄ¿FÄ	¿FÄ¿FÄ¿FÄ¿FÄ¿FÄ"¿FÄ'¿FÄ,¿FÄ1¿FÄ6¿FÄ;¿FÄ@¿FÄE¿FÄJ¿FÄO¿FÄT¿FÄY¿FÄ^¿FÄc¿FÄh¿FÄm¿FÄr¿FÄw¿FÄ|¿FÄÅ¿FÄÜ¿FÄã¿FÄã¿FÄã¿FÄëÄPA¡@W-r@QP¡JPB@QP¡JPB@QP¡JPB@QP¡J	PB@QP¡J
PB@QP¡JPB@QP¡JPB@QP¡JPB@QP¡JPB@QP¡JPB@QP¡JPB@QP¡JPB@QP¡JPB@QP¡JPB@QP¡JPB@QP¡JPB@QP¡JPB@QP¡JPB@QP¡JPB@QP¡JPB@QP¡JPB@QP¡JPB@QP¡JPB@QP¡JPB@QP¡JPB@QP¡JPB@Q P¡J!PB@Q"P¡J#PB@Q$P¡J%PB@Q$P¡J&PB@Q$P¡J'PB(P)P*P@W@[+¨,ROÄ®BÄ÷ÄÄ\ÄBÄ^BÄ‡BÄÈiÅPROCESSESÄÄÎÄ
Ü¿üˆ˜FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄj\ÄBÄ^BÄ‡BÄÚBÄ™BÄÛBÄ:\ÄBÄıBÄ¯\ÄÄí\ÄBÄ¸BÄ˛BÄˇBÄBÄ\ÄBÄ™ÈÄAPPENDBÄÂ\ÄBÄ:\ÄBÄ^BÄÉBÄ™\ÄBÄ^BÄrBÄ™ÄBÄ‡ÄBÄëBÄ»¿1Ä\ÄBÄ\ÄBÄ8BÄ∫–BÄã¿1Ä\ÄBÄ\ÄBÄ8BÄπ–p¿BÄ¯,Å*APPENDÄíPA¡@QP¡JPA@QP¡JPA	îOÄ√BÄ©ÄÄ\ÄBÄ^BÄ‡BÄÈBÄ¡ÄÎÄ*lÜ¿üËÁFÄB¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄj\ÄBÄ^BÄ‡BÄÚBÄ¡BÄÛBÄ:\ÄÄıBÄ¯BÄ:p¿BÄM¨Å.QUEUE-LEFT.p¿BÄM¨ÉOLD-SCREEN-MANAGER-TOP-LEVELp¿BÄMlÄEÄ\ÄBÄí\Ä
BÄÑp¿BÄu¨ÄNEQÄBÄ¸BÄ˛BÄï™ÄWHENp¿BÄMlÉDELAYING-SCREEN-MANAGEMENTp¿BÄulÅLEXPR-SENDp¿BÄulÇDESTRUCTURING-BINDp¿BÄ¯lÅMACROCALLÄBÄˇBÄp¿BÄ¯,ÇWRAPPER-SXHASHES\Ä\Ä\ÄBÄ^BÄy)ÅWRAPPERÄBÄ¡ÜÄÏ∆BÄ\ÄÄ¡BÄ:BÄ:\ÄBÄ¿\ÄBÄ^BÄÊBÄ¿BÄ¡\ÄBÄ^BÄrBÄ¿BÄ¡\ÄBÄ:\ÄBÄ^BÄyBÄ¿BÄ¡\ÄBÄÊBÄÂÄBÄ‡Äp¿BÄMlÉINHIBIT-SCREEN-MANAGEMENTÄ—p¿BÄM,ÉSCREEN-MANAGER-TOP-LEVEL—BÄ—BÄu—p¿BÄM¨ÇSCREEN-MANAGER-QUEUEë1Ä]ÄBÄà]ÄBÄ8]ÄBÄ‡p¿BÄM¨ÇSELECTION-SUBSTITUTE¿BÄ¡¿FÄn¿FÄc¿)ÇINFERIOR-SELECTÄ¿1Ä]ÄBÄà]ÄBÄ8]ÄBÄ‡p¿BÄM,ÅSUPERIOR¿BÄ»¿1Ä\ÄBÄ\ÄBÄ8BÄÎ–BÄû¿1Ä\ÄBÄ\ÄBÄ8BÄÍ–BÄ≥¿FÄ¡0¿1Ä\ÄBÄ\ÄBÄ8BÄÌ–p¿BÄMlÜSCREEN-MANAGE-DELAYING-SCREEN-MANAGEMENT-INTERNALÄ“p¿BÄM¨ÇSCREEN-MANAGE-QUEUEÄíPA¡@UB¡	‰
PBQBJ	PBC›ˇ›Pˇ€UPD¡‰⁄ˇ›Pˇ€U‘“PPí‡ÿ@QP¡JP@@QP¡JP@@QPPPA¸J]hZ¸ÄPC¡DQ¿]sZPCQ±Ê
‰CQEœ‰EQBEUBJP@˜˝OÄBÄƒÄÄ\ÄBÄ^BÄ‡BÄÈBÄœÄÎÄÜ¿ü˘˚FÄ	¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄj\ÄBÄ^BÄ‡BÄÚBÄœBÄÛBÄ:\ÄBÄı\Ä
BÄí\ÄBÄ‡BÄa\ÄFÄBÄˇBÄBÄ‚\Ä\Ä\ÄBÄ^BÄÉBÄÊBÄœÜÄrı}BÄ\ÄBÄœBÄ:BÄ:\ÄBÄ“\ÄBÄ^p¿BÄM,ÇESSENTIAL-EXPOSEBÄ“BÄœ\ÄBÄ¿\ÄBÄ^p¿BÄMlÇESSENTIAL-ACTIVATEBÄ¿BÄœ\ÄBÄ^BÄrBÄ¿BÄœ\ÄBÄ:\ÄBÄ^BÄÉBÄœ\ÄBÄÊBÄ#ÄBÄ‡ÄBÄë\ÄBÄáBÄÄ¿p¿BÄM¨ÅSHEET-EXPOSEí@QPîOÄ6BÄÄÄBÄ2ÄÎÄ&Ü¿øÚ∑FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄj\ÄBÄáBÄÄ\ÄBÄıBÄ:\ÄBÄ:BÄ¯\ÄBÄí\ÄBÄ¸BÄ˛ÄBÄ‡ÄBÄëBÄ»¿1Ä\ÄBÄ\ÄBÄ8BÄ.–1Ä]ÄBÄà]ÄBÄ8]ÄBÄ‡BÄÃBÄ-¿1Ä\ÄBÄ\ÄBÄ8BÄ+–BÄã¿FÄ¡0¿1Ä\ÄBÄ\ÄBÄ8BÄ0–1Ä]ÄBÄà]ÄBÄ8]ÄBÄ‡BÄÃBÄ)¿1Ä\ÄBÄ\ÄBÄ8BÄ'êPA¡ÿÄQP¡JP@ÄQP¡JP@ÄQ	P
PPAÄQP¡JP@POÄYBÄ2ÄÄ\ÄBÄ^BÄ‡BÄÈ)ÅDESELECTÄÎÄ$[Ü¿üÌÁFÄ7¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄj\ÄBÄ^BÄ‡BÄÚBÄ[BÄÛBÄ:\ÄÄıBÄ¯BÄ:BÄœBÄ—BÄ”\ÄBÄí\Ä	BÄÑBÄ◊BÄ¸BÄ˛BÄÿBÄ⁄BÄ‹BÄﬁBÄ‡BÄˇBÄBÄ‚\Ä\Ä\ÄBÄ^BÄyBÄÊBÄ[ÜÄ"nBÄ\ÄÄ[BÄ:BÄ:\ÄBÄ¿\ÄBÄ^BÄÊBÄ¿BÄ[\ÄBÄ:\ÄBÄ^BÄyBÄ¿BÄ[\ÄBÄÊBÄiÄBÄ‡ÄBÄ—BÄÚ—BÄ—BÄÙëBÄ˙¿BÄ[¿FÄX¿FÄM¿BÄû¿1Ä\ÄBÄ\ÄBÄ8BÄm–BÄ≥¿FÄ¡0¿1Ä\ÄBÄ\ÄBÄ8BÄo–BÄ“BÄíPA¡@UB¡‰	PBQBJPBC›ˇ›
Pˇ€UPD¡‰⁄ˇ›Pˇ€U‘“ÿ@QP¡JP@@QPPPA]RZ¸ÄPC¡DQ¿]]ZPCQ±Ê
‰CQEœ‰EQBEUBJP@˜˝OÄzBÄZÄ1Ä\Äp¿BÄ¯,ÉCOMPILE-FLAVOR-METHODS-2\ÄBÄ8BÄ‡ÄCÅINTERFACEÄÄÎÄ
ÜÄ`
FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄÄ\ÄBÄ BÄèBÄ:\ÄBÄè\ÄBÄí\ÄBÄïÄBÄ —BÄŒëBÄ‡¿p¿BÄMÏÇFIND-WINDOW-OF-FLAVORÄ“p¿BÄM¨ÅMAKE-WINDOWÄ“BÄœ¿BÄ¡ÄPàÊPä¿Pà	PåOÄêBÄÄÄÄÉÅCREATE-KEYSÄÄÎÄFÄFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄëBÄ:BÄ:BÄ:BÄ:ÄeÄD¿BÄ‡¿ÏÅRtms Interface¿\ÄBÄÄ¿p¿BÄMÏÅADD-SYSTEM-KEY“)ÅPROGRAMS¿¨ÄRTMS¿ÏÅRtms interface¿p¿BÄMlÉADD-TO-SYSTEM-MENU-COLUMNÄíPPPP†P	PP
P§OÄ£BÄëÄ1Ä\ÄBÄëÄBÄµÄÎÄ	ÜÄ@	FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄµBÄ:BÄ:BÄ:\ÄBÄí\ÄBÄ<BÄ>BÄ@BÄBBÄDBÄFBÄïÄBÄõëBÄ‹¿lÄ Ä¿BÄ¡¿BÄI¿BÄJÄPPêPàP@¡PPéNPíNˇcPìNˇcPõNhC˛GP@ùOÄ∞BÄµÄÄBÄÄÎÄ7ÜÄAFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ\ÄBÄÉÄARGÄBÄVÅITEM-NOÄBÄ:\ÄBÄ~BÄ	\ÄBÄa\ÄFÄÄBÄõ“BÄ1¿ÍÄPRIN1Ä“\ÄBÄáBÄÄÄÄ7‰ÄQÇQîÄ1˚ÁÄ˘ÒÄ5#‰ÄQAœ ‰A7‰AQÇQê¯˝A˙ÒA1¯ÁA5ÚÂAS&	ÊASÅ‰ASÔ˝ASÇQêÊ˝AW@¡P@QA[PÇ°ﬁ˝ROÄ¬BÄÄÄBÄ¿ÄÎÄFÄÄFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ¿\ÄBÄ~BÄVBÄ:BÄ:BÄ:ÄBÄõíÄ5‰ÄS¸ÄQÅQîOÄÀBÄ¿ÄÄBÄ¢ÄÎÄJÜÄ@HFÄ,¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ¢\ÄBÄBÄ:\ÄBÄ~BÄ	\ÄBÄí\ÄBÄïÄBÄõ—BÄŒ—BÄï—BÄ·ëBÄõ“BÄ1¿BÄø“lÄ~%¿BÄﬂ“BÄò¿BÄôÄÄ7ÊÄ1ÊÄÚÄQPîÄ50‰ÄQAœ ‰A7ÊA1ÊAÚAQ¸A5ÙÂAS&	ÊASÅ‰AW¸AWP	êË˝AW@¡@5‰@W¸@QPêﬁ˝P
PêP
PêPPêPàPåROÄÿBÄ¢ÄÄBÄ§ÄÎÄ7äÜÄ@xFÄS¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ§\ÄBÄBÄ:\Ä√ÄITEMSÄÉÅITEM-NUMBERÄÉÅMOD-RELATION√ÅMOD-ATTRIBUTESBÄ_BÄ:BÄó	ÅNUMBERSÄBÄ	BÄ:BÄ:BÄ:BÄ√BÄÃ\ÄBÄí\ÄÄBÄÍÄDOLISTBÄBÄBÄïÄCÅ*PKG-NAME*—√Ç*SYSTEM-RELATION-KEY*Ä—ÉÉ*SYSTEM-RELATION-ATTRIBUTES*—BÄõëÈÅNUMBER-OF-ITEM¿ÇSYSTEM-RELATIONÄ¿\ÄBÄ∏BÄ∫¿ÅQTRIEVEÄ“BÄ¢“BÄß“BÄ‚¿BÄ„“¨ÉDelete the indicated tuple?Ä¿p¿BÄMÏÅMOUSE-CONFIRMÄ“BÄ¬“BÄ‰
¿™ÄANDÄ¿ÍÄEQUALÄ¿BÄ•“BÄ8¿BÄ≠“FÄ ¿BÄ“©ÅDELETE-ITEMÄ¿ÉÄPUTPíPÄQíA¡PP	PPˇ›
™E¡‰ESF¡G€PFSíäPíG¡‰AQGSxÊAQGWy	ÊGQD¡PFSíäB¡FWC¡E≈‚ÁBA‰Pà>‰ÄQHœ‰H5¸Â@QHUí@¡˜˝BQPPE€E—CQ@QK¡J¡I¡¸IQJSKSM¡L¡PLQääPMQäíöCI√¡J≈K≈J‰KÈÁEQ
CPPABv
‰PAQêBQDSDWˇmíPúROÄ˘BÄ§ÄÄBÄ£ÄÎÄ-~)ÜÄ‡-@FÄ´¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ£\ÄBÄBÄ:\ÄBÄ‚BÄ„√ÅATTRIBUTE-VARSCÅMOD-TUPLEÄBÄ‰BÄÂ√ÄBLANKSBÄW
√ÄTUPLEÄBÄ:BÄó	BÄÊBÄ	BÄ:BÄ:BÄ√BÄÃBÄ:BÄ:\ÄBÄí\ÄÄBÄBÄÈBÄBÄBÄï¿ÜÄì ÄÉÉ*LINE-AREA-VALUES-MODIFIEDP*—BÄÍ—BÄÎ—BÄÏ—BÄõëBÄÌ¿BÄÓ¿\ÄBÄ∏BÄ∫¿BÄ“BÄ¢“BÄß“BÄ‚¿BÄ„“BÄ≠“BÄ¬“jÄ+Ä“p¿BÄ¯lÇSIMPLE-MAKE-ARRAYÄ“¨ÄMODÄ¿√ÄABORTÄ¿FÄÏ¿BÄ•“BÄ*¿,ÉModify the relation: ~SÄ¿BÄﬂ“)ÅFUNCTION¿√ÇLINE-AREA-DOMAIN-CHECK¿ÈÅMARGIN-CHOICES¿\ÄÏÄDo ItÄ\ÄÏÄAbortÄ\ÄÍÄTHROWÄ\ÄBÄ8BÄBÄÃ¿p¿BÄMÏÇCHOOSE-VARIABLE-VALUES“BÄ8¿BÄ“ÉÅPRINT-TUPLEÄ“BÄ∫¿BÄ›
¿BÄ‰
¿BÄÙ¿BÄı¿FÄ ¿BÄ„
“BÄ˜¿©ÅINSERT-ITEMÄÄ	PÄQíA¡
PPPPˇ›™I¡‰ISJ¡K€PJSíäPíK¡‰AQKSxÊAQKWyÊPJSíäD¡JWE¡I≈‰ÁD—‰ÄQLœ‰L5¸ÂGQLSäCäíG¡@QLUí@¡Ò˝EQäCˇkGQPEˇa	Jˇ€ˇ€ J™F¡I€I—EQN¡M¡¸MQNSO¡POQíäCM√¡N≈NÛÁIQB√@QM¡I¡	¸MSP¡ISPQä»I≈M≈I‰MÙÁ⁄PPTN€N—BQEQI¡R¡Q¡
¸QQRSISäíCQ√¡R≈I≈R‰IÚÁNQPˇ€PDQöPPPPJ ∫\ˇ‰⁄c‰R€R—BQN¡M¡	¸MQ!PNSûCíCM√¡N≈NıÁRQH¡R€R—HQN¡M¡¸MQNS"äCM√¡N≈N˜ÁRQäGQˇ€ˇ›FQˇ€#≤BC¡DQ$PEQ%PHQ&P'PR€R—EQ@QN¡M¡I¡¸IQMSNSP¡O¡(PPOQäíä!PPQäíöCI√¡M≈N≈M‰NÁÁRQ
C)P*PABv‰+PAQê,PAQCQúROÄ"BÄ£ÄÄBÄÄÎÄÜÄ`FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ\ÄBÄ BÄèBÄ:\ÄBÄèBÄ:ÄBÄ
ë‹ROÄ,BÄÄÄBÄ©ÄÎÄ5@µÜÄ`5`FÄu¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ©\ÄBÄ†BÄ BÄèBÄ:\ÄBÄèBÄBÄ:BÄ:BÄ:ÉÄOLDÄÉÄNEWÄBÄ¢\ÄBÄí\ÄBÄBÄBÄBÄU*ÅSEVENTHÄÍÄSIXTHÄÍÄFIFTHÄBÄÉÍÄTHIRDÄÍÄSECONDBÄÑÄBÄ†—ÅNEW-RENÄ—ÅNEW-DELÄ—ÅNEW-ADDÄ—ÅNEW-KEYÄ—ÅNEW-STOÄ—ÅNEW-IMPÄ—ÅNEW-TUPÄ—BÄò	—ÅNEW-DOCÄ—ÅNEW-DIRÄ—ÅOLD-RENÄ—ÅOLD-DELÄ—ÅOLD-ADDÄ—ÅOLD-KEYÄ—ÅOLD-STOÄ—ÅOLD-IMPÄ—ÅOLD-TUPÄ—ÅOLD-DOCÄ—ÅOLD-DIRÄ—ÅOLD-ATTÄ—BÄõ—É*PROVIDE-ERROR-MESSAGES*ë\ÄBÄ∫√ÅSAVE-DIRECTORYBÄ°BÄW
ÉÇIMPLEMENTATION-TYPEÄBÄ0BÄ¢¿ÉÅGET-RELATION“BÄ2“lÇERROR - Relation Ä¿BÄì“BÄø“,Ñ does not exist in the database ¿BÄ¿FÄº¿\ÄBÄò	ÏÅRelation NameÄBÄ˛¨ÉTo change the relation name.BÄƒ¿¨Ç     Attributes: ~SÄ¿BÄﬂ“\Ä
\ÄBÄAÏÅAdd attributesBÄ˛¨ÜTo add attributes specify attribute descriptor pair.BÄƒ\ÄBÄ@lÇDelete attributesÄBÄ˛,áTo delete attributes, specify a list of the attributes.ÄBÄƒ\ÄBÄ?lÇRename attributesÄBÄ˛ÏáTo rename attributes, specify a list of the type <(old new)>.ÄBÄƒlÄ Ä\ÄBÄD¨ÇImplementation-typeÄBÄ˛ÏÑTo change the type of implementation.ÄBÄƒ\ÄBÄClÇStorage StructureÄBÄ˛,ÖTo change the type of storage structure.BÄƒ\ÄBÄB¨ÄKeyÄBÄ˛ÏÉTo change the key attributes.ÄBÄƒ\ÄBÄFÏÅDocumentationÄBÄ˛ÏÑTo change the relation documentation.ÄBÄƒ\ÄBÄGÏÅSave DirectoryBÄ˛¨áTo change the directory in which this relation can be saved.BÄƒ\ÄBÄE¨ÅTuple formatBÄ˛ÏÖTo change the format in printing the relation.BÄƒ¿BÄ“BÄ*¿lÉChange the features of ~SÄ¿BÄ¿\ÄÏÄDo ItÄ\ÄÏÄAbortÄ\ÄBÄ\ÄBÄ8BÄBÄÃ¿BÄ“\Ä
BÄ†BÄJBÄIBÄHBÄPBÄOBÄNBÄMBÄLBÄK¿\Ä
BÄò	BÄABÄ@BÄ?BÄGBÄFBÄEBÄDBÄCBÄB¿\Ä
BÄó	ÅADD-ATTRÉÅDELETE-ATTRÄÉÅRENAME-ATTRÄBÄ†BÄ°BÄﬂBÄ£BÄ¨BÄ¢¿Å*EQUALPÄ¿BÄ≠“BÄ“BÄ¬“BÄé	íÄQ÷PPˇ›öBA¡AÊ	‰ÄPàPà PàPàRAS¿AW¿A[¿AQB¿AUB¿AYB¿AQBB¿⁄⁄⁄P¿P¿P¿P
¿P	¿P¿P¿P¿P¿P¿!P"PT#Pˇ€$PP%ö&P'ö(Pˇ€)PP%ö*P+P,™\ˇ)ÊA€-PB¡.PC¡/PD¡¸BSCSDSG¡F¡E¡0PEQFQ1ö2àÊAQGQFQ2ä1í3íA¡B≈C≈D≈B‰C‰D‰ÁA‰PAQ4îROÄ BÄ©Ä1Ä\Äp¿BÄ¯,ÑFASL-RECORD-FILE-MACROS-EXPANDED\ÄBÄ8\Ä)\Ä™ÅDEFPARAMETERÜÄI	\Äp¿BÄulÅDEFFLAVORÄÜÄ	Ø\Äp¿BÄulÅDEFMETHODÄÜÄ6y\ÄÍÄDEFUNÄÜÄ'\Äp¿BÄulÅDEFCOMMANDÜÄ.+\ÄBÄÜÄx
\ÄBÄ>ÜÄ{öÕ\ÄBÄ=ÜÄ:}n\ÄBÄ<ÜÄZiÛ\ÄBÄ;ÜÄ{ƒ≤\ÄBÄ:ÜÄ2ª=\ÄBÄÈÜÄ"‚á\ÄBÄÜÄ.Ÿã\ÄBÄ‡ÜÄ.-U\ÄBÄﬁÜÄq\ÄBÄ‹ÜÄc&p\ÄBÄ⁄ÜÄaÇ\ÄBÄÿÜÄ,a\ÄBÄ◊ÜÄ~ki\ÄBÄ˛ÜÄBXõ\ÄBÄ¸ÜÄVó√\ÄBÄÜÄ-i\ÄBÄÜÄ~…z\ÄBÄÜÄ<pë\ÄBÄÜÄ`sN\ÄBÄÜÄ|ƒÙ\ÄBÄÜÄ(Ã¢\ÄBÄÜÄ*˝j\ÄBÄÜÄ=Ã#\ÄBÄFÜÄ3œÚ\ÄBÄDÜÄ*Ô\ÄBÄBÜÄV>\ÄBÄ@ÜÄ&>\ÄBÄ>ÜÄ:>\ÄBÄ<ÜÄ>\ÄBÄ/ÜÄñΩ\ÄBÄUÜÄN¶™\ÄBÄÑÜÄz(á\ÄBÄÉÜÄxıø\ÄBÄñÜÄ%¡ \ÄBÄïÜÄaM*ÄÄName:ÄBÄ¿BÄüBÄ¡\ÄBÄ˛lÑName of the directory to write to.BÄƒ\ÄÄ*ÏÅType of SAVE:ÄBÄ¿BÄ™BÄ¡\ÄBÄ˛¨ÖSave type. It can be either XLD or COMMAND.ÄBÄƒ\ÄÄ*lÅMust Save:BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏáSave the relation even if the relation has not been modified.ÄBÄ¶BÄ*lÑGive parameters for SAVE RELATION:BÄ˘BÄú\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ•ÄÏBÄ¢ÄÌBÄ¢ÄÓBÄ¶Ä˛BÄ®ÄÙBÄ©ÄªBÄ¨Ä˘BÄúÄÃÄ\ÄBÄ^BÄ‡ÇSAVE-TRANSACTIONÄÎÄ
 ÜÄAFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄƒ\ÄBÄmÉÅTRANSACTIONÄBÄ∑BÄªBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏßSave a transaction on disk.

   TRANSACTION - Name of the transaction.
   DIRECTORY   - Name of the directory in which this transaction is to be stored.
   PATHNAME    - Name of the file in which it is to be stored.  (SAVE-TRANSACTION TRANSACTION &REST KEYWORD-LIST &KEY &OPTIONAL DIRECTORY PATHNAME &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿BÄ≈¿BÄû¿BÄ†¿BÄ≠“BÄﬂ“BÄ≈íPA¡Pˇ€PPÅQPÉQPÇQ	¢@√	ö
öAëÅQ@QîOÄ‘ÄƒÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ≈\ÄBÄ8\ÄBÄÍ,ÇSave TransactionBÄÏBÄÿÄÌBÄÿÄÓ\Ä\ÄBÄLÄBÄ˛,ÜUsed to save a transaction on a given directory.BÄÙ\Ä\ÄBÄaBÄ˜ÏÅOther FeaturesBÄª\ÄÄΩ\ÄÄ*lÇTransaction Name:ÄBÄ¿BÄÜBÄ¡\ÄBÄ˛¨ÑName of the transaction to be saved.BÄƒ\ÄÄ*,ÇDirectory Name:ÄBÄ¿BÄüBÄ¡\ÄBÄ˛lÑName of the directory to write to.BÄƒ\ÄÄ*lÅPathname:ÄBÄ¿BÄñBÄ¡\ÄBÄ˛,
The name of the file into which the transaction forms will be stored. It defaults to <transaction>.lispÄBÄƒBÄ*ÏÑGive parameters for SAVE TRANSACTION:ÄBÄ˘BÄ“\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ€ÄÏBÄÿÄÌBÄÿÄÓBÄ‹Ä˛BÄﬁÄÙBÄﬂÄªBÄ‚Ä˘BÄ“ÄÃÄ\ÄBÄ^BÄ‡√ÇDEFINE-IMPLEMENTATIONÄÄÎÄ	ÜÄ@»FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄˆ\ÄBÄm√ÅIMPLEMENTATIONBÄ°BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛¨ÆDefine a new implementation.

   IMPLEMENTATION-NAME - Name of the implementation to be defined. All the implementation-specific
                         accessor functions are expected to be defined.
   DOCUMENTATION       - Description of this implementation.  (DEFINE-IMPLEMENTATION IMPLEMENTATION-NAME &REST KEYWORD-LIST &KEY &OPTIONAL DOCUMENTATION &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿BÄ˜¿BÄ°¿BÄ≠“BÄﬂ“BÄ˜íPA¡Pˇ€PPÅQPÇQí@√ö	öAëÅQ@Q
îOÄBÄˆÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ˜\ÄBÄ8\ÄBÄÍÏÇDefine ImplementationÄBÄÏBÄ
BÄÌBÄ
BÄÓ\Ä\ÄBÄêBÄÕBÄ˛lÑUsed to define an implementation.ÄBÄÙ\Ä\ÄBÄaBÄ˜lÅDefinitionBÄª\ÄBÄΩ\ÄÄ*¨ÇImplementation Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏåName of the implementation. Implementation-dependent routines are expected to be defined by the user.ÄBÄƒ\ÄÄ*ÏÅDocumentation:BÄ¿BÄ¥BÄ¡\ÄBÄ˛ÏÑDocumentation for the implementation.ÄBÄ-BÄ*lÖGive parameters for DEFINE IMPLEMENTATION:BÄ˘BÄ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄBÄÏBÄ
BÄÌBÄ
BÄÓBÄBÄ˛BÄBÄÙBÄBÄªBÄBÄ˘BÄBÄÃÄ\ÄBÄ^BÄ‡ÉÅDEFINE-INDEXÄÎÄ)ÜÄA»FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ$\ÄBÄmBÄ∏CÅINDEX-NAME√ÅKEY-ATTRIBUTESCÇSTORAGE-STRUCTUREÄÅPRIORITYBÄ°BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏøñDefine an index on a relation in the active database.

    RELATION-NAME - Name of the relation on which the index will be defined.
    NAME - Name of the index to be defined
    KEY - List of attributes names which form the key of the index.
    STORAGE-STRUCTURE - The name of a RTMS defined storage structure upon which will be used as the index structure.
    PRIORITY - A numerical value which determines the order in which RTMS will search multiple indices of a relation
               for a possible key. The number one receives the highest consideration, if it fails the next index in
               value is attempted.
    DOCUMENTATION - A string describing this index.  (DEFINE-INDEX RELATION-NAME &REST KEYWORD-LIST &KEY &OPTIONAL NAME DOCUMENTATION STORAGE-STRUCTURE KEY PRIORITY &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄ%¿ÉÄNAME¿BÄ¢¿BÄ¨¿BÄ1¿BÄ°¿BÄ≠“BÄﬂ“BÄ%íPA¡Pˇ€PPÅQPÇQPÉQ	PÑQ
PÖQPÜQ
J∫@√ööAëÅQ@QîOÄ8BÄ$Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ%\ÄBÄ8\ÄBÄÍ¨ÅDefine IndexBÄÏBÄ<BÄÌBÄ<BÄÓ\Ä\ÄBÄê•Ä@IBÄ˛,ÜUsed to define a secondary index on a relation.ÄBÄÙ\Ä\ÄBÄaBÄ˜lÅDefinitionBÄª\Ä	BÄΩ\ÄÄ*ÏÅRelation Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛láName of the relation upon which the index will be defined.BÄƒ\ÄÄ*¨ÅIndex Name:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛,ÑName of the index to be defined.BÄ-\ÄÄ*,ÇKey Attributes:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛láList of attribute names which form the key for this index.BÄƒ\ÄÄ*lÇStorage Structure:BÄ¿¨ÄAVLÄBÄ¡\ÄBÄ˛,ÜThe storage structure used to define the index.ÄBÄ-\ÄÄ*lÅPriority:ÄBÄ¿FÄ
BÄ¡\ÄBÄ˛ÏãA numerical value which indicates the priority given to this index. 1 is the highest priority.BÄë\ÄÄ*ÏÅDocumentation:BÄ¿BÄ¥BÄ¡\ÄBÄ˛¨ÉDocumentation for the index.BÄ-BÄ*lÑGive parameters for DEFINE INDEX:ÄBÄ˘BÄ5\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ?BÄÏBÄ<BÄÌBÄ<BÄÓBÄ@BÄ˛BÄCBÄÙBÄDBÄªBÄGBÄ˘BÄ5BÄÃÄ\ÄBÄ^BÄ‡ÉÅMODIFY-INDEXÄÎÄ+ÜÄBFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄi\ÄBÄmBÄ∏BÄ.√ÅNEW-INDEX-NAMEBÄ/BÄ0BÄ1BÄ°BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏøΩModify an index on a relation in the active database.

    RELATION-NAME - Name of the relation on which the index to be modified is defined
    INDEX-NAME - Name of the index to be modified
    NEW-NAME - New name for the specified index
    KEY - List of attributes names which form the key of the index.
    STORAGE-STRUCTURE - The name of a RTMS defined storage structure upon which will be used as the index structure.
    PRIORITY - A numerical value which determines the order in which RTMS will search multiple indices of a relation
               for a possible key. The number one receives the highest consideration, if it fails the next index in
               value is attempted.
    DOCUMENTATION - A string describing this index.  (MODIFY-INDEX RELATION-NAME INDEX-NAME &REST KEYWORD-LIST &KEY &OPTIONAL NEW-NAME DOCUMENTATION STORAGE-STRUCTURE KEY PRIORITY &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿BÄj¿ÅNEW-NAME¿BÄ¢¿BÄ¨¿BÄ1¿BÄ°¿BÄ≠“BÄﬂ“BÄjíPA¡Pˇ€PPÅQÇQPÉQPÑQ	PÖQ
PÜQPáQ
J∫@√¢öAëÅQÇQ@QúOÄzBÄiÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄj\ÄBÄ8\ÄBÄÍ¨ÅModify IndexBÄÏBÄ~BÄÌBÄ~BÄÓ\Ä\ÄBÄBÄBBÄ˛,ÜUsed to define a secondary index on a relation.ÄBÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\Ä
BÄΩ\ÄÄ*ÏÅRelation Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨àName of the relation upon which the index to be modified is defined.BÄƒ\ÄÄ*¨ÅIndex Name:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛lÑName of the index to be modified.ÄBÄ-\ÄÄ*,ÇNew Index Name:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛ÏÇNew name of the index.BÄ-\ÄÄ*,ÇKey Attributes:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛láList of attribute names which form the key for this index.BÄƒ\ÄÄ*lÇStorage Structure:BÄ¿BÄ:BÄ¡\ÄBÄ˛,ÜThe storage structure used to define the index.ÄBÄ-\ÄÄ*lÅPriority:ÄBÄ¿FÄ
BÄ¡\ÄBÄ˛ÏãA numerical value which indicates the priority given to this index. 1 is the highest priority.BÄë\ÄÄ*ÏÅDocumentation:BÄ¿BÄ¥BÄ¡\ÄBÄ˛¨ÉDocumentation for the index.BÄ-BÄ*lÑGive parameters for DEFINE INDEX:ÄBÄ˘BÄw\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄÅBÄÏBÄ~BÄÌBÄ~BÄÓBÄÇBÄ˛BÄÑBÄÙBÄÖBÄªBÄàBÄ˘BÄwBÄÃÄ\ÄBÄ^BÄ‡ÉDEFINE-STORAGE-STRUCTUREÄÎÄ	ÜÄ@»FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ≠\ÄBÄmBÄ0BÄ°BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛,≤Define a new storage structure.

   STORAGE-STRUCTURE-NAME - Name of the storage-structure to be defined. All the storage-structure-specific
                            accessor functions are expected to be defined.
   DOCUMENTATION          - Description of this storage-structure.  (DEFINE-STORAGE-STRUCTURE STORAGE-STRUCTURE-NAME &REST KEYWORD-LIST &KEY &OPTIONAL DOCUMENTATION &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄÆ¿BÄ°¿BÄ≠“BÄﬂ“BÄÆíPA¡Pˇ€PPÅQPÇQí@√ö	öAëÅQ@Q
îOÄºBÄ≠Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄÆ\ÄBÄ8\ÄBÄÍ,ÉDefine Storage StructureBÄÏBÄ¿BÄÌBÄ¿BÄÓ\Ä\ÄBÄêBÄLÄ˛lÑUsed to define a storagestructure.BÄÙ\Ä\ÄBÄaBÄ˜lÅDefinitionBÄª\ÄBÄΩ\ÄÄ*,ÉStorage structure name:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛¨
Name of the storage structure. Storage-structure-dependent routines are expected to be defined by the user.ÄBÄƒ\ÄÄ*ÏÅDocumentation:BÄ¿BÄ:BÄ¡\ÄBÄ˛,ÖDocumentation for the storage structure.BÄ-BÄ*ÏÖGive parameters for DEFINE STORAGE STRUCTURE:ÄBÄ˘BÄ∫\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ√BÄÏBÄ¿BÄÌBÄ¿BÄÓBÄƒBÄ˛BÄ∆BÄÙBÄ«BÄªBÄ BÄ˘BÄ∫BÄÃÄ\ÄBÄ^BÄ‡√ÅDEFINE-DOMAINÄÄÎÄ#ÜÄAHFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ⁄\ÄBÄm√ÄDOMAINp¿BÄu¨ÄDEFÄBÄ°BÄﬂBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛¨ØDefine new domain. Corresponding predicate is expected to be defined prior to this operation.

   DOMAIN-NAME     - Name of the domain to be defined.
   DOCUMENTATION   - Describes the new domain.
   FORMAT          - Print width for attributes belonging to this domain.  (DEFINE-DOMAIN DOMAIN-NAME &REST KEYWORD-LIST &KEY &OPTIONAL DEFAULT DOCUMENTATION FORMAT &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿BÄ€¿ÅDEFAULTÄ¿BÄ°¿BÄﬂ¿BÄ≠“BÄﬂ“BÄ€íPA¡Pˇ€PPÅQPÇQPÉQ	PÑQ
≤@√
ööAëÅQ@QîOÄÌBÄ⁄Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ€\ÄBÄ8\ÄBÄÍÏÅDefine DomainÄBÄÏBÄÒBÄÌBÄÒBÄÓ\Ä•Ä`DBÄ˛,ÉUsed to define a domain.BÄÙ\Ä\ÄBÄaBÄ˜lÅDefinitionBÄª\ÄBÄΩ\ÄÄ*¨ÅDomain Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛Ï Name of the domain. Domain predicate is expected to be defined prior to this.ÄBÄƒ\ÄÄ*ÏÅDefault value:BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏÉDefault value for this domain.BÄƒ\ÄÄ*ÏÅDocumentation:BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏÉDocumentation for the domain.ÄBÄ-\ÄÄ*,ÇDefault width :ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛ÏÖThe default width to be used for this domain.ÄBÄƒBÄ*lÑGive parameters for DEFINE DOMAIN:BÄ˘BÄÍ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄÙBÄÏBÄÒBÄÌBÄÒBÄÓBÄıBÄ˛BÄ˜BÄÙBÄ¯BÄªBÄ˚BÄ˘BÄÍBÄÃÄ\ÄBÄ^BÄ‡√ÅMODIFY-DOMAINÄÄÎÄ#ÜÄAHFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ\ÄBÄmBÄ‰BÄÊBÄ°BÄﬂBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛l∞Modify the default format, value, and documentation of a domain.

   DOMAIN-NAME - Name of the domain to be modified.
   FORMAT      - New format, i.e the print width, for this domain.
   DEFAULT     - New default value for this domain.
   DOC         - New description of this domain.  (MODIFY-DOMAIN DOMAIN-NAME &REST KEYWORD-LIST &KEY &OPTIONAL FORMAT DEFAULT DOC &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄ¿BÄÏ¿BÄ°¿BÄﬂ¿BÄ≠“BÄﬂ“BÄíPA¡Pˇ€PPÅQPÇQPÉQ	PÑQ
≤@√
ööAëÅQ@QîOÄ"BÄÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ\ÄBÄ8\ÄBÄÍÏÅModify DomainÄBÄÏBÄ&BÄÌBÄ&BÄÓ\Ä\ÄBÄBÄêBÄ˛,ÉUsed to modify a domain.BÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄBÄΩ\ÄÄ*¨ÅDomain Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛lÑName of the domain to be modified.BÄƒ\ÄÄ*ÏÅDefault value:BÄ¿BÄ:BÄ¡\ÄBÄ˛lÑNew default value for this domain.BÄƒ\ÄÄ*ÏÅDocumentation:BÄ¿BÄ:BÄ¡\ÄBÄ˛lÑNew documentation for the domain.ÄBÄ-\ÄÄ*,ÇDefault width :ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛lÜThe new default width to be used for this domain.ÄBÄƒBÄ*lÑGive parameters for MODIFY DOMAIN:BÄ˘BÄ \ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ)BÄÏBÄ&BÄÌBÄ&BÄÓBÄ*BÄ˛BÄ,BÄÙBÄ-BÄªBÄ0BÄ˘BÄ BÄÃÄ\ÄBÄ^BÄ‡CÇDEFINE-TRANSACTIONÄÎÄ"ÜÄAHFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄH\ÄBÄmBÄŒ√ÄFORMSÄBÄ†BÄûBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏØDefine a transaction, a list of database calls.

   TRANSACTION - Name of the transaction.
   FORMS       - List of RTMS calls.
   DIRECTORY   - Name of the directory in which this transaction will be stored.
   PATHNAME    - Name of the file in which it will be stored.  (DEFINE-TRANSACTION TRANSACTION FORMS &REST KEYWORD-LIST &KEY &OPTIONAL DIRECTORY PATHNAME &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄI¿BÄ†¿BÄû¿BÄ≠“BÄﬂ“BÄIíPA¡Pˇ€PPÅQÇQPÉQPÑQ	¢@√	¢
öAëÅQÇQ@QúOÄXBÄHÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄI\ÄBÄ8\ÄBÄÍlÇDefine TransactionBÄÏBÄ\BÄÌBÄ\BÄÓ\Ä\ÄBÄêBÄBÄ˛ÏÉUsed to define a transaction.ÄBÄÙ\Ä\ÄBÄaBÄ˜lÅDefinitionBÄª\ÄBÄΩ\ÄÄ*lÇTransaction Name:ÄBÄ¿BÄÜBÄ¡\ÄBÄ˛,ÉName of the transaction.BÄƒ\ÄÄ*,ÇDatabase calls:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛lÉA list of database calls.ÄBÄƒBÄ—\ÄÄ*lÅPathname :BÄ¿BÄñBÄ¡\ÄBÄ˛¨ÖThe default file in which it will be saved.ÄBÄƒBÄ*,ÖGive parameters for DEFINE TRANSACTION:ÄBÄ˘BÄV\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ_BÄÏBÄ\BÄÌBÄ\BÄÓBÄ`BÄ˛BÄbBÄÙBÄcBÄªBÄfBÄ˘BÄVBÄÃÄ\ÄBÄ^BÄ‡CÇMODIFY-TRANSACTIONÄÎÄ
 ÜÄAFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄz\ÄBÄmBÄŒÄ†BÄûBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛¨®Edit the database calls in a transaction.
   TRANSACTION - Name of the transaction.
   DIRECTORY   - Name of the directory in which this transaction can be found.
   PATHNAME    - Name of the file in which it is stored.  (MODIFY-TRANSACTION TRANSACTION &REST KEYWORD-LIST &KEY &OPTIONAL DIRECTORY PATHNAME &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿BÄ{¿BÄ†¿BÄû¿BÄ≠“BÄﬂ“BÄ{íPA¡Pˇ€PPÅQPÇQPÉQ	¢@√	ö
öAëÅQ@QîOÄ BÄzÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ{\ÄBÄ8\ÄBÄÍlÇModify TransactionBÄÏBÄ
BÄÌBÄ
BÄÓ\Ä\ÄBÄBÄBÄ˛ÏÉUsed to modify a transaction.ÄBÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄÄΩ\ÄÄ*lÇTransaction Name:ÄBÄ¿BÄÜBÄ¡\ÄBÄ˛,ÖName of the transaction to be modified.ÄBÄƒ\ÄÄ*lÅDirectory:BÄ¿BÄüBÄ¡\ÄBÄ˛ÏáDefault directory in which it can be found, if not in memory.ÄBÄƒ\ÄÄ*lÅPathname :BÄ¿BÄñBÄ¡\ÄBÄ˛¨áThe default file in which it can be found, if not in memory.BÄƒBÄ*,ÖGive parameters for MODIFY TRANSACTION:ÄBÄ˘BÄá\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄêBÄÏBÄ
BÄÌBÄ
BÄÓBÄëBÄ˛BÄìBÄÙBÄîBÄªBÄóBÄ˘BÄáBÄÃÄ\ÄBÄ^BÄ‡ÇDEFINE-DATABASEÄÄÎÄ#ÜÄAHFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ´\ÄBÄmBÄ°BÄ∑BÄ°ÉÄENVÄBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛¨∞Define a new database.

   DB-NAME     - Name of the database.
   DIRECTORY   - Name of the directory in which this database is to be saved.
   ENVIRONMENT - Name of the environment to be associated with this database.
   DOCUMENTATION - A string describing this database.  (DEFINE-DATABASE DB-NAME &REST KEYWORD-LIST &KEY &OPTIONAL DIRECTORY ENVIRONMENT DOCUMENTATION &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿√ÄDEFDBÄ¿BÄ†¿BÄ°¿BÄÂ¿BÄ≠“BÄﬂ“BÄªíPA¡Pˇ€PPÅQPÇQPÉQ	PÑQ
≤@√
ööAëÅQ@QîOÄºBÄ´Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ¨\ÄBÄ8\ÄBÄÍ,ÇDefine DatabaseÄBÄÏBÄ¿BÄÌBÄ¿BÄÓ\Ä\ÄBÄêBÄêBÄ˛,ÜUsed to define a database in a given directory.ÄBÄÙ\Ä\ÄBÄaBÄ˜lÅDefinitionBÄª\ÄBÄΩ\ÄÄ*ÏÅDatabase Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏÇName of the database.ÄBÄƒ\ÄÄ*,ÇDirectory Name:ÄBÄ¿BÄüBÄ¡\ÄBÄ˛ÏÖName of the save directory for this database.ÄBÄƒ\ÄÄ*ÏÅDocumentation:BÄ¿BÄ¥BÄ¡\ÄBÄ˛,ÑDocumentation for the database.ÄBÄ-\ÄÄ*¨ÅEnvironment:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨àName of the environment to be used to replace the default settings.ÄBÄƒBÄ*¨ÑGive parameters for DEFINE DATABASE:BÄ˘BÄπ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ√BÄÏBÄ¿BÄÌBÄ¿BÄÓBÄƒBÄ˛BÄ∆BÄÙBÄ«BÄªBÄ BÄ˘BÄπBÄÃÄ\ÄBÄ^BÄ‡ÇMODIFY-DATABASEÄÄÎÄ#ÜÄAHFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ‚\ÄBÄmBÄ°ÉÅNEW-DATABASEBÄ∑BÄ°BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛,±Modify various features of the active database.

  DATABASE      - Name of the database to be modified.
  DATABASE-NAME - New name for this database.
  DIRECTORY     - New directory in which this database is to be saved.
  DOCUMENTATION - New description for this database.  (MODIFY-DATABASE DATABASE &REST KEYWORD-LIST &KEY &OPTIONAL DATABASE-NAME DIRECTORY DOCUMENTATION &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄ„¿√ÅDATABASE-NAMEÄ¿BÄ†¿BÄ°¿BÄ≠“BÄﬂ“BÄ„íPA¡Pˇ€PPÅQPÇQPÉQ	PÑQ
≤@√
ööAëÅQ@QîOÄÛBÄ‚Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ„\ÄBÄ8\ÄBÄÍ,ÇModify DatabaseÄBÄÏBÄ˜BÄÌBÄ˜BÄÓ\Ä\ÄBÄBÄfBÄ˛,ÖUsed to modify the features a database.ÄBÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄBÄΩ\ÄÄ*ÏÅDatabase Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏÇName of the database.ÄBÄƒ\ÄÄ*lÇNew Database Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏÜIf the database is to be renamed specify the new name.BÄƒ\ÄÄ*,ÇDirectory Name:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛, To change the save directory for this database specify a new directory.ÄBÄƒ\ÄÄ*ÏÅDocumentation:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨ÑNew documentation for the database.ÄBÄ-BÄ*¨ÑGive parameters for MODIFY DATABASE:BÄ˘BÄ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ˙BÄÏBÄ˜BÄÌBÄ˜BÄÓBÄ˚BÄ˛BÄ˝BÄÙBÄ˛BÄªBÄ	BÄ˘BÄBÄÃÄ\ÄBÄ^BÄ‡ÇMODIFY-ATTRIBUTEÄÎÄ(ÜÄA»FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ	\ÄBÄmBÄ†BÄ√ÅNEW-ATTRBÄÊBÄ°BÄﬂBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏøModify various features of an attribute in a given relation.

  RELATION       - Name of the relation in which the attribute to be modified exists.
  ATTRIBUTE      - Name of the attribute to be modified.
  ATTRIBUTE-NAME - New name for this attribute.
  DEFAULT-VALUE  - New default value for this attribute.
  DOCUMENTATION  - New description.
  FORMAT         - New print width to be used for this attribute.  (MODIFY-ATTRIBUTE RELATION ATTRIBUTE &REST KEYWORD-LIST &KEY &OPTIONAL ATTRIBUTE-NAME DEFAULT-VALUE DOCUMENTATION FORMAT &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿BÄ	¿√ÅATTRIBUTE-NAME¿BÄÊ¿BÄ°¿BÄﬂ¿BÄ≠“BÄﬂ“BÄ	íPA¡Pˇ€PPÅQÇQPÉQPÑQ	PÖQ
PÜQJ∫@√¢öAëÅQÇQ@QúOÄ*	BÄ	Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ	\ÄBÄ8\ÄBÄÍ,ÇModify AttributeBÄÏBÄ.	BÄÌBÄ.	BÄÓ\Ä\ÄBÄBÄÚBÄ˛,ÖUsed to modify the features a attribute.BÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\Ä	BÄΩ\ÄÄ*ÏÅRelation Name:BÄ¿BÄ~BÄ¡\ÄBÄ˛ÏÇName of the relation.ÄBÄƒ\ÄÄ*,ÇAttribute Name:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛ÏÇName of the attribute.BÄƒ\ÄÄ*¨ÇNew Attribute Name:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛,áIf the attribute is to be renamed specify the new name.ÄBÄƒ\ÄÄ*ÏÅDefault Value:BÄ¿BÄ:BÄ¡\ÄBÄ˛làTo change the default value of this attribute specify a new value.BÄƒ\ÄÄ*ÏÅDocumentation:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨ÑNew documentation for the attribute.BÄ-\ÄÄ*,ÇDefault width :ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛¨ÜThe new default width to be used for this attribute.BÄƒBÄ*ÏÑGive parameters for MODIFY ATTRIBUTE:ÄBÄ˘BÄ'	\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ1	BÄÏBÄ.	BÄÌBÄ.	BÄÓBÄ2	BÄ˛BÄ4	BÄÙBÄ5	BÄªBÄ8	BÄ˘BÄ'	BÄÃÄ\ÄBÄ^BÄ‡ÉÅMODIFY-VIEWÄÄÎÄ
 ÜÄAFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄX	\ÄBÄmÉÄVIEWBÄÊBÄ°BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛¨•Modify a view definition or its documentation.

   VIEW-NAME       - Name of the view.
   VIEW-DEFINITION - New definition of the view.
   VIEW-DOCUMENTATION - New description of the view.  (MODIFY-VIEW VIEW-NAME &REST KEYWORD-LIST &KEY &OPTIONAL VIEW-DEFINITION VIEW-DOCUMENTATION &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿BÄY	¿ÅVIEW-DEF¿ÅVIEW-DOC¿BÄ≠“BÄﬂ“BÄY	íPA¡Pˇ€PPÅQPÇQPÉQ	¢@√	ö
öAëÅQ@QîOÄj	BÄX	Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄY	\ÄBÄ8\ÄBÄÍ¨ÅModify ViewÄBÄÏBÄn	BÄÌBÄn	BÄÓ\Ä\ÄBÄ•Ä VBÄ˛¨ÑUsed to modify the features a view.ÄBÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄÄΩ\ÄÄ*lÅView Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛lÇName of the view.ÄBÄƒ\ÄÄ*,ÇView Definition:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨ÉNew definition of the view.ÄBÄƒ\ÄÄ*ÏÅDocumentation:BÄ¿BÄ:BÄ¡\ÄBÄ˛,ÑNew documentation for the view.ÄBÄ-BÄ*,ÑGive parameters for MODIFY VIEW:BÄ˘BÄf	\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄq	BÄÏBÄn	BÄÌBÄn	BÄÓBÄr	BÄ˛BÄu	BÄÙBÄv	BÄªBÄy	BÄ˘BÄf	BÄÃÄ\ÄBÄ^BÄ‡ÇMODIFY-RELATIONÄÄÎÄ8ÜÄCFÄ&¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ
	\ÄBÄmÉÄRELÄÅNEW-RELÄÅADD-ATTÄÅDEL-ATTÄÅREN-ATTÄBÄ£BÄ¨BÄﬂBÄ¢BÄ†BÄ°BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏøÙModify various features of a relation.

  RELATION             - Name of the relation to be modified.
  RELATION-NAME        - New name for this relation.
  ADD-ATTRIBUTES       - List of new attributes and their description.
  DELETE-ATTRIBUTES    - List of attributes to be destroyed.
  RENAME-ATTRIBUTES    - List of list of OLD-NEW attribute names.
  IMPLEMENTATION-TYPE  - Name of the new implementation type.
  STORAGE-STRUCTURE    - Name of the new storage-structure.
  FORMAT               - List of new print-width values to be used for the attributes.
  KEY                  - List of attributes to form the new key for this relation.
  DOCUMENTATION        - New description of this relation.
  DIRECTORY            - New directory in which this relation is to be saved.  (MODIFY-RELATION RELATION &REST KEYWORD-LIST &KEY &OPTIONAL RELATION-NAME ADD-ATTRIBUTES DELETE-ATTRIBUTES RENAME-ATTRIBUTES IMPLEMENTATION-TYPE STORAGE-STRUCTURE FORMAT KEY DOCUMENTATION DIRECTORY &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄé	¿BÄ†¿√ÅADD-ATTRIBUTES¿CÇDELETE-ATTRIBUTESÄ¿CÇRENAME-ATTRIBUTESÄ¿BÄ£¿BÄ¨¿BÄﬂ¿BÄ¢¿BÄ°¿BÄ†¿BÄ≠“BÄﬂ“BÄé	íPA¡Pˇ€PPÅQPÇQPÉQ	PÑQ
PÖQPÜQPáQPàQP QPãQPäQJ∫@√ööAëÅQ@QîOÄ§	BÄ
	Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄé	\ÄBÄ8\ÄBÄÍ,ÇModify RelationÄBÄÏBÄ®	BÄÌBÄ®	BÄÓ\Ä\ÄBÄBÄBÄ˛,ÖUsed to modify the features a relation.ÄBÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄBÄΩ\ÄÄ*ÏÅRelation Name:BÄ¿BÄ~BÄ¡\ÄBÄ˛ÏÇName of the Relation.ÄBÄƒ\ÄÄ*lÇNew Relation Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏÜIf the relation is to be renamed specify the new name.BÄƒ\ÄÄ*,ÇAdd attributes:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛lãSpecify a list of attribute-descriptor pairs for attributes to be added to this relation.ÄBÄƒ\ÄÄ*lÇDelete attributes:BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏàSpecify a list of attributes in this relation which are to be deleted.BÄƒ\ÄÄ*lÇRename attributes:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨ãTo rename some of the attributes provide a list of the form (<old-attribute new-attribute>).BÄƒ\ÄÄ*¨ÇImplementation Type:BÄ¿BÄ:BÄ¡\ÄBÄ˛, To change the implementation type of this relation specify a new value.ÄBÄƒ\ÄÄ*lÇStorage structure:BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏàTo change the storage structure of this relation specify a new value.ÄBÄƒ\ÄÄ*,ÅFormat:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛,äTo change the format for this relation specify a new format as a list of values.BÄƒ\ÄÄ*¨ÄKey:BÄ¿BÄ:BÄ¡\ÄBÄ˛Ï To change the key for this relation specify a new key as a list of attributes.BÄƒ\ÄÄ*,ÇDirectory Name:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛, To change the save directory for this relation specify a new directory.ÄBÄƒ\ÄÄ*ÏÅDocumentation:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨ÑNew documentation for the relation.ÄBÄ-BÄ*¨ÑGive parameters for MODIFY RELATION:BÄ˘BÄü	\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ´	BÄÏBÄ®	BÄÌBÄ®	BÄÓBÄ¨	BÄ˛BÄÆ	BÄÙBÄØ	BÄªBÄ≤	BÄ˘BÄü	BÄÃÄ\ÄBÄ^BÄ‡CÇDEFINE-ENVIRONMENTÄÎÄRÜÄCHFÄ4¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄÊ	\ÄBÄmBÄÂBÄòÄ†ÉÄERRÄCÅPAR-CHECKÄÅREL-IMPÄÅREL-STOÄ√ÄSTATUSÅSYS-IMPÄÅSYS-STOÄCÅVAL-CHECKÄ™ÄWARNBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛Ïø∂Global variables defining an environment can be set using this function.

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
   WARNINGS     - If T, warning messages are generated.  (DEFINE-ENVIRONMENT ENVIRONMENT &REST KEYWORD-LIST &KEY &OPTIONAL AUTO-SAVE DIRECTORY ERRORS PARAMETER-CHECKING RELATION-IMPLEMENTATION RELATION-STORAGE-STRUCTURE STATUS SYSTEM-IMPLEMENTATION SYSTEM-STORAGE-STRUCTURE VALIDITY WARNINGS &ALLOW-OTHER-KEYS)ÄBÄõ—BÄõëBÄ‹¿lÄ~S¿√ÄDEFENV¿CÅAUTO-SAVEÄ¿ÉÄPARA¿BÄ†¿BÄÚ	¿BÄÛ	¿√ÄERRORS¿BÄÙ	¿ÅVALIDITY¿ÅWARNINGS¿BÄı	¿BÄˆ	¿BÄ≠“BÄﬂ“BÄ˛	íPA¡Pˇ€PPÅQ‰PÇQ	PÖQ
PÉQPÜQPáQPÑQPàQPãQPåQJ¸PÇQ	PÖQ
PÉQPÜQPáQPÑQPàQP QPäQPãQPåQJ∫@√ööAëÅQ@QîOÄ
BÄÊ	Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄÁ	\ÄBÄ8\ÄBÄÍlÇDefine EnvironmentBÄÏBÄ
BÄÌBÄ
BÄÓ\Ä\ÄBÄêBÄeBÄ˛¨ÜUsed to define an environment in a given directory.ÄBÄÙ\Ä\ÄBÄaBÄ˜lÅDefinitionBÄª\ÄBÄΩ\ÄÄ*lÇEnvironment Name:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛,ÉName of the environment.BÄƒ\ÄÄ*lÅAuto save:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨àAutomatically saves all the modified relations after each function.ÄBÄ¶BÄ—\ÄÄ*,ÅErrors:ÄBÄ¿BÄÃBÄ¡\ÄBÄ˛¨ÖControls the printing of the error messages.BÄ¶\ÄÄ*¨ÇParameter Checking:ÄBÄ¿BÄÃBÄ¡\ÄBÄ˛,ÖControls the checking of the parameters.BÄ¶\ÄÄ*,ÉRelation Implementation:BÄ¿BÄΩBÄ¡\ÄBÄ˛ÏÖDefault implementation of the user relations.ÄBÄƒ\ÄÄ*¨ÉRelation storage structure:ÄBÄ¿BÄ¬BÄ¡\ÄBÄ˛lÜDefault storage structure for the user relations.ÄBÄƒ\ÄÄ*,ÅStatus:ÄBÄ¿BÄÃBÄ¡\ÄBÄ˛ÏÖControls the printing of the status messages.ÄBÄ¶\ÄÄ*ÏÇSystem Implementation:BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏãDefault implementation of the system relations. Can not change this when a database is active.BÄƒ\ÄÄ*lÉSystem storage structure:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛låDefault storage structure for the system relations. Can not change this when a database is active.BÄƒ\ÄÄ*lÇValidity Checking:BÄ¿BÄÃBÄ¡\ÄBÄ˛¨äControls the checking of the values during insertion and modification for validity.ÄBÄ¶\ÄÄ*lÅWarnings:ÄBÄ¿BÄÃBÄ¡\ÄBÄ˛ÏÖControls the printing of the warning messages.BÄ¶BÄ*,ÖGive parameters for DEFINE ENVIRONMENT:ÄBÄ˘BÄ¸	\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ
BÄÏBÄ
BÄÌBÄ
BÄÓBÄ
BÄ˛BÄ
BÄÙBÄ
BÄªBÄ
BÄ˘BÄ¸	BÄÃÄ\ÄBÄ^BÄ‡ÇDEFINE-RELATIONÄÄÎÄ.ÜÄBHFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄF
\Ä	BÄmBÄ†ÅATTR-DESBÄüBÄ†BÄ°BÄ¢BÄ£BÄ§BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏøäDefine relations in the active database.

   RELATION-NAME - Name of the relation to be defined.
   ATTRIBUTE-DESCRIPTOR - List of attributes and their descriptions.
   DIRECTORY     - Name of the directory in which this relation is to be saved.
   DOCUMENTATION - Description of this relation.
   FORMAT        - List of print-width values correponding to the attribute-list.
   IMPLEMENTATION-TYPE - Name of the implementation for this relation.
   KEY           - List of attributes comprising the key for this relation.
   STORAGE-STRUCTURE   - Name of the storage structure to be used for this relation.  (DEFINE-RELATION RELATION-NAME ATTRIBUTE-DESCRIPTOR &REST KEYWORD-LIST &KEY &OPTIONAL DIRECTORY DOCUMENTATION FORMAT IMPLEMENTATION-TYPE KEY STORAGE-STRUCTURE &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿√ÄDEFREL¿ÉÅTUPLE-FORMAT¿BÄ†¿BÄ°¿BÄ¢¿BÄ£¿BÄ¨¿BÄ≠“BÄﬂ“BÄV
íPA¡Pˇ€PPÅQÇQPÉQPÑQ	PÖQ
PÜQPáQPàQJ∫@√¢öAëÅQÇQ@QúOÄX
BÄF
Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄG
\ÄBÄ8\ÄBÄÍ,ÇDefine RelationÄBÄÏBÄ\
BÄÌBÄ\
BÄÓ\Ä\ÄBÄêBÄBÄ˛lÉused to define a relation.BÄÙ\Ä\ÄBÄaBÄ˜lÅDefinitionBÄª\ÄBÄΩ\ÄÄ*ÏÅRelation Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨ÑName of the relation to be defined.ÄBÄƒBÄ≈BÄÕBÄ—BÄ’BÄ⁄BÄﬁBÄ‚BÄ*¨ÑGive parameters for DEFINE RELATION:BÄ˘BÄT
\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ_
BÄÏBÄ\
BÄÌBÄ\
BÄÓBÄ`
BÄ˛BÄb
BÄÙBÄc
BÄªBÄf
BÄ˘BÄT
BÄÃÄ\ÄBÄ^BÄ‡ÉÅDEFINE-VIEWÄÄÎÄ
ÜÄA
FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄr
\ÄBÄmÅVIEWNAMEÇVIEW-DEFINITIONÄBÄ°BÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛lûDefine views on the relations.

   VIEW-NAME - Name of the view.
   VIEW-DEF  - Definition of the view.
   DOCUMENTATION - Describes the view.  (DEFINE-VIEW VIEWNAME VIEW-DEF &REST KEYWORD-LIST &KEY &OPTIONAL DOCUMENTATION &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿ÅDEFVIEWÄ¿BÄ≠“BÄﬂ“BÄÇ
íP@¡Pˇ€PPÅQÇQÉQ¢ö@ëÅQÇQÉQ	úOÄÉ
BÄr
Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄs
\ÄBÄ8\ÄBÄÍ¨ÅDefine ViewÄBÄÏBÄá
BÄÌBÄá
BÄÓ\Ä\ÄBÄêBÄt	BÄ˛ÏÇUsed to define a view.BÄÙ\Ä\ÄBÄaBÄ˜lÅDefinitionBÄª\ÄÄΩ\ÄÄ*lÅView Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨ÉSpecify a name for the view.BÄƒ\ÄÄ*,ÇView Definition:BÄ¿BÄ«BÄ¡\ÄBÄ˛lÑSpecify a definition for the view.BÄƒ\ÄÄ*¨ÇView Documentation:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛¨ÑSpecify documentation for the view.ÄBÄƒBÄ*,ÑGive parameters for DEFINE VIEW:BÄ˘BÄÄ
\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄä
BÄÏBÄá
BÄÌBÄá
BÄÓBÄã
BÄ˛BÄ

BÄÙBÄé
BÄªBÄë
BÄ˘BÄÄ
BÄÃÄ\ÄBÄ^BÄ‡ÇDEFINE-ATTRIBUTEÄÎÄ
ÜÄAFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ•
\ÄBÄmBÄ∏BÄP
BÄ¢BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛Ï¥Add a new attribute to a relation.
    All its tuples will get the default value of the attribute for the attribute value.

   RELATION-NAME - Name of the relation.
   ATTRIBUTE-DESCRIPTOR - List of attributes and their descriptions.
   KEY           - If the key for this relation is to be changed, specify it.  (DEFINE-ATTRIBUTE RELATION-NAME ATTRIBUTE-DESCRIPTOR &REST KEYWORD-LIST &KEY &OPTIONAL KEY &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿BÄ¶
¿BÄ¢¿BÄ≠“BÄﬂ“BÄ¶
íPA¡Pˇ€PPÅQÇQPÉQí@√¢	öAëÅQÇQ@Q
úOÄ¥
BÄ•
Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ¶
\ÄBÄ8\ÄBÄÍ,ÇDefine AttributeBÄÏBÄ∏
BÄÌBÄ∏
BÄÓ\Ä\ÄBÄêBÄÚBÄ˛¨ÑUsed to add attributes to relations.BÄÙ\Ä\ÄBÄaBÄ˜lÅDefinitionBÄª\ÄÄΩ\ÄÄ*,ÇRelation name: ÄBÄ¿BÄ~BÄ¡\ÄBÄ˛làThe name of the relation to which new attributes are to be added.ÄBÄƒBÄ≈\ÄÄ*ÏÄKey: ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛,
New key for the relation if it is to be different from the previous value. Specify a list of attributes.BÄƒBÄ*ÏÑGive parameters for DEFINE ATTRIBUTE:ÄBÄ˘BÄ≤
\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄª
BÄÏBÄ∏
BÄÌBÄ∏
BÄÓBÄº
BÄ˛BÄæ
BÄÙBÄø
BÄªBÄ¬
BÄ˘BÄ≤
BÄÃÄ\ÄBÄ^BÄ‡√ÅMODIFY-TUPLESÄÄÎÄ#ÜÄAHFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ“
\ÄBÄmBÄ†ÉÅWHERE-CLAUSEBÄ∫ÍÄVALUESBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛l∑The values of the tuples in a relation can be modified using this function.

   RELATION  - Name of the relation whose tuples are to be modified.
   ATTRIBUTE - List of attributes which are to be modified.
   VALUE     - Corresponding list of values to be used in modifying the above attributes.
   WHERE     - Selection criterion to be used.  (MODIFY-TUPLES RELATION &REST KEYWORD-LIST &KEY &OPTIONAL ATTRIBUTE VALUE WHERE &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿√ÄMODIFY¿√ÄWHEREÄ¿BÄ√¿BÄ›
¿BÄ≠“BÄﬂ“BÄ„
íPA¡Pˇ€PPÅQPÇQPÉQ	PÑQ
≤@√
ööAëÅQ@QîOÄÂ
BÄ“
Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ”
\ÄBÄ8\ÄBÄÍÏÅModify TuplesÄBÄÏBÄÈ
BÄÌBÄÈ
BÄÓ\Ä\ÄBÄ•Ä@MBÄ˛¨ÑUsed to modify tuples in a relation.BÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄBÄΩ\ÄÄ*lÅRelation: BÄ¿BÄ~BÄ¡\ÄBÄ˛ÏÜSpecify the relation whose tuples are to be modified.ÄBÄƒ\ÄÄ*ÏÅWhere clause: BÄ¿BÄÀBÄ¡\ÄBÄ˛ÏÉProvide a selection criteria.ÄBÄƒ\ÄÄ*¨ÅAttributes: BÄ¿BÄéBÄ¡\ÄBÄ˛làSpecify a list of attributes in the above relation to be modified.BÄƒ\ÄÄ*,ÅValues: BÄ¿BÄ–BÄ¡\ÄBÄ˛ÏàSpecify a corresponding list of values to modify the above attributes.BÄƒBÄ*ÏÑGive parameters for MODIFY TUPLES ==>ÄBÄ˘BÄ·
\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄÏ
BÄÏBÄÈ
BÄÌBÄÈ
BÄÓBÄÌ
BÄ˛BÄ
BÄÙBÄÒ
BÄªBÄÙ
BÄ˘BÄ·
BÄÃÄ\ÄBÄ^BÄ‡√ÅDELETE-TUPLESÄÄÎÄ	ÜÄ@ƒFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ\ÄBÄmBÄ†BÄ‹
BÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛Ï£Deletes the tuples which satisfy the WHERE clause from the specified relation.

   RELATION - Name of the relation from which the tuples are to be deleted.
   WHERE    - Selection criterion to be used.  (DELETE-TUPLES RELATION &REST KEYWORD-LIST &KEY &OPTIONAL WHERE &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄ¿BÄ‰
¿BÄ≠“BÄﬂ“BÄíP@¡Pˇ€PPÅQPÇQíö	ö@ëÅQPÇQí
îOÄBÄÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ\ÄBÄ8\ÄBÄÍÏÅDelete TuplesÄBÄÏBÄBÄÌBÄBÄÓ\ÄBÄfBÄ˛¨ÑUsed to delete tuples in a relation.BÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄBÄΩ\ÄÄ*lÅRelation: BÄ¿BÄ~BÄ¡\ÄBÄ˛lÜSpecify a relation whose tuples are to be deleted.BÄƒ\ÄÄ*ÏÅWhere clause: BÄ¿BÄ:BÄ¡\ÄBÄ˛,ÜDeletes the tuples which satisfy this condition.BÄƒBÄ*ÏÑGive parameters for DELETE TUPLES ==>ÄBÄ˘BÄ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ!BÄÏBÄBÄÌBÄBÄÓBÄ"BÄ˛BÄ#BÄÙBÄ$BÄªBÄ'BÄ˘BÄBÄÃÄ\ÄBÄ^BÄ‡ÇRETRIEVE-TUPLESÄÄÎÄ]ÜÄ‡FÄ>¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ7\ÄBÄmBÄ†BÄ∫BÄ‹
BÄNBÄ†BÄ°BÄ¢BÄ£BÄ¨BÄOBÄPBÄQBÄﬂBÄRBÄSBÄTBÄ¬BÄUBÄVBÄWBÄ.BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏøORetrieve some tuples from a relation satisying a where clause.

   RELATION-NAME        - Name of the relation whose tuples are to be retrieved.
   WHERE                - Criterion to be used in selecting the tuples.
   PROJECT              - List of attributes to be projected in the result.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   INDEX-NAME           - Name of the index to use in the retrieval.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   WIDE                 - If T, result is printed out in Attribute: value format rather than as a table.
   NUMBER               - If WIDE is T, this keyword specifies the number of attributes per line.
   OUTPUT               - If the result is to be sent to a file, specify the file-name.
   PRINT                - If NIL, the result is not printed.
   QPRINT               - If T, the result is printed without formatting.
   QUICK-SORT           - Specifies the attributes to sort the result on.
   SORT                 - If any domain-specific, user-defined sort mechanism is to be used, this keyword
                          can be used.
   STREAM               - Specify the window to which the output is to be sent, if it is different than the
                          the *standard-output* or RTMS-interface.
   TUPLES               - If T, the resultant tuples are returned.
   UNIQUE               - If T, only unique tuples are retrieved.   (RETRIEVE RELATION-NAME &REST KEYWORD-LIST &KEY &OPTIONAL DIRECTORY DOCUMENTATION FORMAT IMPLEMENTATION-TYPE INDEX-NAME INTO KEY NUMBER OUTPUT PRINT PROJECT QPRINT QUICK-SORT SORT STREAM STORAGE-STRUCTURE TUPLES UNIQUE WHERE WIDE &ALLOW-OTHER-KEYS)¿ÜÄÀÄBÄõëBÄ‹¿lÄ~S¿BÄ®¿ÅPROJECTÄ¿BÄ‰
¿BÄN¿BÄ†¿BÄ°¿BÄ¢¿BÄ£¿BÄ¨¿BÄO¿BÄ^¿BÄQ¿BÄﬂ¿BÄR¿BÄ_¿BÄT¿BÄ¬¿BÄ`¿BÄV¿BÄW¿BÄ.¿BÄ≠“BÄﬂ“BÄ®íPA¡Pˇ€PPÅQPÇQ±‰ˇ€¸ÇQ	PÉQ
PÑQPÖQPÜQPáQPàQP QPä?BPãQPåQP
QPéQPèQPêQPëQPíQPìQPîQPïQ(J∫@√ööAëÅQ@QîOÄHBÄ7Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ8\ÄBÄ8\ÄBÄÍ,ÇRetrieve TuplesÄBÄÏBÄLBÄÌBÄLBÄÓ\Ä•Ä@RBÄ˛ÏÑUsed to Retrieve tuples in a relation.BÄÙ\Ä\ÄBÄaBÄ˜lÅOperatorsÄBÄª\ÄBÄΩBÄq\ÄÄ*¨ÅAttributes: BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏáProvide a list of attributes. If not all attributes all used.ÄBÄƒBÄ˘
BÄuBÄ—BÄyBÄ⁄BÄﬁBÄ‚BÄ}BÄÅBÄÖBÄÕBÄ BÄ
BÄíBÄñBÄöBÄûBÄ¢\ÄÄ*¨ÅIndex-name:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛làIf the data is to come from an index instead of the base relation.BÄƒBÄ*,ÖGive parameters for RETRIEVE TUPLES ==>ÄBÄ˘BÄD\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄOBÄÏBÄLBÄÌBÄLBÄÓBÄPBÄ˛BÄRBÄÙBÄSBÄªBÄVBÄ˘BÄDBÄÃÄ\ÄBÄ^BÄ‡p¿BÄuÏÄSELECTÄÎÄ ZÜÄ‡ FÄ=¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄf\ÄBÄmBÄ†BÄ‹
BÄNBÄ†BÄ°BÄ¢BÄ£BÄ¨BÄOBÄPBÄQBÄﬂBÄRBÄSBÄTBÄ¬BÄUBÄVBÄWBÄ.BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛Ïø˝Same as Retrieve except that all attributes are retrieved.

   RELATION-NAME        - Name of the relation whose tuples are to be retrieved.
   WHERE                - Criterion to be used in selecting the tuples.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   WIDE                 - If T, result is printed out in Attribute: value format rather than as a table.
   NUMBER               - If WIDE is T, this keyword specifies the number of attributes per line.
   OUTPUT               - If the result is to be sent to a file, specify the file-name.
   PRINT                - If NIL, the result is not printed.
   QPRINT               - If T, the result is printed without formatting.
   QUICK-SORT           - Specifies the attributes to sort the result on.
   SORT                 - If any domain-specific, user-defined sort mechanism is to be used, this keyword
                          can be used.
   STREAM               - Specify the window to which the output is to be sent, if it is different than the
                          the *standard-output* or RTMS-interface.
   TUPLES               - If T, the resultant tuples are returned.
   UNIQUE               - If T, only unique tuples are retrieved.  (SELECT-TUPLES RELATION-NAME &REST KEYWORD-LIST &KEY &OPTIONAL DIRECTORY DOCUMENTATION FORMAT IMPLEMENTATION-TYPE INTO KEY NUMBER OUTPUT PRINT QPRINT QUICK-SORT SORT STREAM STORAGE-STRUCTURE TUPLES UNIQUE WHERE WIDE &ALLOW-OTHER-KEYS)¿ÜÄÇ™ÄBÄõëBÄ‹¿lÄ~S¿√ÅSELECT-TUPLESÄ¿BÄ‰
¿BÄN¿BÄ†¿BÄ°¿BÄ¢¿BÄ£¿BÄ¨¿BÄO¿BÄ^¿BÄQ¿BÄﬂ¿BÄR¿BÄ_¿BÄT¿BÄ¬¿BÄ`¿BÄV¿BÄW¿BÄ.¿BÄ≠“BÄﬂ“BÄG¿BÄ“BÄ®íPA¡Pˇ€PPÅQPÇQ	PÉQ
PÑQPÖQPÜQPáQPàQP ?BPäQPãQPåQP
QPéQPèQPêQPëQPíQPìQPîQ&J∫@√ööAëÅQPˇ€@QöîOÄxBÄfÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄh\ÄBÄ8\ÄBÄÍÏÄSelectBÄÏBÄ|BÄÌBÄ|BÄÓ\Ä\ÄBÄBÄLÄ˛¨ÑUsed to Select tuples in a relation.BÄÙ\Ä\ÄBÄaBÄ˜lÅOperatorsÄBÄª\ÄBÄΩBÄqBÄ˘
BÄuBÄ—BÄyBÄ⁄BÄﬁBÄ‚BÄ}BÄÅBÄÖBÄÕBÄ BÄ
BÄíBÄñBÄöBÄûBÄ¢BÄ[BÄ*ÏÑGive parameters for SELECT TUPLES ==>ÄBÄ˘BÄt\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄBÄÏBÄ|BÄÌBÄ|BÄÓBÄÄBÄ˛BÄÇBÄÙBÄÉBÄªBÄÜBÄ˘BÄtBÄÃÄ\ÄBÄ^BÄ‡BÄGÄÎÄ\ÜÄ‡FÄ=¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄé\ÄBÄmBÄ†BÄ∫BÄNBÄ†BÄ°BÄ¢BÄ£BÄ¨BÄOBÄPBÄQBÄﬂBÄRBÄSBÄTBÄ¬BÄUBÄVBÄWBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛Ïø#Same as Retrieve except that all tuples are retrieved.

   RELATION-NAME        - Name of the relation whose tuples are to be retrieved.
   PROJECT              - List of attributes to be projected in the result.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   INDEX-NAME           - Name of the index to use in the retrieval.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   WIDE                 - If T, result is printed out in Attribute: value format rather than as a table.
   NUMBER               - If WIDE is T, this keyword specifies the number of attributes per line.
   OUTPUT               - If the result is to be sent to a file, specify the file-name.
   PRINT                - If NIL, the result is not printed.
   QPRINT               - If T, the result is printed without formatting.
   QUICK-SORT           - Specifies the attributes to sort the result on.
   SORT                 - If any domain-specific, user-defined sort mechanism is to be used, this keyword
                          can be used.
   STREAM               - Specify the window to which the output is to be sent, if it is different than the
                          the *standard-output* or RTMS-interface.
   TUPLES               - If T, the resultant tuples are returned.
   UNIQUE               - If T, only unique tuples are retrieved.  (PROJECT RELATION-NAME &REST KEYWORD-LIST &KEY &OPTIONAL DIRECTORY DOCUMENTATION FORMAT IMPLEMENTATION-TYPE INDEX-NAME INTO KEY NUMBER OUTPUT PRINT PROJECT QPRINT QUICK-SORT SORT STREAM STORAGE-STRUCTURE TUPLES UNIQUE WIDE &ALLOW-OTHER-KEYS)¿ÜÄäÄBÄõëBÄ‹¿lÄ~S¿BÄG¿BÄN¿BÄ†¿BÄ°¿BÄ¢¿BÄ£¿BÄ¨¿BÄO¿BÄ^¿BÄQ¿BÄﬂ¿BÄR¿BÄ_¿BÄT¿BÄ¬¿BÄ`¿BÄV¿BÄW¿BÄ≠“BÄﬂ“BÄ‰
¿BÄ“BÄ®íPA¡Pˇ€PPÅQPÇQ±‰ˇ€¸ÇQPÉQ	PÑQ
PÖQPÜQPáQPàQP ?BPäQPãQPåQP
QPéQPèQPêQPëQPíQPìQ$J∫@√ööAëÅQPˇ›@QöîOÄùBÄéÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄG\ÄBÄ8\ÄBÄÍ,ÅProjectÄBÄÏBÄ°BÄÌBÄ°BÄÓ\Ä\ÄBÄBÄkBÄ˛ÏÑUsed to Project tuples in a relation.ÄBÄÙ\Ä\ÄBÄaBÄ˜lÅOperatorsÄBÄª\ÄBÄΩBÄqBÄWBÄuBÄ—BÄyBÄ⁄BÄﬁBÄ‚BÄ}BÄÅBÄÖBÄÕBÄ BÄ
BÄíBÄñBÄöBÄûBÄ¢BÄ*ÏÑGive parameters for PROJECT TUPLES ==>BÄ˘BÄö\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ§BÄÏBÄ°BÄÌBÄ°BÄÓBÄ•BÄ˛BÄßBÄÙBÄ®BÄªBÄ´BÄ˘BÄöBÄÃÄ\ÄBÄ^BÄ‡CÇCOMMIT-TRANSACTIONÄÎÄ
 ÜÄAFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ≥\ÄBÄm√ÄTRANSÄBÄ†BÄûBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛¨≠Execute the database calls in a transaction.

   TRANSACTION - Name of the transaction to be commited.
   DIRECTORY   - Name of the directory in which this transaction can be found, if not in memory.
   PATHNAME    - Name of the file in which it can be found.  (COMMIT-TRANSACTION TRANSACTION &REST KEYWORD-LIST &KEY &OPTIONAL DIRECTORY PATHNAME &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿BÄ¥¿BÄ†¿BÄû¿BÄ≠“BÄﬂ“BÄ¥íPA¡Pˇ€PPÅQPÇQPÉQ	¢@√	ö
öAëÅQ@QîOÄ√BÄ≥Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ¥\ÄBÄ8\ÄBÄÍlÇCommit TransactionBÄÏBÄ«BÄÌBÄ«BÄÓ\Ä\ÄBÄ•Ä CBÄ˛¨áCommit a transaction - execute all the database calls in it.BÄÙ\Ä\ÄBÄaBÄ˜lÅOperatorsÄBÄª\ÄÄΩ\ÄÄ*lÉName of the transaction :ÄBÄ¿BÄÜBÄ¡\ÄBÄ˛¨ÑThe name of an existing transaction.BÄƒ\ÄÄ*ÏÇName of the directory:BÄ¿BÄüBÄ¡\ÄBÄ˛¨åName of the directory which contains the transaction file, if the transaction is not in the memory.ÄBÄƒ\ÄÄ*lÅPathname:ÄBÄ¿BÄñBÄ¡\ÄBÄ˛ÏéIf the transaction is not in memory, provide the pathname for the transaction file. It defaults to <transaction>.lisp.BÄƒBÄ*ÏÑGive parameters for COMMIT TRANSACTIONBÄ˘BÄ¡\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ BÄÏBÄ«BÄÌBÄ«BÄÓBÄÀBÄ˛BÄŒBÄÙBÄœBÄªBÄ“BÄ˘BÄ¡BÄÃÄ\ÄBÄ^BÄ‡ÉÄJOINÄÎÄDÜÄCàFÄ.¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄÊ\ÄBÄmBÄNÉÄFROMBÄGBÄ‰
BÄ¬BÄﬂBÄ†BÄ°BÄ¢BÄ£BÄ¨BÄTBÄWBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛Ïø3This function provides the capability to combine two relations into a new relation
   in which the tuples which are to participate in the operation are selected
   by a where clause.

   FROM                 - A list consisting of the relations to be joined.
   PROJECT              - This clause specifies the attributes that are to be in the resultant relation
                          and their associated names in that new relation. It should be of the form
                          (<[relation-name.]attribute-name>). The optional part relation-name can be
                          skipped if the attribute is unique in one of the two relations being joined.
                          If the keyword FROM is not specified, this clause should contain the names
                          of the relations to be joined. Also, if * is given instead of the attribute-name
                          it indicates that RTMS should use all the attributes in that relation.
   WHERE                - Can be used to perform theta-joins. It is a condition used in joining the relations.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   PRINT                - If NIL, the resultant relation will not be printed out.
   TUPLES               - If T, the resultant tuples will be returned.
   UNIQUE               - If T, only unique tuples will be part of the resultant relation.  (JOIN &REST KEYWORD-LIST &KEY FROM &KEY &OPTIONAL PROJECT WHERE INTO DIRECTORY DOCUMENTATION FORMAT IMPLEMENTATION-TYPE KEY STORAGE-STRUCTURE PRINT TUPLES UNIQUE &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄÁ¿BÄ¿BÄG¿BÄN¿BÄ¬¿BÄﬂ¿BÄ†¿BÄ°¿BÄ¢¿BÄ£¿BÄ¨¿BÄT¿BÄ‰
¿BÄW¿BÄ≠“BÄﬂ“BÄ“√ÅJOIN-INTERNALÄíPA¡Pˇ€PPPÇQPÉQ	PÅQ
PÖQPÜQPáQPàQP QPäQPãQPåQPÑQP
QJ∫@√¢öAëPÇQ@QöåOÄ˜BÄÊÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄÁ\ÄBÄ8\ÄBÄÍ¨ÄJoinBÄÏBÄ˚BÄÌBÄ˚BÄÓ\Ä•Ä JBÄ˛,ÉUsed to join relations.ÄBÄÙ\Ä\ÄBÄaBÄ˜lÅOperatorsÄBÄª\ÄBÄΩ\ÄÄ*lÇOutput relation :ÄBÄ¿BÄ‘BÄ¡\ÄBÄ˛ÏéIf not provided, the result of JOIN is stored in a temporary relation unless only the resultant tuples are requested.ÄBÄƒ\ÄÄ*ÏÄFROM :BÄ¿BÄ‡BÄ¡\ÄBÄ˛ÏÖSpecify a list of two relations to be joined.ÄBÄƒ\ÄÄ*lÅProject :ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛,ñThis gives the attributes in the output relation. Example: (rel1.* a3 (rel2.a1 a4)) ==> All the attributes in rel1, attribute A3 of rel2 and atribute A1 of rel2 renamed as A4.ÄBÄƒ\ÄÄ*,ÅWhere :ÄBÄ¿BÄÿBÄ¡\ÄBÄ˛lèThe join clause using the theta-operators. It is a where clause consisting of attributes from the relations being joined.ÄBÄƒ\ÄÄ*,ÅTuples?ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛,ëSpecify if the resultant tuples be returned rather than inserted in a relation. The following parameters can be ignored if this is true.BÄ¶BÄÕBÄ—BÄyBÄ⁄BÄﬁBÄ‚BÄíBÄ¢BÄ*,ÉGive parameters for JOINBÄ˘BÄÙ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ˛BÄÏBÄ˚BÄÌBÄ˚BÄÓBÄˇBÄ˛BÄBÄÙBÄBÄªBÄBÄ˘BÄÙBÄÃÄ\ÄBÄ^BÄ‡ÇDESTROY-DATABASEÄÎÄ	ÜÄ@»FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ!\ÄBÄmBÄ°BÄÅBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛,ßDelete the specified database from memory and all the corresponding files from
   disk if the keyword DISK is T.

   DATABASE - Name of the database to be destroyed.
   DISK     - If T, all the relevant files will be deleted.  (DESTROY-DATABASE DATABASE &REST KEYWORD-LIST &KEY &OPTIONAL DISK &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄ"¿BÄÅ¿BÄ≠“BÄﬂ“BÄ"íPA¡Pˇ€PPÅQPÇQí@√ö	öAëÅQ@Q
îOÄ0BÄ!Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ"\ÄBÄ8\ÄBÄÍ,ÇDestroy DatabaseBÄÏBÄ4BÄÌBÄ4BÄÓ\Ä\Ä•Ä KBÄêBÄ˛lÉUsed to destroy databasesÄBÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄBÄΩ\ÄÄ*ÏÅDatabase Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏÑName of the database to be destroyed.ÄBÄƒ\ÄÄ*ÏÇDelete from the DISK:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛Ï IF YES all the files pertaining to this database are deleted but NOT EXPUNGED.BÄ¶BÄ*ÏÑGive parameters for DESTROY DATABASE:ÄBÄ˘BÄ.\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ7BÄÏBÄ4BÄÌBÄ4BÄÓBÄ8BÄ˛BÄ;BÄÙBÄ<BÄªBÄ?BÄ˘BÄ.BÄÃÄ\ÄBÄ^BÄ‡√ÅDESTROY-DOMAINÄÎÄ
ÜÄ@
ÑFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄO\ÄBÄmBÄ‰BÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏèDestroys the domain definition but keeps the domain predicate to handle previously defined data.  (DESTROY-DOMAIN DOMAIN-NAME)ÄBÄõëBÄ‹¿lÄ~S¿BÄP¿BÄ≠“BÄﬂ“BÄPíP@¡Pˇ€PPÅQíö@ëÅQ	åOÄ]BÄOÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄP\ÄBÄ8\ÄBÄÍÏÅDestroy DomainBÄÏBÄaBÄÌBÄaBÄÓ\Ä•Ä`KBÄ˛,ÉUsed to destroy domains.BÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄBÄΩ\ÄÄ*¨ÅDomain Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨ÑName of the domain to be destroyed.ÄBÄƒBÄ*¨ÑGive parameters for DESTROY DOMAIN:ÄBÄ˘BÄ[\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄdBÄÏBÄaBÄÌBÄaBÄÓBÄeBÄ˛BÄgBÄÙBÄhBÄªBÄkBÄ˘BÄ[BÄÃÄ\ÄBÄ^BÄ‡√ÇDESTROY-IMPLEMENTATIONÄÎÄ
ÜÄ@
ÑFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄw\ÄBÄmBÄBÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛,óDestroys implementation type definition but keeps the accessor functions to handle previously defined relations using this implementation.  (DESTROY-IMPLEMENTATION IMPLEMENTATION-NAME)ÄBÄõëBÄ‹¿lÄ~S¿BÄx¿BÄ≠“BÄﬂ“BÄxíP@¡Pˇ€PPÅQíö@ëÅQ	åOÄÖBÄwÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄx\ÄBÄ8\ÄBÄÍÏÇDestroy ImplementationBÄÏBÄ BÄÌBÄ BÄÓ\Ä\ÄBÄ:BÄÕBÄ˛,ÑUsed to destroy implementations.BÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄBÄΩ\ÄÄ*¨ÇImplementation Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨ÖName of the implementation to be destroyed.ÄBÄƒBÄ*¨ÖGive parameters for DESTROY IMPLEMENTATION:ÄBÄ˘BÄÉ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄåBÄÏBÄ BÄÌBÄ BÄÓBÄ
BÄ˛BÄèBÄÙBÄêBÄªBÄìBÄ˘BÄÉBÄÃÄ\ÄBÄ^BÄ‡√ÅDESTROY-INDEXÄÄÎÄ
ÜÄ@
ƒFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄü\ÄBÄmBÄ∏BÄ.BÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛,üDestroy the specified index which is defined on the specified relation.

   RELATION-NAME - The name of the relation upon which the relation is defined.
   INDEX-NAME - The name of the index to be deleted.  (DESTROY-INDEX RELATION-NAME INDEX-NAME)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄ†¿BÄ≠“BÄﬂ“BÄ†íP@¡Pˇ€PPÅQÇQöö@ëÅQÇQ	îOÄ≠BÄüÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ†\ÄBÄ8\ÄBÄÍÏÅDestroy IndexÄBÄÏBÄ±BÄÌBÄ±BÄÓ\Ä\ÄBÄ:BÄBBÄ˛,ÉUsed to destroy indices.BÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄBÄΩ\ÄÄ*ÏÅRelation Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨àName of the relation on which the index to be destroyed is defined.ÄBÄƒ\ÄÄ*¨ÅIndex Name:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛lÑName of the index to be destroyed.BÄƒBÄ*lÑGive parameters for DESTROY INDEX:BÄ˘BÄ´\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ¥BÄÏBÄ±BÄÌBÄ±BÄÓBÄµBÄ˛BÄ∑BÄÙBÄ∏BÄªBÄªBÄ˘BÄ´BÄÃÄ\ÄBÄ^BÄ‡CÉDESTROY-STORAGE-STRUCTUREÄÄÎÄ
ÜÄ@
ÑFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄÀ\ÄBÄmBÄ0BÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛,óDestroys storage structure definition but keeps the accessor functions to handle previously defined relations using this structure.  (DESTROY-STORAGE-STRUCTURE STORAGE-STRUCTURE-NAME)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄÃ¿BÄ≠“BÄﬂ“BÄÃíP@¡Pˇ€PPÅQíö@ëÅQ	åOÄŸBÄÀÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄÃ\ÄBÄ8\ÄBÄÍlÉDestroy Storage StructureÄBÄÏBÄ›BÄÌBÄ›BÄÓ\Ä\ÄBÄ:BÄLÄ˛¨ÑUsed to destroy storage structures.ÄBÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄBÄΩ\ÄÄ*,ÉStorage structure name:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛ÏÖName of the storage structure to be destroyed.BÄƒBÄ*ÏÖGive parameters for DESTROY STORAGE STRUCTURE:BÄ˘BÄ◊\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ‡BÄÏBÄ›BÄÌBÄ›BÄÓBÄ·BÄ˛BÄ„BÄÙBÄ‰BÄªBÄÁBÄ˘BÄ◊BÄÃÄ\ÄBÄ^BÄ‡ÉÅDESTROY-VIEWÄÎÄ
ÜÄ@
ÑFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄÛ\ÄBÄmBÄb	BÄ:BÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛,áDestroys the view from memory.  (DESTROY-VIEW VIEW-NAME)ÄBÄõëBÄ‹¿lÄ~S¿BÄÙ¿BÄ≠“BÄﬂ“BÄÙíP@¡Pˇ€PPÅQíö@ëÅQ	åOÄBÄÛÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄÙ\ÄBÄ8\ÄBÄÍ¨ÅDestroy ViewBÄÏBÄBÄÌBÄBÄÓ\Ä\ÄBÄ:BÄt	BÄ˛ÏÇUsed to destroy views.BÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄBÄΩ\ÄÄ*lÅView name:BÄ¿BÄ:BÄ¡\ÄBÄ˛lÑName of the view to be destroyed.ÄBÄƒBÄ*lÑGive parameters for DESTROY VIEW:ÄBÄ˘BÄˇ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄBÄÏBÄBÄÌBÄBÄÓBÄ	BÄ˛BÄBÄÙBÄBÄªBÄBÄ˘BÄˇBÄÃÄ\ÄBÄ^BÄ‡ÇDESTROY-RELATIONÄÎÄ	ÜÄ@»FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ\ÄBÄmBÄ†BÄÅBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛l¶Deletes the specified relation from the active database.
   Deletes all the files on disk if keyword DISK is t.

   RELATION - Name of the relation to be destroyed.
   DISK     - If T, the relevant files will be deleted.  (DESTROY-RELATION RELATION &REST KEYWORD-LIST &KEY &OPTIONAL DISK &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿BÄ¿BÄÅ¿BÄ≠“BÄﬂ“BÄíPA¡Pˇ€PPÅQPÇQí@√ö	öAëÅQ@Q
îOÄ*BÄÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ\ÄBÄ8\ÄBÄÍ,ÇDestroy RelationBÄÏBÄ.BÄÌBÄ.BÄÓ\Ä\ÄBÄ:BÄBÄ˛lÉUsed to destroy relationsÄBÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄBÄΩ\ÄÄ*ÏÅRelation Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛ÏÑName of the relation to be destroyed.ÄBÄƒ\ÄÄ*ÏÇDelete from the DISK:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛¨ IF YES the file corresponding to this relation is deleted but NOT EXPUNGED.ÄBÄ¶BÄ*ÏÑGive parameters for DESTROY RELATION:ÄBÄ˘BÄ(\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ1BÄÏBÄ.BÄÌBÄ.BÄÓBÄ2BÄ˛BÄ4BÄÙBÄ5BÄªBÄ8BÄ˘BÄ(BÄÃÄ\ÄBÄ^BÄ‡CÇDESTROY-ATTRIBUTEÄÄÎÄ
 ÜÄAFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄH\ÄBÄmBÄ†BÄ√BÄ¢BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛,ØAttributes in a relation can be deleted using this function.

   RELATION-NAME - Name of the relation from which the attributes are to be deleted.
   ATTRIBUTE     - List of attributes to be destroyed.
   KEY           - List of attributes to form the new key, if so desired.  (DESTROY-ATTRIBUTE RELATION-NAME &REST KEYWORD-LIST &KEY &OPTIONAL ATTRIBUTE KEY &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿BÄI¿BÄ√¿BÄ¢¿BÄ≠“BÄﬂ“BÄIíPA¡Pˇ€PPÅQPÇQPÉQ	¢@√	ö
öAëÅQ@QîOÄWBÄHÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄI\ÄBÄ8\ÄBÄÍlÇDestroy AttributeÄBÄÏBÄ[BÄÌBÄ[BÄÓ\Ä\ÄBÄ:BÄÚBÄ˛lÖUsed to destroy attributes from relationsÄBÄÙ\Ä\ÄBÄaBÄ˜¨ÅManipulationBÄª\ÄÄΩ\ÄÄ*ÏÅRelation Name:BÄ¿BÄ:BÄ¡\ÄBÄ˛,àName of the relation from which attributes are to be destroyed.ÄBÄƒ\ÄÄ*¨ÅAttributes:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛ÏÉList of attributes to destroy.BÄƒ\ÄÄ*¨ÄKey:BÄ¿BÄ:BÄ¡\ÄBÄ˛,èNew key for the relation if it is to be different from the previous value or if any of the key attributes are destroyed.BÄƒBÄ*ÏÑGive parameters for DESTROY ATTRIBUTE:BÄ˘BÄU\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ^BÄÏBÄ[BÄÌBÄ[BÄÓBÄ_BÄ˛BÄaBÄÙBÄbBÄªBÄeBÄ˘BÄUBÄÃÄ\ÄBÄ^BÄ‡ÍÄUNIONÄÄÎÄ9ÜÄCFÄ'¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄy\ÄBÄmBÄBÄNBÄ¬BÄﬂBÄ†BÄ°BÄ¢BÄ£BÄ¨BÄTBÄWBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏøsUnion of tuples in two relations.

   FROM                 - This clause specifies the relations to participate in the UNION operation.
                          In addition, RTMS allows users to specify the attributes in these relations to
                          participate in the operation as well as a where-clause to specify the tuples.
                          It should be of the format: (RelA [(PROJECT <attrA> WHERE where-clause-A)] RelB
                          [(PROJECT <attrB> WHERE where-clause-B)]) where attrA indicates attributes in
                          relation relA and where-clause-A indicates a where-clause involving the attributes
                          in relation relA.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   PRINT                - If NIL, the resultant relation will not be printed out.
   TUPLES               - If T, the resultant tuples will be returned.
   UNIQUE               - If T, only unique tuples will be part of the resultant relation.  (RELATION-UNION &REST KEYWORD-LIST &KEY &OPTIONAL FROM INTO DIRECTORY DOCUMENTATION FORMAT IMPLEMENTATION-TYPE STORAGE-STRUCTURE KEY PRINT TUPLES UNIQUE &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿√ÅRELATION-UNION¿BÄN¿BÄ¿BÄ¬¿BÄﬂ¿BÄ†¿BÄ°¿BÄ¢¿BÄ£¿BÄ¨¿BÄT¿BÄW¿BÄ≠“BÄﬂ“BÄàíPA¡Pˇ€PPPÇQPÅQ	PÉQ
PÑQPÖQPÜQPáQPàQP QPäQPãQJ∫@√íöAë@QåOÄ BÄyÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄz\ÄBÄ8\ÄBÄÍÏÄUnionÄBÄÏBÄ
BÄÌBÄ
BÄÓ\Ä\Ä•Ä O•Ä UBÄ˛ÏÖUsed to form union of two compatible relationsBÄÙ\Ä\ÄBÄaBÄ˜lÅOperatorsÄBÄª\ÄBÄΩ\ÄÄ*ÏÇList of two relations:BÄ¿BÄ:BÄ¡\ÄBÄ˛¨§List of the names of two relations which will take part in the relation union operation. The attributes to be projected and a where clause can be specified for each relation using keywords. For instance, (REL1 (PROJECT <attr> WHERE <where-claue>) REL2 (WHERE <where-clause> PROJECT <attr>)).ÄBÄƒBÄu\ÄÄ*,ÅTuples?ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛,ëSpecify if the resultant tuples be returned rather than inserted in a relation. The following parameters can be ignored if this is true.BÄ¶BÄÕBÄ—BÄyBÄ⁄BÄﬁBÄ‚BÄíBÄ¢BÄ*ÏÖParameters for the set-union of two relationsÄBÄ˘BÄÜ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄêBÄÏBÄ
BÄÌBÄ
BÄÓBÄëBÄ˛BÄïBÄÙBÄñBÄªBÄôBÄ˘BÄÜBÄÃÄ\ÄBÄ^BÄ‡CÅDIFFERENCEÄÎÄ9ÜÄCFÄ'¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ©\ÄBÄmBÄBÄNBÄ¬BÄﬂBÄ†BÄ°BÄ¢BÄ£BÄ¨BÄTBÄWBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛Ïø|Difference of the tuples in two relations.

   FROM                 - This clause specifies the relations to participate in the DIFFERENCE operation.
                          In addition, RTMS allows users to specify the attributes in these relations to
                          participate in the operation as well as a where-clause to specify the tuples.
                          It should be of the format: (RelA [(PROJECT <attrA> WHERE where-clause-A)] RelB
                          [(PROJECT <attrB> WHERE where-clause-B)]) where attrA indicates attributes in
                          relation relA and where-clause-A indicates a where-clause involving the attributes
                          in relation relA.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   PRINT                - If NIL, the resultant relation will not be printed out.
   TUPLES               - If T, the resultant tuples will be returned.
   UNIQUE               - If T, only unique tuples will be part of the resultant relation.  (RELATION-DIFFERENCE &REST KEYWORD-LIST &KEY &OPTIONAL FROM INTO DIRECTORY DOCUMENTATION FORMAT IMPLEMENTATION-TYPE STORAGE-STRUCTURE KEY PRINT TUPLES UNIQUE &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿ÉÇRELATION-DIFFERENCEÄ¿BÄN¿BÄ¿BÄ¬¿BÄﬂ¿BÄ†¿BÄ°¿BÄ¢¿BÄ£¿BÄ¨¿BÄT¿BÄW¿BÄ≠“BÄﬂ“BÄ∏íPA¡Pˇ€PPPÇQPÅQ	PÉQ
PÑQPÖQPÜQPáQPàQP QPäQPãQJ∫@√íöAë@QåOÄπBÄ©Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ™\ÄBÄ8\ÄBÄÍlÅDifferenceBÄÏBÄΩBÄÌBÄΩBÄÓ\Ä\ÄBÄìBÄêBÄ˛¨ÜUsed to form difference of two compatible relationsÄBÄÙ\Ä\ÄBÄaBÄ˜lÅOperatorsÄBÄª\ÄBÄΩ\ÄÄ*ÏÇList of two relations:BÄ¿BÄ:BÄ¡\ÄBÄ˛,•List of the names of two relations which will take part in the relation difference operation. The attributes to be projected and a where clause can be specified for each relation using keywords. For instance, (REL1 (PROJECT <attr> WHERE <where-claue>) REL2 (WHERE <where-clause> PROJECT <attr>)).BÄƒBÄu\ÄÄ*,ÅTuples?ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛,ëSpecify if the resultant tuples be returned rather than inserted in a relation. The following parameters can be ignored if this is true.BÄ¶BÄÕBÄ—BÄyBÄ⁄BÄﬁBÄ‚BÄíBÄ¢BÄ*lÜParameters for the set-difference of two relationsBÄ˘BÄ∂\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄ¿BÄÏBÄΩBÄÌBÄΩBÄÓBÄ¡BÄ˛BÄ√BÄÙBÄƒBÄªBÄ«BÄ˘BÄ∂BÄÃÄ\ÄBÄ^BÄ‡™ÅINTERSECTIONÄÎÄ9ÜÄCFÄ'¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ◊\ÄBÄmBÄBÄNBÄ¬BÄﬂBÄ†BÄ°BÄ¢BÄ£BÄ¨BÄTBÄWBÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛Ïø}Intersection of tuples in two relations.

   FROM                 - This clause specifies the relations to participate in the INTERSECTION operation.
                          In addition, RTMS allows users to specify the attributes in these relations to
                          participate in the operation as well as a where-clause to specify the tuples.
                          It should be of the format: (RelA [(PROJECT <attrA> WHERE where-clause-A)] RelB
                          [(PROJECT <attrB> WHERE where-clause-B)]) where attrA indicates attributes in
                          relation relA and where-clause-A indicates a where-clause involving the attributes
                          in relation relA.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   PRINT                - If NIL, the resultant relation will not be printed out.
   TUPLES               - If T, the resultant tuples will be returned.
   UNIQUE               - If T, only unique tuples will be part of the resultant relation.  (RELATION-INTERSECTION &REST KEYWORD-LIST &KEY &OPTIONAL FROM INTO DIRECTORY DOCUMENTATION FORMAT IMPLEMENTATION-TYPE STORAGE-STRUCTURE KEY PRINT TUPLES UNIQUE &ALLOW-OTHER-KEYS)ÄBÄõëBÄ‹¿lÄ~S¿√ÇRELATION-INTERSECTIONÄ¿BÄN¿BÄ¿BÄ¬¿BÄﬂ¿BÄ†¿BÄ°¿BÄ¢¿BÄ£¿BÄ¨¿BÄT¿BÄW¿BÄ≠“BÄﬂ“BÄÊíPA¡Pˇ€PPPÇQPÅQ	PÉQ
PÑQPÖQPÜQPáQPàQP QPäQPãQJ∫@√íöAë@QåOÄÁBÄ◊Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄÿ\ÄBÄ8\ÄBÄÍ¨ÅIntersectionBÄÏBÄÎBÄÌBÄÎBÄÓ\Ä\ÄBÄìBÄÕBÄ˛ÏÜUsed to form intersection of two compatible relationsÄBÄÙ\Ä\ÄBÄaBÄ˜lÅOperatorsÄBÄª\ÄBÄΩ\ÄÄ*ÏÇList of two relations:BÄ¿BÄ:BÄ¡\ÄBÄ˛l•List of the names of two relations which will take part in the relation intersection operation. The attributes to be projected and a where clause can be specified for each relation using keywords. For instance, (REL1 (PROJECT <attr> WHERE <where-claue>) REL2 (WHERE <where-clause> PROJECT <attr>)).BÄƒBÄu\ÄÄ*,ÅTuples?ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛,ëSpecify if the resultant tuples be returned rather than inserted in a relation. The following parameters can be ignored if this is true.BÄ¶BÄÕBÄ—BÄyBÄ⁄BÄﬁBÄ‚BÄíBÄ¢BÄ*¨ÜParameters for the set-intersection of two relationsBÄ˘BÄ‰\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄÓBÄÏBÄÎBÄÌBÄÎBÄÓBÄÔBÄ˛BÄÒBÄÙBÄÚBÄªBÄıBÄ˘BÄ‰BÄÃÄ\ÄBÄ^BÄ‡ÅAVERAGEÄÄÎÄ(ÜÄA»FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ\ÄBÄmBÄ†BÄüBÄWBÄ‰
CÄBYBÄ¬BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏøIAverage of the values of a given attribute in a relation satisfying a where clause.

   RELATION-NAME  - Name of the relation.
   ATTRIBUTE-NAME - Name of the attribute whose average is to be found.
   UNIQUE         - If T, only unique values will be used.
   WHERE          - If a selection criterion is provided, only the satisfying values will be used.
   BY             - Name of the attribute to group average of the above attribute by.
   TUPLES         - If T, the resultant values will be returned rather than printed out as a table.  (AVERAGE RELATION-NAME ATTRIBUTE-NAME &REST KEYWORD-LIST &KEY &OPTIONAL UNIQUE WHERE BY TUPLES &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄ¿BÄW¿BÄ‰
¿BÄ¿BÄ¬¿BÄ≠“BÄﬂ“BÄíPA¡Pˇ€PPÅQÇQPÉQPÑQ	PÖQ
PÜQJ∫@√¢öAëÅQÇQ@QúOÄBÄÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ\ÄBÄ8\ÄBÄÍ,ÅAverageÄBÄÏBÄBÄÌBÄBÄÓ\Ä\ÄBÄìBÄÚBÄ˛làUsed to compute the average of the attribute values in a relation.BÄÙ\Ä\ÄBÄaBÄ˜lÅOperatorsÄBÄª\Ä	BÄΩ\ÄÄ*ÏÅRelation name:BÄ¿BÄ~BÄ¡\ÄBÄ˛làName of the relation which contains the attribute to be averaged.ÄBÄƒ\ÄÄ*,ÇAttribute name:ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛¨ÖName of the attribute in the above relation.BÄƒ\ÄÄ*,ÅUnique?ÄBÄ¿BÄ:BÄ¡\ÄBÄ˛läIf true, only the unique values of the attribute will be used in the calculations.BÄ¶BÄ˘
\ÄÄ*lÄByBÄ¿BÄ:BÄ¡\ÄBÄ˛ÏàSpecify the attribute to be used in grouping the data into categories.BÄƒBÄñBÄ*,ÉParameters for average:ÄBÄ˘BÄ\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄBÄÏBÄBÄÌBÄBÄÓBÄBÄ˛BÄBÄÙBÄ BÄªBÄ#BÄ˘BÄBÄÃÄ\ÄBÄ^BÄ‡ÉÄSUMÄÄÎÄ(ÜÄA»FÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄ;\ÄBÄmBÄ†BÄüBÄWBÄ‰
BÄBÄ¬BÄ:\ÄBÄßBÄ:\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛ÏøASum of the values of a given attribute in a relation satisfying a where clause.

   RELATION-NAME  - Name of the relation.
   ATTRIBUTE-NAME - Name of the attribute whose sum is to be found.
   UNIQUE         - If T, only unique values will be used.
   WHERE          - If a selection criterion is provided, only the satisfying values will be used.
   BY             - Name of the attribute to group sum of the above attribute by.
   TUPLES         - If T, the resultant values will be returned rather than printed out as a table.  (SUM RELATION-NAME ATTRIBUTE-NAME &REST KEYWORD-LIST &KEY &OPTIONAL UNIQUE WHERE BY TUPLES &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄ<¿BÄW¿BÄ¿BÄ¬¿BÄ‰
¿BÄ≠“BÄﬂ“BÄ<íPA¡Pˇ€PPÅQÇQPÉQPÖQ	PÜQ
PÑQJ∫@√¢öAëÅQÇQ@QúOÄJBÄ;Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ<\ÄBÄ8\ÄBÄÍ¨ÄSumÄBÄÏBÄNBÄÌBÄNBÄÓ\Ä\ÄBÄìBÄLÄ˛ÏáUsed to compute the sum of the attribute values in a relation.BÄÙ\Ä\ÄBÄaBÄ˜lÅOperatorsÄBÄª\Ä	BÄΩ\ÄÄ*ÏÅRelation name:BÄ¿BÄ~BÄ¡\ÄBÄ˛,àName of the relation which contains the attrLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540758. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "LISP" :NAME "MACROS" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :CHARACTERS T :NOT-BACKED-UP T :CREATION-DATE 2749846222. :AUTHOR "REL3" :LENGTH-IN-BYTES 11371. :LENGTH-IN-BLOCKS 12. :BYTE-SIZE 8.)

;;; -*- Mode:Common-Lisp; Package:RTMS; Fonts:(*CODE-FONT* *COMMENT-FONT* *STRING-FONT*); Base:10 -*-
;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved
;;; MACROS
;;;
;;; This file contains the following Explorer extensions to CommonLisp d as Indicated in the June 1985 Explorer Lisp
;;; Reference
;;;
;;; This file comtains the following obsolete functions
;;;
;;; This file contains the following functions which are unknown in CommonLisp
;;;
;;; The following function contains flavor references and thus are incompatable with CommonLisp. Their removal will not
;;; effect the functionality of RTMS.
;;;

(defmacro abort-transaction* (&rest ignore)
  `(abort-transaction))

(defmacro active-database* ()
  `(active-database))

(defmacro active-db* ()
  `(active-database))

(defmacro andp (&rest predicates)
  `(and* (quote ,predicates) t))


(defmacro attach-relation* (relation-name &rest keyword-list)
  `(attach-relation (quote ,relation-name) (quote ,keyword-list)))

(defmacro average* (relation-name attribute-name &rest keyword-list)
`(average (quote ,relation-name) (quote ,attribute-name) (quote ,keyword-list)))

(defmacro begin-transaction* (&rest ignore)
  `(begin-transaction))

(defmacro commit-transaction* (transaction &rest keyword-list)
  `(commit-transaction (quote ,transaction) (quote ,keyword-list)))

(defmacro count-rtms* (relation-name attribute-name &rest keyword-list)
`(count-rtms (quote ,relation-name) (quote ,attribute-name) (quote ,keyword-list)))

(defmacro define-attribute* (relation attr-des &rest keyword-list)
`(define-attribute (quote ,relation) (quote ,attr-des) (quote ,keyword-list)))

(defmacro defattr* (relation attr-des &rest keyword-list)
`(define-attribute (quote ,relation) (quote ,attr-des) (quote ,keyword-list)))

(defmacro defdb* (db-name &rest keyword-list)
`(define-database (quote ,db-name) (quote ,keyword-list)))

(defmacro define-database* (db-name &rest keyword-list)
`(define-database (quote ,db-name) (quote ,keyword-list)))

(defmacro define-domain* (domain-name &rest keyword-list)
 `(define-domain (quote ,domain-name) (quote ,keyword-list)))

(defmacro defenv* (environment-name &rest keyword-list)
`(define-environment (quote ,environment-name) (quote ,keyword-list)))

(defmacro define-environment* (environment-name &rest keyword-list)
`(define-environment (quote ,environment-name) (quote ,keyword-list)))

(defmacro define-implementation* (implementation-name &rest keyword-list)
  `(define-implementation (quote ,implementation-name) (quote ,keyword-list)))

(defmacro define-index* (relation &rest keyword-list)
`(define-index (quote ,relation) (quote ,keyword-list)))

(defmacro define-storage-structure* (storage-structure-name &rest keyword-list)
  `(define-storage-structure (quote ,storage-structure-name) (quote ,keyword-list)))

(defmacro defrel* (relation-name attribute-descriptor &rest keyword-list)
  `(define-relation (quote ,relation-name) (quote ,attribute-descriptor) (quote ,keyword-list)))

(defmacro define-relation* (relation-name attribute-descriptor &rest keyword-list)
  `(define-relation (quote ,relation-name) (quote ,attribute-descriptor) (quote ,keyword-list)))

(defmacro define-transaction* (transaction forms &rest keyword-list)
  `(define-transaction (quote ,transaction) (quote ,forms) (quote ,keyword-list)))

(defmacro defrel-restore (relation-name attribute-descriptor &rest keyword-list)
  `(defrel-restore* (quote ,relation-name) (quote ,attribute-descriptor) (quote ,keyword-list)))

(defmacro defview* (viewname viewdef)
  `(define-view (quote ,viewname) (quote ,viewdef)))

(defmacro define-view* (viewname viewdef)
  `(define-view (quote ,viewname) (quote ,viewdef)))

(defmacro delete-tuples* (relation &rest keyword-list)
  `(delete-tuples (quote ,relation) (quote ,keyword-list)))

(defmacro destroy-attr* (relation &rest keyword-list)
`(destroy-attribute (quote ,relation) (quote ,keyword-list)))

(defmacro destroy-attribute* (relation &rest keyword-list)
`(destroy-attribute (quote ,relation) (quote ,keyword-list)))

(defmacro destroy-db* (db-name &rest keyword-list)
  `(destroy-database (quote ,db-name) (quote ,keyword-list)))

(defmacro destroy-database* (db-name &rest keyword-list)
  `(destroy-database (quote ,db-name) (quote ,keyword-list)))

(defmacro destroy-domain* (domain-name)
  `(destroy-domain (quote ,domain-name)))

(defmacro destroy-implementation* (implementation-name)
  `(destroy-implementation (quote ,implementation-name)))

(defmacro destroy-index* (relation-name index-name &rest keyword-list)
  `(destroy-index (quote ,relation-name) (quote ,index-name) (quote ,keyword-list)))

(defmacro destroy-relation* (relation-name &rest keyword-list)
  `(destroy-relation (quote ,relation-name) (quote ,keyword-list)))

(defmacro destroy-rel* (relation-name &rest keyword-list)
  `(destroy-relation (quote ,relation-name) (quote ,keyword-list)))

(defmacro destroy-storage-structure* (storage-structure-name)
  `(destroy-storage-structure (quote ,storage-structure-name)))

(defmacro destroy-view* (view-name)
  `(destroy-view (quote ,view-name)))

(defmacro describe* (&optional object &rest ignore)
  `(help (quote ,object)))

(defmacro detach-relation* (relation-name &rest keyword-list)
  `(detach-relation (quote ,relation-name) (quote ,keyword-list)))

(defmacro end-transaction* (&rest ignore)
  `(end-transaction))

(defmacro envstat* ()
  (environment-status))

(defmacro environment-status* ()
  (environment-status))

(defmacro equalp* (&rest items)
  `(*equalp (quote ,items)))

(defmacro gep* (&rest items)
  `(gep (quote ,items)))

(defmacro gtp* (&rest items)
  `(gtp (quote ,items)))

(defmacro help* (&optional object &rest ignore)
  `(help (quote ,object)))

(defmacro insert* (relation &rest keyword-list)
  `(insert (quote ,relation) (quote ,keyword-list)))

(defmacro insert-tuples* (relation &rest keyword-list)
  `(insert (quote ,relation) (quote ,keyword-list)))

(defmacro join* (&rest keyword-list)
  `(join (quote ,keyword-list)))

(defmacro lep* (&rest items)
  `(lep (quote ,items)))

(defmacro ltp* (&rest items)
  `(ltp (quote ,items)))

(defmacro loaddb* (dbname &rest keyword-list)
  `(load-database (quote ,dbname) (quote ,keyword-list)))

(defmacro load-database* (dbname &rest keyword-list)
  `(load-database (quote ,dbname) (quote ,keyword-list)))

(defmacro load-env* (envname &rest keyword-list)
  `(load-environment (quote ,envname) (quote ,keyword-list)))

(defmacro load-environment* (envname &rest keyword-list)
  `(load-environment (quote ,envname) (quote ,keyword-list)))

(defmacro load-rel* (relation-name &rest keyword-list)
  `(load-relation (quote ,relation-name) (quote ,keyword-list)))

(defmacro load-relation* (relation-name &rest keyword-list)
  `(load-relation (quote ,relation-name) (quote ,keyword-list)))

(defmacro maptuple* (dbfunction relation-name)
  `(maptuple (quote ,dbfunction) (quote ,relation-name)))

(defmacro mapt* (dbfunction relation-name)
  `(mapt (quote ,dbfunction) (quote ,relation-name)))

(defmacro modify* (relation &rest keyword-list)
  `(modify (quote ,relation) (quote ,keyword-list)))

(defmacro modify-attribute* (relation attribute &rest keyword-list)
  `(modify-attribute (quote ,relation) (quote ,attribute) (quote ,keyword-list)))

(defmacro modify-database* (database &rest keyword-list)
  `(modify-database (quote ,database) (quote ,keyword-list)))

(defmacro modify-relation* (relation &rest keyword-list)
  `(modify-relation (quote ,relation) (quote ,keyword-list)))

(defmacro modify-domain* (domain-name &rest keyword-list)
  `(modify-domain (quote ,domain-name) (quote ,keyword-list)))

(defmacro modify-transaction* (transaction &rest keyword-list)
  `(modify-transaction (quote ,transaction) (quote ,keyword-list)))

(defmacro modify-tuples* (relation &rest keyword-list)
  `(modify (quote ,relation) (quote ,keyword-list)))

(defmacro modify-view* (view-name &rest keyword-list)
  `(modify-view (quote ,view-name) (quote ,keyword-list)))

(defmacro maximum* (relation-name attribute-name &rest keyword-list)
  `(maximum (quote ,relation-name) (quote ,attribute-name) (quote ,keyword-list)))

(defmacro minimum* (relation-name attribute-name &rest keyword-list)
  `(minimum (quote ,relation-name) (quote ,attribute-name) (quote ,keyword-list)))

(defmacro notp* (&rest items)
  `(notp (quote ,items)))

(defmacro printrel* (relation &rest keyword-list)
  `(print-relation (quote ,relation) (quote ,keyword-list)))

(defmacro print-relation* (relation &rest keyword-list)
  `(print-relation (quote ,relation) (quote ,keyword-list)))

(defmacro project* (relation-name &rest keyword-list)
  `(project (quote ,relation-name) (quote ,keyword-list)))

(defmacro r (relation-name &rest keyword-list)
  `(retrieve (quote ,relation-name) (quote ,keyword-list)))

(defmacro relation-difference* (&rest keyword-list)
  `(relation-difference (quote ,keyword-list)))

(defmacro relation-intersection* (&rest keyword-list)
  `(relation-intersection (quote ,keyword-list)))

(defmacro relation-union* (&rest keyword-list)
  `(relation-union  (quote ,keyword-list)))

(defmacro rename-attr* (relation-name &rest attributes)
  `(rename-attribute (quote ,relation-name) (quote ,attributes)))


(defmacro rename-attribute* (relation-name &rest attributes)
  `(rename-attribute (quote ,relation-name) (quote ,attributes)))

(defmacro rename-database* (&rest databases)
  `(rename-database (quote ,databases)))

(defmacro rename-db* (&rest databases)
  `(rename-database (quote ,databases)))

(defmacro rename-relation* (&rest relations)
  `(rename-relation (quote ,relations)))

(defmacro rename-rel* (&rest relations)
  `(rename-relation (quote ,relations)))

(defmacro retrieve* (relation-name &rest keyword-list)
  `(retrieve (quote ,relation-name) (quote ,keyword-list)))

(defmacro rtms-count* (relation-name attribute-name &rest keyword-list)
`(count-rtms (quote ,relation-name) (quote ,attribute-name) (quote ,keyword-list)))

(defmacro save-db* (&optional (dbname *active-db*) &rest keyword-list)
  `(save-database (quote ,dbname) (quote ,keyword-list)))

(defmacro save-database* (&optional (dbname *active-db*) &rest keyword-list)
  `(save-database (quote ,dbname) (quote ,keyword-list)))

(defmacro save-env* (&optional (envname *environment-name*) &rest keyword-list)
  `(save-environment (quote ,envname) (quote ,keyword-list)))

(defmacro save-environment* (&optional (envname *environment-name*) &rest keyword-list)
  `(save-environment (quote ,envname) (quote ,keyword-list)))

(defmacro save-rel* (relation-name &rest keyword-list)
  `(save-relation (quote ,relation-name) (quote ,keyword-list)))

(defmacro save-relation* (relation-name &rest keyword-list)
  `(save-relation  (quote ,relation-name) (quote ,keyword-list)))

(defmacro save-transaction* (transaction-name &rest keyword-list)
  `(save-transaction (quote ,transaction-name) (quote ,keyword-list)))

(defmacro select-tuples* (relation-name &rest keyword-list)
  `(select-tuples (quote ,relation-name) (quote ,keyword-list)))

(defmacro sum* (relation-name attribute-name &rest keyword-list)
  `(sum (quote ,relation-name) (quote ,attribute-name) (quote ,keyword-list)))

(defmacro size* (relation-name &rest keyword-list)
  `(size (quote ,relation-name) (quote ,keyword-list)))

ÄÏBÄZBÄÌBÄZBÄÓBÄ^BÄÙBÄBÄ˘BÄUBÄÃÄ\ÄBÄ^BÄ‡BÄÄÎÄÜÄ@DFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄf\ÄBÄmBÄ:\ÄÅCOMMANDÄ\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛¨ãIntroduction to the interface. Help on any database object (COMMAND / RELATION / ATTRIBUTE).Äp¿BÄu¨ÄSELF—√Å*HELP-SUBMENU*ëÈÅSUBMENU-CHOOSE¿)ÅEXECUTEÄÄPPÃCˇì@¡‰PP@ïROÄyBÄfÄ1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄ^BÄ‡BÄ\ÄBÄ8\ÄBÄÍ¨ÄHelpBÄÏBÄ}BÄÌBÄ}BÄÙBÄjBÄ˛,ãIntroduction to the interface. Help on any database object (COMMAND/RELATION/ATTRIBUTE).BÄ˘BÄs\ÄBÄ8BÄ¸\ÄBÄˇ\ÄBÄ8BÄ¸\ÄBÄ8\ÄBÄ:BÄÍBÄÄBÄÏBÄ}BÄÌBÄ}BÄÙBÄjBÄ˛BÄÅBÄ˘BÄsBÄÃÄ\ÄBÄ^BÄ‡BÄaÄÎÄÜÄ@DFÄ¿$Ä¿BÄ:BÄd]ÄFÄÄ:BÄ:BÄ:BÄÃFÄÄjBÄà\ÄBÄmBÄ:\ÄBÄp\ÄÄí\ÄBÄïBÄoBÄ‡BÄ˛,êSelect a database command from a menu. A choose-variable-values window will be presented to get the arguments LMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540761. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "XLD" :NAME "MACROS" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760359691. :AUTHOR "REL3" :LENGTH-IN-BYTES 11689. :LENGTH-IN-BLOCKS 23. :BYTE-SIZE 16.)                                     pp2Ä\Ä©ÅCOMPILE-DATA\ÄÏÄSW-MFG,ÅGODZILLAÜÄá§ÕFÄFÄ\Äp¿,ÅCOMPILER,ÅVERSIONÄ\ÄFÄFÄp¿BÄ),ÇOPTIMIZE-SWITCHÄÜÄ©ÉQFASL-SOURCE-FILE-UNIQUE-IDÄ1Ä\Äp¿lÄFSÏÇMAKE-FASLOAD-PATHNAMEÄ\ÄÍÄQUOTEÄBÄ$\ÄBÄ8™ÄNILÄ\ÄBÄ8\Ä¨ÄRTMS\ÄBÄ8ÏÄMACROS\ÄBÄ8¨ÄLISP\ÄBÄ8FÄ©ÄBASEFÄ
ÈÄFONTSÄ\Ä©Å*CODE-FONT*ÄÈÅ*COMMENT-FONT*ÈÅ*STRING-FONT*Ä)ÅPACKAGEÄ©ÄRTMS©ÄMODE©ÅCOMMON-LISPÄÄCÇABORT-TRANSACTION*ÄÎÄÜÄ$@FÄ¿$Ä¿BÄ:p¿¨ÄTICLÏÄART-QÄ]ÄFÄÄ:BÄ:BÄ:jÄTÄFÄp¿¨ÄSYSÄlÇDEBUG-INFO-STRUCTÄBÄP\Äp¿BÄ\lÅ*MACROARG*jÅ&OPTIONALÄp¿BÄ\lÇ*MACROENVIRONMENT*BÄ:BÄ:\Ä©ÅEXPR-SXHASHÄÜÄ&,*©ÇDESCRIPTIVE-ARGLISTÄ\ÄÍÄ&RESTÄÍÄIGNOREÄ\ÄCÇABORT-TRANSACTIONÄÄD¿p¿BÄTÏÄMACROÄBÄnOÄqBÄPÄÄÇACTIVE-DATABASE*ÄÎÄÜÄ$@FÄ
¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄrBÄ_BÄ:BÄ:\ÄBÄfÜÄ
åπBÄhBÄ:Äp¿BÄ\,ÉMACRO-REPORT-ARGS-ERRORÄ“\ÄÇACTIVE-DATABASEÄÄÄQJô‰ÄQJJòD¿BÄpBÄÄOÄÅBÄrÄÄCÅACTIVE-DB*ÄÎÄÜÄ$@FÄ
¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÇBÄ_BÄ:BÄ:\ÄBÄfÜÄ:FyBÄhBÄ:ÄBÄ}“\ÄBÄÄÄQJô‰ÄQJJòD¿BÄpBÄ
OÄéBÄÇÄÄÉÄANDPÄÎÄÜÄ$@FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄèBÄ_BÄ:BÄ:\ÄÄfÜÄV)ÇMACROS-EXPANDEDÄ\Äp¿BÄ\lÅXR-BQ-LISTp¿BÄ\¨ÅXR-BQ-LIST*ÄBÄh\ÄBÄjCÅPREDICATESÄÉÄAND*¿BÄ8¿™ÄLIST“\ÄBÄY¿ÍÄLIST*ÄíPPÄUíPúD¿BÄpBÄ•OÄ¶BÄèÄÄÇATTACH-RELATION*ÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄßBÄ_BÄ:\Ä√ÅRELATION-NAMEÄ\ÄÄfÜÄ"7¯BÄô\ÄBÄúBÄh\ÄBÄ∞BÄjÉÅKEYWORD-LISTÄBÄ}“ÇATTACH-RELATIONÄ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄ∑OÄ∏BÄßÄÄÅAVERAGE*ÄÎÄÜÄÑHFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄπBÄ_BÄ:\ÄBÄ∞√ÅATTRIBUTE-NAME\ÄÄfÜÄJ{—BÄô\ÄBÄúBÄh\ÄBÄ∞BÄ¬BÄjBÄµÄBÄ}“ÅAVERAGEÄ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡Ä[A¡PP@QíPAQíPÄQBí§D¿BÄpBÄ»OÄ…BÄπÄÄCÇBEGIN-TRANSACTION*ÄÎÄÜÄ$@FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ BÄ_BÄ:BÄ:\ÄBÄfÜÄbﬁ|BÄh\ÄBÄjBÄkÄ\ÄCÇBEGIN-TRANSACTIONÄÄD¿BÄpBÄ◊OÄÿBÄ ÄÄÉÇCOMMIT-TRANSACTION*ÄÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄŸBÄ_BÄ:\ÄÉÅTRANSACTIONÄ\ÄÄfÜÄ~≠)BÄô\ÄBÄúBÄh\ÄBÄ‚BÄjBÄµÄBÄ}“CÇCOMMIT-TRANSACTION¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄËOÄÈBÄŸÄÄÉÅCOUNT-RTMS*ÄÄÎÄÜÄÑHFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÍBÄ_BÄ:\ÄBÄ∞BÄ¬\ÄÄfÜÄJ[BÄô\ÄBÄúBÄh\ÄBÄ∞BÄ¬BÄjBÄµÄBÄ}“CÅCOUNT-RTMS¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡Ä[A¡PP@QíPAQíPÄQBí§D¿BÄpBÄ¯OÄ˘BÄÍÄÄCÇDEFINE-ATTRIBUTE*ÄÄÎÄÜÄÑHFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ˙BÄ_BÄ:\ÄÅRELATIONÅATTR-DES\ÄÄfÜÄ«êBÄô\ÄBÄúBÄh\ÄBÄBÄBÄjBÄµÄBÄ}“ÇDEFINE-ATTRIBUTE¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡Ä[A¡PP@QíPAQíPÄQBí§D¿BÄpBÄ
OÄBÄ˙ÄÄÅDEFATTR*ÄÎÄÜÄÑHFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄBÄ_BÄ:\ÄBÄBÄ\ÄÄfÜÄ&O¡BÄô\ÄBÄúBÄh\ÄBÄBÄBÄjBÄµÄBÄ}“BÄ	¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡Ä[A¡PP@QíPAQíPÄQBí§D¿BÄpBÄOÄBÄÄÄ√ÄDEFDB*ÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄBÄ_BÄ:\ÄÅDB-NAMEÄ\ÄÄfÜÄ6Î‘BÄô\ÄBÄúBÄh\ÄBÄ$BÄjBÄµÄBÄ}“ÇDEFINE-DATABASEÄ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄ*OÄ+BÄÄÄÇDEFINE-DATABASE*ÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ,BÄ_BÄ:\ÄBÄ$\ÄÄfÜÄbîƒBÄô\ÄBÄúBÄh\ÄBÄ$BÄjBÄµÄBÄ}“BÄ)¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄ9OÄ:BÄ,ÄÄ√ÅDEFINE-DOMAIN*ÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ;BÄ_BÄ:\ÄÉÅDOMAIN-NAMEÄ\ÄÄfÜÄ
qÁBÄô\ÄBÄúBÄh\ÄBÄDBÄjBÄµÄBÄ}“√ÅDEFINE-DOMAINÄ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄJOÄKBÄ;ÄÄÅDEFENV*ÄÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄLBÄ_BÄ:\ÄÇENVIRONMENT-NAME\ÄÄfÜÄ.Î∫BÄô\ÄBÄúBÄh\ÄBÄUBÄjBÄµÄBÄ}“CÇDEFINE-ENVIRONMENT¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄ[OÄ\BÄLÄÄÉÇDEFINE-ENVIRONMENT*ÄÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ]BÄ_BÄ:\ÄBÄU\ÄÄfÜÄR~BÄô\ÄBÄúBÄh\ÄBÄUBÄjBÄµÄBÄ}“BÄZ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄjOÄkBÄ]ÄÄ√ÇDEFINE-IMPLEMENTATION*ÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄlBÄ_BÄ:\ÄÉÇIMPLEMENTATION-NAMEÄ\ÄÄfÜÄn"BÄô\ÄBÄúBÄh\ÄBÄuBÄjBÄµÄBÄ}“√ÇDEFINE-IMPLEMENTATIONÄ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄ{OÄ|BÄlÄÄ√ÅDEFINE-INDEX*ÄÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ}BÄ_BÄ:\ÄBÄ\ÄÄfÜÄ~?ÎBÄô\ÄBÄúBÄh\ÄBÄBÄjBÄµÄBÄ}“ÉÅDEFINE-INDEX¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄãOÄåBÄ}ÄÄCÉDEFINE-STORAGE-STRUCTURE*ÄÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ
BÄ_BÄ:\Ä√ÇSTORAGE-STRUCTURE-NAME\ÄÄfÜÄå∑BÄô\ÄBÄúBÄh\ÄBÄñBÄjBÄµÄBÄ}“ÉDEFINE-STORAGE-STRUCTURE¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄúOÄùBÄ
ÄÄÅDEFREL*ÄÄÎÄÜÄÑHFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄûBÄ_BÄ:\ÄBÄ∞ÉÇATTRIBUTE-DESCRIPTOR\ÄÄfÜÄ2h·BÄô\ÄBÄúBÄh\ÄBÄ∞BÄßBÄjBÄµÄBÄ}“ÇDEFINE-RELATIONÄ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡Ä[A¡PP@QíPAQíPÄQBí§D¿BÄpBÄ≠OÄÆBÄûÄÄÇDEFINE-RELATION*ÄÎÄÜÄÑHFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄØBÄ_BÄ:\ÄBÄ∞BÄß\ÄÄfÜÄ
≠!BÄô\ÄBÄúBÄh\ÄBÄ∞BÄßBÄjBÄµÄBÄ}“BÄ¨¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡Ä[A¡PP@QíPAQíPÄQBí§D¿BÄpBÄºOÄΩBÄØÄÄÉÇDEFINE-TRANSACTION*ÄÄÎÄÜÄÑHFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄæBÄ_BÄ:\ÄBÄ‚√ÄFORMSÄ\ÄÄfÜÄN©ÓBÄô\ÄBÄúBÄh\ÄBÄ‚BÄ«BÄjBÄµÄBÄ}“CÇDEFINE-TRANSACTION¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡Ä[A¡PP@QíPAQíPÄQBí§D¿BÄpBÄÕOÄŒBÄæÄÄ√ÅDEFREL-RESTOREÄÎÄÜÄÑHFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄœBÄ_BÄ:\ÄBÄ∞BÄß\ÄÄfÜÄ:».BÄô\ÄBÄúBÄh\ÄBÄ∞BÄßBÄjBÄµÄBÄ}“ÇDEFREL-RESTORE*Ä¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡Ä[A¡PP@QíPAQíPÄQBí§D¿BÄpBÄ›OÄﬁBÄœÄÄÅDEFVIEW*ÄÎÄÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄﬂBÄ_BÄ:\ÄÅVIEWNAME\ÄÄfÜÄB@ÆBÄô\ÄBÄúBÄh\ÄBÄËÅVIEWDEFÄÄBÄ}“ÉÅDEFINE-VIEWÄ¿BÄ8¿BÄ¢íÄQJô‰ÄQJô‰ÄQJJòÄW@¡PP@QíPÄ[íúD¿BÄpBÄÔOÄBÄﬂÄÄÉÅDEFINE-VIEW*ÄÎÄÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÒBÄ_BÄ:\ÄBÄË\ÄÄfÜÄVs&BÄô\ÄBÄúBÄh\ÄBÄËBÄÌÄBÄ}“BÄÓ¿BÄ8¿BÄ¢íÄQJô‰ÄQJô‰ÄQJJòÄW@¡PP@QíPÄ[íúD¿BÄpBÄ˛OÄˇBÄÒÄÄ√ÅDELETE-TUPLES*ÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄBÄ_BÄ:\ÄBÄ\ÄÄfÜÄ~ﬂ◊BÄô\ÄBÄúBÄh\ÄBÄBÄjBÄµÄBÄ}“√ÅDELETE-TUPLESÄ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄOÄBÄÄÄ√ÅDESTROY-ATTR*ÄÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄBÄ_BÄ:\ÄBÄ\ÄÄfÜÄzLBÄô\ÄBÄúBÄh\ÄBÄBÄjBÄµÄBÄ}“CÇDESTROY-ATTRIBUTEÄ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄOÄBÄÄÄCÇDESTROY-ATTRIBUTE*ÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ BÄ_BÄ:\ÄBÄ\ÄÄfÜÄ~?QBÄô\ÄBÄúBÄh\ÄBÄBÄjBÄµÄBÄ}“BÄ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄ-OÄ.BÄ ÄÄÉÅDESTROY-DB*ÄÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ/BÄ_BÄ:\ÄBÄ$\ÄÄfÜÄ
{VBÄô\ÄBÄúBÄh\ÄBÄ$BÄjBÄµÄBÄ}“ÇDESTROY-DATABASE¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄ=OÄ>BÄ/ÄÄCÇDESTROY-DATABASE*ÄÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ?BÄ_BÄ:\ÄBÄ$\ÄÄfÜÄbt6BÄô\ÄBÄúBÄh\ÄBÄ$BÄjBÄµÄBÄ}“BÄ<¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄLOÄMBÄ?ÄÄÇDESTROY-DOMAIN*ÄÄÎÄ	ÜÄ$@FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄNBÄ_BÄ:BÄ:\ÄÄfÜÄ~îøBÄô\ÄBÄúBÄh\ÄBÄDÄBÄ}“√ÅDESTROY-DOMAIN¿BÄ8¿BÄ¢íÄQJô‰ÄQJô‰ÄQJJòPPÄWíîD¿BÄpBÄ[OÄ\BÄNÄÄÉDESTROY-IMPLEMENTATION*ÄÄÎÄ	ÜÄ$@FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ]BÄ_BÄ:BÄ:\ÄÄfÜÄ„BÄô\ÄBÄúBÄh\ÄBÄuÄBÄ}“√ÇDESTROY-IMPLEMENTATION¿BÄ8¿BÄ¢íÄQJô‰ÄQJô‰ÄQJJòPPÄWíîD¿BÄpBÄjOÄkBÄ]ÄÄ√ÅDESTROY-INDEX*ÄÎÄÜÄÑHFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄlBÄ_BÄ:\ÄBÄ∞CÅINDEX-NAME\ÄÄfÜÄ&∑BÄô\ÄBÄúBÄh\ÄBÄ∞BÄuBÄjBÄµÄBÄ}“√ÅDESTROY-INDEXÄ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡Ä[A¡PP@QíPAQíPÄQBí§D¿BÄpBÄ{OÄ|BÄlÄÄCÇDESTROY-RELATION*ÄÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ}BÄ_BÄ:\ÄBÄ∞\ÄÄfÜÄ"◊ÍBÄô\ÄBÄúBÄh\ÄBÄ∞BÄjBÄµÄBÄ}“ÇDESTROY-RELATION¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄãOÄåBÄ}ÄÄÉÅDESTROY-REL*ÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ
BÄ_BÄ:\ÄBÄ∞\ÄÄfÜÄvkBÄô\ÄBÄúBÄh\ÄBÄ∞BÄjBÄµÄBÄ}“BÄä¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄöOÄõBÄ
ÄÄCÉDESTROY-STORAGE-STRUCTURE*ÄÎÄ	ÜÄ$@FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄúBÄ_BÄ:BÄ:\ÄÄfÜÄfI€BÄô\ÄBÄúBÄh\ÄBÄñÄBÄ}“CÉDESTROY-STORAGE-STRUCTUREÄ¿BÄ8¿BÄ¢íÄQJô‰ÄQJô‰ÄQJJòPPÄWíîD¿BÄpBÄ©OÄ™BÄúÄÄ√ÅDESTROY-VIEW*ÄÄÎÄ	ÜÄ$@FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ´BÄ_BÄ:BÄ:\ÄÄfÜÄ:O†BÄô\ÄBÄúBÄh\ÄCÅVIEW-NAMEÄÄBÄ}“ÉÅDESTROY-VIEW¿BÄ8¿BÄ¢íÄQJô‰ÄQJô‰ÄQJJòPPÄWíîD¿BÄpBÄπOÄ∫BÄ´ÄÄCÅDESCRIBE*ÄÄÎÄÜÄÑDFÄ
¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄªBÄ_BÄ:\Ä√ÄOBJECT\ÄÄfÜÄZ„xBÄô\ÄBÄúBÄh\ÄBÄbBÄƒBÄjBÄkÄÉÄHELP¿BÄ8¿BÄ¢íÄW@¡PP@QíîD¿BÄpBÄ OÄÀBÄªÄÄÇDETACH-RELATION*ÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÃBÄ_BÄ:\ÄBÄ∞\ÄÄfÜÄ"7¯BÄô\ÄBÄúBÄh\ÄBÄ∞BÄjBÄµÄBÄ}“ÇDETACH-RELATIONÄ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄ⁄OÄ€BÄÃÄÄÇEND-TRANSACTION*ÄÎÄÜÄ$@FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ‹BÄ_BÄ:BÄ:\ÄBÄfÜÄFgBÄh\ÄBÄjBÄkÄ\ÄÇEND-TRANSACTIONÄÄD¿BÄpBÄÈOÄÍBÄ‹ÄÄÅENVSTAT*ÄÎÄÜÄ$@FÄ
¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÎBÄ_BÄ:BÄ:\ÄBÄfÜÄﬁjBÄhBÄ:ÄBÄ}“CÇENVIRONMENT-STATUSíÄQJô‰ÄQJJòÑD¿BÄpBÄˆOÄ˜BÄÎÄÄÉÇENVIRONMENT-STATUS*ÄÄÎÄÜÄ$@FÄ
¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ¯BÄ_BÄ:BÄ:\ÄBÄfÜÄ&ÖöBÄhBÄ:ÄBÄ}“BÄıíÄQJô‰ÄQJJòÑD¿BÄpBÄOÄBÄ¯ÄÄÅEQUALP*ÄÄÎÄÜÄ$@FÄ	¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄBÄ_BÄ:BÄ:\ÄÄfÜÄ::&BÄô\ÄBÄúBÄh\ÄBÄj√ÄITEMSÄÄÅ*EQUALPÄ¿BÄ8¿BÄ¢íPPÄUíîD¿BÄpBÄOÄBÄÄÄÉÄGEP*ÄÎÄÜÄ$@FÄ	¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄBÄ_BÄ:BÄ:\ÄÄfÜÄÚVBÄô\ÄBÄúBÄh\ÄBÄjBÄÄÉÄGEPÄ¿BÄ8¿BÄ¢íPPÄUíîD¿BÄpBÄ!OÄ"BÄÄÄÉÄGTP*ÄÎÄÜÄ$@FÄ	¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ#BÄ_BÄ:BÄ:\ÄÄfÜÄcFBÄô\ÄBÄúBÄh\ÄBÄjBÄÄÉÄGTPÄ¿BÄ8¿BÄ¢íPPÄUíîD¿BÄpBÄ0OÄ1BÄ#ÄÄ√ÄHELP*ÄÄÎÄÜÄÑDFÄ
¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ2BÄ_BÄ:\ÄBÄƒ\ÄÄfÜÄvLABÄô\ÄBÄúBÄh\ÄBÄbBÄƒBÄjBÄkÄBÄ…¿BÄ8¿BÄ¢íÄW@¡PP@QíîD¿BÄpBÄ?OÄ@BÄ2ÄÄÅINSERT*ÄÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄABÄ_BÄ:\ÄBÄ\ÄÄfÜÄ~ﬂôBÄô\ÄBÄúBÄh\ÄBÄBÄjBÄµÄBÄ}“√ÄINSERT¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄOOÄPBÄAÄÄ√ÅINSERT-TUPLES*ÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄQBÄ_BÄ:\ÄBÄ\ÄÄfÜÄïHBÄô\ÄBÄúBÄh\ÄBÄBÄjBÄµÄBÄ}“BÄN¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄ^OÄ_BÄQÄÄ√ÄJOIN*ÄÄÎÄÜÄ$@FÄ	¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ`BÄ_BÄ:BÄ:\ÄÄfÜÄ:KBÄô\ÄBÄúBÄh\ÄBÄjBÄµÄÉÄJOIN¿BÄ8¿BÄ¢íPPÄUíîD¿BÄpBÄmOÄnBÄ`ÄÄÉÄLEP*ÄÎÄÜÄ$@FÄ	¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄoBÄ_BÄ:BÄ:\ÄÄfÜÄrﬂBÄô\ÄBÄúBÄh\ÄBÄjBÄÄÉÄLEPÄ¿BÄ8¿BÄ¢íPPÄUíîD¿BÄpBÄ|OÄ}BÄoÄÄÉÄLTP*ÄÎÄÜÄ$@FÄ	¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ~BÄ_BÄ:BÄ:\ÄÄfÜÄ„œBÄô\ÄBÄúBÄh\ÄBÄjBÄÄÉÄLTPÄ¿BÄ8¿BÄ¢íPPÄUíîD¿BÄpBÄãOÄåBÄ~ÄÄÅLOADDB*ÄÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ
BÄ_BÄ:\Ä√ÄDBNAME\ÄÄfÜÄZ\0BÄô\ÄBÄúBÄh\ÄBÄñBÄjBÄµÄBÄ}“√ÅLOAD-DATABASEÄ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄúOÄùBÄ
ÄÄ√ÅLOAD-DATABASE*ÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄûBÄ_BÄ:\ÄBÄñ\ÄÄfÜÄZB°BÄô\ÄBÄúBÄh\ÄBÄñBÄjBÄµÄBÄ}“BÄõ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄ´OÄ¨BÄûÄÄCÅLOAD-ENV*ÄÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ≠BÄ_BÄ:\ÄÅENVNAMEÄ\ÄÄfÜÄ6v®BÄô\ÄBÄúBÄh\ÄBÄ∂BÄjBÄµÄBÄ}“ÇLOAD-ENVIRONMENT¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄºOÄΩBÄ≠ÄÄCÇLOAD-ENVIRONMENT*ÄÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄæBÄ_BÄ:\ÄBÄ∂\ÄÄfÜÄYXBÄô\ÄBÄúBÄh\ÄBÄ∂BÄjBÄµÄBÄ}“BÄª¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄÀOÄÃBÄæÄÄCÅLOAD-REL*ÄÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÕBÄ_BÄ:\ÄBÄ∞\ÄÄfÜÄrb©BÄô\ÄBÄúBÄh\ÄBÄ∞BÄjBÄµÄBÄ}“√ÅLOAD-RELATIONÄ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄ€OÄ‹BÄÕÄÄ√ÅLOAD-RELATION*ÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ›BÄ_BÄ:\ÄBÄ∞\ÄÄfÜÄ"˜xBÄô\ÄBÄúBÄh\ÄBÄ∞BÄjBÄµÄBÄ}“BÄ⁄¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄÍOÄÎBÄ›ÄÄCÅMAPTUPLE*ÄÄÎÄÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÏBÄ_BÄ:\ÄCÅDBFUNCTION\ÄÄfÜÄBy˜BÄô\ÄBÄúBÄh\ÄBÄıBÄ∞ÄBÄ}“ÅMAPTUPLE¿BÄ8¿BÄ¢íÄQJô‰ÄQJô‰ÄQJJòÄW@¡PP@QíPÄ[íúD¿BÄpBÄ˚OÄ¸BÄÏÄÄ√ÄMAPT*ÄÄÎÄÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ˝BÄ_BÄ:\ÄBÄı\ÄÄfÜÄBπ•BÄô\ÄBÄúBÄh\ÄBÄıBÄ∞ÄBÄ}“ÉÄMAPT¿BÄ8¿BÄ¢íÄQJô‰ÄQJô‰ÄQJJòÄW@¡PP@QíPÄ[íúD¿BÄpBÄOÄBÄ˝ÄÄÅMODIFY*ÄÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄBÄ_BÄ:\ÄBÄ\ÄÄfÜÄ~ﬂÎBÄô\ÄBÄúBÄh\ÄBÄBÄjBÄµÄBÄ}“√ÄMODIFY¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄOÄBÄÄÄCÇMODIFY-ATTRIBUTE*ÄÄÎÄÜÄÑHFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄBÄ_BÄ:\ÄBÄCÅATTRIBUTEÄ\ÄÄfÜÄ~fÄô\ÄBÄúBÄh\ÄBÄBÄ&BÄjBÄµÄBÄ}“ÇMODIFY-ATTRIBUTE¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡Ä[A¡PP@QíPAQíPÄQBí§D¿BÄpBÄ,OÄ-BÄÄÄÇMODIFY-DATABASE*ÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ.BÄ_BÄ:\ÄÅDATABASE\ÄÄfÜÄ::yBÄô\ÄBÄúBÄh\ÄBÄ7BÄjBÄµÄBÄ}“ÇMODIFY-DATABASEÄ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄ=OÄ>BÄ.ÄÄÇMODIFY-RELATION*ÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ?BÄ_BÄ:\ÄBÄ\ÄÄfÜÄ~≥BÄô\ÄBÄúBÄh\ÄBÄBÄjBÄµÄBÄ}“ÇMODIFY-RELATIONÄ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄMOÄNBÄ?ÄÄ√ÅMODIFY-DOMAIN*ÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄOBÄ_BÄ:\ÄBÄD\ÄÄfÜÄ
ëOBÄô\ÄBÄúBÄh\ÄBÄDBÄjBÄµÄBÄ}“√ÅMODIFY-DOMAINÄ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄ]OÄ^BÄOÄÄÉÇMODIFY-TRANSACTION*ÄÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ_BÄ_BÄ:\ÄBÄ‚\ÄÄfÜÄ~≠cBÄô\ÄBÄúBÄh\ÄBÄ‚BÄjBÄµÄBÄ}“CÇMODIFY-TRANSACTION¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄmOÄnBÄ_ÄÄ√ÅMODIFY-TUPLES*ÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄoBÄ_BÄ:\ÄBÄ\ÄÄfÜÄRµ BÄô\ÄBÄúBÄh\ÄBÄBÄjBÄµÄBÄ}“BÄ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄ|OÄ}BÄoÄÄÉÅMODIFY-VIEW*ÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ~BÄ_BÄ:\ÄBÄ∑\ÄÄfÜÄNjjBÄô\ÄBÄúBÄh\ÄBÄ∑BÄjBÄµÄBÄ}“ÉÅMODIFY-VIEWÄ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄåOÄ
BÄ~ÄÄÅMAXIMUM*ÄÎÄÜÄÑHFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄéBÄ_BÄ:\ÄBÄ∞BÄ¬\ÄÄfÜÄJªBÄô\ÄBÄúBÄh\ÄBÄ∞BÄ¬BÄjBÄµÄBÄ}“ÅMAXIMUMÄ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡Ä[A¡PP@QíPAQíPÄQBí§D¿BÄpBÄúOÄùBÄéÄÄÅMINIMUM*ÄÎÄÜÄÑHFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄûBÄ_BÄ:\ÄBÄ∞BÄ¬\ÄÄfÜÄJ[¡BÄô\ÄBÄúBÄh\ÄBÄ∞BÄ¬BÄjBÄµÄBÄ}“ÅMINIMUMÄ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡Ä[A¡PP@QíPAQíPÄQBí§D¿BÄpBÄ¨OÄ≠BÄûÄÄ√ÄNOTP*ÄÄÎÄÜÄ$@FÄ	¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÆBÄ_BÄ:BÄ:\ÄÄfÜÄ2kNBÄô\ÄBÄúBÄh\ÄBÄjBÄÄÉÄNOTP¿BÄ8¿BÄ¢íPPÄUíîD¿BÄpBÄªOÄºBÄÆÄÄCÅPRINTREL*ÄÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄΩBÄ_BÄ:\ÄBÄ\ÄÄfÜÄ¯BÄô\ÄBÄúBÄh\ÄBÄBÄjBÄµÄBÄ}“√ÅPRINT-RELATION¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄÀOÄÃBÄΩÄÄÇPRINT-RELATION*ÄÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÕBÄ_BÄ:\ÄBÄ\ÄÄfÜÄ~?ìBÄô\ÄBÄúBÄh\ÄBÄBÄjBÄµÄBÄ}“BÄ ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄ⁄OÄ€BÄÕÄÄÅPROJECT*ÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ‹BÄ_BÄ:\ÄBÄ∞\ÄÄfÜÄ"◊"BÄô\ÄBÄúBÄh\ÄBÄ∞BÄjBÄµÄBÄ}“ÅPROJECTÄ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄÍOÄÎBÄ‹ÄÄCÄRÄÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÏBÄ_BÄ:\ÄBÄ∞\ÄÄfÜÄ
ˇBÄô\ÄBÄúBÄh\ÄBÄ∞BÄjBÄµÄBÄ}“ÅRETRIEVE¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄ˙OÄ˚BÄÏÄÄÉÇRELATION-DIFFERENCE*ÄÎÄÜÄ$@FÄ	¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ¸BÄ_BÄ:BÄ:\ÄÄfÜÄZ@‰BÄô\ÄBÄúBÄh\ÄBÄjBÄµÄÉÇRELATION-DIFFERENCEÄ¿BÄ8¿BÄ¢íPPÄUíîD¿BÄpBÄ	OÄ
BÄ¸ÄÄ√ÇRELATION-INTERSECTION*ÄÎÄÜÄ$@FÄ	¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄBÄ_BÄ:BÄ:\ÄÄfÜÄ”îBÄô\ÄBÄúBÄh\ÄBÄjBÄµÄ√ÇRELATION-INTERSECTIONÄ¿BÄ8¿BÄ¢íPPÄUíîD¿BÄpBÄOÄBÄÄÄÇRELATION-UNION*ÄÄÎÄÜÄ$@FÄ	¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄBÄ_BÄ:BÄ:\ÄÄfÜÄFÎÙBÄô\ÄBÄúBÄh\ÄBÄjBÄµÄ√ÅRELATION-UNION¿BÄ8¿BÄ¢íPPÄUíîD¿BÄpBÄ'OÄ(BÄÄÄÉÅRENAME-ATTR*ÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ)BÄ_BÄ:\ÄBÄ∞\ÄÄfÜÄ
bºBÄô\ÄBÄúBÄh\ÄBÄ∞BÄjCÅATTRIBUTESÄBÄ}“ÇRENAME-ATTRIBUTE¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄ8OÄ9BÄ)ÄÄCÇRENAME-ATTRIBUTE*ÄÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ:BÄ_BÄ:\ÄBÄ∞\ÄÄfÜÄ"≠-BÄô\ÄBÄúBÄh\ÄBÄ∞BÄjBÄ6ÄBÄ}“BÄ7¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄGOÄHBÄ:ÄÄÇRENAME-DATABASE*ÄÎÄÜÄ$@FÄ	¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄIBÄ_BÄ:BÄ:\ÄÄfÜÄnaÊBÄô\ÄBÄúBÄh\ÄBÄjCÅDATABASESÄÄÇRENAME-DATABASEÄ¿BÄ8¿BÄ¢íPPÄUíîD¿BÄpBÄWOÄXBÄIÄÄCÅRENAME-DB*ÄÎÄÜÄ$@FÄ	¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄYBÄ_BÄ:BÄ:\ÄÄfÜÄj\œBÄô\ÄBÄúBÄh\ÄBÄjBÄUÄBÄV¿BÄ8¿BÄ¢íPPÄUíîD¿BÄpBÄeOÄfBÄYÄÄÇRENAME-RELATION*ÄÎÄÜÄ$@FÄ	¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄgBÄ_BÄ:BÄ:\ÄÄfÜÄ
*IBÄô\ÄBÄúBÄh\ÄBÄjCÅRELATIONSÄÄÇRENAME-RELATIONÄ¿BÄ8¿BÄ¢íPPÄUíîD¿BÄpBÄuOÄvBÄgÄÄÉÅRENAME-REL*ÄÄÎÄÜÄ$@FÄ	¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄwBÄ_BÄ:BÄ:\ÄÄfÜÄ."ëBÄô\ÄBÄúBÄh\ÄBÄjBÄsÄBÄt¿BÄ8¿BÄ¢íPPÄUíîD¿BÄpBÄÉOÄÑBÄwÄÄCÅRETRIEVE*ÄÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÖBÄ_BÄ:\ÄBÄ∞\ÄÄfÜÄ"7`BÄô\ÄBÄúBÄh\ÄBÄ∞BÄjBÄµÄBÄ}“BÄ˘¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄíOÄìBÄÖÄÄÉÅRTMS-COUNT*ÄÄÎÄÜÄÑHFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄîBÄ_BÄ:\ÄBÄ∞BÄ¬\ÄÄfÜÄv_BÄô\ÄBÄúBÄh\ÄBÄ∞BÄ¬BÄjBÄµÄBÄ}“BÄ˜¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡Ä[A¡PP@QíPAQíPÄQBí§D¿BÄpBÄ°OÄ¢BÄîÄÄÅSAVE-DB*ÄÎÄÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ£BÄ_BÄ:\ÄBÄñ\ÄÄfÜÄ&”,BÄô\ÄBÄúBÄh\ÄBÄb\ÄBÄñÉÅ*ACTIVE-DB*ÄBÄjBÄµÄBÄ±ë√ÅSAVE-DATABASEÄ¿BÄ8¿BÄ¢íÄ‰ÄW¸P@¡PP@QíPÄYíúD¿BÄpBÄ≥OÄ¥BÄ£ÄÄ√ÅSAVE-DATABASE*ÄÎÄÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄµBÄ_BÄ:\ÄBÄñ\ÄÄfÜÄôBÄô\ÄBÄúBÄh\ÄBÄb\ÄBÄñBÄ±BÄjBÄµÄBÄ±ëBÄ≤¿BÄ8¿BÄ¢íÄ‰ÄW¸P@¡PP@QíPÄYíúD¿BÄpBÄ√OÄƒBÄµÄÄCÅSAVE-ENV*ÄÄÎÄÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ≈BÄ_BÄ:\ÄBÄ∂\ÄÄfÜÄTBÄô\ÄBÄúBÄh\ÄBÄb\ÄBÄ∂CÇ*ENVIRONMENT-NAME*BÄjBÄµÄBÄ”ëÇSAVE-ENVIRONMENT¿BÄ8¿BÄ¢íÄ‰ÄW¸P@¡PP@QíPÄYíúD¿BÄpBÄ’OÄ÷BÄ≈ÄÄCÇSAVE-ENVIRONMENT*ÄÄÎÄÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ◊BÄ_BÄ:\ÄBÄ∂\ÄÄfÜÄ*l¬BÄô\ÄBÄúBÄh\ÄBÄb\ÄBÄ∂BÄ”BÄjBÄµÄBÄ”ëBÄ‘¿BÄ8¿BÄ¢íÄ‰ÄW¸P@¡PP@QíPÄYíúD¿BÄpBÄÂOÄÊBÄ◊ÄÄCÅSAVE-REL*ÄÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÁBÄ_BÄ:\ÄBÄ∞\ÄÄfÜÄH®BÄô\ÄBÄúBÄh\ÄBÄ∞BÄjBÄµÄBÄ}“√ÅSAVE-RELATIONÄ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄıOÄˆBÄÁÄÄ√ÅSAVE-RELATION*ÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ˜BÄ_BÄ:\ÄBÄ∞\ÄÄfÜÄ"7ËBÄô\ÄBÄúBÄh\ÄBÄ∞BÄjBÄµÄBÄ}“BÄÙ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄOÄÄ˜ÄÄCÇSAVE-TRANSACTION*ÄÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÄ_BÄ:\ÄÇTRANSACTION-NAME\ÄÄfÜÄFØ∞BÄô\ÄBÄúBÄh\ÄBÄÄjBÄµÄBÄ}“ÇSAVE-TRANSACTION¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄOÄÄÄÄ√ÅSELECT-TUPLES*ÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÄ_BÄ:\ÄBÄ∞\ÄÄfÜÄ"˜∫BÄô\ÄBÄúBÄh\ÄBÄ∞BÄjBÄµÄBÄ}“√ÅSELECT-TUPLESÄ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄ%OÄ&ÄÄÄÉÄSUM*ÄÎÄÜÄÑHFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ'Ä_BÄ:\ÄBÄ∞BÄ¬\ÄÄfÜÄJªïBÄô\ÄBÄúBÄh\ÄBÄ∞BÄ¬BÄjBÄµÄBÄ}“ÉÄSUMÄ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡Ä[A¡PP@QíPAQíPÄQBí§D¿BÄpBÄ5OÄ6Ä'ÄÄ√ÄSIZE*ÄÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ7Ä_BÄ:\ÄBÄ∞\ÄÄfÜÄ"7äBÄô\ÄBÄúBÄh\ÄBÄ∞BÄjBÄµÄBÄ}“ÉÄSIZE¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄEOÄFÄ7Ä1Ä\Äp¿BÄ\,ÑFASL-RECORD-FILE-MACROS-EXPANDED\ÄBÄ8\Ä\Ä*ÅDEFMACROÜÄ;√∫\ÄBÄûÜÄN¶™\ÄBÄúÜÄ.ŸãÄÄBÄBÄBÄBÄBÄBÄBÄBÄ˙BÄ	BÄBÄBÄBÄBÄˇBÄ˝BÄ˚BÄÓBÄ¯BÄˆBÄ˜BÄÛBÄÚBÄBÄÔBÄÌBÄÎBÄÈBÄÄÄFÄ#¿BÄ¿BÄ¿BÄ¿BÄ¿BÄ¿BÄ¿BÄ¿BÄ¿BÄLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540764. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "LISP" :NAME "MISC-INTERNAL" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :CHARACTERS T :NOT-BACKED-UP T :CREATION-DATE 2749846239. :AUTHOR "REL3" :LENGTH-IN-BYTES 7201. :LENGTH-IN-BLOCKS 8. :BYTE-SIZE 8.)

;;; -*- Mode:Common-Lisp; Package:RTMS; Fonts:(*CODE-FONT* *COMMENT-FONT* *STRING-FONT*); Base:10 -*-
;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved
;;; MISC-INTERNAL
(defun add-dot (relation attribute)
  (read-from-string (concatenate 'string (string-upcase relation) "." attribute)))

(defun commit-system-relation (&aux insert-name qtrieve-var)
  (cond ((> (length (getp 'system-relation 'commit-tuples)) 0)
 (setf insert-name (string-upcase (concatenate 'string "INSERT-" *system-relation-base-implementation*
     "-" *system-relation-storage-structure*)))

 ;;
 ;;  Insert the tuples into the SYSTEM-RELATION relation
 ;;
 (funcall (find-symbol insert-name *pkg-string*) 'system-relation *system-relation-attributes*
  (getp 'system-relation 'commit-tuples) *system-relation-key* 'system-relation)
 (setf qtrieve-var (caar (qtrieve 'system-relation *system-relation-attributes* '("CARDINALITY")
    *system-relation-key*
    (list 'string-equal 'relation-name "SYSTEM-RELATION"))))
 (delete-or-modify 'system-relation t (list 'string-equal 'relation-name "SYSTEM-RELATION")
   '("MODIFIEDP" "CARDINALITY")
   (list t (+ qtrieve-var (length (getp 'system-relation 'commit-tuples)))))))
  ;;
  ;;  Insert the tuples into the SYSTEM-ATTRIBUTE relation
  ;;
  (cond ((> (length (getp 'system-attribute 'commit-tuples)) 0)
 (funcall (find-symbol insert-name *pkg-string*) 'system-attribute *system-attribute-attributes*
  (getp 'system-attribute 'commit-tuples) *system-attribute-key* 'system-attribute)
 (setf qtrieve-var (caar (qtrieve 'system-relation *system-relation-attributes* '("CARDINALITY")
    *system-relation-key*
    (list 'string-equal 'relation-name "SYSTEM-ATTRIBUTE"))))
 (delete-or-modify 'system-relation t (list 'string-equal 'relation-name "SYSTEM-ATTRIBUTE")
   '("MODIFIEDP" "CARDINALITY")
   (list t (+ qtrieve-var (length (getp 'system-attribute 'commit-tuples)))))))
  ;;
  ;;  Insert the tuples into the SYSTEM-OPTFUNC relation
  ;;
  (cond ((> (length (getp 'system-optfunc 'commit-tuples)) 0)
 (funcall (find-symbol insert-name *pkg-string*) 'system-optfunc *system-optfunc-attributes*
  (getp 'system-optfunc 'commit-tuples) *system-optfunc-key* 'system-optfunc)
 (setf qtrieve-var (caar (qtrieve 'system-relation *system-relation-attributes* '("CARDINALITY")
    *system-relation-key*
    (list 'string-equal 'relation-name "SYSTEM-OPTFUNC"))))
 (delete-or-modify 'system-relation t (list 'string-equal 'relation-name "SYSTEM-OPTFUNC")
   '("MODIFIEDP" "CARDINALITY")
   (list t (+ qtrieve-var (length (getp 'system-optfunc 'commit-tuples)))))))
  ;;
  ;;  Insert the tuples into the SYSTEM-WHEREOPT relation
  ;;
  (cond ((> (length (getp 'system-whereopt 'commit-tuples)) 0)
 (funcall (find-symbol insert-name *pkg-string*) 'system-whereopt *system-whereopt-attributes*
  (getp 'system-whereopt 'commit-tuples) *system-whereopt-key* 'system-whereopt)
 (setf qtrieve-var (caar (qtrieve 'system-relation *system-relation-attributes* '("CARDINALITY")
    *system-relation-key*
    (list 'string-equal 'relation-name "SYSTEM-WHEREOPT"))))
 (delete-or-modify 'system-relation t (list 'string-equal 'relation-name "SYSTEM-WHEREOPT")
   '("MODIFIEDP" "CARDINALITY")
   (list t (+ qtrieve-var (length (getp 'system-whereopt 'commit-tuples)))))))
  ;;
  ;;  Insert the tuples into the SYSTEM-IMPLEMENTATION relation
  ;;
  (cond ((> (length (getp 'system-implementation 'commit-tuples)) 0)
 (funcall (find-symbol insert-name *pkg-string*) 'system-implementation
  *system-implementation-attributes* (getp 'system-implementation 'commit-tuples)
  *system-implementation-key* 'system-implementation)
 (setf qtrieve-var (caar (qtrieve 'system-relation *system-relation-attributes* '("CARDINALITY")
    *system-relation-key*
    (list 'string-equal 'relation-name "SYSTEM-IMPLEMENTATION"))))
 (delete-or-modify 'system-relation t (list 'string-equal 'relation-name "SYSTEM-IMPLEMENTATION")
   '("MODIFIEDP" "CARDINALITY")
   (list t (+ qtrieve-var (length (getp 'system-implementation 'commit-tuples)))))))
  ;;
  ;;  Insert the tuples into the SYSTEM-STORAGE-STRUCTURE relation
  ;;
  (cond ((> (length (getp 'system-storage-structure 'commit-tuples)) 0)
 (funcall (find-symbol insert-name *pkg-string*) 'system-storage-structure
  *system-storage-structure-attributes* (getp 'system-storage-structure 'commit-tuples)
  *system-storage-structure-key* 'system-storage-structure)
 (setf qtrieve-var (caar (qtrieve 'system-relation *system-relation-attributes* '("CARDINALITY")
    *system-relation-key*
    (list 'string-equal 'relation-name "SYSTEM-STORAGE-STRUCTURE"))))
 (delete-or-modify 'system-relation t (list 'string-equal 'relation-name "SYSTEM-STORAGE-STRUCTURE")
 '("MODIFIEDP" "CARDINALITY")
 (list t (+ qtrieve-var (length (getp 'system-storage-structure 'commit-tuples)))))))
  ;;
  ;;  Insert the tuples into the SYSTEM-DOMAIN relation
  ;;
  (cond ((> (length (getp 'system-domain 'commit-tuples)) 0)
 (funcall (find-symbol insert-name *pkg-string*) 'system-domain *system-domain-attributes*
  (getp 'system-domain 'commit-tuples) *system-domain-key* 'system-domain)
 (setf qtrieve-var (caar (qtrieve 'system-relation *system-relation-attributes* '("CARDINALITY")
    *system-relation-key*
    (list 'string-equal 'relation-name "SYSTEM-DOMAIN"))))
 (delete-or-modify 'system-relation t (list 'string-equal 'relation-name "SYSTEM-DOMAIN")
 '("MODIFIEDP" "CARDINALITY")
 (list t (+ qtrieve-var (length (getp 'system-domain 'commit-tuples)))))))
  ;;
  ;; Clear the property list so that tuples are not reinserted next time
  ;;
 (mapcar (function (lambda (sys-rel)
     (putp sys-rel nil 'commit-tuples)))
 *system-relations*))

(defun default-tuple-format (domain-list &aux (result nil))
  (do ((dom domain-list (cdr dom)))
      ((null dom) result)
    (setf result (append result (list (caar (qtrieve 'system-domain *system-domain-attributes*
        '(default-print-width)  *system-domain-key*
        `(string-equal domain-name ,(car dom)))))))))

(defun get-default-value (domain)
  (caar (qtrieve 'system-domain *system-domain-attributes* '(default-value) *system-domain-key*
 `(string-equal domain-name ,domain))))

(defun init-where-opt (&aux function-list)
  (setf *where-opt* '())
  (setf *where-opt-macros* '())
  (setf function-list (qtrieve 'system-whereopt *system-whereopt-attributes* '(function-name)
       *system-whereopt-key* t))
  (do ((function function-list (cdr function)))
      ((null function) t)
    (setf *where-opt* (append (car function) *where-opt*))
    (push (concatenate 'string (caar function) "*") *where-opt-macros*)))

(defun remove-dot-attr (rel-attr)
  (setf rel-attr (string rel-attr))
  (read-from-string (subseq rel-attr (+ 1 (search "." rel-attr)) (length rel-attr))))

(defun remove-dot-rel (rel-attr &aux relation-index)
  (setf rel-attr (string rel-attr))
  (setf relation-index (search "." rel-attr))
  (cond ((equal relation-index nil)
 nil)
(t
 (read-from-string (subseq rel-attr 0 relation-index)))))
Ä_BÄ:\ÄBÄ\ÄÄfÜÄzLBÄô\ÄBÄúBÄh\ÄBÄBÄjBÄµÄBÄ}“CÇDESTROY-ATTRIBUTEÄ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄOÄBÄÄÄCÇDESTROY-ATTRIBUTE*ÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ BÄ_BÄ:\ÄBÄ\ÄÄfÜÄ~?QBÄô\ÄBÄúBÄh\ÄBÄBÄjBÄµÄBÄ}“BÄ¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄ-OÄ.BÄ ÄÄÉÅDESTROY-DB*ÄÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ/BÄ_BÄ:\ÄBÄ$\ÄÄfÜÄ
{VBÄô\ÄBÄúBÄh\ÄBÄ$BÄjBÄµÄBÄ}“ÇDESTROY-DATABASE¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄ=OÄ>BÄ/ÄÄCÇDESTROY-DATABASE*ÄÄÎÄ	ÜÄÑDFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ?BÄ_BÄ:\ÄBÄ$\ÄÄfÜÄbt6BÄô\ÄBÄúBÄh\ÄBÄ$BÄjBÄµÄBÄ}“BÄ<¿BÄ8¿BÄ¢íÄQJôÊÄQJˇ€òÄW@¡PP@QíPÄYíúD¿BÄpBÄLOÄMBÄ?ÄÄÇDESTROY-DOMAIN*ÄÄÎÄ	ÜÄ$@FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄNBÄ_BÄ:BÄLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540768. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "XLD" :NAME "MISC-INTERNAL" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760360316. :AUTHOR "REL3" :LENGTH-IN-BYTES 2066. :LENGTH-IN-BLOCKS 5. :BYTE-SIZE 16.)                                pp2Ä\Ä©ÅCOMPILE-DATA\ÄÏÄSW-MFG,ÅGODZILLAÜÄá§åœFÄFÄ\Äp¿,ÅCOMPILER,ÅVERSIONÄ\ÄFÄFÄp¿BÄ),ÇOPTIMIZE-SWITCHÄÜÄ©ÉQFASL-SOURCE-FILE-UNIQUE-IDÄ1Ä\Äp¿lÄFSÏÇMAKE-FASLOAD-PATHNAMEÄ\ÄÍÄQUOTEÄBÄ$\ÄBÄ8™ÄNILÄ\ÄBÄ8\Ä¨ÄRTMS\ÄBÄ8ÏÅMISC-INTERNALÄ\ÄBÄ8¨ÄLISP\ÄBÄ8FÄ©ÄBASEFÄ
ÈÄFONTSÄ\Ä©Å*CODE-FONT*ÄÈÅ*COMMENT-FONT*ÈÅ*STRING-FONT*Ä)ÅPACKAGEÄ©ÄRTMS©ÄMODE©ÅCOMMON-LISPÄÄÅADD-DOTÄÄÎÄFÄÄFÄ¿$Ä¿BÄ:p¿¨ÄTICLÏÄART-QÄ]ÄFÄÄ:BÄ:BÄ:jÄTÄFÄp¿¨ÄSYSÄlÇDEBUG-INFO-STRUCTÄBÄP\ÄÅRELATIONCÅATTRIBUTEÄBÄ:BÄ:BÄ:ÄÍÄSTRING¿ÍÅSTRING-UPCASEÄ“lÄ.Ä¿™ÅCONCATENATEÄ“*ÇREAD-FROM-STRINGíPÄQäPÅQ¢åOÄgBÄPÄÄ√ÇCOMMIT-SYSTEM-RELATIONÄÎÄ3™áÜÄ@3FÄ›¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄhBÄ:BÄ:\ÄÉÅINSERT-NAMEÄÉÅQTRIEVE-VARÄBÄ:BÄ:BÄ:\Ä)ÇMACROS-EXPANDEDÄ\Ä™ÄPROGp¿¨ÄZLCÄ,ÅDO-NAMEDp¿BÄTÏÇINHIBIT-STYLE-WARNINGS™ÄSETFÄCÇ*SYSTEM-RELATIONS*—ÉÇ*SYSTEM-DOMAIN-KEY*Ä—CÉ*SYSTEM-DOMAIN-ATTRIBUTES*—√É*SYSTEM-STORAGE-STRUCTURE-KEY*—√Ñ*SYSTEM-STORAGE-STRUCTURE-ATTRIBUTES*Ä—ÉÉ*SYSTEM-IMPLEMENTATION-KEY*Ä—CÑ*SYSTEM-IMPLEMENTATION-ATTRIBUTES*—√Ç*SYSTEM-WHEREOPT-KEY*Ä—ÉÉ*SYSTEM-WHEREOPT-ATTRIBUTES*—ÉÇ*SYSTEM-OPTFUNC-KEY*—ÉÉ*SYSTEM-OPTFUNC-ATTRIBUTES*Ä—√Ç*SYSTEM-ATTRIBUTE-KEY*—√É*SYSTEM-ATTRIBUTE-ATTRIBUTES*Ä—√Ç*SYSTEM-RELATION-KEY*Ä—ÉÉ*SYSTEM-RELATION-ATTRIBUTES*—ÉÅ*PKG-STRING*—ÉÑ*SYSTEM-RELATION-STORAGE-STRUCTURE*Ä—√Ñ*SYSTEM-RELATION-BASE-IMPLEMENTATION*ÄëÇSYSTEM-RELATIONÄ¿√ÅCOMMIT-TUPLESÄ¿ÉÄGETP“BÄb¿,ÅINSERT-Ä¿lÄ-Ä¿BÄe“BÄc“™ÅFIND-SYMBOLÄ“\Ä¨ÅCARDINALITYÄ¿™ÅSTRING-EQUAL¿√ÅRELATION-NAMEÄ¿,ÇSYSTEM-RELATIONÄ¿™ÄLIST“ÅQTRIEVEÄ“\ÄlÅMODIFIEDPÄ¨ÅCARDINALITYÄ¿ÇDELETE-OR-MODIFY“ÇSYSTEM-ATTRIBUTE¿,ÇSYSTEM-ATTRIBUTE¿√ÅSYSTEM-OPTFUNC¿ÏÅSYSTEM-OPTFUNC¿ÇSYSTEM-WHEREOPTÄ¿,ÇSYSTEM-WHEREOPTÄ¿√ÇSYSTEM-IMPLEMENTATIONÄ¿ÏÇSYSTEM-IMPLEMENTATIONÄ¿ÉSYSTEM-STORAGE-STRUCTURE¿,ÉSYSTEM-STORAGE-STRUCTURE¿√ÅSYSTEM-DOMAINÄ¿ÏÅSYSTEM-DOMAINÄ¿ÉÄPUTPíPPíJô.‰PPPPP™ä@√PíB¡PPPPíPPB©PPPPP P!P"ö#™BA¡Pˇ›P P!P"ö$Pˇ›AQPPíäCˇa"í%®&PPíJô'‰@QPíB¡&PP&PPíP&PB©PPPPP P'P"ö#™BA¡Pˇ›P P'P"ö$Pˇ›AQ&PPíäCˇa"í%®(PPíJô'‰@QPíB¡(PP(PPíP(PB©PPPPP P)P"ö#™BA¡Pˇ›P P)P"ö$Pˇ›AQ(PPíäCˇa"í%®*PPíJô'‰@QPíB¡*PP*PPí
P*PB©PPPPP P+P"ö#™BA¡Pˇ›P P+P"ö$Pˇ›AQ*PPíäCˇa"í%®,PPíJô'‰@QPíB¡,P	P,PPíP,PB©PPPPP P-P"ö#™BA¡Pˇ›P P-P"ö$Pˇ›AQ,PPíäCˇa"í%®.PPíJô'‰@QPíB¡.PP.PPíP.PB©PPPPP P/P"ö#™BA¡Pˇ›P P/P"ö$Pˇ›AQ.PPíäCˇa"í%®0PPíJô'‰@QPíB¡0PP0PPíP0PB©PPPPP P1P"ö#™BA¡Pˇ›P P1P"ö$Pˇ›AQ0PPíäCˇa"í%®B€B—PD¡C¡	¸CQDSˇ€P2öCC√¡D≈DıÁBOÄ≠BÄhÄÄÉÇDEFAULT-TUPLE-FORMATÄÎÄ
 ÜÄ@HFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÆ\ÄÉÅDOMAIN-LISTÄBÄ:\Ä√ÄRESULTÉÄDOMÄ\ÄBÄt\Äp¿BÄ\lÅXR-BQ-LISTBÄvBÄ|ÄBÄ~—BÄëBÄ™¿\ÄÉÇDEFAULT-PRINT-WIDTHÄ¿BÄó¿ÉÅDOMAIN-NAMEÄ¿BÄö“BÄõ“p¿BÄ\,Å*APPENDÄíÄQA¡‰@QPPPPPPAS	ö
™B	äí@¡A≈Á@OÄƒBÄÆÄÄCÇGET-DEFAULT-VALUEÄÄÎÄFÄ@FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ≈\Ä√ÄDOMAINBÄ:BÄ:\ÄBÄt\ÄBÄæÄBÄ~—BÄëBÄ™¿\Ä√ÅDEFAULT-VALUEÄ¿BÄó¿BÄ¡¿BÄö“BÄõíPPPPPPÄQ	ö
™BˇOÄ”BÄ≈ÄÄ√ÅINIT-WHERE-OPTÄÎÄ(ÜÄ@FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ‘BÄ:BÄ:\Ä√ÅFUNCTION-LISTÄ*ÅFUNCTION\ÄBÄt\Ä™ÄPUSHBÄvBÄ|ÄBÄÑ—BÄÖ—CÇ*WHERE-OPT-MACROS*—ÉÅ*WHERE-OPT*ÄëBÄ§¿\Äp¿BÄTÏÅFUNCTION-NAMEÄ¿BÄõ“BÄ√“BÄb¿lÄ*Ä¿BÄeí⁄⁄PPPPˇ›	™@√A¡‰ASP
í¿PAQBPö\¿A≈ÛÁSOÄËBÄ‘ÄÄÇREMOVE-DOT-ATTRÄÄÎÄFÄ@FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÈ\ÄÅREL-ATTRBÄ:BÄ:\ÄBÄt\ÄBÄ|ÄBÄb“lÄ.Ä¿p¿BÄ\,ÅSEARCH*Ä“ÍÄSUBSEQ“BÄfíÄQäÄ√PÄQíˇkÄQäCöåOÄ˘BÄÈÄÄ√ÅREMOVE-DOT-RELÄÎÄÜÄ@DFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ˙\ÄBÄÚBÄ:\Ä√ÅRELATION-INDEX\ÄBÄt\ÄBÄ|ÄBÄb“lÄ.Ä¿BÄ˜“BÄ¯“BÄfíÄQäÄ¡PÄQí@¡ÊRÄQJ@QöåOÄBÄ˙Ä1Ä\Äp¿BÄ\,ÑFASL-RECORD-FILE-MACROS-EXPANDED\ÄBÄ8\Ä\ÄÍÄDEFUNÄÜÄ'\ÄBÄ·ÜÄ•ò\ÄBÄæÜÄ.Ÿã\ÄBÄ|ÜÄ[ÊÑ\ÄBÄ{ÜÄ(Ã¢\ÄBÄyÜÄ*˝j\ÄBÄvÜÄ=Ã#ÄÄp 'system-implementation 'commit-tuples)))))))
  ;;
  ;;  Insert the tuples into the SYSTEM-STORAGE-STRUCTURE relation
  ;;
  (cond ((> (length (getp 'system-storage-structure 'commit-tuples)) 0)
 (funcall (find-symbol insert-name *pkg-string*) 'system-storage-structure
  *system-storage-structure-attributes* (getp 'system-storage-structure 'commit-tuples)
  *system-storage-structure-key* 'system-storage-structure)
 (setf qtrieve-var (caar (qtrieve 'system-relation *system-relation-attributes* '("CARDINALITY")
    *system-relation-key*
    (list 'string-equal 'relation-name "SYSTEM-STORAGE-STRUCTURE"))))
 (delete-or-modify 'system-relation t (list 'string-equal 'relation-name "SYSTEM-STORAGE-STRUCTURE")
 '("MODIFIEDP" "CARDINALITY")
 (list t (+ qtrieve-var (length (getp 'system-storage-structure 'commit-tuples)))))))
  ;;
  ;;  Insert the tuples into the SYSTEM-DOMAIN relation
  ;;
  (cond ((> (length (getp 'system-domain 'commit-tuples)) 0)
 (funcLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540771. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "LISP" :NAME "MISC-USER" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :CHARACTERS T :NOT-BACKED-UP T :CREATION-DATE 2749846253. :AUTHOR "REL3" :LENGTH-IN-BYTES 1062. :LENGTH-IN-BLOCKS 2. :BYTE-SIZE 8.)

;;; -*- Mode:Common-Lisp; Package:RTMS; Fonts:(*CODE-FONT* *COMMENT-FONT* *STRING-FONT*); Base:10 -*-
;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved
;;; MISC-USER
;;;
;;; This file contains the following Explorer extensions to CommonLisp d as Indicated in the June 1985 Explorer Lisp
;;; Reference
;;;
;;; This file comtains the following obsolete functions
;;;
;;; This file contains the following functions which are unknown in CommonLisp
;;;
;;; The following function contains flavor references and thus are incompatable with CommonLisp. Their removal will not
;;; effect the functionality of RTMS.
;;;

(defun recover-all ()
  (do ((relations *system-relations* (cdr relations)))
      ((null relations) t)
    (putp (car relations) nil 'entry-point)
    (putp (car relations) nil 'commit-tuples))
  (setf *active-db* nil)
  (if *save-user-id*
      (setf user-id *save-user-id*))
  (setf *restore-operation* nil))

(defun recover-restore ()
  (setf *restore-operation* nil))
M-IMPLEMENTATION-KEY*Ä—CÑ*SYSTEM-IMPLEMENTATION-ATTRIBUTES*—√Ç*SYSTEM-WHEREOPT-KEY*Ä—ÉÉ*SYSTEM-WHEREOPT-ATTRIBUTES*—ÉÇ*SYSTEM-OPTFUNC-KEY*—ÉÉ*SYSTEM-OPTFUNC-ATTRIBUTES*Ä—√Ç*SYSTEM-ATTRIBUTE-KEY*—√É*SYSTEM-ATTRIBUTE-ATTRIBUTES*Ä—√Ç*SYSTEM-RELATION-KEY*Ä—ÉÉ*SYSTEM-RELATION-ATTRIBUTES*—ÉÅ*PKG-STRING*—ÉÑ*SYSTEM-RELATION-STORAGE-STRUCTURE*Ä—√Ñ*SYSTEM-RELATION-BASE-IMPLEMENTATION*ÄëÇSYSTEM-RELATIONÄ¿√ÅCOMMIT-TUPLESÄ¿ÉÄGETP“BÄb¿,ÅINSERT-Ä¿lÄ-Ä¿BÄe“BÄc“™ÅFIND-SYMBOLÄ“\Ä¨ÅCARDINALITYÄ¿™ÅSTRING-EQUAL¿√ÅRELATION-NAMEÄ¿,ÇSYSTEM-RELATIONÄ¿™ÄLIST“ÅQTRIEVEÄ“\ÄlÅMODIFIEDPÄ¨ÅCARDINALITYÄ¿ÇDELETE-OR-MODIFY“ÇSYSTEM-ATTRIBUTE¿,ÇSYSTEM-ATTRIBUTE¿√ÅSYSTEM-OPTFUNC¿ÏÅSYSTEM-OPTFUNC¿ÇSYSTEM-WHEREOPTÄ¿,ÇSYSTEM-WHEREOPTÄ¿√ÇSYSTEM-IMPLEMENTATIONÄ¿ÏÇSYSTEM-IMPLEMENTATIONÄ¿ÉSYSTEM-STORAGE-STRUCTURE¿,ÉSYSTEM-STORAGE-STRUCTURE¿√ÅSYSTEM-DOMAINÄ¿ÏÅSYSTEM-DOMAINÄ¿ÉÄPUTPíPPíJô.‰PPPPP™ä@√PíB¡PPPPíPPB©PPPPP P!P"ö#™BA¡Pˇ›P P!PLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540774. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "XLD" :NAME "MISC-USER" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760359818. :AUTHOR "REL3" :LENGTH-IN-BYTES 516. :LENGTH-IN-BLOCKS 2. :BYTE-SIZE 16.)                                     pp2Ä\Ä©ÅCOMPILE-DATA\ÄÏÄSW-MFG,ÅGODZILLAÜÄá§öÕFÄFÄ\Äp¿,ÅCOMPILER,ÅVERSIONÄ\ÄFÄFÄp¿BÄ),ÇOPTIMIZE-SWITCHÄÜÄ©ÉQFASL-SOURCE-FILE-UNIQUE-IDÄ1Ä\Äp¿lÄFSÏÇMAKE-FASLOAD-PATHNAMEÄ\ÄÍÄQUOTEÄBÄ$\ÄBÄ8™ÄNILÄ\ÄBÄ8\Ä¨ÄRTMS\ÄBÄ8lÅMISC-USERÄ\ÄBÄ8¨ÄLISP\ÄBÄ8FÄ©ÄBASEFÄ
ÈÄFONTSÄ\Ä©Å*CODE-FONT*ÄÈÅ*COMMENT-FONT*ÈÅ*STRING-FONT*Ä)ÅPACKAGEÄ©ÄRTMS©ÄMODE©ÅCOMMON-LISPÄÄÉÅRECOVER-ALLÄÄÎÄ
ÜÄ@FÄ¿$Ä¿BÄ:p¿¨ÄTICLÏÄART-QÄ]ÄFÄÄ:BÄ:BÄ:jÄTÄFÄp¿¨ÄSYSÄlÇDEBUG-INFO-STRUCTÄBÄPBÄ:BÄ:\ÄCÅRELATIONSÄ\Ä)ÇMACROS-EXPANDEDÄ\Ä™ÄSETF™ÄPROGÄÉÇ*RESTORE-OPERATION*Ä—p¿BÄT,ÅUSER-IDÄ—√Å*SAVE-USER-ID*—ÉÅ*ACTIVE-DB*Ä—CÇ*SYSTEM-RELATIONS*ëÉÅENTRY-POINTÄ¿ÉÄPUTP“√ÅCOMMIT-TUPLESÄÄP@¡
‰@Sˇ€P	ò@Sˇ€
P	ò@≈ˆÁ⁄‰P¿⁄ROÄoBÄPÄÄÇRECOVER-RESTOREÄÄÎÄFÄFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄpBÄ:BÄ:BÄ:\ÄBÄb\ÄBÄdÄBÄfë⁄ROÄzBÄpÄ1Ä\Äp¿BÄ\,ÑFASL-RECORD-FILE-MACROS-EXPANDED\ÄBÄ8\Ä\ÄÍÄDEFUNÄÜÄ'\ÄBÄeÜÄ=Ã#\ÄBÄdÜÄ[ÊÑÄÄetf *restore-operation* nil))
M-IMPLEMENTATION-KEY*Ä—CÑ*SYSTEM-IMPLEMENTATION-ATTRIBUTES*—√Ç*SYSTEM-WHEREOPT-KEY*Ä—ÉÉ*SYSTEM-WHEREOPT-ATTRIBUTES*—ÉÇ*SYSTEM-OPTFUNC-KEY*—ÉÉ*SYSTEM-OPTFUNC-ATTRIBUTES*Ä—√Ç*SYSTEM-ATTRIBUTE-KEY*—√É*SYSTEM-ATTRIBUTE-ATTRIBUTES*Ä—√Ç*SYSTEM-RELATION-KEY*Ä—ÉÉ*SYSTEM-RELATION-ATTRIBUTES*—ÉÅ*PKG-STRING*—ÉÑ*SYSTEM-RELATION-STORAGE-STRUCTURE*Ä—√Ñ*SYSTEM-RELATION-BASE-IMPLEMENTATION*ÄëÇSYSTEM-RELATIONÄ¿√ÅCOMMIT-TUPLESÄ¿ÉÄGETP“BÄb¿,ÅINSERT-Ä¿lÄ-Ä¿BÄe“BÄc“™ÅFIND-SYMBOLÄ“\Ä¨ÅCARDINALITYÄ¿™ÅSTRING-EQUAL¿√ÅRELATION-NAMEÄ¿,ÇSYSTEM-RELATIONÄ¿™ÄLIST“ÅQTRIEVEÄ“\ÄlÅMODIFIEDPÄ¨ÅCARDINALITYÄ¿ÇDELETE-OR-MODIFY“ÇSYSTEM-ATTRIBUTE¿,ÇSYSTEM-ATTRIBUTE¿√ÅSYSTEM-OPTFUNC¿ÏÅSYSTEM-OPTFUNC¿ÇSYSTEM-WHEREOPTÄ¿,ÇSYSTEM-WHEREOPTÄ¿√ÇSYSTEM-IMPLEMENTATIONÄ¿ÏÇSYSTEM-IMPLEMENTATIONÄ¿ÉSYSTEM-STORAGE-STRUCTURE¿,ÉSYSTEM-STORAGE-STRUCTURE¿√ÅSYSTEM-DOMAINÄ¿ÏÅSYSTEM-DOMAINÄ¿ÉÄPUTPíPPíJô.‰PPPPP™ä@√PíB¡PPPPíPPB©PPPPP P!P"ö#™BA¡Pˇ›P P!PLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540777. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "LISP" :NAME "MODIFY-AVL" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :CHARACTERS T :NOT-BACKED-UP T :CREATION-DATE 2749846273. :AUTHOR "REL3" :LENGTH-IN-BYTES 48083. :LENGTH-IN-BLOCKS 47. :BYTE-SIZE 8.)

;;; -*- Mode:Common-Lisp; Package:RTMS; Fonts:(*CODE-FONT* *COMMENT-FONT* *STRING-FONT*); Base:10 -*-
;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved
;;; MODIFY-AVL
;;;
;;; The following function contains flavor references and thus are incompatable with CommonLisp. Their removal will not
;;; effect the functionality of RTMS.
;;;     modify-flavor-avl
;;;     modify-flavor-tuples
;;;     flavor-avl-key-modify
;;;     process-flavor-avl-modify
;;;

(defun modify-list-avl (relation-name attribute-list key-attributes modify-attributes modify-values
where-clause dom-def indices
        &aux (domains nil) key-domain-list key-value insert-tuples mode (modified-tuples nil)
(number-modified 0) package-name rebalancep temp-attribute-list termination-condition
total-insert-tuples (total-number-modified 0) tree tuples)
  ;;
  ;;  Obtain the key from the where clause from the particular relation.
  ;;
  indices
  (cond ((member (string-upcase relation-name) *system-relations* :test 'string-equal)
 (setf key-domain-list (eval (read-from-string
        (concatenate 'string *pkg-string* ":*" (string-upcase relation-name)
         "-KEY-DOMAINS*")))))
(t
 (do ((domain-element dom-def (cdr domain-element)))
     ((null domain-element) t)
   (setf domains (append (list (second (car domain-element))) domains)))
 (setf key-domain-list (convert-attributes (car (project-list (list (reverse domains)) attribute-list
           key-attributes))))))
  (cond ((symbolp relation-name)
 (setf package-name (package-name (symbol-package relation-name))))
(t
 (setf package-name *pkg-string*)))
  (setf key-value (extract-key-avl attribute-list key-attributes key-domain-list where-clause package-name)
tree (getp relation-name 'entry-point))
  ;;
  ;;  If there is no usable key specified in the where-clause, set the key value to indicate that the entire tree should be searched.
  ;;
  (cond ((null key-value)
 (setf key-value (list (list key-attributes) '((t)) '((t))))))
  ;;
  ;;  Take each key extracted from the WHERE clause and modify the tuples selected by the individual keys.
  ;;
  (let ((string-attribute-list (convert-attributes attribute-list))
(string-modify-attributes (convert-attributes modify-attributes)))
    (do ((key% (first key-value) (cdr key%))
 (beginning-value% (second key-value) (cdr beginning-value%))
 (termination-clause% (third key-value) (cdr termination-clause%)))
((null key%) number-modified)
      (cond ((equal (caar termination-clause%) t)
     (setf termination-condition t))
    (t
     (setf termination-condition (list 'lep (caar key%) (caar termination-clause%)))))
      ;;
      ;;  Since EVAL-WHERE has to be called once per node while searching, the process can be sped up by separating the eval overhead
      ;; from the actual operation. The call to prereq initializes all subsequent calls to SUPER-FAST-EVAL-WHERE.
      ;;
      (multiple-value-setq (where-clause temp-attribute-list)
   (eval-where-prereq where-clause attribute-list relation-name))
      (progv temp-attribute-list nil
(setf mode "TERMINATE"
      number-modified 1
      insert-tuples '(bogus tuple))
(do ((beginning-value (car beginning-value%) beginning-value))
    ((or (equal number-modified 0) (string-equal mode "FINISHED") (null insert-tuples)) t)
  (setf number-modified 0)
  (multiple-value-setq (tree mode rebalancep number-modified beginning-value insert-tuples)
       (list-avl-key-modify tree string-attribute-list key-attributes key-domain-list
       beginning-value termination-condition "LOCATE"
       relation-name where-clause rebalancep number-modified nil
       modify-values string-modify-attributes temp-attribute-list
       dom-def))
  (setf total-insert-tuples (append insert-tuples total-insert-tuples))
  (setf total-number-modified (+ total-number-modified number-modified)
rebalancep nil)))))
    (putp relation-name tree 'entry-point)
    (cond (total-insert-tuples
   ;;
   ;;  Must modify the tuples and then insert them
   ;;
   (progv temp-attribute-list nil
     (setf tuples nil)
     (do ((tuple total-insert-tuples (cdr tuple)))
 ((null tuple) t)
       (setf modified-tuples (cons (modify-tuple attribute-list modify-attributes (car tuple)
     modify-values dom-def relation-name
     temp-attribute-list)
     modified-tuples))))
     (setf total-insert-tuples (insert-list-avl relation-name attribute-list modified-tuples
    key-attributes relation-name))))
    (values total-number-modified total-insert-tuples))

(defun modify-flavor-avl (relation-name attribute-list key-attributes modify-attributes modify-values
  where-clause dom-def indices
  &aux (domains nil) key-domain-list key-value insert-tuples mode (modified-tuples nil)
  (number-modified 0) package-name rebalancep temp-attribute-list termination-condition
  total-insert-tuples (total-number-modified 0) tree tuples)
  "This function will is the driver for the function which will modify the tuples of the specified list avl
   represented relation. A count of the total number of tuples modified is returned.

   RELATION-NAME     - The name of the relation whose tuples will be modified.
   ATTRIBUTE-LIST    - A list of all of the attributes in the relation in string form.
   KEY-ATTRIBUTES    - A list of the attributes which make form the key of the relation.
   MODIFY-ATTRIBUTES - A list of the attributes to be modified.
   MODIFY-VALUES     - A list of the expressions by which the attributes will be modified.
   WHERE-CLAUSE      - An s-expression which is used as a predicate to select the tuples to be modified.
   DOM-DEF           - A list of elements. Each element is a list containing the name of the attribute, the
                       domain of the element and the default value of the attribute.
   INDICES           - A boolean value which indicates of there are any indices defined on this relation."
  ;;
  ;;  Obtain the key from the where clause from the particular relation.
  ;;
  indices
  (cond ((member (string-upcase relation-name) *system-relations* :test 'string-equal)
 (setf key-domain-list (eval (read-from-string
        (concatenate 'string *pkg-string* ":*" (string-upcase relation-name)
         "-KEY-DOMAINS*")))))
(t
 (do ((domain-element dom-def (cdr domain-element)))
     ((null domain-element) t)
   (setf domains (append (list (second (car domain-element))) domains)))
 (setf key-domain-list (car (project-list (list (reverse domains)) attribute-list key-attributes)))))
  (cond ((symbolp relation-name)
 (setf package-name (package-name (symbol-package relation-name))))
(t
 (setf package-name *pkg-string*)))
  (setf key-value (extract-key-avl attribute-list key-attributes key-domain-list where-clause package-name)
tree (getp relation-name 'entry-point))
  ;;
  ;;  If there is no usable key specified in the where-clause, set the key value to indicate that the entire tree should be searched.
  ;;
  (cond ((null key-value)
 (setf key-value (list (list key-attributes) '((t)) '((t))))))
  ;;
  ;;  Take each key extracted from the WHERE clause and modify the tuples selected by the individual keys.
  ;;
  (do ((key% (first key-value) (cdr key%))
       (beginning-value% (second key-value) (cdr beginning-value%))
       (termination-clause% (third key-value) (cdr termination-clause%)))
      ((null key%) number-modified)
    (cond ((equal (caar termination-clause%) t)
   (setf termination-condition t))
  (t
   (setf termination-condition (list 'lep (caar key%) (caar termination-clause%)))))
    ;;
    ;;  Since EVAL-WHERE has to be called once per node while searching, the process can be sped up by separating
    ;; the eval overhead from the actual operation. The call to prereq initializes all subsequent calls to SUPER-FAST-EVAL-WHERE.
    ;;
    (multiple-value-setq (where-clause temp-attribute-list)
      (eval-where-prereq where-clause attribute-list relation-name))
    (progv temp-attribute-list nil
      (setf mode "TERMINATE"
    number-modified 1
    insert-tuples '(bogus tuple))
      (do ((beginning-value (car beginning-value%) beginning-value))
  ((or (equal number-modified 0)(string-equal mode "FINISHED")(null insert-tuples)) t)
(setf number-modified 0)
(multiple-value-setq (tree mode rebalancep number-modified beginning-value insert-tuples)
  (flavor-avl-key-modify tree (convert-attributes attribute-list) key-attributes key-domain-list
  beginning-value termination-condition "LOCATE"  relation-name where-clause
  rebalancep number-modified nil modify-values modify-attributes
  temp-attribute-list dom-def))
(setf total-insert-tuples (append insert-tuples total-insert-tuples))
(setf total-number-modified (+ total-number-modified number-modified)
      rebalancep nil))))
    (putp relation-name tree 'entry-point)
    (cond (total-insert-tuples
   ;;
   ;;  Must modify the tuples and then insert them
   ;;
   (progv temp-attribute-list nil
     (setf tuples nil)
     (do ((tuple total-insert-tuples (cdr tuple)))
 ((null tuple) t)
       (setf modified-tuples (cons (modify-tuple attribute-list modify-attributes (car tuple)
     modify-values dom-def relation-name
     temp-attribute-list)
     modified-tuples)))
     (setf total-insert-tuples (insert-flavor-avl relation-name attribute-list modified-tuples
      key-attributes relation-name)))))
    (values total-number-modified total-insert-tuples))

(defun modify-struct-avl (relation-name attribute-list key-attributes modify-attributes modify-values
  where-clause dom-def indices
  &aux (domains nil) key-domain-list key-value insert-tuples mode (modified-tuples nil)
  (number-modified 0) package-name rebalancep temp-attribute-list termination-condition
  total-insert-tuples (total-number-modified 0) tree tuples)
  "This function will is the driver for the function which will modify the tuples of the specified list avl
   represented relation. A count of the total number of tuples modified is returned.

   RELATION-NAME     - The name of the relation whose tuples will be modified.
   ATTRIBUTE-LIST    - A list of all of the attributes in the relation in string form.
   KEY-ATTRIBUTES    - A list of the attributes which make form the key of the relation.
   MODIFY-ATTRIBUTES - A list of the attributes to be modified.
   MODIFY-VALUES     - A list of the expressions by which the attributes will be modified.
   WHERE-CLAUSE      - An s-expression which is used as a predicate to select the tuples to be modified.
   DOM-DEF           - A list of elements. Each element is a list containing the name of the attribute, the
                       domain of the element and the default value of the attribute.
   INDICES           - A boolean value which indicates if there are indices defined on the relation."
  ;;
  ;;  Obtain the key from the where clause from the particular relation.
  ;;
  indices
  (cond ((member (string-upcase relation-name) *system-relations* :test 'string-equal)
 (setf key-domain-list (eval (read-from-string
        (concatenate 'string *pkg-string* ":*" (string-upcase relation-name)
         "-KEY-DOMAINS*")))))
(t
 (do ((domain-element dom-def (cdr domain-element)))
     ((null domain-element) t)
   (setf domains (append (list (second (car domain-element))) domains)))
 (setf key-domain-list (car (project-list (list (reverse domains)) attribute-list key-attributes)))))
  (cond ((symbolp relation-name)
 (setf package-name (package-name (symbol-package relation-name))))
(t
 (setf package-name *pkg-string*)))
  (setf key-value (extract-key-avl attribute-list key-attributes key-domain-list where-clause package-name)
tree (getp relation-name 'entry-point))
  ;;
  ;;  If there is no usable key specified in the where-clause, set the key value to indicate that the entire tree should be searched.
  ;;
  (cond ((null key-value)
 (setf key-value (list (list key-attributes) '((t)) '((t))))))
  ;;
  ;;  Take each key extracted from the WHERE clause and modify the tuples selected by the individual keys.
  ;;
  (do ((key% (first key-value) (cdr key%))
       (beginning-value% (second key-value) (cdr beginning-value%))
       (termination-clause% (third key-value) (cdr termination-clause%)))
      ((null key%) number-modified)
    (cond ((equal (caar termination-clause%) t)
   (setf termination-condition t))
  (t
   (setf termination-condition (list 'lep (caar key%) (caar termination-clause%)))))
    ;;
    ;;  Since EVAL-WHERE has to be called once per node while searching, the process can be sped up by separating
    ;; the eval overhead from the actual operation. The call to prereq initializes all subsequent calls to SUPER-FAST-EVAL-WHERE.
    ;;
    (multiple-value-setq (where-clause temp-attribute-list)
      (eval-where-prereq where-clause attribute-list relation-name))
    (progv temp-attribute-list nil
      (setf mode "TERMINATE"
    number-modified 1
    insert-tuples '(bogus tuple))
      (do ((beginning-value (car beginning-value%) beginning-value))
  ((or (equal number-modified 0) (string-equal mode "FINISHED") (null insert-tuples)) t)
(setf number-modified 0)
(multiple-value-setq (tree mode rebalancep number-modified beginning-value insert-tuples)
  (struct-avl-key-modify tree (convert-attributes attribute-list) key-attributes key-domain-list
  beginning-value termination-condition "LOCATE"  relation-name where-clause
  rebalancep number-modified nil modify-values modify-attributes
  temp-attribute-list dom-def))
(setf total-insert-tuples (append insert-tuples total-insert-tuples))
 (setf total-number-modified (+ total-number-modified number-modified)
      rebalancep nil))))
    (putp relation-name tree 'entry-point)
    (cond (total-insert-tuples
   ;;
   ;;  Must modify the tuples and then insert them
   ;;
   (progv temp-attribute-list nil
     (setf tuples nil)
     (do ((tuple total-insert-tuples (cdr tuple)))
 ((null tuple) t)
       (setf modified-tuples (cons (modify-tuple attribute-list modify-attributes (car tuple)
     modify-values dom-def relation-name
       temp-attribute-list)
     modified-tuples)))
     (setf total-insert-tuples (insert-struct-avl relation-name attribute-list modified-tuples
      key-attributes relation-name)))))
    (values total-number-modified total-insert-tuples))

(defun modify-flavor-tuples (relation attribute-list modify-attributes modify-values where dom-def tuples
     temp-attribute-list
     &aux atom-attribute-list atom-modify-attributes flavor-package data
     conv-attribute-list)
  temp-attribute-list where

  (setf flavor-package (package-name (symbol-package (typep (car tuples)))))
  (setf atom-attribute-list (unconvert-attributes attribute-list flavor-package)
atom-modify-attributes (unconvert-attributes modify-attributes flavor-package))
  (setf conv-attribute-list (project-flavor-prereq attribute-list))
  (setf data (fast-project-flavor tuples conv-attribute-list))
  (mapcar (function (lambda (tuple a-tuple)
      (mapc
(function
  (lambda (attr val &aux tempval)
    (setf tempval  (eval (sublis (form-alist (quote-tuple a-tuple) atom-attribute-list)
    val)))
    (cond ((or (member (string-upcase relation) *system-relations* :test 'string-equal)
        (not *validity-checking*) (dom-check tempval attr dom-def))
    (set-in-instance tuple attr tempval)))))
atom-modify-attributes modify-values)
      tuple))
  tuples data))


(defun modify-struct-tuples (relation attribute-list modify-attributes modify-values where dom-def tuples
     temp-attribute-list
     &aux atom-attribute-list atom-modify-attributes delormod? struct-attribute-list
     (num-modified 0) temp-struct struct-modify-attributes
     (string-relation-name (string relation)))
  temp-attribute-list where

  (setf struct-attribute-list (unconvert-attributes (mapcar #'(lambda (attr)
     (concatenate 'string string-relation-name
           attr))
        attribute-list))
struct-modify-attributes (unconvert-attributes (mapcar #'(lambda (attr)
        (concatenate 'string string-relation-name
         attr))
           modify-attributes)))
  (setf atom-attribute-list (unconvert-attributes attribute-list)
atom-modify-attributes (unconvert-attributes modify-attributes))
  (mapc (function (lambda (struct-tuple)
             (setf delormod? nil)
     (setf temp-struct (mapcar (function (lambda (attr)
      (funcall attr struct-tuple)))
        struct-attribute-list))
             (cond ((or (equal where t)
 (super-fast-eval-where (list temp-struct) temp-attribute-list where))
    (mapcar (function (lambda (attr val struct-attr &aux tempval)
   (setf tempval (eval (sublis (form-alist (quote-tuple temp-struct)
        atom-attribute-list)
          val)))
   (if (or (member (string-upcase relation) *system-relations*
     :test 'string-equal)
    (not *validity-checking*) (dom-check tempval attr dom-def))
       (progn
  (setf delormod? t)
  (eval `(setf (,struct-attr ,struct-tuple) ',tempval))))))
                            atom-modify-attributes modify-values struct-modify-attributes)
    (if delormod?
(setf num-modified (+ 1 num-modified)))))))
  tuples)
  num-modified)

(defun modavl (tree rebalancep temp-tree &aux modtree)
  (cond ((fourth tree)
 (multiple-value-setq (modtree rebalancep temp-tree)
   (modavl (fourth tree) rebalancep temp-tree))
 (rplaca (cdddr tree) modtree)
 (cond (rebalancep
(multiple-value-setq (tree rebalancep)
  (balance2 tree rebalancep)))))
(t
 (rplaca temp-tree (car tree))
 (setf tree (third tree)
       rebalancep t)))
  (values tree rebalancep temp-tree))

;;; This function is a recursive function which will search the tree for the beginning node and then begin its traversal of the tree modifying
;;; tuples. If modification of the key is involved, the tuples will be deleted and the tree rebalanced as necessary. The deleted tuple will be
;;; modified and added to the insert-tuple list to be added at a later time.
;;;
;;; The values returned are : tree mode rebalancep number-modified beginning-value insert-tuples
(defun list-avl-key-modify (tree attribute-list key-attributes domains beginning-value termination-clause mode
    relation-name where-clause rebalancep number-modified insert-tuples modify-values
    modify-attributes temp-attribute-list dom-def
    &aux comparison-operator current-node-key-value mod-tree)
  ;;
  ;;  Locate the node where the search will begin
  ;;
  (cond ((and (not (equal tree nil)) (or (string-equal mode 'locate) (string-equal mode 'locate-stage-2)
   (string-equal mode "DELETE-SEARCH")))
 (setf current-node-key-value (car (project-list (list (caar tree)) attribute-list key-attributes)))
 (if (equal (car beginning-value) t)
     (setf comparison-operator 'less-than)
     (setf comparison-operator (node-compare beginning-value current-node-key-value domains)))
 (cond
   ;;
   ;;  The beginning reference key value is less than the current node value, take the left branch
   ;;
       ((and (equal comparison-operator 'less-than) (string-equal mode 'locate))
(multiple-value-setq (mod-tree mode rebalancep number-modified beginning-value insert-tuples)
  (list-avl-key-modify (third tree) attribute-list key-attributes domains beginning-value
        termination-clause mode relation-name where-clause rebalancep
        number-modified insert-tuples modify-values modify-attributes
        temp-attribute-list dom-def))
(rplaca (cddr tree) mod-tree)
(cond (rebalancep
       (multiple-value-setq (tree rebalancep)
 (balance1 tree rebalancep))
       (if rebalancep
   (setf mode "TERMINATE")
   (setf mode "RESTART"))))
(cond ((and (not (string-equal mode "RESTART"))(not (string-equal mode "TERMINATE"))
    (not (string-equal mode "FINISHED")))
       (multiple-value-setq (mod-tree mode rebalancep number-modified beginning-value
         insert-tuples)
 (process-list-avl-modify tree attribute-list where-clause number-modified rebalancep
     mode beginning-value relation-name termination-clause
     key-attributes domains insert-tuples modify-values
     modify-attributes temp-attribute-list dom-def))
       (setf tree mod-tree))))
       ((and (equal comparison-operator 'greater-than) (string-equal mode 'locate))
(multiple-value-setq (mod-tree mode rebalancep number-modified beginning-value insert-tuples)
  (list-avl-key-modify (cadddr tree) attribute-list key-attributes domains beginning-value
        termination-clause mode relation-name where-clause rebalancep
        number-modified insert-tuples modify-values modify-attributes
        temp-attribute-list dom-def))
(rplaca (cdddr tree) mod-tree)
(cond (rebalancep
       (multiple-value-setq (tree rebalancep)
 (balance2 tree rebalancep))
       (if rebalancep
   (setf mode "TERMINATE")
   (setf mode "RESTART")))))
       ((string-equal comparison-operator 'equal)
;;
;;  Found a node that is equal to the current tuple as far as the key goes. This might not however be the only
;; node in the tree which is equavilent with the current key value. This is because the key used in the retrieval
;; may not be the complete key of the relation. Because of this, must continue to travel along the left path until
;; the node is no longer equal.
;;
(setf mode  "LOCATE-STAGE-2")
(multiple-value-setq (mod-tree mode rebalancep number-modified beginning-value insert-tuples)
  (list-avl-key-modify (caddr tree) attribute-list key-attributes domains beginning-value
        termination-clause mode relation-name where-clause rebalancep
        number-modified insert-tuples modify-values modify-attributes
        temp-attribute-list dom-def))
;;
;;  The first time control is passed to this location, the beginning node has been found
;;
;;
;;  Need to determine if the current node is to be deleted. This is done by EVALuating the where clause for the
;; current node and determining if the key of the tuple will be modified. Also must be wary for more than a single
;; tuple per node. If tuples get deleted from within the node but not the node itself, searching can continue. This
;; function must only be rewound out when a rebalancing needs to be done.
;;
(if (or (string-equal mode "LOCATE") (string-equal mode "LOCATE-STAGE-2"))
    (setf mode "DELETE-SEARCH"))
(rplaca (cddr tree) mod-tree)
(cond (rebalancep
       (multiple-value-setq (tree rebalancep)
 (balance1 tree rebalancep))
       (if rebalancep
   (setf mode "TERMINATE")
   (setf mode "RESTART"))))
(cond ((and (not (string-equal mode "RESTART"))(not (string-equal mode "TERMINATE"))
    (not (string-equal mode "FINISHED")))
       (multiple-value-setq (mod-tree mode rebalancep number-modified beginning-value
         insert-tuples)
 (process-list-avl-modify tree attribute-list where-clause number-modified rebalancep
     mode beginning-value relation-name termination-clause
     key-attributes domains insert-tuples modify-values
     modify-attributes temp-attribute-list dom-def))
       (setf tree mod-tree))))))
 ((null tree)
  (setf mode "DELETE-SEARCH")))
  (cond ((string-equal mode "RESTART")
 (multiple-value-setq (tree mode rebalancep number-modified beginning-value insert-tuples)
   (list-avl-key-modify tree attribute-list key-attributes domains beginning-value termination-clause
 "LOCATE" relation-name where-clause rebalancep number-modified insert-tuples
 modify-values modify-attributes temp-attribute-list dom-def))))
  (values tree mode rebalancep number-modified beginning-value insert-tuples))

;;; This function is a recursive function which will search the tree for the beginning node and then begin its traversal of the tree modifying
;;; tuples. If modification of the key is involved, the tuples will be deleted and the tree rebalanced as necessary. The deleted tuple will be
;;; modified and added to the insert-tuple list to be added at a later time.
;;;
;;; The values returned are : tree mode rebalancep number-modified beginning-value insert-tuples
(defun flavor-avl-key-modify (tree attribute-list key-attributes domains beginning-value termination-clause mode
      relation-name where-clause rebalancep number-modified insert-tuples modify-values
      modify-attributes temp-attribute-list dom-def
      &aux comparison-operator current-node-key-value mod-tree)
  ;;
  ;;  Locate the node where the search will begin
  ;;
  (cond ((and (not (equal tree nil)) (or (string-equal mode 'locate) (string-equal mode 'locate-stage-2)
   (string-equal mode "DELETE-SEARCH")))
 (setf current-node-key-value (car (project-flavor (list (caar tree)) attribute-list key-attributes)))
 (if (equal (car beginning-value) t)
     (setf comparison-operator 'less-than)
     (setf comparison-operator (node-compare beginning-value current-node-key-value domains)))
 (cond
   ;;
   ;;  The beginning reference key value is less than the current node value, take the left branch
   ;;
       ((and (equal comparison-operator 'less-than) (string-equal mode 'locate))
(multiple-value-setq (mod-tree mode rebalancep number-modified beginning-value insert-tuples)
  (flavor-avl-key-modify (third tree) attribute-list key-attributes domains beginning-value
   termination-clause mode relation-name where-clause rebalancep
   number-modified insert-tuples modify-values modify-attributes
   temp-attribute-list dom-def))
(rplaca (cddr tree) mod-tree)
(cond (rebalancep
       (multiple-value-setq (tree rebalancep)
 (balance1 tree rebalancep))
       (if rebalancep
   (setf mode "TERMINATE")
   (setf mode "RESTART"))))
(cond ((and (not (string-equal mode "RESTART"))(not (string-equal mode "TERMINATE"))
    (not (string-equal mode "FINISHED")))
       (multiple-value-setq (mod-tree mode rebalancep number-modified beginning-value
         insert-tuples)
 (process-flavor-avl-modify tree attribute-list where-clause number-modified rebalancep
       mode beginning-value relation-name termination-clause
       key-attributes domains insert-tuples modify-values
       modify-attributes temp-attribute-list dom-def))
       (setf tree mod-tree))))
       ((and (equal comparison-operator 'greater-than) (string-equal mode 'locate))
(multiple-value-setq (mod-tree mode rebalancep number-modified beginning-value insert-tuples)
  (flavor-avl-key-modify (cadddr tree) attribute-list key-attributes domains beginning-value
        termination-clause mode relation-name where-clause rebalancep
        number-modified insert-tuples modify-values modify-attributes
        temp-attribute-list dom-def))
(rplaca (cdddr tree) mod-tree)
(cond (rebalancep
       (multiple-value-setq (tree rebalancep)
 (balance2 tree rebalancep))
       (if rebalancep
   (setf mode "TERMINATE")
   (setf mode "RESTART")))))
       ((string-equal comparison-operator 'equal)
;;
;;  Found a node that is equal to the current tuple as far as the key goes. This might not however be the only
;; node in the tree which is equavilent with the current key value. This is because the key used in the retrieval
;; may not be the complete key of the relation. Because of this, must continue to travel along the left path until
;; the node is no longer equal.
;;
(setf mode  "LOCATE-STAGE-2")
(multiple-value-setq (mod-tree mode rebalancep number-modified beginning-value insert-tuples)
  (flavor-avl-key-modify (caddr tree) attribute-list key-attributes domains beginning-value
        termination-clause mode relation-name where-clause rebalancep
        number-modified insert-tuples modify-values modify-attributes
        temp-attribute-list dom-def))
;;
;;  The first time control is passed to this location, the beginning node has been found
;;
;;
;;  Need to determine if the current node is to be deleted. This is done by EVALuating the where clause for the current
;; node and determining if the key of the tuple will be modified. Also must be wary for more than a single tuple per
;; node. If tuples get deleted from within the node but not the node itself, searching can continue. This function must
;; only be rewound out when a rebalancing needs to be done.
;;
(if (or (string-equal mode "LOCATE") (string-equal mode "LOCATE-STAGE-2"))
    (setf mode "DELETE-SEARCH"))
(rplaca (cddr tree) mod-tree)
(cond (rebalancep
       (multiple-value-setq (tree rebalancep)
 (balance1 tree rebalancep))
       (if rebalancep
   (setf mode "TERMINATE")
   (setf mode "RESTART"))))
(cond ((and (not (string-equal mode "RESTART"))(not (string-equal mode "TERMINATE"))
    (not (string-equal mode "FINISHED")))
       (multiple-value-setq (mod-tree mode rebalancep number-modified beginning-value
         insert-tuples)
 (process-flavor-avl-modify tree attribute-list where-clause  number-modified
       rebalancep mode beginning-value relation-name
       termination-clause key-attributes domains insert-tuples
       modify-values modify-attributes temp-attribute-list
       dom-def))
       (setf tree mod-tree))))))
 ((null tree)
  (setf mode "DELETE-SEARCH")))
  (cond ((string-equal mode "RESTART")
 (multiple-value-setq (tree mode rebalancep number-modified beginning-value insert-tuples)
   (flavor-avl-key-modify tree attribute-list key-attributes domains beginning-value termination-clause
 "LOCATE" relation-name where-clause rebalancep number-modified insert-tuples
 modify-values modify-attributes temp-attribute-list dom-def))))
  (values tree mode rebalancep number-modified beginning-value insert-tuples))

;;; This function is a recursive function which will search the tree for the beginning node and then begin its traversal of the tree modifying
;;; tuples. If modification of the key is involved, the tuples will be deleted and the tree rebalanced as necessary. The deleted tuple will be
;;; modified and added to the insert-tuple list to be added at a later time.
;;;
;;; The values returned are : tree mode rebalancep number-modified beginning-value insert-tuples
(defun struct-avl-key-modify (tree attribute-list key-attributes domains beginning-value termination-clause mode
      relation-name where-clause rebalancep number-modified insert-tuples modify-values
      modify-attributes temp-attribute-list dom-def
      &aux comparison-operator current-node-key-value mod-tree)
  ;;
  ;;  Locate the node where the search will begin
  ;;
  (cond ((and (not (equal tree nil)) (or (string-equal mode 'locate) (string-equal mode 'locate-stage-2)
   (string-equal mode "DELETE-SEARCH")))
 (setf current-node-key-value (car (project-struct (list (caar tree)) attribute-list key-attributes
       relation-name)))
 (if (equal (car beginning-value) t)
     (setf comparison-operator 'less-than)
     (setf comparison-operator (node-compare beginning-value current-node-key-value domains)))
 (cond
   ;;
   ;;  The beginning reference key value is less than the current node value, take the left branch
   ;;
       ((and (equal comparison-operator 'less-than) (string-equal mode 'locate))
(multiple-value-setq (mod-tree mode rebalancep number-modified beginning-value insert-tuples)
  (struct-avl-key-modify (third tree) attribute-list key-attributes domains beginning-value
   termination-clause mode relation-name where-clause rebalancep
   number-modified insert-tuples modify-values modify-attributes
   temp-attribute-list dom-def))
(rplaca (cddr tree) mod-tree)
(cond (rebalancep
       (multiple-value-setq (tree rebalancep)
 (balance1 tree rebalancep))
       (if rebalancep
   (setf mode "TERMINATE")
   (setf mode "RESTART"))))
(cond ((and (not (string-equal mode "RESTART"))(not (string-equal mode "TERMINATE"))
    (not (string-equal mode "FINISHED")))
       (multiple-value-setq (mod-tree mode rebalancep number-modified beginning-value
         insert-tuples)
 (process-struct-avl-modify tree attribute-list where-clause number-modified rebalancep
       mode beginning-value relation-name termination-clause
       key-attributes domains insert-tuples modify-values
       modify-attributes temp-attribute-list dom-def))
       (setf tree mod-tree))))
       ((and (equal comparison-operator 'greater-than) (string-equal mode 'locate))
(multiple-value-setq (mod-tree mode rebalancep number-modified beginning-value insert-tuples)
  (struct-avl-key-modify (cadddr tree) attribute-list key-attributes domains beginning-value
   termination-clause mode relation-name where-clause rebalancep
   number-modified insert-tuples modify-values modify-attributes
   temp-attribute-list dom-def))
(rplaca (cdddr tree) mod-tree)
(cond (rebalancep
       (multiple-value-setq (tree rebalancep)
 (balance2 tree rebalancep))
       (if rebalancep
   (setf mode "TERMINATE")
   (setf mode "RESTART")))))
       ((string-equal comparison-operator 'equal)
;;
;;  Found a node that is equal to the current tuple as far as the key goes. This might not however be the only
;; node in the tree which is equavilent with the current key value. This is because the key used in the retrieval
;; may not be the complete key of the relation. Because of this, must continue to travel along the left path until
;; the node is no longer equal.
;;
(setf mode  "LOCATE-STAGE-2")
(multiple-value-setq (mod-tree mode rebalancep number-modified beginning-value insert-tuples)
  (struct-avl-key-modify (caddr tree) attribute-list key-attributes domains beginning-value
   termination-clause mode relation-name where-clause rebalancep
   number-modified insert-tuples modify-values modify-attributes
   temp-attribute-list dom-def))
;;
;;  The first time control is passed to this location, the beginning node has been found
;;
;;
;;  Need to determine if the current node is to be deleted. This is done by EVALuating the where clause for the current
;; node and determining if the key of the tuple will be modified. Also must be wary for more than a single tuple per
;; node. If tuples get deleted from within the node but not the node itself, searching can continue. This function must
;; only be rewound out when a rebalancing needs to be done.
;;
(if (or (string-equal mode "LOCATE") (string-equal mode "LOCATE-STAGE-2"))
    (setf mode "DELETE-SEARCH"))
(rplaca (cddr tree) mod-tree)
(cond (rebalancep
       (multiple-value-setq (tree rebalancep)
 (balance1 tree rebalancep))
       (if rebalancep
   (setf mode "TERMINATE")
   (setf mode "RESTART"))))
(cond ((and (not (string-equal mode "RESTART"))(not (string-equal mode "TERMINATE"))
    (not (string-equal mode "FINISHED")))
       (multiple-value-setq (mod-tree mode rebalancep number-modified beginning-value
         insert-tuples)
 (process-struct-avl-modify tree attribute-list where-clause number-modified rebalancep
       mode beginning-value relation-name termination-clause
       key-attributes domains insert-tuples modify-values
       modify-attributes temp-attribute-list dom-def))
       (setf tree mod-tree))))))
 ((null tree)
  (setf mode "DELETE-SEARCH")))
  (cond ((string-equal mode "RESTART")
 (multiple-value-setq (tree mode rebalancep number-modified beginning-value insert-tuples)
   (struct-avl-key-modify tree attribute-list key-attributes domains beginning-value termination-clause
 "LOCATE" relation-name where-clause rebalancep number-modified insert-tuples
 modify-values modify-attributes temp-attribute-list dom-def))))
  (values tree mode rebalancep number-modified beginning-value insert-tuples))

(defun process-list-avl-modify (tree attribute-list where-clause number-modified rebalancep mode
 beginning-value relation-name termination-clause key-attributes domains
 insert-tuples modify-values modify-attributes temp-attribute-list dom-def
 &aux delete-tuples (key-modified nil) mod-tree new-node temp-tree tuples)
  ;;
  ;;  Now that the tuples which satisfy the where-clause from the current node have been gathered, they need to be deleted, if the key
  ;; attributes are modified or modified if th4e modification does not involve the key.
  ;;
  (do ((key% key-attributes (cdr key%)))
      ((or (null key%) key-modified) key-modified)
    (setf key-modified (member (car key%) modify-attributes :test 'string-equal)))
  (cond (key-modified
 (setf delete-tuples (super-fast-eval-where (first tree) temp-attribute-list where-clause))
 (cond ((> (length delete-tuples) 0)
(setf insert-tuples (append delete-tuples insert-tuples))
(mapc (function (lambda (node-tuple)
   (cond ((not (member node-tuple insert-tuples))
   (setf tuples (append (list node-tuple) tuples))))))
      (first tree))
(setf number-modified (+ number-modified (length delete-tuples)))
;;
;;  No tuples are left in the node, delete the node
;;
(cond ((null tuples)
       (setf beginning-value (car (project-list (list (caar tree)) attribute-list
     key-attributes)))
       (rplaca tree (list (caar tree)))
       (setf mode "TERMINATE"
     temp-tree tree)
       (cond ((equal (car (fourth temp-tree)) nil)
      (setf tree (third temp-tree)
     rebalancep t))
     ((equal (car (third temp-tree)) nil)
      (setf tree (fourth temp-tree)
     rebalancep t))
     (t
      (multiple-value-setq (mod-tree rebalancep temp-tree)
 (modavl (third tree) rebalancep temp-tree))
      (rplaca (cddr temp-tree) mod-tree)
      (cond (rebalancep
      (multiple-value-setq (tree rebalancep)
        (balance1 tree rebalancep)))))))
      (t
       (rplaca tree tuples))))))
((null key-modified)
 ;;
 ;;  The tuples can simply be modified within the node they reside
 ;;
 (setf new-node nil)
 (do ((tuple% (first tree) (cdr tuple%)))
     ((null tuple%) t)
   (cond ((super-fast-eval-where (list (car tuple%)) temp-attribute-list where-clause)
  (setf number-modified (+ number-modified 1))
  (setf new-node (cons (modify-tuple attribute-list modify-attributes (car tuple%)
        modify-values dom-def relation-name temp-attribute-list)
        new-node)))
 (t
  (setf new-node (cons (car tuple%) new-node)))))
 (rplaca tree new-node)))
  ;;
  ;;  If the current node is not to be deleted and it does not invalidate the termination clause, process the right subtree for deletion.
  ;;
  (cond ((not (string-equal mode "TERMINATE"))
 (cond ((super-fast-eval-where (first tree) temp-attribute-list termination-clause)
(multiple-value-setq (mod-tree mode rebalancep number-modified beginning-value insert-tuples)
  (list-avl-key-modify (cadddr tree) attribute-list key-attributes domains beginning-value
        termination-clause "LOCATE" relation-name where-clause rebalancep
        number-modified insert-tuples modify-values modify-attributes
        temp-attribute-list dom-def))
(rplaca (cdddr tree) mod-tree)
(cond (rebalancep
       (multiple-value-setq (tree rebalancep)
 (balance2 tree rebalancep))
       (setf mode "TERMINATE"))))
       ((first tree)
(setf mode "FINISHED"
      beginning-value nil)))))
  (values tree mode rebalancep number-modified beginning-value insert-tuples))

(defun process-flavor-avl-modify (tree attribute-list where-clause number-modified rebalancep mode
   beginning-value relation-name termination-clause key-attributes domains
   insert-tuples modify-values modify-attributes temp-attribute-list dom-def
   &aux delete-tuples delete-flavor-tuple (key-modified nil) mod-tree temp-tree
   tuples)
  ;;
  ;;  Now that the tuples which satisfy the where-clause from the current node have been gathered, they need to be
  ;; deleted, if the key  attributes are modified or modified if th4e modification does not involve the key.
  ;;
  domains
  (do ((key% key-attributes (cdr key%)))
      ((or (null key%) key-modified) key-modified)
    (setf key-modified (member (car key%) modify-attributes :test 'string-equal)))
  (mapc (function (lambda (flavor-tuple list-tuple)
    (cond ((super-fast-eval-where (list list-tuple) temp-attribute-list where-clause)
   (setf delete-tuples (cons list-tuple delete-tuples)
  delete-flavor-tuple (cons flavor-tuple delete-flavor-tuple))))))
(car tree) (project-flavor (car tree) temp-attribute-list temp-attribute-list))
  (cond (key-modified
 ;;
 ;;  Any tuples to delete from the current node ??
 ;;
 (cond ((> (length delete-tuples) 0)
(setf insert-tuples (append delete-tuples insert-tuples))
(mapc (function (lambda (node-tuple)
   (if (not (member node-tuple delete-flavor-tuple))
       (setf tuples (append (list node-tuple) tuples)))))
      (first tree))
(setf number-modified (+ number-modified (length delete-tuples)))
;;
;;  No tuples are left in the node, delete the node
;;
(cond ((null tuples)
       (setf beginning-value (car (project-flavor (list (caar tree)) attribute-list
       key-attributes)))
       (rplaca tree (list (caar tree)))
       (setf mode "TERMINATE"
     temp-tree tree)
       (cond ((equal (car (fourth temp-tree)) nil)
      (setf tree (third temp-tree)
     rebalancep t))
     ((equal (car (third temp-tree)) nil)
      (setf tree (fourth temp-tree)
     rebalancep t))
     (t
      (multiple-value-setq (mod-tree rebalancep temp-tree)
 (modavl (third tree) rebalancep temp-tree))
      (rplaca (cddr temp-tree) mod-tree)
      (if rebalancep
   (multiple-value-setq (tree rebalancep)
          (balance1 tree rebalancep))))))
      (t
       (rplaca tree tuples))))))
((null key-modified)
 ;;
 ;;  The tuples can simply be modified within the node they reside
 ;;
 (setf number-modified (+ number-modified (length delete-flavor-tuple)))
 (modify-flavor-tuples relation-name attribute-list modify-attributes modify-values termination-clause
       dom-def delete-flavor-tuple temp-attribute-list)))
  ;;
  ;;  If the current node is not to be deleted and it does not invalidate the termination clause, process the right subtree for deletion.
  ;;
  (cond ((not (string-equal mode "TERMINATE"))
 (cond ((super-fast-eval-where (project-flavor (first tree) temp-attribute-list temp-attribute-list)
        temp-attribute-list termination-clause)
(multiple-value-setq (mod-tree mode rebalancep number-modified beginning-value insert-tuples)
  (flavor-avl-key-modify (cadddr tree) attribute-list key-attributes domains
        beginning-value termination-clause "LOCATE" relation-name where-clause
        rebalancep number-modified insert-tuples modify-values
        modify-attributes temp-attribute-list dom-def))
(rplaca (cdddr tree) mod-tree)
(cond (rebalancep
       (multiple-value-setq (tree rebalancep)
 (balance2 tree rebalancep))
       (setf mode "TERMINATE"))))
       ((first tree)
(setf mode "FINISHED"
      beginning-value nil)))))
  (values tree mode rebalancep number-modified beginning-value insert-tuples))

(defun process-struct-avl-modify (tree attribute-list where-clause number-modified rebalancep mode
   beginning-value relation-name termination-clause key-attributes domains
   insert-tuples modify-values modify-attributes temp-attribute-list dom-def
   &aux delete-tuples delete-struct-tuple (key-modified nil) mod-tree
   temp-tree tuples
   (string-temp-attributes (convert-attributes temp-attribute-list)))
  ;;
  ;;  Now that the tuples which satisfy the where-clause from the current node have been gathered, they need to be
  ;; deleted, if the key  attributes are modified or modified if th4e modification does not involve the key.
  ;;
  domains
  (do ((key% key-attributes (cdr key%)))
      ((or (null key%) key-modified) key-modified)
    (setf key-modified (member (car key%) modify-attributes :test 'string-equal)))
  (mapc (function (lambda (struct-tuple list-tuple)
    (cond ((super-fast-eval-where (list list-tuple) temp-attribute-list where-clause)
   (setf delete-tuples (cons list-tuple delete-tuples)
  delete-struct-tuple (cons struct-tuple delete-struct-tuple))))))
(car tree) (project-struct (car tree) string-temp-attributes string-temp-attributes relation-name))
  (cond (key-modified
 ;;
 ;;  Any tuples to delete from the current node ??
 ;;
 (cond ((> (length delete-tuples) 0)
(setf insert-tuples (append delete-tuples insert-tuples))
(mapc (function (lambda (node-tuple)
   (if (not (member node-tuple delete-struct-tuple))
       (setf tuples (append (list node-tuple) tuples)))))
      (first tree))
(setf number-modified (+ number-modified (length delete-tuples)))
;;
;;  No tuples are left in the node, delete the node
;;
(cond ((null tuples)
       (setf beginning-value (car (project-struct (list (caar tree)) attribute-list
       key-attributes relation-name)))
       (rplaca tree (list (caar tree)))
       (setf mode "TERMINATE"
     temp-tree tree)
       (cond ((equal (car (fourth temp-tree)) nil)
      (setf tree (third temp-tree)
     rebalancep t))
     ((equal (car (third temp-tree)) nil)
      (setf tree (fourth temp-tree)
     rebalancep t))
     (t
      (multiple-value-setq (mod-tree rebalancep temp-tree)
 (modavl (third tree) rebalancep temp-tree))
      (rplaca (cddr temp-tree) mod-tree)
      (if rebalancep
   (multiple-value-setq (tree rebalancep)
          (balance1 tree rebalancep))))))
      (t
       (rplaca tree tuples))))))
((null key-modified)
 ;;
 ;;  The tuples can simply be modified within the node they reside
 ;;
 (setf number-modified (+ number-modified
   (modify-struct-tuples relation-name attribute-list modify-attributes
        modify-values termination-clause dom-def
        delete-struct-tuple temp-attribute-list)))))
  ;;
  ;;  If the current node is not to be deleted and it does not invalidate the termination clause, process the right subtree for deletion.
  ;;
  (cond ((not (string-equal mode "TERMINATE"))
 (cond ((super-fast-eval-where (project-struct (first tree) string-temp-attributes
          string-temp-attributes relation-name)
        temp-attribute-list termination-clause)
(multiple-value-setq (mod-tree mode rebalancep number-modified beginning-value insert-tuples)
  (struct-avl-key-modify (cadddr tree) attribute-list key-attributes domains
        beginning-value termination-clause "LOCATE" relation-name where-clause
        rebalancep number-modified insert-tuples modify-values
        modify-attributes temp-attribute-list dom-def))
(rplaca (cdddr tree) mod-tree)
(cond (rebalancep
       (multiple-value-setq (tree rebalancep)
 (balance2 tree rebalancep))
       (setf mode "TERMINATE"))))
       ((first tree)
(setf mode "FINISHED"
      beginning-value nil)))))
  (values tree mode rebalancep number-modified beginning-value insert-tuples))
ÄoBÄ‡BÄ˛¨∞Define a new database.

   DB-NALMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540781. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "XLD" :NAME "MODIFY-AVL" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760360011. :AUTHOR "REL3" :LENGTH-IN-BYTES 6865. :LENGTH-IN-BLOCKS 14. :BYTE-SIZE 16.)                                  pp2Ä\Ä©ÅCOMPILE-DATA\ÄÏÄSW-MFG,ÅGODZILLAÜÄá§[ŒFÄFÄ\Äp¿,ÅCOMPILER,ÅVERSIONÄ\ÄFÄFÄp¿BÄ),ÇOPTIMIZE-SWITCHÄÜÄ©ÉQFASL-SOURCE-FILE-UNIQUE-IDÄ1Ä\Äp¿lÄFSÏÇMAKE-FASLOAD-PATHNAMEÄ\ÄÍÄQUOTEÄBÄ$\ÄBÄ8™ÄNILÄ\ÄBÄ8\Ä¨ÄRTMS\ÄBÄ8lÅMODIFY-AVL\ÄBÄ8¨ÄLISP\ÄBÄ8FÄ©ÄBASEFÄ
ÈÄFONTSÄ\Ä©Å*CODE-FONT*ÄÈÅ*COMMENT-FONT*ÈÅ*STRING-FONT*Ä)ÅPACKAGEÄ©ÄRTMS©ÄMODE©ÅCOMMON-LISPÄÄÇMODIFY-LIST-AVLÄÄÎÄ(pÜÄ‚(FÄò¿$Ä¿BÄ:p¿¨ÄTICLÏÄART-QÄ]ÄFÄÄ:BÄ:BÄ:jÄTÄFÄp¿¨ÄSYSÄlÇDEBUG-INFO-STRUCTÄBÄP\Ä√ÅRELATION-NAMEÄ√ÅATTRIBUTE-LIST√ÅKEY-ATTRIBUTESCÇMODIFY-ATTRIBUTESÄ√ÅMODIFY-VALUESÄÉÅWHERE-CLAUSEÅDOM-DEFÄÅINDICESÄBÄ:\ÄÅDOMAINSÄÇKEY-DOMAIN-LISTÄCÅKEY-VALUEÄ√ÅINSERT-TUPLESÄÉÄMODEÇMODIFIED-TUPLESÄÇNUMBER-MODIFIEDÄ™ÅPACKAGE-NAMECÅREBALANCEPÉÇTEMP-ATTRIBUTE-LISTÄ√ÇTERMINATION-CONDITIONÄÉÇTOTAL-INSERT-TUPLESÄ√ÇTOTAL-NUMBER-MODIFIEDÄÉÄTREE√ÄTUPLES√ÅDOMAIN-ELEMENT√ÇSTRING-ATTRIBUTE-LISTÄÉSTRING-MODIFY-ATTRIBUTESÉÄKEY%ÇBEGINNING-VALUE%ÉÇTERMINATION-CLAUSE%ÄBÄ:BÄ:ÇBEGINNING-VALUEÄ√ÄTUPLEÄ\Ä)ÇMACROS-EXPANDEDÄ\ÄÍÄUNLESSÍÄTHIRDÄÍÄFIRSTÄÍÄSECOND™ÄPROG™ÄSETF¿ÜÄÄÉÅ*PKG-STRING*—CÇ*SYSTEM-RELATIONS*ëÍÅSTRING-UPCASEÄ“™ÅSTRING-EQUAL¿p¿BÄ\¨ÅMEMBER-TESTÄ“ÍÄSTRING¿lÄ:*¿ÏÅ-KEY-DOMAINS*Ä¿™ÅCONCATENATEÄ“*ÇREAD-FROM-STRING“™ÄEVAL“*ÅREVERSEÄ“™ÄLIST“ÉÅPROJECT-LIST“CÇCONVERT-ATTRIBUTES“BÄp“ÇEXTRACT-KEY-AVLÄ“ÉÅENTRY-POINTÄ¿ÉÄGETP“\Ä\ÄBÄY¿BÄY¿ÉÄLEPÄ¿FÄê¿CÇEVAL-WHERE-PREREQÄ“jÅMAKUNBOUND“lÅTERMINATEÄ¿\Ä√ÄBOGUSÄBÄ¿ÏÄLOCATE¿ÜÄê¿ÉÇLIST-AVL-KEY-MODIFYÄ“p¿BÄ\,Å*APPENDÄ“,ÅFINISHED¿p¿BÄ\ÏÅSTRING-EQUAL*Ä“ÉÄPUTP“ÉÅMODIFY-TUPLE“ÇINSERT-LIST-AVLÄíFﬂLﬂÄQäPPò
‰	PP
PÄQäP™ää¸ÜQO¡‰OQ
B@]@¡O≈˙Á@QääÅQÇQöBäA¡ÄÚÄQùCä¸PG¡ÅQÇQAQÖQGQ™B¡ÄQPíM¡BÊÇQäPPöB¡ÅQäÉQäQ¡P¡BSBWB[T¡S¡R¡`‰TQB&‰J›¸PRQBTQBöJ¡ÖQÅQÄQPPAI¡Ö¡IQV€U¡CU‰USöCVSÄVÊUSàU≈V≈Û˝PD¡JF¡PC¡SSW¡"¸FﬂMQPQÇQAQWQJQPÄQÖQHQFQˇ€ÑQQQIQÜQ P!PAC¡W¡F¡H¡D¡M¡CQKQ"íK¡LQFaL¡H€JF'ÊDQ#P$êÊC’ÁR≈S≈T≈R†ÁÄQMQP%òK*‰IQV€U¡CU‰USöCVSÄVÊUSàU≈V≈Û˝N€KQX¡‰ÅQÉQXSÑQÜQÄQIQJ&∫E]E¡X≈ÛÁÄQÅQEQÇQÄQ'™K¡LQKQÇOÄ±BÄPÄÄCÇMODIFY-FLAVOR-AVLÄÄÎÄ(mÜÄ‚(FÄï¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ≤\ÄBÄ`BÄaBÄbBÄcBÄdBÄeBÄfBÄgBÄ:\ÄBÄiBÄjBÄkBÄlBÄmBÄnBÄoBÄpBÄqBÄrBÄsBÄtBÄuBÄvBÄwBÄxBÄ{BÄ|BÄ}BÄ:BÄ:BÄ~BÄ\ÄBÄÅ\ÄÄÉBÄÑBÄÖBÄÜBÄáBÄàÈÅDOCUMENTATIONÄÏøˆThis function will is the driver for the function which will modify the tuples of the specified list avl
   represented relation. A count of the total number of tuples modified is returned.

   RELATION-NAME     - The name of the relation whose tuples will be modified.
   ATTRIBUTE-LIST    - A list of all of the attributes in the relation in string form.
   KEY-ATTRIBUTES    - A list of the attributes which make form the key of the relation.
   MODIFY-ATTRIBUTES - A list of the attributes to be modified.
   MODIFY-VALUES     - A list of the expressions by which the attributes will be modified.
   WHERE-CLAUSE      - An s-expression which is used as a predicate to select the tuples to be modified.
   DOM-DEF           - A list of elements. Each element is a list containing the name of the attribute, the
                       domain of the element and the default value of the attribute.
   INDICES           - A boolean value which indicates of there are any indices defined on this relation.¿ÜÄÄBÄä—BÄãëBÄå“BÄ
¿BÄè“BÄê¿lÄ:*¿ÏÅ-KEY-DOMAINS*Ä¿BÄì“BÄî“BÄï“BÄñ“BÄó“BÄò“BÄp“BÄö“BÄõ¿BÄú“\Ä\ÄBÄY¿BÄY¿BÄü¿FÄê¿BÄ°“BÄ¢“lÅTERMINATEÄ¿\ÄBÄ•BÄ¿BÄô“ÏÄLOCATE¿ÜÄê¿√ÇFLAVOR-AVL-KEY-MODIFYÄ“BÄ™“,ÅFINISHED¿BÄ≠“BÄÆ“BÄØ“CÇINSERT-FLAVOR-AVLÄíFﬂLﬂÄQäPPò
‰	PP
PÄQäP™ää¸ÜQO¡‰OQ
B@]@¡O≈˙Á@QääÅQÇQöBA¡ÄÚÄQùCä¸PG¡ÅQÇQAQÖQGQ™B¡ÄQPíM¡BÊÇQäPPöB¡BSBWB[R¡Q¡P¡a‰RQB&‰J›¸PPQBRQBöJ¡ÖQÅQÄQPPAI¡Ö¡IQT€S¡CS‰SSöCTSÄTÊSSàS≈T≈Û˝PD¡JF¡PC¡QSU¡#¸FﬂMQÅQäÇQAQUQJQPÄQÖQHQFQˇ€ÑQÉQIQÜQ P!PAC¡U¡F¡H¡D¡M¡CQKQ"íK¡LQFaL¡H€JF'ÊDQ#P$êÊC‘ÁP≈Q≈R≈PüÁÄQMQP%òK*‰IQT€S¡CS‰SSöCTSÄTÊSSàS≈T≈Û˝N€KQV¡‰ÅQÉQVSÑQÜQÄQIQJ&∫E]E¡V≈ÛÁÄQÅQEQÇQÄQ'™K¡LQKQÇOÄÕBÄ≤ÄÄCÇMODIFY-STRUCT-AVLÄÄÎÄ(mÜÄ‚(FÄï¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄŒ\ÄBÄ`BÄaBÄbBÄcBÄdBÄeBÄfBÄgBÄ:\ÄBÄiBÄjBÄkBÄlBÄmBÄnBÄoBÄpBÄqBÄrBÄsBÄtBÄuBÄvBÄwBÄxBÄ{BÄ|BÄ}BÄ:BÄ:BÄ~BÄ\ÄBÄÅ\ÄÄÉBÄÑBÄÖBÄÜBÄáBÄàBÄæÏøÙThis function will is the driver for the function which will modify the tuples of the specified list avl
   represented relation. A count of the total number of tuples modified is returned.

   RELATION-NAME     - The name of the relation whose tuples will be modified.
   ATTRIBUTE-LIST    - A list of all of the attributes in the relation in string form.
   KEY-ATTRIBUTES    - A list of the attributes which make form the key of the relation.
   MODIFY-ATTRIBUTES - A list of the attributes to be modified.
   MODIFY-VALUES     - A list of the expressions by which the attributes will be modified.
   WHERE-CLAUSE      - An s-expression which is used as a predicate to select the tuples to be modified.
   DOM-DEF           - A list of elements. Each element is a list containing the name of the attribute, the
                       domain of the element and the default value of the attribute.
   INDICES           - A boolean value which indicates if there are indices defined on the relation.Ä¿ÜÄÄBÄä—BÄãëBÄå“BÄ
¿BÄè“BÄê¿lÄ:*¿ÏÅ-KEY-DOMAINS*Ä¿BÄì“BÄî“BÄï“BÄñ“BÄó“BÄò“BÄp“BÄö“BÄõ¿BÄú“\Ä\ÄBÄY¿BÄY¿BÄü¿FÄê¿BÄ°“BÄ¢“lÅTERMINATEÄ¿\ÄBÄ•BÄ¿BÄô“ÏÄLOCATE¿ÜÄê¿√ÇSTRUCT-AVL-KEY-MODIFYÄ“BÄ™“,ÅFINISHED¿BÄ≠“BÄÆ“BÄØ“CÇINSERT-STRUCT-AVLÄíFﬂLﬂÄQäPPò
‰	PP
PÄQäP™ää¸ÜQO¡‰OQ
B@]@¡O≈˙Á@QääÅQÇQöBA¡ÄÚÄQùCä¸PG¡ÅQÇQAQÖQGQ™B¡ÄQPíM¡BÊÇQäPPöB¡BSBWB[R¡Q¡P¡a‰RQB&‰J›¸PPQBRQBöJ¡ÖQÅQÄQPPAI¡Ö¡IQT€S¡CS‰SSöCTSÄTÊSSàS≈T≈Û˝PD¡JF¡PC¡QSU¡#¸FﬂMQÅQäÇQAQUQJQPÄQÖQHQFQˇ€ÑQÉQIQÜQ P!PAC¡U¡F¡H¡D¡M¡CQKQ"íK¡LQFaL¡H€JF'ÊDQ#P$êÊC‘ÁP≈Q≈R≈PüÁÄQMQP%òK*‰IQT€S¡CS‰SSöCTSÄTÊSSàS≈T≈Û˝N€KQV¡‰ÅQÉQVSÑQÜQÄQIQJ&∫E]E¡V≈ÛÁÄQÅQEQÇQÄQ'™K¡LQKQÇOÄËBÄŒÄÄÉÇMODIFY-FLAVOR-TUPLESÄÎÄ*hÜÄ‚FÄ>¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÈ\ÄÅRELATIONBÄaBÄcBÄd√ÄWHEREÄBÄfBÄwBÄrBÄ:\ÄÉÇATOM-ATTRIBUTE-LISTÄ√ÇATOM-MODIFY-ATTRIBUTES√ÅFLAVOR-PACKAGEÉÄDATAÉÇCONV-ATTRIBUTE-LISTÄBÄ:BÄ:BÄ:BÄ:BÄÅA-TUPLEÄBÄ:BÄ:ÉÄATTRÉÄVALÄÅTEMPVALÄ\ÄBÄÅ\ÄBÄáp¿¨ÄZLCÄ,ÅDO-NAMEDp¿BÄTÏÇINHIBIT-STYLE-WARNINGSBÄà¿ÜÄÄÉÇ*VALIDITY-CHECKING*Ä—BÄãëÍÄTYPEPÄ“BÄp“ÉÇUNCONVERT-ATTRIBUTES“√ÇPROJECT-FLAVOR-PREREQÄ“ÉÇFAST-PROJECT-FLAVORÄ“ÉÅQUOTE-TUPLEÄ“CÅFORM-ALIST“p¿BÄ\,ÅSUBLIS*Ä“BÄï“BÄå“BÄ
¿BÄè“CÅDOM-CHECKÄ“p¿BÄT,ÇSET-IN-INSTANCEÄíÜSäùCäB¡ÅQBQí@¡ÇQBQíA¡ÅQ	äD¡ÜQDQ
íC¡E—ÜQCQH¡G¡F¡4¸FQGSHSJ¡I¡AQÉQL¡K¡!¸KSLSN¡M¡O€JQä@QíNQíäO¡ÄQäPPòÊ‰OQMQÖQò‰IQMQOQòK≈L≈K‰L‹ÁIQCF√¡G≈H≈G‰H»ÁEOÄBÄÈÄÄÉÇMODIFY-STRUCT-TUPLESÄÎÄLÆÜÄ‚FÄb¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ\ÄBÄÚBÄaBÄcBÄdBÄÛBÄfBÄwBÄrBÄ:\ÄBÄıBÄˆCÅDELORMOD?Ä√ÇSTRUCT-ATTRIBUTE-LISTÄÉÅNUM-MODIFIEDÉÅTEMP-STRUCTÄÉSTRUCT-MODIFY-ATTRIBUTESÉÇSTRING-RELATION-NAMEBÄ:BÄ:BÄ:BÄ:ÉÅSTRUCT-TUPLEBÄ:BÄ:BÄ:BÄ:BÄ˚BÄ¸ÉÅSTRUCT-ATTRÄBÄ˝\ÄBÄÅ\Äp¿BÄ\lÅXR-BQ-LISTBÄáBÄBÄBÄà¿ÜÄÄBÄ—BÄãëBÄê“BÄê¿BÄì“BÄ“BÄó“√ÇSUPER-FAST-EVAL-WHEREÄ“BÄ“BÄ“BÄ“BÄï“BÄå“BÄ
¿BÄè“BÄ“BÄà¿BÄ8ÄDﬂÄQäG¡H—ÅQJ¡I¡	¸IQPGQJSöCI√¡J≈JıÁHQ	äC¡J€J—ÇQH¡K¡	¸KQPGQHSöCK√¡H≈HıÁJQ	äF¡ÅQ	ä@¡ÇQ	äA¡ÜQJ¡b‰JSL¡B€I€I—CQN¡M¡¸MQLQNSˇãCM√¡N≈NˆÁIQE¡ÑQ±ÊEQ
äáQÑQòB‰N€N—AQÉQFQI¡H¡P¡O¡/¸OQPSHSISS¡R¡Q¡T€EQä@QíRQíäT¡ÄQäPPòÊ‰TQQQÖQò‰B›PSQLQ
íPTQ
í
öä¸ˇ€CO√¡P≈H≈I≈P‰H‰IÀÁB‰D…J≈ûÁDOÄ+BÄÄÄ√ÄMODAVLÄÎÄ-ÜÄ@ƒFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ,\ÄBÄvBÄqCÅTEMP-TREEÄBÄ:\ÄÅMODTREEÄ\ÄBÄÅ\ÄBÄÑBÄàÍÄFOURTHÄFÄ–¿BÄ,“FÄê¿ÅBALANCE2íÄQ‰ÄQBÅQÇQPPAÇ¡Å¡@¡ÄQB@Q¿Å‰ÄQÅQPPAÅ¡Ä¡¸ÇQÄS¿Ä[Ä¡Å›ÄQÅQÇQÉOÄ>BÄ,ÄÄBÄ®ÄÎÄÜÄ‡FÄô¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ®\ÄBÄvBÄaBÄbBÄiBÄ~CÇTERMINATION-CLAUSEBÄmBÄ`BÄeBÄqBÄoBÄlBÄdBÄcBÄrBÄfBÄ:\ÄÉÇCOMPARISON-OPERATORÄ√ÇCURRENT-NODE-KEY-VALUEÅMOD-TREE\ÄBÄÅ\ÄBÄÑBÄà¿ÜÄÄ√ÄLOCATE¿BÄ≠“√ÅLOCATE-STAGE-2¿ÏÅDELETE-SEARCHÄ¿BÄó“BÄò“BÄY¿CÅLESS-THANÄ¿ÉÅNODE-COMPARE“ÜÄê¿BÄ®“ÉÅGREATER-THAN¿FÄê¿BÄ=“lÅTERMINATEÄ¿,ÅRESTARTÄ¿ÍÄEQUALÄ¿ÏÅLOCATE-STAGE-2¿ÏÄLOCATE¿ÅBALANCE1“,ÅFINISHED¿ÉPROCESS-LIST-AVL-MODIFYÄíÄ÷‰ÜQPêÊÜQPêÊÜQPê»‰ÄQBäÅQÇQ	öBA¡ÑS
&‰P¸ÑQAQÉQö@√&‰ÜQPê‰Ä[ÅQÇQÉQÑQÖQÜQáQàQ QäQãQåQ
QéQèQPPAã¡Ñ¡ä¡ ¡Ü¡B¡]¸@Q&1‰ÜQPê-‰ÄQBÅQÇQÉQÑQÖQÜQáQàQ QäQãQåQ
QéQèQPPAã¡Ñ¡ä¡ ¡Ü¡B¡ÄQBBQ¿ s‰ÄQ QPPA ¡Ä¡ ‰Pg¸Pe¸@QPêb‰PÜ¡Ä[ÅQÇQÉQÑQÖQÜQáQàQ QäQãQåQ
QéQèQPPAã¡Ñ¡ä¡ ¡Ü¡B¡ÜQPêÊÜQPê‰PÜ¡ÄYBQ¿ ‰ÄQ QPPA ¡Ä¡ ‰P¸PÜ¡ÜQPê'ÊÜQPê#ÊÜQPêÊÄQÅQàQäQ QÜQÑQáQÖQÇQÉQãQåQ
QéQèQPPAã¡Ñ¡ä¡ ¡Ü¡B√Ä¡¸ÄÊPÜ¡ÜQPê‰ÄQÅQÇQÉQÑQÖQPáQàQ QäQãQåQ
QéQèQPPAã¡Ñ¡ä¡ ¡Ü¡Ä¡ÄQÜQ QäQÑQãQÜOÄ_BÄ®ÄÄBÄ ÄÎÄÜÄ‡FÄô¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ \ÄBÄvBÄaBÄbBÄiBÄ~BÄGBÄmBÄ`BÄeBÄqBÄoBÄlBÄdBÄcBÄrBÄfBÄ:\ÄBÄIBÄJBÄK\ÄBÄÅ\ÄBÄÑBÄà¿ÜÄÄBÄO¿BÄ≠“BÄP¿ÏÅDELETE-SEARCHÄ¿BÄó“√ÅPROJECT-FLAVOR“BÄY¿BÄR¿BÄS“ÜÄê¿BÄ “BÄU¿FÄê¿BÄ=“lÅTERMINATEÄ¿,ÅRESTARTÄ¿BÄY¿ÏÅLOCATE-STAGE-2¿ÏÄLOCATE¿BÄ\“,ÅFINISHED¿CÉPROCESS-FLAVOR-AVL-MODIFYÄíÄ÷‰ÜQPêÊÜQPêÊÜQPê»‰ÄQBäÅQÇQ	öBA¡ÑS
&‰P¸ÑQAQÉQö@√&‰ÜQPê‰Ä[ÅQÇQÉQÑQÖQÜQáQàQ QäQãQåQ
QéQèQPPAã¡Ñ¡ä¡ ¡Ü¡B¡]¸@Q&1‰ÜQPê-‰ÄQBÅQÇQÉQÑQÖQÜQáQàQ QäQãQåQ
QéQèQPPAã¡Ñ¡ä¡ ¡Ü¡B¡ÄQBBQ¿ s‰ÄQ QPPA ¡Ä¡ ‰Pg¸Pe¸@QPêb‰PÜ¡Ä[ÅQÇQÉQÑQÖQÜQáQàQ QäQãQåQ
QéQèQPPAã¡Ñ¡ä¡ ¡Ü¡B¡ÜQPêÊÜQPê‰PÜ¡ÄYBQ¿ ‰ÄQ QPPA ¡Ä¡ ‰P¸PÜ¡ÜQPê'ÊÜQPê#ÊÜQPêÊÄQÅQàQäQ QÜQÑQáQÖQÇQÉQãQåQ
QéQèQPPAã¡Ñ¡ä¡ ¡Ü¡B√Ä¡¸ÄÊPÜ¡ÜQPê‰ÄQÅQÇQÉQÑQÖQPáQàQ QäQãQåQ
QéQèQPPAã¡Ñ¡ä¡ ¡Ü¡Ä¡ÄQÜQ QäQÑQãQÜOÄvBÄ ÄÄBÄÂÄÎÄÄÜÄ‡FÄö¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÂ\ÄBÄvBÄaBÄbBÄiBÄ~BÄGBÄmBÄ`BÄeBÄqBÄoBÄlBÄdBÄcBÄrBÄfBÄ:\ÄBÄIBÄJBÄK\ÄBÄÅ\ÄBÄÑBÄà¿ÜÄÄBÄO¿BÄ≠“BÄP¿ÏÅDELETE-SEARCHÄ¿BÄó“√ÅPROJECT-STRUCT“BÄY¿BÄR¿BÄS“ÜÄê¿BÄÂ“BÄU¿FÄê¿BÄ=“lÅTERMINATEÄ¿,ÅRESTARTÄ¿BÄY¿ÏÅLOCATE-STAGE-2¿ÏÄLOCATE¿BÄ\“,ÅFINISHED¿CÉPROCESS-STRUCT-AVL-MODIFYÄíÄ◊‰ÜQPêÊÜQPêÊÜQPê…‰ÄQBäÅQÇQáQ	¢BA¡ÑS
&‰P¸ÑQAQÉQö@√&‰ÜQPê‰Ä[ÅQÇQÉQÑQÖQÜQáQàQ QäQãQåQ
QéQèQPPAã¡Ñ¡ä¡ ¡Ü¡B¡]¸@Q&1‰ÜQPê-‰ÄQBÅQÇQÉQÑQÖQÜQáQàQ QäQãQåQ
QéQèQPPAã¡Ñ¡ä¡ ¡Ü¡B¡ÄQBBQ¿ s‰ÄQ QPPA ¡Ä¡ ‰Pg¸Pe¸@QPêb‰PÜ¡Ä[ÅQÇQÉQÑQÖQÜQáQàQ QäQãQåQ
QéQèQPPAã¡Ñ¡ä¡ ¡Ü¡B¡ÜQPêÊÜQPê‰PÜ¡ÄYBQ¿ ‰ÄQ QPPA ¡Ä¡ ‰P¸PÜ¡ÜQPê'ÊÜQPê#ÊÜQPêÊÄQÅQàQäQ QÜQÑQáQÖQÇQÉQãQåQ
QéQèQPPAã¡Ñ¡ä¡ ¡Ü¡B√Ä¡¸ÄÊPÜ¡ÜQPê‰ÄQÅQÇQÉQÑQÖQPáQàQ QäQãQåQ
QéQèQPPAã¡Ñ¡ä¡ ¡Ü¡Ä¡ÄQÜQ QäQÑQãQÜOÄ
BÄÂÄÄBÄ^ÄÎÄ`◊ÜÄ‡(FÄw¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ^\ÄBÄvBÄaBÄeBÄoBÄqBÄmBÄ~BÄ`BÄGBÄbBÄiBÄlBÄdBÄcBÄrBÄfBÄ:\Ä
√ÅDELETE-TUPLESÄÉÅKEY-MODIFIEDBÄKÅNEW-NODEBÄ5BÄwBÄ{BÄ:CÅNODE-TUPLE√ÄTUPLE%\ÄBÄÅ\ÄÄÑBÄ:BÄBÄÖBÄáBÄà¿ÜÄ
ÄBÄ
¿BÄè“BÄ*“BÄ™“p¿BÄ\lÅMEMBER-EQL“BÄó“BÄò“lÅTERMINATEÄ¿FÄ–¿BÄ,“FÄê¿BÄ\“BÄØ“BÄ≠“ÏÄLOCATE¿ÜÄê¿BÄ®“BÄ=“,ÅFINISHEDÄ QF¡¸FS
QPöA¡F≈‰A˜ÂAV‰ÄSéQÇQö@√Jôk‰@QãQíã¡ÄSG¡
‰GSH√ãQêÊHQE]E¡G≈ˆÁ@QäCÉaÉ¡E4ÊÄQB	äÅQ Q
öBÜ¡ÄQÄQB	ä¿PÖ¡ÄQD¡DUÊD[Ä¡Ñ›<¸DQÊDQB˜˝Ä[ÑQDQPPAD¡Ñ¡B¡DYBQ¿Ñ(‰ÄQÑQPPAÑ¡Ä¡ ¸ÄQEQ¸C€ÄSI¡‰IS	äéQÇQò‰É…ÅQ
QISåQèQáQéQJ∫¸ISC]C¡I≈ÍÁÄQCQ¿ÖQPê4ÊÄSéQàQò*‰ÄQBÅQ QäQÜQàQPáQÇQÑQÉQãQåQ
QéQèQPPAã¡Ü¡É¡Ñ¡Ö¡B¡ÄQBBQ¿Ñ‰ÄQÑQPPAÑ¡Ä¡PÖ¡¸Ä‰PÖ¡Ü€ÄQÖQÑQÉQÜQãQÜOÄßBÄ^ÄÄBÄuÄÎÄhÁÜÄ‡0FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄu\ÄBÄvBÄaBÄeBÄoBÄqBÄmBÄ~BÄ`BÄGBÄbBÄiBÄlBÄdBÄcBÄrBÄfBÄ:\ÄBÄóÉÇDELETE-FLAVOR-TUPLEÄBÄòBÄKBÄ5BÄwBÄ{BÄ:BÄ:ÉÅFLAVOR-TUPLECÅLIST-TUPLEBÄö\ÄBÄÅ\ÄÄÑBÄ:BÄÖBÄBÄáBÄà¿ÜÄÄBÄ
¿BÄè“BÄm“BÄó“BÄ*“BÄ™“BÄ†“lÅTERMINATEÄ¿FÄ–¿BÄ,“FÄê¿BÄ\“BÄÈ“BÄ≠“ÏÄLOCATE¿ÜÄê¿BÄ “BÄ=“,ÅFINISHEDÄ QF¡¸FS
QPöB¡F≈‰B˜ÂÄSÄSéQéQöH¡G¡¸GSHSJ¡I¡JQäéQÇQò‰JQ@]@¡IQA]A¡G≈H≈G‰HÍÁBS‰@QJô]‰@QãQ	íã¡ÄSG¡
‰GSK√AQ
êÊKQE]E¡G≈ˆÁ@QäCÉaÉ¡E4ÊÄQBäÅQ QöBÜ¡ÄQÄQBä¿PÖ¡ÄQD¡DUÊD[Ä¡Ñ›.¸DQÊDQB˜˝Ä[ÑQDQPPAD¡Ñ¡C¡DYCQ¿Ñ‰ÄQÑQPPAÑ¡Ä¡¸ÄQEQ¿¸AQäCÉaÉ¡áQÅQ
QåQàQèQAQéQJ∏ÖQPê7ÊÄSéQéQöéQàQò*‰ÄQBÅQ QäQÜQàQPáQÇQÑQÉQãQåQ
QéQèQPPAã¡Ü¡É¡Ñ¡Ö¡C¡ÄQBCQ¿Ñ‰ÄQÑQPPAÑ¡Ä¡PÖ¡¸Ä‰PÖ¡Ü€ÄQÖQÑQÉQÜQãQÜOÄΩBÄuÄÄBÄåÄÎÄjÏÜÄ‡4FÄÇ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄå\ÄBÄvBÄaBÄeBÄoBÄqBÄmBÄ~BÄ`BÄGBÄbBÄiBÄlBÄdBÄcBÄrBÄfBÄ:\ÄBÄóÉÇDELETE-STRUCT-TUPLEÄBÄòBÄKBÄ5BÄw√ÇSTRING-TEMP-ATTRIBUTESBÄ{BÄ:BÄ:BÄ#BÄ≥BÄö\ÄBÄÅ\ÄÄÑBÄ:BÄÖBÄBÄáBÄà¿ÜÄÄBÄô“BÄ
¿BÄè“BÄÑ“BÄó“BÄ*“BÄ™“BÄ†“lÅTERMINATEÄ¿FÄ–¿BÄ,“FÄê¿BÄ\“BÄ“BÄ≠“ÏÄLOCATE¿ÜÄê¿BÄÂ“BÄ=“,ÅFINISHEDÄéQäF¡ QG¡¸GS
QPöB¡G≈‰B˜ÂÄSÄSFQFQáQ¢I¡H¡¸HSISK¡J¡KQäéQÇQ	ò‰KQ@]@¡JQA]A¡H≈I≈H‰IÍÁBT‰@QJô]‰@QãQ
íã¡ÄSH¡
‰HSL√AQêÊLQE]E¡H≈ˆÁ@QäCÉaÉ¡E5ÊÄQBäÅQ QáQ¢BÜ¡ÄQÄQBä¿PÖ¡ÄQD¡DUÊD[Ä¡Ñ›-¸DQÊDQB˜˝Ä[ÑQDQPPAD¡Ñ¡C¡DYCQ¿Ñ‰ÄQÑQPPAÑ¡Ä¡¸ÄQEQ¿¸ÉQáQÅQ
QåQàQèQAQéQJ∫ˇaÉ¡ÖQPê8ÊÄSFQFQáQ¢éQàQ	ò*‰ÄQBÅQ QäQÜQàQPáQÇQÑQÉQãQåQ
QéQèQPPAã¡Ü¡É¡Ñ¡Ö¡C¡ÄQBCQ¿Ñ‰ÄQÑQPPAÑ¡Ä¡PÖ¡¸Ä‰PÖ¡Ü€ÄQÖQÑQÉQÜQãQÜOÄ“BÄåÄ1Ä\Äp¿BÄ\,ÑFASL-RECORD-FILE-MACROS-EXPANDED\ÄBÄ8\Ä\ÄÍÄDEFUNÄÜÄ'\ÄBÄ:ÜÄxıø\ÄBÄ(ÜÄ.Ÿã\ÄBÄÜÄ(Ã¢\ÄBÄÜÄ*˝j\ÄBÄàÜÄ[ÊÑ\ÄBÄáÜÄ=Ã#\ÄBÄÜÜÄ{öÕ\ÄBÄÖÜÄz(á\ÄBÄÑÜÄ:}n\ÄBÄÉÜÄ6ÄÄtribute-list dom-def))
(setf total-insert-tuples (append insert-tuples total-insert-tuples))
 (setf total-number-modified (+ total-number-modified number-modified)
      rebalancep nil))))
    (putp relation-name tree 'entry-point)
    (cond (total-insert-tuples
   ;;
   ;;  Must modify the tuples and then insert them
   ;;
   (progv temp-attribute-list nil
     (setf tuples nil)
     (do ((tuple total-insert-tuples (cdr tuple)))
 ((null tuple) t)
       (setf modified-tuples (cons (modify-tuple attribute-list modify-attributes (car tuple)
     modify-values dom-def relation-name
LMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540784. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "LISP" :NAME "MODIFY-REL" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :CHARACTERS T :NOT-BACKED-UP T :CREATION-DATE 2749846288. :AUTHOR "REL3" :LENGTH-IN-BYTES 15494. :LENGTH-IN-BLOCKS 16. :BYTE-SIZE 8.)

;;; -*- Mode:Common-Lisp; Package:RTMS; Fonts:(*CODE-FONT* *COMMENT-FONT* *STRING-FONT*); Base:10 -*-
;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved
;;; MODIFY-REL
;;;
;;; This file contains the following Explorer extensions to CommonLisp d as Indicated in the June 1985 Explorer Lisp
;;; Reference
;;;     firstn
;;;     errset
;;;
;;; The following function contains flavor references and thus are incompatable with CommonLisp. Their removal will not
;;; effect the functionality of RTMS.
;;;

(defun create-attr-descriptor (attribute-list system-attribute-list
       &aux attr-des-pair attr-def attr-doc dom found-attr attr-tuple)
  (setf attr-des-pair nil)
  (mapcar (function (lambda (attr-name)
      (setf found-attr nil)
      (do ((tuples system-attribute-list (cdr tuples)))
  ((or found-attr (null tuples)))
(setf attr-tuple (car tuples))
(if (car (errset (string-equal (second attr-tuple) attr-name) nil))
    (progn
      (setf found-attr t)
      (setf attr-def (fourth attr-tuple)
     attr-doc (fifth attr-tuple)
     dom (third attr-tuple))
      (setf attr-des-pair (append attr-des-pair (list (second attr-tuple)
            (list 'dom dom 'def attr-def
           'doc attr-doc)))))))
      (if (null found-attr)
  (setf attr-des-pair (append attr-des-pair (if (listp attr-name)
      attr-name
      (list attr-name)))))))
  attribute-list)
  attr-des-pair)

(defun get-system-attribute-list (relation-name)
  (funcall (find-symbol (concatenate 'string "RETRIEVE-" *system-relation-base-implementation*
      "-" *system-relation-storage-structure*) *pkg-string*)
   'system-attribute *system-attribute-attributes* *system-attribute-attributes*
   *system-attribute-key* (list 'string-equal 'relation-name (string-upcase relation-name))
   nil 'system-attribute))

(defun redefine-rel (relation-name attr-des-pair imp ss key tuple-format-list doc dir tuple-list
     &aux temp-rel (status? *provide-status-messages*))
  (block redefine-rel
  (setf temp-rel (read-from-string (string (gensym))))
  (setf *provide-status-messages* nil)
  (if (not (define-relation temp-rel attr-des-pair
     'imp imp 'sto ss 'key key 'format tuple-format-list 'doc doc 'dir dir))
      (progn
(setf *provide-status-messages* status?)
(return-from redefine-rel nil)))
  (delete-or-modify 'system-index t (list 'string-equal 'relation-name (string relation-name))
     '("RELATION-NAME") (list (eval `(string-upcase (quote ,temp-rel)))))
  ;;
  ;; To take care of cases like in modify-rel when this routine is called without the relation "relation-name" having been defined. But some
  ;; times it might be the case that it is defined.
  ;;
  (if (relationp relation-name)
      (destroy-relation relation-name))
  ;; rename temp-rel to relation-name
  (delete-index-tuples temp-rel imp)
  (if (not (rename-relation temp-rel (read-from-string (string relation-name))))
      (progn
(setf *provide-status-messages* status?)
(return-from redefine-rel nil)))
  (cond (tuple-list
 (insert (read-from-string (string relation-name)) 'tuples tuple-list)))
  (setf *provide-status-messages* status?)
  (return-from redefine-rel relation-name)))

(defun modify-relation (relation &rest keyword-list
&key &optional relation-name add-attributes delete-attributes rename-attributes
implementation-type storage-structure format key documentation directory
&allow-other-keys
&aux rel attr imp sto format1 key1 doc dir mod-attr mod-vals rename-attrs new-attrs
current-attributes tuples delete-attrs old-vals)
 "Modify various features of a relation.

  RELATION             - Name of the relation to be modified.
  RELATION-NAME        - New name for this relation.
  ADD-ATTRIBUTES       - List of new attributes and their description.
  DELETE-ATTRIBUTES    - List of attributes to be destroyed.
  RENAME-ATTRIBUTES    - List of list of OLD-NEW attribute names.
  IMPLEMENTATION-TYPE  - Name of the new implementation type.
  STORAGE-STRUCTURE    - Name of the new storage-structure.
  FORMAT               - List of new print-width values to be used for the attributes.
  KEY                  - List of attributes to form the new key for this relation.
  DOCUMENTATION        - New description of this relation.
  DIRECTORY            - New directory in which this relation is to be saved."
 relation-name add-attributes delete-attributes rename-attributes implementation-type storage-structure format
 key documentation directory
 (block modify-relation
       (cond (*parameter-checking*
      (if (not (active-database))
  (return-from modify-relation nil))))
       (if (null (setf relation (validate-sym relation)))
   (return-from modify-relation nil))
      (setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
     ((or (null keyword-list)(not (listp (car keyword-list)))) keyword-list)))
       (setf keyword-list (get-keyword-value-prereq '(rel add-attr delete-attr rename-attr imp sto format key
      doc dir)
    keyword-list))
       (setf current-attributes (caadr (get-relation relation '("ATTRIBUTES") t)))
       (cond ((null current-attributes)
      (if *provide-error-messages*
  (format *standard-output* "~%ERROR - Relation ~s does not exist in the database ~s"
  relation *active-db*))
      (return-from modify-relation nil)))
       (cond-every ((setf format1 (car (get-keyword-value '(format) keyword-list)))
    (setf mod-attr (append mod-attr '("TUPLE-FORMAT")))
    (setf mod-vals (append mod-vals (list `(quote ,format1)))))
   ((setf dir (car (get-keyword-value '(dir) keyword-list)))
    (setf mod-attr (append mod-attr '("SAVE-DIRECTORY")))
    (setf mod-vals (append mod-vals (list `(quote ,dir)))))
   ((setf doc (car (get-keyword-value '(doc) keyword-list)))
    (setf mod-attr (append mod-attr '("DOCUMENTATION")))
    (setf mod-vals (append mod-vals (list `(quote ,doc)))))
   ((setf key1 (car (get-keyword-value '(key) keyword-list)))
    (setf mod-attr (append mod-attr '("KEY")))
    (setf mod-vals (append mod-vals (list `(quote ,key1))))))
       (setf imp (car (get-keyword-value '(imp) keyword-list))
     sto (car (get-keyword-value '(sto) keyword-list)))
       (if (or imp sto)
   (progn
     (setf tuples (retrieve relation 'tuples t))
     (setf current-attributes
   (cadr (get-relation relation '("ATTRIBUTES" "SAVE-DIRECTORY" "DOC" "TUPLE-FORMAT"
     "IMPLEMENTATION-TYPE" "STORAGE-STRUCTURE" "KEY") t)))
     (setf attr (copy-list (car current-attributes)))
     (if (car (get-keyword-value '(delete-attr) keyword-list))
 (progn
   (mapc #'(lambda (att)
     (if (member att attr :test 'string-equal)
  (setf attr (delete att attr))
  (if *provide-error-messages*
      (format *standard-output*
       "~%ERROR - ~S is not an attribute in the relation ~S. It can not be deleted."
       att relation))))
 (convert-attributes (car (get-keyword-value '(delete-attr) keyword-list))))
   (setf tuples (project-list tuples (car current-attributes) attr))))
     (setf attr (create-attr-descriptor attr (get-system-attribute-list relation)))
     (if (car (get-keyword-value '(rename-attr) keyword-list))
 (mapc #'(lambda (att)
   (if (and (listp att) (member (car att) attr :test 'string-equal))
       (setf attr (subst (cadr att) (car att) attr))
       (if *provide-error-messages*
    (format *standard-output*
     "~%ERROR - ~S is not an attribute in the relation ~S. It can not be renamed."
     (if (listp att)
         (car att)
         att)
     relation))))
       (convert-attributes (car (get-keyword-value '(rename-attr) keyword-list)))))
     (if (car (get-keyword-value '(add-attr) keyword-list))
 (setf attr (append attr (car (get-keyword-value '(add-attr) keyword-list)))))
     (setf old-vals (list *validity-checking* *provide-status-messages*))
     (setf *validity-checking* nil *provide-status-messages* nil)
     (if (not (redefine-rel (setf rel (or (car (get-keyword-value '(rel) keyword-list)) relation))
     attr (or imp (fifth current-attributes)) (or sto (sixth current-attributes))
     (or key1 (seventh current-attributes))
     (or format1 (fourth current-attributes)) (or doc (third current-attributes))
     (or dir (second current-attributes)) tuples))
 (return-from modify-relation nil))
     (setf *validity-checking* (car old-vals)
   *provide-status-messages* (cadr old-vals))
     (return-from modify-relation rel)))
       (if mod-attr
   (funcall (find-symbol (concatenate 'string "MODIFY-" *system-relation-base-implementation*
   "-" *system-relation-storage-structure*) *pkg-string*)
    'system-relation *system-relation-attributes* *system-relation-key* mod-attr mod-vals
    `(string-equal relation-name ,(string relation))
    (mapcar #'(lambda (dom-def)
 (list (first dom-def) (read-from-string (concatenate 'string *pkg-name*
              (second dom-def)))))
    (retrieve 'system-attribute 'project '(attribute-name domain-function) 'tuples t
       'where `(string-equal relation-name ,(string-upcase relation))))
    nil))
       (setf current-attributes (cadr (get-relation relation '("ATTRIBUTES" "IMPLEMENTATION-TYPE"
           "STORAGE-STRUCTURE" "KEY") t)))
       (setf rename-attrs (car (get-keyword-value '(rename-attr) keyword-list)))
       (setf new-attrs (car (get-keyword-value '(add-attr) keyword-list)))
       (setf delete-attrs (car (get-keyword-value '(delete-attr) keyword-list)))
       (if rename-attrs
   (rename-attribute relation rename-attrs))
       (if new-attrs
   (define-attribute relation new-attrs))
       (if delete-attrs
   (destroy-attribute relation 'attribute delete-attrs))
       (if key1
   (progn
     (setf tuples (retrieve relation 'tuples t))
     (delete-tuples relation 'where t)
     (insert-tuples relation 'tuples tuples)))
       (if (setf rel (car (get-keyword-value '(rel) keyword-list)))
   (rename-relation relation rel))
       (return-from modify-relation t)))

(defun modify-database (database &rest keyword-list
&key &optional database-name directory documentation
&allow-other-keys
&aux temp)
 "Modify various features of the active database.

  DATABASE      - Name of the database to be modified.
  DATABASE-NAME - New name for this database.
  DIRECTORY     - New directory in which this database is to be saved.
  DOCUMENTATION - New description for this database."
 database-name directory documentation
 (block modify-database
       (cond (*parameter-checking*
      (if (not (active-database))
    (return-from modify-database nil))))
       (if (null (setf database (validate-sym database t)))
   (return-from modify-database nil))
       (cond ((not (string-equal database *active-db*))
      (if *provide-error-messages*
  (format *standard-output* "~%ERROR - The database to modify has to be the active database ~S"
  *active-db*))
      (return-from modify-database nil)))
       (setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
      ((or (null keyword-list)(not (listp (car keyword-list)))) keyword-list)))
       (setf keyword-list (get-keyword-value-prereq '(database dir doc) keyword-list))
       (cond-every ((setf temp (car (get-keyword-value '(dir) keyword-list)))
    (set (read-from-string (concatenate 'string *pkg-name* "*SAVE-DIRECTORY*")) temp))
   ((setf temp (car (get-keyword-value '(doc) keyword-list)))
    (set (read-from-string (concatenate 'string *pkg-name* "*DATABASE-DOCUMENTATION*")) temp))
   ((setf temp (car (get-keyword-value '(database) keyword-list)))
    (rename-database database temp)))
       (return-from modify-database (or temp database))))

(defun modify-attribute (relation attribute &rest keyword-list
 &key &optional attribute-name default-value documentation format
 &allow-other-keys &aux temp attributes tuple-format num)
 "Modify various features of an attribute in a given relation.

  RELATION       - Name of the relation in which the attribute to be modified exists.
  ATTRIBUTE      - Name of the attribute to be modified.
  ATTRIBUTE-NAME - New name for this attribute.
  DEFAULT-VALUE  - New default value for this attribute.
  DOCUMENTATION  - New description.
  FORMAT         - New print width to be used for this attribute."
 attribute-name default-value documentation format
 (block modify-attribute
       (cond (*parameter-checking*
      (if (not (active-database))
  (return-from modify-attribute nil))))
       (if (not (setf relation (validate-sym relation)))
   (return-from modify-attribute nil))
       (setf tuple-format (cadr (get-relation relation '("ATTRIBUTES" "TUPLE-FORMAT") nil)))
       (cond ((null (car tuple-format))
      (if *provide-error-messages*
  (format *standard-output* "~%ERROR - Relation ~s does not exist in the database ~s"
  relation *active-db*))
      (return-from modify-attribute nil)))
       (setf attribute (car (convert-attributes attribute)))
       (if (not (member attribute (car tuple-format) :test 'string-equal))
   (progn
     (if *provide-error-messages*
 (format *standard-output* "~%ERROR - ~S is not an attribute in the relation ~S"
 attribute relation))
     (return-from modify-attribute nil)))
       (setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
      ((or (null keyword-list)(not (listp (car keyword-list)))) keyword-list)))
       (setf keyword-list (get-keyword-value-prereq '(attr def doc format) keyword-list))
       (cond-every ((setf temp (car (get-keyword-value '(def) keyword-list)))
    (if (dom-check temp attribute
    (mapcar #'(lambda (dom-def)
         (list (first dom-def) (read-from-string
            (concatenate 'string *pkg-name*
             (second dom-def)))))
     (retrieve 'system-attribute
        'project '(attribute-name domain-function) 'tuples t
        'where
        `(string-equal relation-name ,(string-upcase relation)))))
(delete-or-modify 'system-attribute t
    `(and (string-equal relation-name ,(string-upcase relation))
   (string-equal attribute-name ,(string-upcase attribute)))
    '("DEFAULT-VALUE") (list temp))))
   ((setf temp (car (get-keyword-value '(format) keyword-list)))
    (if (not (numberp temp))
(if *provide-warning-messages*
    (format *standard-output* "~%WARNING - ~S is not a valid format value." temp))
(progn
  (setf attributes (car tuple-format)
 tuple-format (cadr tuple-format))
  (setf num (position attribute attributes :test 'equal))
  (setf tuple-format (append (firstn num tuple-format) (list temp)
        (nthcdr (+ num 1) tuple-format)))
  (delete-or-modify 'system-relation t
      `(string-equal relation-name ,(string-upcase relation))
      '("TUPLE-FORMAT") (list `(quote ,tuple-format))))))
   ((setf temp (car (get-keyword-value '(doc) keyword-list)))
    (delete-or-modify 'system-attribute t
       `(and (string-equal relation-name ,(string-upcase relation))
      (string-equal attribute-name ,(string-upcase attribute)))
       '("DOC") (list temp)))
   ((setf temp (car (get-keyword-value '(attr) keyword-list)))
    (rename-attribute relation attribute temp)))
       (return-from modify-attribute (or temp attribute))))
stem-relations* :test 'string-equal)
        (not *validity-checking*) (dom-check tempval attr dom-def))
    (set-in-instance tuple attr tempval)))))
atom-modify-attributes modify-values)
      tuple))
  tuples data))


(defun modify-struct-tuples (relation attribute-list modify-attributes modify-values where dom-def tuples
     temp-attribute-list
     &aux atom-attribute-list atom-modify-attributes delormod? struct-attribute-list
     (num-modified 0) temp-struct struct-modify-attributes
     (string-relation-name (string relation)))
  temp-attribute-list where

  (setf struct-attribute-list (unconvert-attributes (mapcar #'(lambda (attr)
     (concatenate 'string string-relation-name
           attr))
        attribute-list))
struct-modify-attributes (unconvert-attributes (mapcar #'(lambda (attr)
        (concatenate 'string string-relatLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540787. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "XLD" :NAME "MODIFY-REL" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760360646. :AUTHOR "REL3" :LENGTH-IN-BYTES 4758. :LENGTH-IN-BLOCKS 10. :BYTE-SIZE 16.)                                  pp2Ä\Ä©ÅCOMPILE-DATA\ÄÏÄSW-MFG,ÅGODZILLAÜÄá§÷–FÄFÄ\Äp¿,ÅCOMPILER,ÅVERSIONÄ\ÄFÄFÄp¿BÄ),ÇOPTIMIZE-SWITCHÄÜÄ©ÉQFASL-SOURCE-FILE-UNIQUE-IDÄ1Ä\Äp¿lÄFSÏÇMAKE-FASLOAD-PATHNAMEÄ\ÄÍÄQUOTEÄBÄ$\ÄBÄ8™ÄNILÄ\ÄBÄ8\Ä¨ÄRTMS\ÄBÄ8lÅMODIFY-REL\ÄBÄ8¨ÄLISP\ÄBÄ8FÄ©ÄBASEFÄ
ÈÄFONTSÄ\Ä©Å*CODE-FONT*ÄÈÅ*COMMENT-FONT*ÈÅ*STRING-FONT*Ä)ÅPACKAGEÄ©ÄRTMS©ÄMODE©ÅCOMMON-LISPÄÄ√ÇCREATE-ATTR-DESCRIPTORÄÎÄ,fÜÄ@¥FÄ:¿$Ä¿BÄ:p¿¨ÄTICLÏÄART-QÄ]ÄFÄÄ:BÄ:BÄ:jÄTÄFÄp¿¨ÄSYSÄlÇDEBUG-INFO-STRUCTÄBÄP\Ä√ÅATTRIBUTE-LIST√ÇSYSTEM-ATTRIBUTE-LISTÄBÄ:\Ä√ÅATTR-DES-PAIRÄÅATTR-DEFÅATTR-DOCÉÄDOMÄCÅFOUND-ATTRCÅATTR-TUPLEBÄ:BÄ:BÄ:CÅATTR-NAMEÄ√ÄTUPLESBÄ:BÄ:\Ä)ÇMACROS-EXPANDEDÄ\ÄÍÄTHIRDÄÍÄFIFTHÄÍÄFOURTHÍÄSECONDp¿BÄTlÇCONDITION-BIND-IFÄp¿BÄTÏÅCONDITION-BINDp¿BÄTÏÇCATCH-CONTINUATION-IFÄp¿BÄTlÇCATCH-CONTINUATIONp¿BÄTÏÄERRSET™ÄPROGp¿¨ÄZLCÄ,ÅDO-NAMEDp¿BÄTÏÇINHIBIT-STYLE-WARNINGS™ÄSETFÄp¿lÄEH¨Ç*CONDITION-HANDLERS*ëp¿,ÄÏÄG0360Ä¿FÄB¿ÍÄERRORÄ¿p¿BÄ\ÏÅERRSET-HANDLER¿p¿BÄ\ÏÅSTRING-EQUAL*Ä“™ÄLIST“BÄf¿p¿BÄT¨ÄDEFÄ¿ÉÄDOCÄ¿p¿BÄ\,Å*APPENDÄíF—ÄQH¡G¡O¸GQHSI¡D€ÅQJ¡3¸JSE¡PPTPPPˇ€JCK√PJCL√÷EWIQí	äJ!BJ!B\‰D›EQBA¡EUBB¡E[C¡@QEW
PCQPAQPBQ	≤	íí@¡J≈DÊJ…ÁD
Ê@QI5‰IQ¸IQ	äí@√¸ˇ€CG√¡H≈HØÁ@OÄïBÄPÄÄCÉGET-SYSTEM-ATTRIBUTE-LISTÄÄÎÄ(ÜÄ@DFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄñ\Ä√ÅRELATION-NAMEÄBÄ:BÄ:BÄ:Ä√Ç*SYSTEM-ATTRIBUTE-KEY*—√É*SYSTEM-ATTRIBUTE-ATTRIBUTES*Ä—ÉÅ*PKG-STRING*—ÉÑ*SYSTEM-RELATION-STORAGE-STRUCTURE*Ä—√Ñ*SYSTEM-RELATION-BASE-IMPLEMENTATION*ÄëÍÄSTRING¿lÅRETRIEVE-Ä¿lÄ-Ä¿™ÅCONCATENATEÄ“™ÅFIND-SYMBOLÄ“ÇSYSTEM-ATTRIBUTE¿™ÅSTRING-EQUAL¿BÄü¿ÍÅSTRING-UPCASEÄ“BÄèíP	PP
PP™Pí@¡PPPPPPÄQäöˇ€PJ@ΩOÄ≠BÄñÄÄÉÅREDEFINE-RELÄÎÄ"aÜÄBHFÄ?¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÆ\Ä	BÄüBÄcÉÄIMPÄCÄSSÉÄKEYÄCÇTUPLE-FORMAT-LISTÄBÄíÉÄDIRÄCÅTUPLE-LISTBÄ:\ÄÅTEMP-RELÅSTATUS?Ä\ÄBÄl\Äp¿BÄ\lÅXR-BQ-LISTBÄÇÄCÉ*PROVIDE-STATUS-MESSAGES*ÄëÍÄGENSYM“BÄ•“*ÇREAD-FROM-STRING“BÄ∑¿ÉÄSTOÄ¿BÄπ¿ÍÄFORMAT¿BÄí¿BÄª¿ÇDEFINE-RELATIONÄ“ÉÅSYSTEM-INDEX¿BÄ´¿BÄü¿BÄè“\ÄÏÅRELATION-NAMEÄ¿BÄ¨¿BÄ8¿™ÄEVAL“ÇDELETE-OR-MODIFY“CÅRELATIONPÄ“ÇDESTROY-RELATION“ÉÇDELETE-INDEX-TUPLESÄ“ÇRENAME-RELATIONÄ“BÄj¿√ÄINSERTíPA¡Çää@¡⁄@QÅQPÇQPÉQ	PÑQ
PÖQPÜQPáQJ∏ÊAQ¿RPˇ›PPÄQäöPPP@Qííää®ÄQà‰ÄQà@QÇQê@QÄQääêﬂÂà‰ÄQääPàQòAQ¿ÄOÄ‘BÄÆÄÄÇMODIFY-RELATIONÄÄÎÄU÷ÜÄ‡U@FÄ+¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ’\ÄÅRELATIONÍÄ&RESTÄÉÅKEYWORD-LIST™Ä&KEYjÅ&OPTIONALÄBÄü√ÅADD-ATTRIBUTESCÇDELETE-ATTRIBUTESÄCÇRENAME-ATTRIBUTESÄÉÇIMPLEMENTATION-TYPEÄCÇSTORAGE-STRUCTUREÄBÄ»BÄπÍÅDOCUMENTATIONÄjÅDIRECTORYÄjÇ&ALLOW-OTHER-KEYSÄBÄ:\Ä#BÄ‡BÄüBÄ„BÄ‰BÄÂBÄÊBÄÁBÄ»BÄπBÄËBÄÈÉÄRELÄÉÄATTRBÄ∑BÄ«ÅFORMAT1ÄÉÄKEY1BÄíBÄªÅMOD-ATTRÅMOD-VALSÉÅRENAME-ATTRSCÅNEW-ATTRSÄCÇCURRENT-ATTRIBUTESBÄjÉÅDELETE-ATTRSÅOLD-VALSBÄ‡BÄ:BÄ:ÉÄATTÄBÄ:BÄ:ÅDOM-DEFÄBÄ:\ÄBÄl\ÄÍÄFIRSTÄBÄÅBÄqBÄnBÄp*ÅSEVENTHÄÍÄSIXTHÄBÄoBÄBÄ√p¿BÄTlÅCOND-EVERYBÄ|BÄÇÈÅDOCUMENTATIONÄÏøáModify various features of a relation.

  RELATION             - Name of the relation to be modified.
  RELATION-NAME        - New name for this relation.
  ADD-ATTRIBUTES       - List of new attributes and their description.
  DELETE-ATTRIBUTES    - List of attributes to be destroyed.
  RENAME-ATTRIBUTES    - List of list of OLD-NEW attribute names.
  IMPLEMENTATION-TYPE  - Name of the new implementation type.
  STORAGE-STRUCTURE    - Name of the new storage-structure.
  FORMAT               - List of new print-width values to be used for the attributes.
  KEY                  - List of attributes to form the new key for this relation.
  DOCUMENTATION        - New description of this relation.
  DIRECTORY            - New directory in which this relation is to be saved.Ä¿ÜÄ£ ÄCÅ*PKG-NAME*—√Ç*SYSTEM-RELATION-KEY*Ä—ÉÉ*SYSTEM-RELATION-ATTRIBUTES*—BÄ¢—BÄ£—BÄ§—BÄƒ—ÉÇ*VALIDITY-CHECKING*Ä—ÉÅ*ACTIVE-DB*Ä—É*PROVIDE-ERROR-MESSAGES*—ÉÇ*PARAMETER-CHECKING*ë\Ä
ÈÅRELATION-NAMEÄÈÅADD-ATTRIBUTESiÇDELETE-ATTRIBUTESÄiÇRENAME-ATTRIBUTESÄ©ÇIMPLEMENTATION-TYPEÄiÇSTORAGE-STRUCTUREÄÈÄFORMAT©ÄKEYÄBÄiÅDIRECTORYÄ¿p¿BÄ\ÏÅSTORE-KEYARGSÄ“ÇACTIVE-DATABASEÄ“ÉÅVALIDATE-SYM“\Ä
BÄÏÅADD-ATTRÉÅDELETE-ATTRÄÉÅRENAME-ATTRÄBÄ∑BÄ«BÄ»BÄπBÄíBÄª¿ÉGET-KEYWORD-VALUE-PREREQ“\ÄlÅATTRIBUTES¿ÉÅGET-RELATION“ÍÄTERPRI“lÇERROR - Relation Ä¿™ÅWRITE-STRING“ÍÄPRIN1Ä“,Ñ does not exist in the database ¿\ÄBÄ»¿CÇGET-KEYWORD-VALUEÄ“\Ä¨ÅTUPLE-FORMAT¿BÄî“BÄ8¿BÄè“\ÄBÄª¿\ÄÏÅSAVE-DIRECTORY¿\ÄBÄí¿\ÄÏÅDOCUMENTATIONÄ¿\ÄBÄπ¿\Ä¨ÄKEYÄ¿\ÄBÄ∑¿\ÄBÄ«¿BÄj¿ÅRETRIEVE“\ÄlÅATTRIBUTESÏÅSAVE-DIRECTORY¨ÄDOCÄ¨ÅTUPLE-FORMAT¨ÇIMPLEMENTATION-TYPEÄlÇSTORAGE-STRUCTUREÄ¨ÄKEYÄ¿jÅCOPY-LISTÄ“\ÄBÄ¿CÇCONVERT-ATTRIBUTES“BÄ´¿p¿BÄ\¨ÅMEMBER-TESTÄ“p¿BÄ\,ÅDELETE*Ä“,ÅERROR - ¿ÏÑ is not an attribute in the relation Ä¿,É. It can not be deleted.¿ÉÅPROJECT-LIST“BÄñ“BÄP“\ÄBÄ¿p¿BÄ\lÅSUBST-EQLÄ“,É. It can not be renamed.¿\ÄBÄ¿\ÄBÄÏ¿BÄÆ“BÄ•¿,ÅMODIFY-Ä¿lÄ-Ä¿BÄ®“BÄ©“ÇSYSTEM-RELATIONÄ¿BÄü¿BÄ•“BÄ™¿ÅPROJECTÄ¿\Ä√ÅATTRIBUTE-NAMEÇDOMAIN-FUNCTIONÄ¿√ÄWHEREÄ¿BÄ¨“BÄ∆“\ÄlÅATTRIBUTES¨ÇIMPLEMENTATION-TYPEÄlÇSTORAGE-STRUCTUREÄ¨ÄKEYÄ¿ÇRENAME-ATTRIBUTE“ÇDEFINE-ATTRIBUTE“CÅATTRIBUTEÄ¿CÇDESTROY-ATTRIBUTEÄ“√ÅDELETE-TUPLESÄ“√ÅINSERT-TUPLESÄ“BÄ“í@‰@QPˇ›A—†‰ÄÊRÄQäÄ¡ÊR@Q¸[S[¡‰[Sˇ5˙Á[Q@¡P@Qí@¡ÄQPˇ›ö	BW¡Ê	‰ÄPàÄQàPàPàRP@QíBO¡‰ˇ€PíS¡ˇ€ POQ!í!äíT√]¡\›"P@QíBR¡‰SQ#PíS¡TQ PRQ!í!äíT√]¡\›$P@QíBQ¡‰SQ%PíS¡TQ PQQ!í!äíT√]¡\›&P@QíBP¡‰SQ'PíS¡TQ PPQ!í!äíT√]¡\›(P@QíBM¡)P@QíBN¡MÊNß‰ÄQ*Pˇ›+öX¡ÄQ,Pˇ›öBW¡WS-äL¡.P@Qí&‰.P@QíB/ä\¡‰\S^√LQ0P1ò‰^QLQ2íL¡¸‰Ä3Pà^Qà4PàÄQà5Pà\≈ÊÁXQWSLQ6öX¡LQÄQ7ä8íL¡9P@Qí)‰9P@QíB/ä\¡"‰\S^¡^5‰^SLQ0P1ò‰^W^SLQ:öL¡¸‰Ä3Pà^5‰^S¸^Qà4PàÄQà;Pà\≈ﬁÁ<P@Qí‰LQ<P@QíBíL¡P
P!íZ¡⁄
⁄=P@QíB‚ÄQK√LQMQ‚WUBNQ‚WYBPQ‚WQBBOQ‚WQBQQ‚W[RQ‚WWXQ	J>∏ÊRZS¿ZW
¿KS9‰?P@P	PAPPB™PCíb¡DPPPSQTQ0PEPÄQFä!ö]€]—GPHPIP*Pˇ›JP0PEPÄQKä!öJ+∫`¡_¡¸_Q`Sa¡aS?PPaWBöLä!íC_√¡`≈`Á]Qˇ€JbπÄQMPˇ›öBW¡9P@QíBU¡<P@QíBV¡.P@QíBY¡U‰ÄQUQNêV‰ÄQVQOêY‰ÄQPPYQQòP‰ÄQ*Pˇ›+öX¡ÄQJPˇ›RòÄQ*PXQSò=P@QíBK¡‰ÄQKQTêSOÄaBÄ’ÄÄÇMODIFY-DATABASEÄÄÎÄ-vÜÄ``FÄI¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄb\Ä	ÅDATABASEBÄﬂBÄ‡BÄ·BÄ‚√ÅDATABASE-NAMEÄBÄÈBÄËBÄÍBÄ:\ÄBÄ‡BÄlBÄÈBÄËÉÄTEMPBÄ‡BÄ:BÄ:\ÄBÄl\ÄBÄˇBÄ|BÄÇBÄl¢Modify various features of the active database.

  DATABASE      - Name of the database to be modified.
  DATABASE-NAME - New name for this database.
  DIRECTORY     - New directory in which this database is to be saved.
  DOCUMENTATION - New description for this database.ÄÄBÄ—BÄ—BÄ—BÄ	ë\ÄÈÅDATABASE-NAMEÄBÄBÄ¿BÄ“BÄ“BÄ“BÄé“BÄ “ÏáERROR - The database to modify has to be the active database Ä¿BÄ"“BÄ#“\ÄBÄkBÄªBÄí¿BÄ“\ÄBÄª¿BÄ&“BÄ•¿,Ç*SAVE-DIRECTORY*¿BÄ®“BÄ∆“\ÄBÄí¿,É*DATABASE-DOCUMENTATION*¿\ÄBÄk¿ÇRENAME-DATABASEÄí@‰@QPˇ›A—†‰	ÄÊRÄQˇ›
íÄ¡ÊRÄQPêÊ‰ÄPàPàR@Q¸ESE¡‰ESˇ5˙ÁEQ@¡P@Qí@¡P@QíBD¡	‰PPPöäDQ»BG¡F›P@QíBD¡	‰PPPöäDQ»BG¡F›P@QíBD¡‰ÄQDQíG¡F›DQ‚ÄˇOÄ|BÄbÄÄÇMODIFY-ATTRIBUTEÄÎÄ<t$ÜÄ‡<ÄFÄ∞¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ}\ÄBÄﬁBÄ]BÄﬂBÄ‡BÄ·BÄ‚BÄS√ÅDEFAULT-VALUEÄBÄËBÄ»BÄÍBÄ:\ÄBÄ‡BÄSBÄÜBÄËBÄ»BÄnCÅATTRIBUTESÉÅTUPLE-FORMATÉÄNUMÄBÄ‡BÄ:BÄ:BÄ:BÄ:BÄ:BÄ¯\ÄBÄl\ÄBÄqBÄ˚BÄ√BÄBÄÅBÄˇBÄ|BÄÇBÄ¨≥Modify various features of an attribute in a given relation.

  RELATION       - Name of the relation in which the attribute to be modified exists.
  ATTRIBUTE      - Name of the attribute to be modified.
  ATTRIBUTE-NAME - New name for this attribute.
  DEFAULT-VALUE  - New default value for this attribute.
  DOCUMENTATION  - New description.
  FORMAT         - New print width to be used for this attribute.Ä¿ÜÄAÄCÉ*PROVIDE-WARNING-MESSAGES*—BÄ—BÄ—BÄ—BÄ	ë\ÄÈÅATTRIBUTE-NAMEÈÅDEFAULT-VALUEÄBÄBÄ¿BÄ“BÄ“BÄ“\ÄlÅATTRIBUTES¨ÅTUPLE-FORMAT¿BÄ“BÄ “lÇERROR - Relation Ä¿BÄ"“BÄ#“,Ñ does not exist in the database ¿BÄ?“BÄ´¿BÄA“,ÅERROR - ¿ÏÑ is not an attribute in the relation Ä¿\ÄBÄÌBÄëBÄíBÄ»¿BÄ“\ÄBÄë¿BÄ&“BÄ™¿BÄQ¿\ÄBÄSBÄT¿BÄj¿BÄU¿BÄü¿BÄ¨“BÄè“BÄ4“BÄ•¿BÄ®“BÄ∆“CÅDOM-CHECKÄ“™ÄANDÄ¿BÄS¿\ÄÏÅDEFAULT-VALUEÄ¿BÄŒ“\ÄBÄ»¿lÅWARNING - ¿ÏÉ is not a valid format value.Ä¿ÍÄEQUALÄ¿p¿BÄ\lÅPOSITION*Ä“p¿BÄTÏÄFIRSTN“ÍÄAPPEND“BÄP¿\Ä¨ÅTUPLE-FORMAT¿BÄ8¿\ÄBÄí¿\Ä¨ÄDOCÄ¿\ÄBÄÌ¿BÄ[í@‰@Q	Pˇ›A—
†‰ÄÊRÄQäÄ¡ÊRÄQPˇ€öBG¡GÊ	‰ÄPàÄQàPàPàRÅQäBÅ√GSPòÊ	‰ÄPàÅQàPàÄQàR@Q¸ISI¡‰ISˇ5˙ÁIQ@¡P@Qí@¡P@QíBE¡<‰EQÅQL—PPP Pˇ›!PP"PÄQ#ä$öJ%∫N¡M¡¸MQNSO¡OS&PPOW'ö(ä$íCM√¡N≈NÁLQ)ò‰Pˇ›*PP"PÄQ#ä$öP+PÅQ#ä$ö$ö,PEQ$ä-™¸ˇ€K¡J›.P@QíBE¡.‰E1Ê‰Ä/PàEQà0PàK€ ¸GSF¡GWG¡ÅQFQ1P2öH√GQ3íEQ$äHkGQ
C4öG¡5Pˇ›P"PÄQ#ä$ö6P7PGQ$í$ä-™K¡J›8P@QíBE¡‰Pˇ›*PP"PÄQ#ä$öP+PÅQ#ä$ö$ö9PEQ$ä-™K¡J›:P@QíBE¡‰ÄQÅQEQ;öK¡J›EQ‚ÅˇOÄ∞BÄ}Ä1Ä\Äp¿BÄ\,ÑFASL-RECORD-FILE-MACROS-EXPANDED\ÄBÄ8\Ä\ÄÍÄDEFUNÄÜÄ'\ÄBÄˇÜÄ∆9\ÄBÄ˝ÜÄ{ƒ≤\ÄBÄ¸ÜÄ2ª=\ÄBÄ˚ÜÄz(á\ÄBÄ√ÜÄ.Ÿã\ÄBÄÇÜÄ[ÊÑ\ÄBÄÅÜÄ(Ã¢\ÄBÄÜÄ*˝j\ÄBÄ|ÜÄ=Ã#\ÄBÄ{ÜÄ-i\ÄBÄyÜÄ~…z\ÄBÄwÜÄ<pë\ÄBÄuÜÄ`sN\ÄBÄsÜÄ|ƒÙ\ÄBÄqÜÄ{öÕ\ÄBÄpÜÄxıø\ÄBÄoÜÄZiÛ\ÄBÄnÜÄ:}nÄÄ     (setf new-attrs (car (get-keyword-value '(add-attr) keyword-list)))
       (setf delete-attrs (car (get-keyword-value '(delete-attr) keyword-list)))
       (if rename-attrs
   (rename-attribute relation rename-attrs))
       (if new-attrs
   (define-attribute relation new-attrs))
       (if delete-attrs
   (destroy-attribute relation 'attribute delete-attrs))
       (if key1
   (progn
     (setf tuples (retrieve relation 'tuples t))
     (delete-tuples relation 'where t)
     (insert-tuples relation 'tuples tuples)))
       (if (setf rel (car (get-keyword-value '(rel) keyword-list)))
   (rename-relation relation rel))
       (return-from modify-relation t)))

(defun modify-database (database &rest keywLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540790. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "LISP" :NAME "MOVE-DATABASE" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :CHARACTERS T :NOT-BACKED-UP T :CREATION-DATE 2749846300. :AUTHOR "REL3" :LENGTH-IN-BYTES 1746. :LENGTH-IN-BLOCKS 2. :BYTE-SIZE 8.)

;;; -*- Mode:Common-Lisp; Package:RTMS; Fonts:(CODE-FONT COMMENT-FONT STRING-FONT); Base:10 -*-
;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved.
;;; Moves the current database into a temporary location in virtual memory which can be retrieved at a later time. This prevents
;;; the destruction and reloading of a database when another is needed for a short time. This is great for testing RTMS. It is done in
;;; quick and dirty way. At some time in the future it will have to be redone.
(defun move-database (database-name &optional (direction 'output))
  (cond ((equal direction 'output)
 (putp database-name *save-directory* 'save-directory)
 (mapc (function (lambda (%relation-name)
   (putp (intern-local (string-upcase
    (string-append database-name "-" (car %relation-name)))
         *pkg-string*)
  (getp (intern-local (string-upcase (car %relation-name)) *pkg-string*)
        'entry-point)
  'entry-point)))
       (retrieve 'system-relation 'project '(relation-name) 'tuples t)))
((equal direction 'input)
 (putp 'system-relation
       (getp (intern-local (string-upcase (string-append database-name "-" "SYSTEM-RELATION"))
    *pkg-string*)
     'entry-point)
       'entry-point)
 (setf *active-db* database-name)
 (setf *save-directory* (getp database-name 'save-directory))
 (mapc (function (lambda (%relation-name)
   (putp (intern-local (string-upcase (car %relation-name)) *pkg-string*)
  (getp (intern-local (string-upcase
          (string-append database-name "-" (car %relation-name)))
        *pkg-string*)
        'entry-point)
  'entry-point)))
       (retrieve 'system-relation 'project '(relation-name) 'tuples t))))
  t)
PPPPPPÄQäöˇ€PJ@ΩOÄ≠BÄñÄÄÉÅREDEFINE-RELÄÎÄ"aÜÄBHFÄ?¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÆ\Ä	BÄüBÄcÉÄIMPÄCÄSSÉÄKEYÄCÇTUPLE-FORMAT-LISTÄBÄíÉÄDIRÄCÅTUPLE-LISTBÄ:\ÄÅTEMP-RELÅSTATUS?Ä\ÄBÄl\Äp¿BÄ\lÅXR-BQ-LISTBÄÇÄCÉ*PROVIDE-STATUS-MESSAGES*ÄëÍÄGENSYM“BÄLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540793. :SYSTEM-TYPE :LOGICAL :VERSION 2. :TYPE "LISP" :NAME "PRINT" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :CHARACTERS T :NOT-BACKED-UP T :CREATION-DATE 2755785296. :AUTHOR "REL3" :LENGTH-IN-BYTES 13236. :LENGTH-IN-BLOCKS 13. :BYTE-SIZE 8.)

;;; -*- Mode:Common-Lisp; Package:RTMS; Fonts:(*CODE-FONT* *COMMENT-FONT* *STRING-FONT*); Base:10 -*-
;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved
;;; PRINT
;;;
;;; This file contains the following Explorer extensions to CommonLisp as Indicated in the June 1985 Explorer Lisp
;;; Reference
;;;     listarray
;;;     errset

;;; Change history
;;; 04.30.87  MRR   Fixed typo in printrel-internal* that caused a bug (SPR#4531).

(defun print-tuple (tuple tuple-format pathname output-to-window blanks stream
    &aux attr-list len)
  (cond ((null output-to-window)
 (print-tuple* tuple tuple-format pathname blanks stream))
(t
 (mapcar (function (lambda (%tuple)
     (setf attr-list  (list "|"))
     (mapc (function (lambda (tup tupfmt)
         (setf tup (with-output-to-string (x) (prin1 tup x)))
         (setf len (length tup))
         (setf attr-list
        (append
          attr-list
          (cond ((> len tupfmt)
          (list (list (concatenate 'string
          (subseq tup 0
              (- tupfmt 1)) "@") tup)))
         (t
          (list (list (concatenate 'string
          tup
          (subseq blanks 0
              (- tupfmt len))) tup))))
          (list "|")))))
    %tuple tuple-format)
     (mapc (function (lambda (x)
         (cond ((listp x)
         (cond-every (pathname
        (princ (car x) pathname))
       (stream
        (princ (car x) stream))))
        (t
         (cond-every (pathname
        (princ x pathname))
       (stream
        (princ x stream)))))))
    attr-list)
     (cond-every (pathname
    (terpri pathname))
   (stream
    (terpri stream)))
     attr-list))
 tuple))))

(defun print-tuple* (tuple tuple-format pathname blanks stream
     &aux len)
  (mapc (function (lambda (%tuple)
    (cond-every (pathname
  (princ "|" pathname))
 (stream
  (princ "|" stream)))
    (mapc (function (lambda (tup tupfmt)
       (setf tup (format nil "~S" tup))
       (setf len (length tup))
       (cond ((> len tupfmt)
       (setf tup (concatenate 'string (subseq tup 0 (- tupfmt 1)) "@"))
       (cond-every (pathname
      (princ tup pathname))
     (stream
      (princ tup stream))))
      (t
       (setf tup (concatenate 'string tup (subseq blanks 0
           (- tupfmt len))))
       (cond-every (pathname
      (princ tup pathname))
     (stream
      (princ tup stream)))))
       (cond-every (pathname
      (princ "|" pathname))
     (stream
      (princ "|" stream)))))
  %tuple tuple-format)
    (cond-every (pathname
  (terpri pathname))
 (stream
  (terpri stream)))))
tuple))

(defun print-tuple-wide (tuples project-attributes number-per-line output-to-window pathname stream
 &aux (items nil) att-sts
 (screen (if (> (send tv:default-screen ':height) (send tv:default-screen ':width))
      85.
      110.)))
  (setf att-sts (mapcar #'(lambda (attr)
    (length (format nil "~S" attr)))
project-attributes))
  (cond ((null output-to-window)
 (print-tuple-wide* tuples project-attributes number-per-line pathname stream att-sts screen))
(t
 (mapc (function (lambda (tuple &aux (line-item nil) (line-length 0.) (number-in-line 0.))
   (mapc (function (lambda (attr val at-st &aux val-st dum-val)
       (setf val-st (length (format nil "~S" val)))
       (cond ((or (equal number-per-line number-in-line)
    (> (+ 5 line-length at-st val-st) screen))
       (cond-every (pathname
      (terpri pathname))
     (stream
      (terpri stream)))
       (setf number-in-line 0. line-length 0.)
       (setf items (cons line-item items))
       (setf line-item nil)))
       (setf line-item
      (append line-item
       (list (list ':item1 attr 'attribute) ": "
      (list (setf dum-val
           (if (stringp val)
        (with-output-to-string (x)
          (prin1 val x))
        val))
            dum-val) "  ")))
       (cond-every (pathname
      (prin1 attr pathname)
      (princ ": " pathname)
      (prin1 val pathname)
      (princ "  " pathname))
     (stream
      (prin1 attr stream)
      (princ ": " stream)
      (prin1 val stream)
      (princ "  " stream)))
       (setf number-in-line (+ 1 number-in-line))
       (setf line-length (+ 5 line-length at-st val-st))))
  project-attributes tuple att-sts)
   (if line-item
       (setf items (cons line-item items)))
   (setf items (cons " " items))
   (cond-every (pathname
  (terpri pathname))
        (stream
  (terpri stream)
  (terpri stream)))))
       tuples)
 (reverse items))))

(defun print-tuple-wide* (tuples project-attributes number-per-line pathname stream att-sts screen)
  (mapc (function (lambda (tuple &aux (line-item nil) (line-length 0.) (number-in-line 0.))
    (mapc (function (lambda (attr val at-st &aux val-st)
       (setf val-st (length (format nil "~S" val)))
       (cond ((or (equal number-per-line number-in-line)
    (> (+ 5 line-length at-st val-st) screen))
       (cond-every (pathname
      (terpri pathname))
     (stream
      (terpri stream)))
       (setf number-in-line 0. line-length 0.)
       (setf line-item nil)))
       (cond-every (pathname
      (prin1 attr pathname)
      (princ ": " pathname)
      (prin1 val pathname)
      (princ "  " pathname))
     (stream
      (prin1 attr stream)
        (princ ": " stream)
      (prin1 val stream)
      (princ "  " stream)))
       (setf number-in-line (+ 1 number-in-line))
       (setf line-length (+ 5 line-length at-st val-st))))
  project-attributes tuple att-sts)
    (cond-every (pathname
  (terpri pathname))
 (stream
  (terpri stream)
  (terpri stream)))))
tuples))

(defun print-wide-format (relation project-attributes number-per-line output-to-window pathname item-list
  list-of-tuples attributes card stream
    &aux (new-items nil) temp)
  attributes
  (if output-to-window
      (progn
(setf new-items (cons (list "Relation:  " (list ':item1 relation 'relation)
     "  Database:  " (list ':item1 *active-db* 'database)
     "  Cardinality:  " card)
      new-items))
(putp relation (if (setf temp (length (send *output-window* ':items)))
   temp
   1)
      ':index)
(setf new-items (cons (list " ") new-items)))
      (progn
(terpri stream)
(format stream "Relation: ~S  Database:  ~S  Cardinality: ~S" relation *active-db* card)
(terpri stream)
(terpri stream)))
  (if pathname
      (progn
(terpri pathname)
(format pathname "~%Relation: ~S  Database:  ~S  Cardinality: ~S" relation *active-db* card)
(terpri pathname)
(terpri pathname)))
  (if list-of-tuples
      (setf item-list (append item-list (reverse new-items)
      (print-tuple-wide list-of-tuples project-attributes number-per-line
   output-to-window pathname stream))))
  (if pathname
      (close pathname))
  (if output-to-window
      (send *output-window* ':set-items item-list))
  relation)

(defun printrel-internal* (relation list-of-tuples project-attributes number-per-line wide-format stream
   output-to-file tuple-format header tail card
   &optional (print? t) (return-tuples nil)
   &aux attributes pathname output-to-window rowline attr-list item-list qtrieve-var
   (new-item-list nil) blanks first last)
  (block printrel-internal*
(cond ((not (stringp relation))
       (setf project-attributes (mapcar (function (lambda (attribute)
        (read-from-string (string-upcase attribute))))
   project-attributes))))
  (setf output-to-window (car (errset (send  *output-window* ':exposed-p) nil)))
  (if (null print?)
      (setf output-to-window nil))
  (cond (output-to-file
 (setf pathname (cond ((listp output-to-file)
       (car (errset (eval (append '(open) output-to-file '(:direction :output))))))
      (t
       (car (errset (open (setf pathname output-to-file) ':direction :output) nil))))) ;mrr 04.30.87
 (if (and (null pathname) *provide-error-messages*)
     (format *standard-output* "~%ERROR - ~S is a bad file." output-to-file)))
(t
 (setf pathname output-to-file)))
  (if output-to-window
      (setf item-list (listarray (send *output-window* ':items))))
  (if output-to-window (scroll-to-bottom))
  ;;If the relation is to be printed wide, return-from printrel-internal* with the following call.
  (if (and (not stream) (not output-to-window) print?)
      (setf stream *standard-output*))
  (if return-tuples
(progn
  (if stream
      (format stream "~%~s" list-of-tuples))
  (if pathname
      (progn
(format pathname "~%~s" list-of-tuples)
(close pathname)))
  (return-from printrel-internal* t)))
  (if wide-format
      (progn
(if number-per-line
    number-per-line
  (setf number-per-line -1))
(return-from printrel-internal* (print-wide-format (string-upcase relation) project-attributes
       number-per-line output-to-window
       pathname item-list list-of-tuples attributes card
       stream))))
  ;;
  ;;Form the row-line
  (setf tuple-format (mapcar (function (lambda (attr form)
   (if form
       form
       (length (format nil "~S" attr)))))
     project-attributes tuple-format))
  (setf rowline  (make-array (+ 1 (length project-attributes)
 (apply (function +) tuple-format)) ':type 'art-string ':initial-value 45))
  (setf blanks (make-array (+ 1 (length project-attributes)
      (apply (function +) tuple-format)) ':type 'art-string ':initial-value 32))
  ;;If the header is true, then print the header information.
  (if header
      (progn
(if output-to-window
    (progn
      (setf new-item-list (cons " " new-item-list))
      (setf new-item-list  (cons (list "Relation :  " (list ':item1 (string-upcase relation) 'relation)
         "    Database :  "
         (list ':item1  *active-db* 'database)
         "    Cardinality :  " card)
   new-item-list))
      (setf new-item-list (cons rowline new-item-list))
      (putp relation (if (setf qtrieve-var  (length (send *output-window* ':items)))
  qtrieve-var
  1)
    ':index)
      (setf first (+ 4 qtrieve-var))))
;;Eventhough the code for *standard-output* and pathname looks alike, we have to repeat inorder
;;to allow for both options when in lisp-listener.
(if stream
  (progn
    (terpri stream)
    (format stream  "Relation :  ~S    Database :  ~S    Cardinality :  ~S"
    (string-upcase relation) *active-db* card)
    (terpri stream)
    (format stream rowline)
    (terpri stream)))
(if pathname
    (progn
      (format pathname "~%Relation :  ~S    Database :  ~S   Cardinality :  ~S"
      (string-upcase relation) *active-db* card)
      (terpri pathname)
      (format pathname rowline)
      (terpri pathname)))
(setf attr-list (list "|"))
(mapcar (function (lambda (tup tupfmt &aux len)
    (setf tup (format nil "~s" tup))
    (setf len (length tup))
    (setf attr-list (append
        attr-list
        (list (cond ((> len tupfmt)
       (list ':item1 (list
         (concatenate 'string
           (subseq tup 0 (- tupfmt 1))
           "@") tup)
      'attribute))
      (t
       (list ':item1
      (list (concatenate 'string
       tup
       (subseq blanks 0 (- tupfmt len)))
            tup)
      'attribute))))
        (list "|")))))
project-attributes tuple-format)
(if output-to-window
    (setf new-item-list (cons attr-list new-item-list)))
(mapcar (function (lambda (x &aux y)
    (cond ((listp x)
    (cond-every (pathname
   (princ (if (listp (setf y (cadr x)))
       (car y)
       y)
          pathname))
         (stream
   (princ (if (listp (setf y (cadr x)))
       (car y)
       y)
          stream))))
   (t
    (cond-every (pathname
   (princ x pathname))
         (stream
   (princ x stream)))))))
attr-list)
(if pathname
    (progn
      (terpri pathname)
      (format pathname rowline)
      (terpri pathname)))
(if output-to-window
    (setf new-item-list (cons rowline new-item-list)))
(if stream
    (progn
      (terpri stream)
      (format stream rowline)
      (terpri stream)))))
  ;;Now see if there are any list-of-tuples. Otherwise, print all tuples.
  (setf item-list (append item-list (reverse new-item-list)))
  (if list-of-tuples
      (setf item-list (append item-list (print-tuple list-of-tuples tuple-format pathname output-to-window
        blanks stream))))
  (setf last (- (length item-list) 1))
  ;;See if the tail is to be printed.
  (if pathname
      (progn
(format pathname rowline)
(close pathname)))
  (if stream
      (format stream rowline))
  (if (and tail output-to-window)
      (setf item-list (append item-list (list rowline))))
  (if output-to-window
      (progn
(send *output-window* ':set-items item-list)
(putp relation (list first last) 'items)))
 (return-from printrel-internal* relation)))
ring-equal))
   (progn
     (if *provide-error-messages*
 (format *standLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540797. :SYSTEM-TYPE :LOGICAL :VERSION 2. :TYPE "XLD" :NAME "PRINT" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760360237. :AUTHOR "REL3" :LENGTH-IN-BYTES 3284. :LENGTH-IN-BLOCKS 7. :BYTE-SIZE 16.)  pp2Ä\Ä©ÅCOMPILE-DATA\ÄÏÄSW-MFG,ÅGODZILLAÜÄá§>œFÄFÄ\Äp¿,ÅCOMPILER,ÅVERSIONÄ\ÄFÄFÄp¿BÄ),ÇOPTIMIZE-SWITCHÄÜÄ©ÉQFASL-SOURCE-FILE-UNIQUE-IDÄ1Ä\Äp¿lÄFSÏÇMAKE-FASLOAD-PATHNAMEÄ\ÄÍÄQUOTEÄBÄ$\ÄBÄ8™ÄNILÄ\ÄBÄ8\Ä¨ÄRTMS\ÄBÄ8ÏÄPRINTÄ\ÄBÄ8¨ÄLISP\ÄBÄ8FÄ©ÄBASEFÄ
ÈÄFONTSÄ\Ä©Å*CODE-FONT*ÄÈÅ*COMMENT-FONT*ÈÅ*STRING-FONT*Ä)ÅPACKAGEÄ©ÄRTMS©ÄMODE©ÅCOMMON-LISPÄÄÉÅPRINT-TUPLEÄÄÎÄCñÜÄA¥FÄS¿$Ä¿BÄ:p¿¨ÄTICLÏÄART-QÄ]ÄFÄÄ:BÄ:BÄ:jÄTÄFÄp¿¨ÄSYSÄlÇDEBUG-INFO-STRUCTÄBÄP\Ä√ÄTUPLEÄÉÅTUPLE-FORMAT*ÅPATHNAMEÇOUTPUT-TO-WINDOW√ÄBLANKSÍÄSTREAMBÄ:\ÄCÅATTR-LISTÄÉÄLENÄBÄ:BÄ:BÄ:√Ä%TUPLEBÄ:BÄ:ÉÄTUPÄ√ÄTUPFMTCÄXÄBÄ:BÄ:\Ä)ÇMACROS-EXPANDEDÄ\Äp¿BÄTlÅCOND-EVERYÍÇWITH-OUTPUT-TO-STRINGÄ™ÄSETF™ÄPROGp¿¨ÄZLCÄ,ÅDO-NAMEDp¿BÄTÏÇINHIBIT-STYLE-WARNINGSÄÉÅPRINT-TUPLE*“lÄ|Ä¿™ÄLIST“jÉMAKE-STRING-OUTPUT-STREAMÄ“ÍÄPRIN1Ä“*ÉGET-OUTPUT-STREAM-STRING“ÍÄSTRING¿ÍÄSUBSEQ“lÄ@Ä¿™ÅCONCATENATEÄ“ÍÄAPPEND“ÍÄPRINCÄ“ÍÄTERPRIíÉÊÄQÅQÇQÑQÖQ¨B—ÄQD¡C¡v¸CQDSE¡Pä@¡EQÅQG¡F¡+¸FSGSI¡H¡ÇJ¡HQJQêJQäH√äCA¡@QAQI#‰	PHQJIm
öP¸	PHQÑQJIQAc
ööHQíäPäö@¡F≈G≈F‰G“Á@QF¡'‰FSJ¡J5‰K€L€Ç‰JSÇQíL¡K›Ö‰JSÖQíL¡K›¸L€K€Ç‰JQÇQíK¡L›Ö‰JQÖQíK¡L›F≈ŸÁL€K€Ç‰ÇQäK¡L›Ö‰ÖQäK¡L›@QCC√¡D≈DàÁBOÄáBÄPÄÄBÄzÄÎÄ@åÜÄAhFÄL¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄz\ÄBÄ`BÄaBÄbBÄdBÄeBÄ:\Ä
BÄhBÄ:BÄiBÄ:BÄ:BÄ:BÄjBÄkBÄ:BÄ:\ÄBÄn\ÄBÄsBÄqBÄtBÄwBÄyÄlÄ|Ä¿BÄÖ“lÄ~S¿ÍÄFORMAT“BÄÄ¿BÄÅ“lÄ@Ä¿BÄÉ“BÄÜíÄQA¡|‰ASB¡C€D€Ç‰PÇQíD¡C›Ñ‰PÑQíD¡C›BQÅQC¡E¡R¸ESCSG¡F¡ˇ€PFQöF√äC@√G#‰PFQJGmö	P
öF¡H€I€Ç‰FQÇQíI¡H›Ñ‰FQÑQíI¡H›¸PFQÉQJGQ@cö
öF¡I€H€Ç‰FQÇQíH¡I›Ñ‰FQÑQíH¡I›I€H€Ç‰PÇQíH¡I›Ñ‰PÑQíH¡I›E≈C≈E‰C´ÁI€H€Ç‰ÇQäH¡I›Ñ‰ÑQäH¡I›A≈ÑÁÄOÄóBÄzÄÄÇPRINT-TUPLE-WIDEÄÎÄe·ÜÄ·ÄFÄ|¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄò\Ä√ÄTUPLESCÇPROJECT-ATTRIBUTESÇNUMBER-PER-LINEÄBÄcBÄbBÄeBÄ:\Ä√ÄITEMSÄÅATT-STSÄ√ÄSCREENBÄ:BÄ:BÄ:ÉÄATTRBÄ`CÅLINE-ITEMÄÉÅLINE-LENGTHÄ√ÅNUMBER-IN-LINEBÄ:BÄ:BÄ:ÉÄVALÄ√ÄAT-STÄ√ÄVAL-STÅDUM-VALÄBÄ:BÄ:BÄl\ÄBÄn\ÄBÄrBÄqBÄtBÄwBÄyp¿BÄT¨ÄSENDBÄs¿ÜÄ√Äp¿lÄTVÏÅDEFAULT-SCREENëÈÄHEIGHT¿ÈÄWIDTHÄ¿lÄ~S¿BÄï“CÇPRINT-TUPLE-WIDE*Ä“BÄÜ“ÈÄITEM1Ä¿CÅATTRIBUTEÄ¿BÄ|“lÄ: ¿BÄ}“BÄ~“BÄ“lÄ  ¿p¿BÄ\,Å*APPENDÄ“BÄÖ“lÄ Ä¿*ÅREVERSEÄíPäPäy‰UJ¸nJB¡C—ÅQE¡D¡¸DQESF¡ˇ€PFQöäCCD√¡E≈EÚÁCQA¡É	ÊÄQÅQÇQÑQÖQAQBQJ	ºÄQD¡ö‰DSG¡H€IﬂJﬂÅQGQAQM¡L¡K¡o¸KSLSMSO¡N¡F¡P€Q€ˇ€PNQöäCP¡ÇQJ+ÊJIaOaPaB#‰R€S€Ñ‰ÑQ
äS¡R›Ö‰ÖQ
äS¡R›JﬂIﬂHQ@]@¡H€HQPFQPöPN7‰ÇT¡NQTQêTQä¸NQQ√QQíP¢íH¡S€R€Ñ‰FQÑQêPÑQêNQÑQêPÑQíR¡S›Ö‰FQÖQêPÖQêNQÖQêPÖQíR¡S›J…JIaOaPaI¡K≈L≈M≈K‰L‰MåÁH‰HQ@]@¡P@]@¡S€R€Ñ‰ÑQ
äR¡S›Ö‰ÖQ
àÖQ
äR¡S›D≈fÁ@QåOÄƒBÄòÄÄBÄªÄÎÄ
=ÑÜÄA
¯FÄG¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄª\ÄBÄ°BÄ¢BÄ£BÄbBÄeBÄ¶BÄßBÄ:\ÄBÄ:BÄ`BÄ©BÄ™BÄ´BÄ:BÄ:BÄ:BÄ®BÄ¨BÄ≠BÄÆBÄ:BÄ:\ÄBÄn\ÄBÄqBÄsBÄtBÄwBÄyÄlÄ~S¿BÄï“BÄÜ“BÄ~“lÄ: ¿BÄÖ“lÄ  ÄÄQ@¡v‰@SA¡B€CﬂDﬂÅQAQÖQG¡F¡E¡S¸ESFSGSJ¡I¡H¡K€ˇ€PIQöäCK¡ÇQD+ÊJCaJaKaÜ#‰L€M€É‰ÉQäM¡L›Ñ‰ÑQäM¡L›DﬂCﬂB€M€L€É‰HQÉQêPÉQêIQÉQê	PÉQíL¡M›Ñ‰HQÑQêPÑQêIQÑQê	PÑQíL¡M›D…JCaJaKaC¡E≈F≈G≈E‰F‰G®ÁM€L€É‰ÉQäL¡M›Ñ‰ÑQàÑQäL¡M›@≈äÁÄOÄ”BÄªÄÄCÇPRINT-WIDE-FORMATÄÄÎÄ*mÜÄBàFÄC¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ‘\Ä
ÅRELATIONBÄ¢BÄ£BÄcBÄbCÅITEM-LISTÄ√ÅLIST-OF-TUPLESCÅATTRIBUTESÉÄCARDBÄeBÄ:\ÄCÅNEW-ITEMSÄÉÄTEMP\ÄBÄn\ÄBÄ≥BÄsÄÇ*OUTPUT-WINDOW*Ä—ÉÅ*ACTIVE-DB*Äë¨ÅRelation:  Ä¿BÄº¿BÄ›¿BÄ|“ÏÅ  Database:  Ä¿ÅDATABASE¿,Ç  Cardinality:  ¿ÈÄITEMSÄ¿ÈÄINDEXÄ¿ÉÄPUTP“lÄ Ä¿BÄÜ“¨ÖRelation: ~S  Database:  ~S  Cardinality: ~S¿BÄï“ÏÖ~%Relation: ~S  Database:  ~S  Cardinality: ~S¿BÄ√“BÄò“BÄÑ“ÍÄCLOSEÄ“iÅSET-ITEMSÄÄÉ‰PPÄQPö	PPP
PöPàQ≤C@¡ÄQPääCA¡‰AQ¸JPòPä@]@¡¸ Qà QPÄQPàQ® Qà QàÑ‰ÑQàÑQPÄQPàQ®ÑQàÑQàÜ‰ÖQ@QäÜQÅQÇQÉQÑQ Q≤öÖ¡Ñ‰ÑQàÉ‰PÖQêÄOÄıBÄ‘ÄÄCÇPRINTREL-INTERNAL*ÄÎÄCGÜÄÍC¿FÄE¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄˆ\ÄBÄ›BÄﬂBÄ¢BÄ£ÉÅWIDE-FORMATÄBÄe√ÅOUTPUT-TO-FILEBÄa√ÄHEADERÉÄTAILBÄ·jÅ&OPTIONALÄ\Ä√ÄPRINT?BÄY\Ä√ÅRETURN-TUPLESÄBÄ:BÄ:\ÄBÄbBÄcÅROWLINEÄBÄgBÄﬁÉÅQTRIEVE-VARÄ√ÅNEW-ITEM-LISTÄBÄdÍÄFIRSTÄ™ÄLASTBÄ:BÄ:BÄ:BÄΩBÄ:BÄ:BÄ®ÉÄFORMBÄjBÄkBÄhBÄlCÄYÄBÄ:\ÄBÄn\ÄBÄqBÄ≥p¿BÄTlÇCONDITION-BIND-IFÄp¿BÄTÏÅCONDITION-BINDp¿BÄTÏÇCATCH-CONTINUATION-IFÄp¿BÄTlÇCATCH-CONTINUATIONp¿BÄTÏÄERRSETBÄtBÄwBÄyBÄs¿ÜÄ1òfÄp¿lÄEH¨Ç*CONDITION-HANDLERS*—BÄË—jÇ*STANDARD-OUTPUT*Ä—É*PROVIDE-ERROR-MESSAGES*—BÄÁëÍÅSTRING-UPCASEÄ“*ÇREAD-FROM-STRING“p¿,ÄÏÄG5649Ä¿FÄ¥¿ÍÄERRORÄ¿p¿BÄ\ÏÅERRSET-HANDLER¿iÅEXPOSED-PÄ¿BÄ|“p¿BÄ$ÏÄG5657Ä¿FÄÿ¿\Ä™ÄOPEN¿\ÄiÅDIRECTIONÄÈÄOUTPUT¿BÄÑ“™ÄEVAL“p¿BÄ$ÏÄG5665Ä¿FÄÓ¿BÄ2¿BÄ3¿BÄ0“BÄÜ“,ÅERROR - ¿™ÅWRITE-STRING“BÄ~“,Ç is a bad file.Ä¿BÄÌ¿p¿BÄTlÅLISTARRAYÄ“ÇSCROLL-TO-BOTTOM“¨Ä~%~s¿BÄï“BÄÛ“BÄ‘“lÄ~S¿jÄ+Ä“p¿BÄ\lÇSIMPLE-MAKE-ARRAYÄ“lÄ Ä¿¨ÅRelation :  ¿BÄº¿BÄ›¿,Ç    Database :  ¿BÄÎ¿¨Ç    Cardinality :  Ä¿BÄÓ¿BÄÔ“ÏÜRelation :  ~S    Database :  ~S    Cardinality :  ~SÄ¿ÏÜ~%Relation :  ~S    Database :  ~S   Cardinality :  ~S¿lÄ|Ä¿lÄ~s¿BÄÄ¿BÄÅ“lÄ@Ä¿BÄÉ“BÄΩ¿BÄÖ“BÄ√“BÄ¡“BÄP“BÄÙ¿BÄ•ÄvÊã›Ä7ÊJ—ÇQL¡K¡	¸KQLSM√	ä
äCK√¡L≈LıÁJQÇ¡PPTPPPˇ€JCJ√PJCN√÷PääJ!BJ!B\BA¡ãÊA€Ü@‰Ü5‰PPTPPPˇ›JCK√PJCO√÷PÜQPöääJ!BJ!B¸PPTPPPˇ€JCL√PJCJ√÷ÜQ@√PPö‰˝\B@¡Ê
‰ÄPàÜQà Pà¸ÜQ@¡A‰!Pä"äD¡A‰#ÄÖÊAÊã‰PÖ¡å‰Ö‰ÖQ$PÅQ%ò@‰@Q$PÅQ%ò@Q&àSÑ‰ÉÊLÉ¡ÄQ	äÇQÉQAQ@QDQÅQˇ€äQÖQ
J'ºO€O—ÇQáQL¡K¡J¡¸JQKSLSQ¡P¡Q‰QQ¸ˇ€(PPQ%öäCCJ√¡K≈L≈K‰LÈÁOQá¡ÇQäCˇkáQ)PEˇa	Jˇ€ˇ€-J*™B¡ÇQäCˇkáQ)PEˇa	Jˇ€ˇ€ J*™G¡à„‰A$‰+PCF¡,P-PÄQ	ä.Pö/P-PP0Pö1PäQ≤F]F¡BQF]F¡ÄQ!PääCE¡‰EQ¸J2P3òJEaH¡Ö‰ÖQàÖQ4PÄQ	äPäQ%®ÖQàÖQBQ%êÖQà@‰@Q5PÄQ	äPäQ%®@Qà@QBQ%ê@Qà6PäC¡O€O—ÇQáQL¡K¡J¡/¸JQKSLSS¡R¡T€ˇ€7PRQ%öR√äCT¡CQ-PTQS#‰8PRQJSm9ö:P¸8PRQGQJSQTc9ö;öRQí<Pöä6PäöC√CJ√¡K≈L≈K‰LÕÁA‰CQF]F¡O€O—CQL¡K¡9¸KQLSU¡V€U5‰J€W€@‰UWV√ˇ5‰VS¸VQ@Q=íW¡J›Ö‰UWV√ˇ5‰VS¸VQÖQ=íW¡J›WQ¸W€J€@‰UQ@Q=íJ¡W›Ö‰UQÖQ=íJ¡W›JQCK√¡L≈L≈Á@‰@Qà@QBQ%ê@QàA‰BQF]F¡Ö‰ÖQàÖQBQ%êÖQàDQFQ>ä?íD¡Å
‰DQÅQáQ@QAQGQÖQ@≤?íD¡DQäCˇmI¡@‰@QBQ%ê@Q&àÖ‰ÖQBQ%ê ‰A‰DQBQä?íD¡A	‰APDQêÄQHQIQíBP3òÄOÄLBÄˆÄ1Ä\Äp¿BÄ\,ÑFASL-RECORD-FILE-MACROS-EXPANDED\ÄBÄ8\Ä\ÄÍÄDEFUNÄÜÄ'\ÄBÄÜÄ-i\ÄBÄÜÄ~…z\ÄBÄÜÄ<pë\ÄBÄÜÄ`sN\ÄBÄÜÄ|ƒÙ\ÄBÄ≥ÜÄaM*\ÄBÄyÜÄ(Ã¢\ÄBÄwÜÄ*˝j\ÄBÄtÜÄ=Ã#\ÄBÄsÜÄ[ÊÑ\ÄBÄrÜÄ5%ì\ÄBÄqÜÄ∆9ÄÄ))
      (progn
(terpri stream)
(format stream "Relation: ~S  Database:  ~S  Cardinality: ~S" relation *active-db* card)
(terpri stream)
(terpri stream)))
  (if pathname
      (progn
(terpri pathname)
(format pathname "~%Relation: ~S  Database:  ~S  Cardinality: ~S" relation *active-db* card)
(terpri pathname)
(terpri pathname)))
  (if list-of-tuples
      (setf item-list (append item-list (reverse new-items)
      (print-tuple-wide list-of-tuples project-attributes number-per-line
   output-to-window pathname stream))))
  (if pathname
      (close pathname))
  (if output-to-windLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540800. :SYSTEM-TYPE :LOGICAL :VERSION 2. :TYPE "LISP" :NAME "RELATION-OPS" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :CHARACTERS T :NOT-BACKED-UP T :CREATION-DATE 2751306599. :AUTHOR "REL3" :LENGTH-IN-BYTES 54309. :LENGTH-IN-BLOCKS 54. :BYTE-SIZE 8.)

;;; -*- Mode:Common-Lisp; Package:RTMS; Fonts:(*CODE-FONT* *COMMENT-FONT* *STRING-FONT*); Base:10 -*-
;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved
;;; RELATION-OPS
;;;
;;; This file contains the following Explorer extensions to CommonLisp d as Indicated in the June 1985 Explorer Lisp
;;; Reference
;;;     errset

;;; If the INTO parameter has been provided, it determines the name of the output relation. PROJECT is used to indicate the attributes from
;;; relations relA and relB to be used in the output relation. If the user wants to rename any attributes, he will have to do it the way it is
;;; done for relB.b1. If any attributes are unique, then the prefix "relX." is not necessary (same thing applies to WHERE clause). Finally, the
;;; user can use "*" to indicate that he wants all attributes from that relation are to be used. WHERE is used to indicate the attributes to
;;; which the THETA-OPERATORS are applied. The individual where-clauses are implicitly anded.
;;;
;;; Right now JOIN is done as follows: Take a tuple in relA and substitute the values for all the relA attributes in the WHERE clause. Call
;;; retrieve for relB with the modified WHERE clause as the where-cl. Form a projected cartesan product of all the tuples returned and the
;;; tuple in relA. This is repeated for all tuples in relA and the result is inserted into relC.
;;;
(defun join-internal (keyword-list
      &aux rela-attributes-user relb-attributes-user unknown-attributes-user print all-attrs
      from-clause jrelb-project jrela-format jrelb-format jrelc-format temp join-attrc
      join-insert-list where attrsa attrsb a-join-attrc reader-package jrela jrelb jrelc impa
      ssa attr-imp jrelb-implementation-type jrelb-storage-structure jrelb-key)
  (block join-internal
   (if (not (active-database))
        (return-from join-internal nil))
  (setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
    ((or (null keyword-list)(not (listp (car keyword-list)))) keyword-list)))
  (cond ((or (null keyword-list) (null (car keyword-list)))
 (cond (*provide-error-messages*
(format *standard-output* "~%ERROR - No parameters passed to JOIN")
(return-from join-internal nil)))))
   (setf jrela nil
 join-attrc nil
 a-join-attrc nil
 jrelb nil)
   (setf print (member 'print keyword-list))
   (if print
       (setf print (cadr print)))
   (setf keyword-list (get-keyword-value-prereq '(into where from imp sto key format dir doc print tuples
          project unique)
          keyword-list))
   (setf jrelc (car (get-keyword-value '(into) keyword-list))
 where (or (car (get-keyword-value '(where) keyword-list)) t)
 from-clause (car (get-keyword-value '(project) keyword-list)))
   ;;
   ;;The original from-clause used to be what the project-clause now is.
   ;;
   (if (and (null (car (get-keyword-value '(tuples) keyword-list))) (null jrelc) (null print)
    (null (member 'print  keyword-list)))
       (setf print t))
   (if (and (setf temp (car (get-keyword-value '(from) keyword-list))) (not (listp temp)))
       (setf temp (list temp)))
   (cond-every ((first temp)
(if (null (setf jrela (validate-sym (first temp))))
    (return-from join-internal nil)))
       ((second temp)
(if (null (setf jrelb (validate-sym (second temp))))
    (return-from join-internal nil)))
       ((null (first temp))
(setf jrela nil))
       ((null (second temp))
(setf jrelb nil))
       ((third temp)
(if *provide-warning-messages*
    (format *standard-output* "~%WARNING - More than two relations are provided for joining. The first two will be considered."))))
   ;;
   ;;Parse the FROM clause.
   ;;
  (cond ((and (null jrela) (null jrelb)(null from-clause))
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - The relations to be joined are not provided."))
 (return-from join-internal nil))
((null jrelb)
 (setf jrelb jrela)))
  (cond ((null from-clause)
 (setf from-clause (list (add-dot jrela "*") (add-dot jrelb "*")))))
  (cond (jrela
 (setf reader-package (package-name (symbol-package jrela))))
(jrelb
 (setf reader-package (package-name (symbol-package jrelb)))))
  (multiple-value-setq (from-clause jrela jrelb join-attrc a-join-attrc unknown-attributes-user
     rela-attributes-user relb-attributes-user)
    (parse-from-clause from-clause jrela jrelb join-attrc a-join-attrc unknown-attributes-user
       rela-attributes-user relb-attributes-user))
  ;;
  ;;  This section was added to handle the problem of a NIL reader-package. Reader-package must not be NIL. 12/12/85 smc
  ;;
  (cond ((null reader-package)
 (cond ((symbolp jrela)
(setf reader-package (package-name (symbol-package jrela))))
       ((symbolp jrelb)
(setf reader-package (package-name (symbol-package jrelb))))
       (t
(setf reader-package *pkg-string*)))))
  (if (or (string-equal reader-package "GLOBAL")(equal reader-package 'global))
      (setf reader-package *pkg-string*))

  ;;See if two relations have been provided. If only one is provided, we will perform self-join.
  ;;
  (cond ((and (null jrela) (null jrelb))
 (if *provide-error-messages*
     (format *standard-output*
     "~%ERROR - The FROM clause has not specified the relations to be joined."))
 (return-from join-internal nil))
((null jrelb)
 (setf jrelb jrela)))
  ;;
  ;; Get the attributes of relation A
  ;;
  ;; jrela is reset because it might have been a view name. In this case, the view defination would have been executed and the name of
   ;; the resultant relation would be returned.
  ;;
  (setf attrsa (get-relation jrela '("ATTRIBUTES" "IMPLEMENTATION-TYPE" "STORAGE-STRUCTURE" "KEY" "TUPLE-FORMAT"
      "CARDINALITY") nil))
  (cond ((null (cadr attrsa))
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - Relation ~s does not exist" jrela))
 (return-from join-internal nil)))
  (setf attrsb (get-relation jrelb '("ATTRIBUTES" "IMPLEMENTATION-TYPE" "STORAGE-STRUCTURE" "KEY" "TUPLE-FORMAT"
      "CARDINALITY") nil))
  (cond ((null (cadr attrsb))
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - Relation ~s does not exist" jrelb))
 (return-from join-internal nil)))
  ;;
  ;;  Determine the smallest relation and make it the second relation. This is done to improve the speed.
  ;;
  (if (> (sixth (second attrsa)) (sixth (second attrsb)))
      (progn
(setf temp attrsa)
(setf attrsa attrsb)
(setf attrsb temp)
(setf temp rela-attributes-user)
(setf rela-attributes-user relb-attributes-user)
(setf relb-attributes-user temp)))
  (setf jrela (car attrsa)
attrsa (cadr attrsa)
impa (second attrsa)
ssa (third attrsa)
jrela-format (fifth attrsa)
attrsa (first attrsa))
  ;;
  ;; Get the attributes of relation B
  ;;
  ;; jrelb is reset because it might have been a view name. In this case, the view definition would have been executed and the name of
   ;; the resultant relation would be returned.
  ;;
  (setf jrelb (car attrsb)
attrsb (cadr attrsb)
jrelb-implementation-type (second attrsb)
jrelb-storage-structure (third attrsb)
jrelb-key (fourth attrsb)
jrelb-format (fifth attrsb)
attrsb (first attrsb))
  ;;
  ;;  Determine if the attributes specified are really from the specified relations
  ;;
  (cond ((or (not (symbolp jrela))(not (symbolp jrelb)))
 (cond ((symbolp jrela)
(setf jrelb (find-symbol (string-upcase jrelb))))
       ((symbolp jrelb)
(setf jrela (find-symbol (string-upcase jrela))))
       (t
(setf jrelb (find-symbol (string-upcase jrelb))
      jrela (find-symbol (string-upcase jrela)))))))
  (setf rela-attributes-user (convert-attributes rela-attributes-user)
relb-attributes-user (convert-attributes relb-attributes-user)
unknown-attributes-user (convert-attributes unknown-attributes-user))
  (mapc (function (lambda (attribute)
    (cond ((not (member attribute attrsa :test 'string-equal))
   (if *provide-error-messages*
       (format *standard-output* "~%ERROR - ~s is not an attribute of relation ~s"
        attribute jrela))
   (return-from join-internal nil)))))
rela-attributes-user)
  (mapc (function (lambda (attribute)
    (cond ((not (member attribute attrsb :test 'string-equal))
   (if *provide-error-messages*
       (format *standard-output* "~%ERROR - ~s is not an attribute of relation ~s"
        attribute jrelb))
   (return-from join-internal nil)))))
relb-attributes-user)
  ;;
  ;;  Determine if the unknown attributes are from either relation. If so, make sure that they have been specified correctly in the
   ;; attribute-name lists.
  ;;
  (mapc (function (lambda (attribute)
    (cond ((and (not (member attribute attrsa :test 'string-equal))
 (not (member attribute attrsb :test 'string-equal)))
   (if *provide-error-messages*
       (format *standard-output* "~%ERROR - ~s is not an attribute of either relation"
        attribute))
   (return-from join-internal nil))
  ((and (member attribute attrsa :test 'string-equal)
 (member attribute attrsb :test 'string-equal))
   (cond (*provide-error-messages*
   (format *standard-output*
    "~%ERROR - ~s is an attribute of both relations: ~S and ~S."
    attribute jrela jrelb)
   (format *standard-output*
    "~%        It is unclear which attribute should be used")))
   (return-from join-internal nil)))))
unknown-attributes-user)
  ;;
  ;;  Put the attribute names in the form in which they will appear in the resultant relation. Must expand the relx.* forms to the final names.
  ;;
  (multiple-value-setq (join-attrc a-join-attrc jrelb-project)
    (parse-join-attributes join-attrc a-join-attrc jrelb-project attrsa attrsb jrela jrelb))
  ;;
  ;;  Make a list of all of the attribute which are in both relations with the proper relation name appened to each.
  ;;
  ;; I need the following list in EVAL
  ;;
  (setf all-attrs (append (mapcar (function (lambda (attr)
        (add-dot jrela attr)))
   attrsa)
  (mapcar (function (lambda (attr)
        (add-dot jrelb attr)))
   jrelb-project)))

  ;;
  ;;  Make sure that no attribute was specified more than once
  ;;
  (setf temp nil
join-attrc (reverse join-attrc))
  (do ((attribute join-attrc (cdr attribute)))
      ((null attribute) t)
    (cond ((member (car attribute) (cdr attribute) :test 'string-equal)
   (setf temp (append temp (list  (concatenate 'string (string-upcase jrelb) "."
          (string-upcase (car attribute)))))))
  (t
   (setf temp (append temp (list (string-upcase (car attribute))))))))
  (setf join-attrc (reverse temp))
  (setf jrelc-format (car (project-list (list (append jrela-format jrelb-format)) all-attrs a-join-attrc)))

  ;;See if the resultant relAtion exists.
  (if jrelc
      (progn
(setf attr-imp nil)
(multiple-value-setq (jrelc attr-imp)
  (join-into jrelc join-attrc a-join-attrc keyword-list jrelc-format impa ssa attr-imp))
(if (not jrelc)
    (return-from join-internal nil))))
  (setf join-insert-list (join-eval jrela jrelb attrsa attrsb jrelb-project jrelb-key where
     jrelb-storage-structure jrelb-implementation-type reader-package))
  (cond ((string-equal (string-upcase jrela) (string-upcase jrelb))
 (setf all-attrs nil)
 (do ((attribute a-join-attrc (cdr attribute)))
     ((null attribute) t)
   (if (member (car attribute) (cdr attribute) :test 'string-equal)
       (setf all-attrs (append all-attrs (list (remove-dot-attr (car attribute)))))
       (setf all-attrs (append all-attrs (list (car attribute))))))
 (setf a-join-attrc all-attrs)
 (setf all-attrs (append attrsa (mapcar (function (lambda (attr)
        (add-dot jrelb attr)))
   jrelb-project)))))
  (setf join-insert-list (project-list join-insert-list all-attrs a-join-attrc))
  ;;
  ;;  Complete the join by inserting the tuples formed by the join
  ;;
  (if (and (car (get-keyword-value '(unique) keyword-list)) join-insert-list)
      (setf join-insert-list (unique-tuples join-insert-list)))
  (if (and jrelc join-insert-list)
      (progn
(funcall (find-symbol
   (concatenate 'string "INSERT-"
   (if (cadr attr-imp)
       (concatenate 'string (string-upcase (second (second attr-imp))) "-"
      (string-upcase (third (second attr-imp))))
       (concatenate 'string (string-upcase
         (or (car (get-keyword-value '(imp) keyword-list))
      impa))
        "-"
        (string-upcase
          (or (car (get-keyword-value '(sto) keyword-list))
       ssa))))) *pkg-string*)
 jrelc join-attrc join-insert-list
 (if (cadr attr-imp)
     (fourth (second attr-imp))
     join-attrc)
 jrelc)
(delete-or-modify 'system-relation t (list 'string-equal 'relation-name (string-upcase (string jrelc)))
  '("MODIFIEDP" "CARDINALITY") (list t (+ (if (cadr attr-imp)
           (fifth (second attr-imp))
           0)
       (length join-insert-list))))
(delete-or-modify 'system-relation t (list 'string-equal 'relation-name "SYSTEM-RELATION")
  '("MODIFIEDP") (list t))))
  (cond ((car (get-keyword-value '(tuples) keyword-list))
 (return-from join-internal join-insert-list))
((and print jrelc)
 (printrel-internal* jrelc join-insert-list join-attrc nil nil nil nil
     (if (second attr-imp)
  (sixth (second attr-imp))
  (or (car (get-keyword-value '(format) keyword-list)) jrelc-format))
     t t (length join-insert-list))
 (return-from join-internal jrelc))
(print
 (printrel-internal* 'join join-insert-list join-attrc nil nil nil nil
     (if (< (length (car (get-keyword-value '(format) keyword-list)))
     (length join-attrc))
  (append (car (get-keyword-value '(format) keyword-list))
   (make-list (length join-attrc) ':initial-element *default-anyp-width*))
       (car (get-keyword-value '(format) keyword-list))) t t (length join-insert-list))
 (return-from join-internal t)))
 (return-from join-internal (or jrelc jrela))))

(defun join (&rest keyword-list
     &key from &key &optional project where into directory documentation format implementation-type
     key storage-structure print tuples unique
     &allow-other-keys)
  "This function provides the capability to combine two relations into a new relation
   in which the tuples which are to participate in the operation are selected
   by a where clause.

   FROM                 - A list consisting of the relations to be joined.
   PROJECT              - This clause specifies the attributes that are to be in the resultant relation
                          and their associated names in that new relation. It should be of the form
                          (<[relation-name.]attribute-name>). The optional part relation-name can be
                          skipped if the attribute is unique in one of the two relations being joined.
                          If the keyword FROM is not specified, this clause should contain the names
                          of the relations to be joined. Also, if * is given instead of the attribute-name
                          it indicates that RTMS should use all the attributes in that relation.
   WHERE                - Can be used to perform theta-joins. It is a condition used in joining the relations.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   PRINT                - If NIL, the resultant relation will not be printed out.
   TUPLES               - If T, the resultant tuples will be returned.
   UNIQUE               - If T, only unique tuples will be part of the resultant relation."
   from where into directory documentation format project implementation-type key storage-structure print
   tuples unique
   (join-internal keyword-list))

(defun process-where (tuplea attrsa where-exp jrela jrelb attrsb
      &aux new-where temp)
  (block process-where
   (if (and (listp where-exp)
    (apply 'or (mapcar #'(lambda (x)
    (listp x))
       (cdr where-exp))))
       (return-from process-where (cons (car where-exp) (mapcar (function (lambda (x)
     (process-where tuplea attrsa x jrela jrelb attrsb)))
       (cdr where-exp)))))
   (cond ((not (listp where-exp))
  (setf new-where (car (process-where tuplea attrsa (list where-exp) jrela jrelb attrsb))))
 ((equal (car where-exp) 'quote)
  (setf new-where where-exp))
 (t
  (setf jrela (string-upcase jrela)
jrelb (string-upcase jrelb))
  (mapc (function (lambda (x)
    (cond ((and (and (or (symbolp x) (stringp x)) (POSITION #\. (string-upcase x)))
  (member (string-upcase (remove-dot-attr x)) attrsa :test 'string-equal)
  (string-equal (string-upcase (remove-dot-rel x)) jrela))
    ;;
    ;;We need to quote the value being substituted in the where clause from the tupleA.
    ;;Consider (EQ attrA attrB) if we substitute RAJINI for attrA and call RETRIEVE*
    ;;with (EQ RAJINI attrB) as the where clause it will give an error stating that
    ;;the variable RAJINI is unbound.
    ;;
    (setf temp `(quote ,(nth (position (string-upcase (remove-dot-attr x))
           attrsa :test 'equal)
       tuplea)))
    ;;
    ;;To allow WHERE of the form (EQUAL rel1.a1 rel1.a1) and instead of substituting the rel.a1
    ;;value in both occurences and hence end up with a cartesan product, we have the following
    ;;check.
    ;;
    (if (and (member temp new-where) (equal (string-upcase (remove-dot-rel x))
         jrelb))
        (setf new-where (append new-where (list (remove-dot-attr x))))
        (setf new-where (append new-where (list temp)))))
   ((member (string-upcase x) attrsa :test 'string-equal)
    (setf temp `(quote ,(nth (position (string-upcase x) attrsa :test 'equal)
        tuplea)))
    (setf new-where (append new-where (list temp))))
   ((and (and (or (symbolp x) (stringp x)) (POSITION #\. (string-upcase x)))
  (member (string-upcase (remove-dot-attr x)) attrsb :test 'string-equal)
  (string-equal (string-upcase (remove-dot-rel x)) jrelb))
    (setf new-where (append new-where (list (remove-dot-attr x)))))
   (t
       (setf new-where (append new-where (list x)))))))
where-exp)))
   (return-from process-where new-where)))

(defun process-set-relation (relation-name project-list
     &aux attributes domains)
  (setf attributes (get-relation relation-name '("ATTRIBUTES" "DOMAINS") nil))
  (cond ((null (cadr attributes))
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - The ~s relation is not defined in the database ~s"
     relation-name *active-db*))
 (setf relation-name nil))
(t
 (setf relation-name (car attributes)
       domains (second (cadr attributes))
       attributes (caadr attributes))
 (cond (project-list
(if (not (listp project-list))
    (setf project-list (list project-list)))
(mapc #'(lambda (att)
  (cond ((not (member att attributes :test 'string-equal))
  (if *provide-error-messages*
      (format *standard-output*
       "~%ERROR - ~S is not an attribute in the relation ~S."
       att relation-name))
  (setf relation-name nil))))
      project-list)
(setf domains (mapcar #'(lambda (attr)
    (nth (- (length attributes) (length (member attr attributes
             :test 'string-equal)))
         domains))
      project-list))
(setf attributes project-list)))))
  (values relation-name attributes domains))

(defun set-compatibility (rela relb attrsa attrsb
  &aux attributesa attributesb domainsa domainsb)
  (block set-compatibility
  ;;
  ;;  Obtain the attribute of the specified relations and bring them into memory if they do not already reside there.
  ;;
  (multiple-value-setq (rela attributesa domainsa)
    (process-set-relation rela attrsa))
  (if (null rela)
      (return-from set-compatibility nil))
  ;;
  ;;  Process the second relation
  ;;
  (multiple-value-setq (relb attributesb domainsb)
    (process-set-relation relb attrsb))
  (if (null relb)
      (return-from set-compatibility nil))
  ;;
  ;;  Do they have the same number of attributes ?
  ;;
  (cond ((null (equal (length attributesa) (length attributesb)))
 (if *provide-error-messages*
     (format *standard-output*
     "~%ERROR - Relations ~S and ~S do not have the same number of attributes, thus they are not compatible."
     rela relb))
 (return-from set-compatibility nil)))
  (mapc (function (lambda (attra attrb doma domb)
    (cond ((null (equal doma domb))
   (if *provide-error-messages*
       (format *standard-output*
        "~%ERROR - Attribute ~s of relation ~S and attribute ~s of relation ~S are not compatible domains"
        attra rela attrb relb))
   (return-from set-compatibility nil)))))
attributesa attributesb domainsa domainsb)
  (return-from set-compatibility t)))

(defun set-create-relc (rela relc keyword-list attrsa doc
&aux attribute-descriptor attributes dir dom imp key modp  qtrieve-var ss tuple-format)
  (setf qtrieve-var (car (qtrieve 'system-relation *system-relation-attributes*
   '("MODIFIEDP" "SAVE-DIRECTORY" "ATTRIBUTES" "IMPLEMENTATION-TYPE"
     "STORAGE-STRUCTURE" "KEY" "TUPLE-FORMAT" "DOC" "DOMAINS")
   *system-relation-key*
   (list 'string-equal 'relation-name (string-upcase rela)))))
  (setf modp (first qtrieve-var)
dir (or (car (get-keyword-value '(dir) keyword-list)) (second qtrieve-var))
attributes (third qtrieve-var)
imp (or (car (get-keyword-value '(imp) keyword-list)) (fourth qtrieve-var))
ss (or (car (get-keyword-value '(sto) keyword-list)) (fifth qtrieve-var) )
doc (or (car (get-keyword-value '(doc) keyword-list)) doc)
key (car (get-keyword-value '(key) keyword-list))
tuple-format (or (car (get-keyword-value '(format) keyword-list)) (nth 6 qtrieve-var))
dom (nth 8 qtrieve-var))
  (cond (attrsa
 (if (not (listp attrsa))
     (setf attrsa (list attrsa)))
 (setf dom (mapcar #'(lambda (attr)
       (nth (- (length attributes) (length (member attr attributes
         :test 'string-equal))) dom))
   attrsa))
 (setf attributes attrsa)))
  (do ((dom dom (cdr dom))
       (attributes attributes (cdr attributes)))
      ((null attributes) t)
    (setf attribute-descriptor (append attribute-descriptor (list (car attributes) (list 'dom (car dom))))))
  (defrel relc attribute-descriptor (list 'imp imp 'sto ss 'key key 'format tuple-format 'doc doc 'dir dir)))

(defun relation-difference (&rest keyword-list
    &key &optional from into directory documentation format implementation-type
    storage-structure key print tuples unique
    &allow-other-keys
    &aux tempa temp-tuples temp table rela relb relc print1 tuples1 attrsa wherea
    attrsb whereb attrsc reader-package)
  "Difference of the tuples in two relations.

   FROM                 - This clause specifies the relations to participate in the DIFFERENCE operation.
                          In addition, RTMS allows users to specify the attributes in these relations to
                          participate in the operation as well as a where-clause to specify the tuples.
                          It should be of the format: (RelA [(PROJECT <attrA> WHERE where-clause-A)] RelB
                          [(PROJECT <attrB> WHERE where-clause-B)]) where attrA indicates attributes in
                          relation relA and where-clause-A indicates a where-clause involving the attributes
                          in relation relA.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   PRINT                - If NIL, the resultant relation will not be printed out.
   TUPLES               - If T, the resultant tuples will be returned.
   UNIQUE               - If T, only unique tuples will be part of the resultant relation."
   from into directory documentation format unique implementation-type storage-structure key print tuples
  (block relation-difference
(if (not (active-databasep))
    (return-from relation-difference nil))
  (multiple-value-setq (rela attrsa wherea relb attrsb whereb relc attrsc tuples1 reader-package print1)
  (validate-relation-ops keyword-list "DIFFERENCE"))
  (if (not rela)
      (return-from relation-difference nil))
  (setf table (make-hash-table :test 'equal))
  (setf temp (retrieve (find-symbol (string-upcase relb)) 'project attrsb 'tuples t
       'unique (car (get-keyword-value '(unique) keyword-list)) 'where whereb))
  (mapc #'(lambda (tuple)
    (puthash tuple (cons tuple (gethash tuple table)) table))
temp)
  (mapc (function (lambda (tuplea)
    (cond ((not (gethash tuplea table))
   ;;This tuple is in relA but not in relB
   (setf temp-tuples (cons tuplea temp-tuples))))))
(retrieve (find-symbol (string-upcase rela)) 'tuples t 'project attrsa
  'unique (car (get-keyword-value '(unique) keyword-list)) 'where wherea))
  (cond ((and temp-tuples relc)
 (setf tempa *validity-checking*)
 (setf *validity-checking* nil)
 (insert relc 'tuples temp-tuples 'attributes attrsc)
 (setf *validity-checking* tempa)))
  (clrhash table)
  (cond (tuples1 (return-from relation-difference temp-tuples))
((and print1 relc)
 (setf tempa (cadr (get-relation relc '("ATTRIBUTES" "TUPLE-FORMAT") nil)))
 (printrel-internal* relc temp-tuples (unconvert-attributes (or attrsc (car tempa)) reader-package)
     nil nil nil nil (cadr tempa) t t (length temp-tuples))
 (return-from relation-difference relc))
(print1
 (setf tempa (cadr (get-relation rela '("ATTRIBUTES" "TUPLE-FORMAT") nil)))
 (printrel-internal* 'difference temp-tuples
     (unconvert-attributes (or attrsa (car tempa)) reader-package)
     nil nil nil nil (or (car (get-keyword-value '(format) keyword-list))
    (if attrsa
        (car (project-list (cdr tempa) (car tempa) attrsa))
        (cadr tempa)))
     t t (length temp-tuples))
(return-from relation-difference t)))
  (return-from relation-difference (or relc rela))))

(defun relation-intersection (&rest keyword-list
      &key &optional from into directory documentation format implementation-type
      storage-structure key print tuples unique
      &allow-other-keys
      &aux tempa (tempb nil) temp table rela relb relc tuples1 print1 attrsa wherea
      attrsb whereb attrsc reader-package)
   "Intersection of tuples in two relations.

   FROM                 - This clause specifies the relations to participate in the INTERSECTION operation.
                          In addition, RTMS allows users to specify the attributes in these relations to
                          participate in the operation as well as a where-clause to specify the tuples.
                          It should be of the format: (RelA [(PROJECT <attrA> WHERE where-clause-A)] RelB
                          [(PROJECT <attrB> WHERE where-clause-B)]) where attrA indicates attributes in
                          relation relA and where-clause-A indicates a where-clause involving the attributes
                          in relation relA.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   PRINT                - If NIL, the resultant relation will not be printed out.
   TUPLES               - If T, the resultant tuples will be returned.
   UNIQUE               - If T, only unique tuples will be part of the resultant relation."
   from into directory documentation format implementation-type storage-structure key print tuples unique
  (block relation-intersection
(if (not (active-databasep))
    (return-from relation-intersection nil))
  (multiple-value-setq (rela attrsa wherea relb attrsb whereb relc attrsc tuples1 reader-package print1)
  (validate-relation-ops keyword-list "DIFFERENCE"))
  (if (not rela)
      (return-from relation-intersection nil))
  (setf temp (retrieve rela 'tuples t 'project attrsa 'where wherea))
  (setf table (make-hash-table :test 'equal))
  (mapc #'(lambda (tuple)
    (puthash tuple (cons tuple (gethash tuple table)) table))
temp)
  (mapc (function (lambda (tupleb)
    (cond ((gethash tupleb table)
   ;;This tuple is in both relA and relB
   (setf tempb (cons tupleb tempb))))))
(retrieve (find-symbol (string-upcase relb)) 'tuples t
  'unique (car (get-keyword-value '(unique) keyword-list)) 'where whereb 'project attrsb))
  (cond ((and tempb relc)
 (setf tempa *validity-checking*)
 (setf *validity-checking* nil)
 (insert relc 'tuples tempb 'attributes attrsc)
 (setf *validity-checking* tempa)))
  (clrhash table)
  (cond (tuples1 (return-from relation-intersection tempb))
((and print1 relc)
 (setf tempa (cadr (get-relation relc '("ATTRIBUTES" "TUPLE-FORMAT") nil)))
 (printrel-internal* relc tempb (unconvert-attributes (or attrsc (car tempa)) reader-package)
     nil nil nil nil (cadr tempa) t t (length tempb))
 (return-from relation-intersection relc))
(print1
 (setf tempa (cadr (get-relation rela '("ATTRIBUTES" "TUPLE-FORMAT") nil)))
 (printrel-internal* 'intersection tempb (unconvert-attributes (or attrsa (car tempa)) reader-package)
     nil nil nil nil (or (car (get-keyword-value '(format) keyword-list))
    (if attrsa
        (car (project-list (cdr tempa) (car tempa) attrsa))
        (cadr tempa)))
     t t (length tempb))
 (return-from relation-intersection t)))
  (return-from relation-intersection (or relc rela))))

(defun relation-union (&rest keyword-list
       &key &optional from into directory documentation format implementation-type
       storage-structure key print tuples unique
       &allow-other-keys
       &aux tempa rela relb relc table tuples1 print1 temp temp-union attrsa wherea attrsb
       whereb attrsc reader-package)
   "Union of tuples in two relations.

   FROM                 - This clause specifies the relations to participate in the UNION operation.
                          In addition, RTMS allows users to specify the attributes in these relations to
                          participate in the operation as well as a where-clause to specify the tuples.
                          It should be of the format: (RelA [(PROJECT <attrA> WHERE where-clause-A)] RelB
                          [(PROJECT <attrB> WHERE where-clause-B)]) where attrA indicates attributes in
                          relation relA and where-clause-A indicates a where-clause involving the attributes
                          in relation relA.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   PRINT                - If NIL, the resultant relation will not be printed out.
   TUPLES               - If T, the resultant tuples will be returned.
   UNIQUE               - If T, only unique tuples will be part of the resultant relation."
   from into directory documentation format implementation-type storage-structure key print tuples unique
  (block relation-union
(if (not (active-databasep))
    (return-from relation-union nil))
  (multiple-value-setq (rela attrsa wherea relb attrsb whereb relc attrsc tuples1 reader-package print1)
  (validate-relation-ops keyword-list "DIFFERENCE"))
  (if (not rela)
      (return-from relation-union nil))
  (setf table (make-hash-table :test 'equal))
  (setf temp (retrieve (find-symbol (string-upcase rela))
       'unique (car (get-keyword-value '(unique) keyword-list)) 'project attrsa
       'where wherea 'tuples t))
  (mapc #'(lambda (tuple)
    (puthash tuple (cons tuple (gethash tuple table)) table))
temp)
  (if relc
      (insert relc 'tuples temp 'attributes attrsc))
  (setf temp-union nil)
  (mapc (function (lambda (tuple)
    ;;See if this tuple exists in RELA.
    (cond ((null (gethash tuple table))
   (setf temp-union (cons tuple temp-union))))))
(retrieve (find-symbol (string-upcase relb)) 'tuples t
  'unique (car (get-keyword-value '(unique) keyword-list)) 'project attrsb 'where whereb))
  (cond ((and temp-union relc)
 (setf tempa *validity-checking*)
 (setf *validity-checking* nil)
 (insert relc 'tuples temp-union 'attributes attrsc)
 (setf *validity-checking* tempa)))
  (clrhash table)
  (cond (tuples1 (return-from relation-union (append temp temp-union)))
((and print1 relc)
 ;;Since we don't have all tuples it makes sense to call RETRIEVE.
 (retrieve (find-symbol (string-upcase relc)) 'project attrsc)
 (return-from relation-union relc))
(print1
 (setf tempa (cadr (get-relation rela '("ATTRIBUTES" "TUPLE-FORMAT") nil)))
 (printrel-internal* 'union (append temp temp-union)
     (unconvert-attributes (or attrsa (car tempa)) reader-package)
     nil nil nil nil (or (car (get-keyword-value '(format) keyword-list))
    (if attrsa
        (car (project-list (cdr tempa) (car tempa) attrsa))
      (cadr tempa)))
     t t (length (append temp temp-union)))
 (return-from relation-union t)))
  (return-from relation-union (or relc rela))))

(defun validate-where (where-clause rela-name relb-name attsa attsb
       &aux test-attribute test-relation)
  (block validate-where
  (cond ((null where-clause)
 (return-from validate-where t))
((equal where-clause t)
 (return-from validate-where t))
((or (> (length where-clause) 3) (listp (first where-clause)) (listp (second where-clause))
     (listp (third where-clause)))
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - ~s improperly formed where subclause" where-clause))
 (return-from validate-where nil))
(t
 (setf test-relation (remove-dot-rel (second where-clause)))
 (setf test-attribute (remove-dot-attr (second where-clause)))
 (cond ((null test-relation)
(cond ((not (or (member test-attribute attsa :test 'string-equal)
 (member test-attribute attsb :test 'string-equal)))
       (if *provide-error-messages*
   (format *standard-output* "~%ERROR - ~s is an unrecognized attribute"
    test-attribute))
       (return-from validate-where nil))))
       ((equal rela-name test-relation)
(cond ((null (member test-attribute attsa :test 'string-equal))
       (if *provide-error-messages*
   (format *standard-output* "~%ERROR -~s is an unrecognized attribute"
    (second where-clause)))
       (return-from validate-where nil))))
       ((equal relb-name test-relation)
(cond ((null (member test-attribute attsb :test 'string-equal))
       (if *provide-error-messages*
   (format *standard-output* "~%ERROR - ~s is an unrecognized attribute"
    (second where-clause)))
       (return-from validate-where nil))))
       (t
(if *provide-error-messages*
    (format *standard-output* "~%ERROR - ~s is not a relation in the ~s database"
    test-relation *active-db*))
(return-from validate-where nil)))
 (setf test-relation (remove-dot-rel (third where-clause)))
 (setf test-attribute (remove-dot-attr (third where-clause)))
 (cond ((null test-relation)
(cond ((not (or (member test-attribute attsa :test 'string-equal)
 (member test-attribute attsb :test 'string-equal)))
       (if *provide-error-messages*
   (format *standard-output* "~%ERROR - ~s is an unrecognized attribute"
    test-attribute))
       (return-from validate-where nil))))
       ((equal rela-name test-relation)
(cond ((null (member test-attribute attsa :test 'string-equal))
       (if *provide-error-messages*
   (format *standard-output* "~%ERROR - ~s is an unrecognized attribute"
    (third where-clause)))
       (return-from validate-where nil))
      (t
       (return-from validate-where t))))
       ((equal relb-name test-relation)
(cond ((null (member test-attribute attsb :test 'string-equal))
       (if *provide-error-messages*
   (format *standard-output* "~%ERROR - ~s is an unrecognized attribute"
    (third where-clause)))
       (return-from validate-where nil))
      (t
       (return-from validate-where t))))
       (t
(if *provide-error-messages*
    (format *standard-output* "~%ERROR - ~s is not a relation in the ~s database"
    (remove-dot-rel (third where-clause)) *active-db*))
(return-from validate-where nil)))))))

(defun pre-relation-ops (keyword-list
 &aux temp rela relb relc tuples1 attrsa wherea attrsb whereb attrsc var reader-package)
  ;;
  ;;  Obtain the names of the relations which will be involved in the operation
  ;;
  (if (and (setf temp (car (get-keyword-value '(from) keyword-list))) (not (listp temp)))
      (setf temp (list temp)))
  (setf rela (validate-sym (first temp)))
  (cond (rela
 (setf reader-package (package-name (symbol-package rela)))
 ;;
 ;;  A project list and a where clause may be specified in the from clause. This allows the relation operation to be performed on
 ;; a subset of the each relation.
 ;;
(if (and (listp (setf var (second temp))) (> (length var) 1))
    (setf wherea (car (get-keyword-value '(where) var))
  attrsa (convert-attributes (car (get-keyword-value '(project) var)))
  temp (append (list rela) (nthcdr 2 temp))))))
  (setf relb (validate-sym (second temp)))
  (cond (relb
 (setf reader-package (package-name (symbol-package relb)))
 (if (and (listp (setf var (third temp))) (> (length var) 1))
     (setf whereb (car (get-keyword-value '(where) var))
   attrsb (convert-attributes (car (get-keyword-value '(project) var)))))))
  ;;
  ;;  The relations which will participate have been obtained, process the rest of the keywords.
  ;;
  (setf relc (car (get-keyword-value '(into) keyword-list))
tuples1 (car (get-keyword-value '(tuples) keyword-list)))
  (if (and (listp relc) (listp (setf var (second relc))) (> (length var) 1))
      (setf attrsc (convert-attributes (car (get-keyword-value '(project) var)))))
  (setf relc (validate-sym (if (listp relc) (first relc) relc)))
  (values rela attrsa wherea relb attrsb whereb relc attrsc tuples1 reader-package))

(defun parse-from-clause (from-clause jrela jrelb join-attrc a-join-attrc unknown-attributes-user
  rela-attributes-user relb-attributes-user
  &aux period-index mystery-relation-name)
  (mapc (function (lambda (attr &aux temp1)
(cond ((listp attr)
       ;;
       ;;  The attribute was specified in a list, this probably means that the user has supplied an new name for this attribute.
       ;; Use the new attribute-name if it is provided, else the name remains the same
       ;;
       (setf join-attrc (append join-attrc (if (cadr attr)
          (list (car attr))
            attr)))
       (setf a-join-attrc (append a-join-attrc (list (cadr attr))))
       (cond ((setf period-index (POSITION #\. (string (cadr attr))))
      ;;
      ;;  Set the relation name if both have not already been found
      ;;
      (setf temp1 (remove-dot-rel (cadr attr)))
      (if jrela
  (if (and (null jrelb)
    (null (string-equal (string-upcase jrela) (string-upcase temp1))))
      (setf jrelb (validate-sym temp1)))
  (setf jrela (string-upcase (remove-dot-rel (validate-sym (cadr attr) t))))))))
      ;;
      ;; The attribute name was not a list, process it
      ;;
      (t
       (cond ((setf period-index (POSITION #\. (string attr)))
      ;;
      ;;  Set the relation name if both have not already been found
      ;;
      (setf temp1 (remove-dot-rel attr))
      (if jrela
  (if (and (null jrelb)
    (null (string-equal (string-upcase jrela) (string-upcase temp1))))
      (setf jrelb (validate-sym temp1)))
  (setf jrela (string-upcase (remove-dot-rel (validate-sym attr t)))))
      ;;
      ;;  Add attribute to the attribute lists
      ;;
      (setf join-attrc (append join-attrc (list attr)))
      (setf a-join-attrc (append a-join-attrc (list attr))))
     (t
      (setf join-attrc (append join-attrc (list attr)))
      (setf a-join-attrc (append a-join-attrc (list attr)))
      (setf unknown-attributes-user (append unknown-attributes-user (list attr)))))))
;;
;;  Determine, if possible, which relation the user has said that this relation belongs.
;;
  (if (listp attr)
      (setf attr (cadr attr)))
  (cond ((and period-index
      (not (string-equal (subseq (string attr) (+ period-index 1) (+ period-index 2)) "*")))
 (setf mystery-relation-name (read-from-string (subseq (string attr) 0 period-index)))
 (cond ((string-equal mystery-relation-name jrela)
(setf rela-attributes-user (append rela-attributes-user (list (remove-dot-attr attr)))))
       ((string-equal mystery-relation-name jrelb)
(setf relb-attributes-user (append relb-attributes-user (list (remove-dot-attr attr)))))
       (t
(setf unknown-attributes-user (append unknown-attributes-user
          (list (remove-dot-attr attr)))))))
(period-index)
(t
 (setf unknown-attributes-user (append unknown-attributes-user (list attr)))))))
from-clause)
  (values from-clause jrela jrelb join-attrc a-join-attrc unknown-attributes-user rela-attributes-user
  relb-attributes-user))

(defun parse-join-attributes (join-attrc a-join-attrc jrelb-project attrsa attrsb jrela jrelb
      &aux temp)
  (setf temp join-attrc
join-attrc nil)
  (mapc (function (lambda (attr)
     (cond ((and (or (symbolp attr) (stringp attr))
 (string-equal (string-upcase attr) (string-upcase (add-dot jrela "*"))))
    (setf join-attrc (append join-attrc attrsa)))
   ((and (or (symbolp attr) (stringp attr))
 (string-equal (string-upcase attr) (string-upcase (add-dot jrelb "*"))))
    (setf join-attrc (append join-attrc attrsb)))
   ;;
   ;; I am not sure if the following two clauses are necessary. But I think they are useful in situations like,
   ;; (rel1.* rel2.s4) where s4 is not an attribute of rel1 and we want the output relation to contain s4 not rel2.s4.
   ;;
   ((and (POSITION #\. (string attr)) (string-equal jrela (remove-dot-rel attr)))
    (setf join-attrc (append join-attrc (list (string-upcase (remove-dot-attr attr))))))
   ((and (POSITION #\. (string attr)) (string-equal jrelb (remove-dot-rel attr)))
    (setf join-attrc (append join-attrc (list (string-upcase (remove-dot-attr attr))))))
   (t
    (setf join-attrc (append join-attrc (list attr)))))))
  temp)
  ;;
  ;;  Form an attribute list for the final relation with the relation names appended to each attribute name. These will be the attribute
  ;; names as they orginally appeared in the join relations.
  ;;
  (setf temp (convert-attributes a-join-attrc)
a-join-attrc nil
jrelb-project nil)
  (cond ((and (string-equal (string-upcase jrela) (string-upcase jrelb)) ;self join
      (equal temp (convert-attributes (list (add-dot jrela "*") (add-dot jrelb "*")))))
 (setf jrelb-project attrsa)))
  (mapc (function (lambda (attr)
    (cond ((equal attr (string-upcase (add-dot jrela "*")))
   (setf a-join-attrc (append a-join-attrc
         (mapcar (function (lambda (attr1)
        (string-upcase
          (add-dot jrela attr1))))
          attrsa))))
  ((equal attr (string-upcase (add-dot jrelb "*")))
   (setf a-join-attrc (append a-join-attrc
         (mapcar (function (lambda (attr2)
        (string-upcase
          (add-dot jrelb attr2))))
          attrsb)))
   (setf jrelb-project attrsb))
  (t
   (cond ((POSITION #\. (string attr) :test 'string-equal)
   (cond ((member (string-upcase (remove-dot-attr attr)) attrsb
    :test 'string-equal)
   (setf jrelb-project (append jrelb-project
          (list (string-upcase
           (remove-dot-attr attr)))))))
   (setf a-join-attrc (append a-join-attrc (list attr))))
  (t
   (cond ((and (member (string-upcase attr) attrsa :test 'string-equal)
        (null (member (add-dot jrela attr) a-join-attrc
        :test 'string-equal)))
   (setf a-join-attrc (append a-join-attrc
         (list (string-upcase
          (add-dot jrela attr))))))
  (t
   (setf jrelb-project (append jrelb-project (list (string-upcase attr))))
   (setf a-join-attrc (append a-join-attrc
         (list (string-upcase
          (add-dot jrelb attr)))))))))))))
temp)
  (values join-attrc a-join-attrc jrelb-project))

(defun join-into (jrelc join-attrc a-join-attrc keyword-list jrelc-format impa ssa attr-imp
  &aux temp)
  (setf attr-imp (get-relation jrelc '("ATTRIBUTES" "IMPLEMENTATION-TYPE" "STORAGE-STRUCTURE" "KEY"
        "CARDINALITY" "TUPLE-FORMAT")
     nil))
  (if (null (cadr attr-imp))
      (progn

;;Form the attribute descriptor pair. Consider one attribute at a time and get the descriptor values from the system-attribute
;;relation. Right now the resultant key is the list of all resultant attributes. In the following MAPCAR, a-attr is of the form rela.a1
;; and attr is the actual attribute in the output relation.
(setf temp join-attrc
      join-attrc nil)
(mapc (function (lambda (a-attr attr &aux attd)
       (setf attd (car (qtrieve 'system-attribute *system-attribute-attributes*
    '("DOMAIN-FUNCTION" "DEFAULT-VALUE" "DOC")
    *system-attribute-key*
        (list 'and (list 'string-equal 'relation-name
    (string (remove-dot-rel a-attr)))
       (list 'string-equal 'attribute-name
      (string (remove-dot-attr a-attr)))))))
       (setf join-attrc (append join-attrc
    (list attr (list 'dom (first attd) 'def (second attd)
       'doc (third attd)))))))
a-join-attrc temp)
(cond ((null (defrel jrelc join-attrc
       (list 'imp (or (car (get-keyword-value '(imp) keyword-list)) impa)
     'sto (or (car (get-keyword-value '(sto) keyword-list)) ssa)
     'key (car (get-keyword-value '(key) keyword-list))
     'format (or (car (get-keyword-value '(format) keyword-list)) jrelc-format)
     'dir (car (get-keyword-value '(dir) keyword-list))
     'doc (or (car (get-keyword-value '(doc) keyword-list)) "..."))))
       (values nil attr-imp))
      (t
       (values jrelc attr-imp))))
    (values (setf jrelc (car attr-imp)) attr-imp)))

(defun join-eval (jrela jrelb attrsa attrsb jrelb-project jrelb-key where jrelb-storage-structure
  jrelb-implementation-type reader-package
  &aux key-value tupleb retrieve-function key-function tuples index-name attr1 attr2 num1 num2
  table table1 temp1 temp join-insert-list)
   (setf join-insert-list nil)
  (multiple-value-setq (index-name key-value jrelb-storage-structure jrelb-key)
    (extract-key jrelb attrsb jrelb-key nil jrelb-storage-structure where nil))
   (cond ((equal where t)
  (setf tupleb (funcall (find-symbol (concatenate 'string "RETRIEVE-" jrelb-implementation-type "-"
        jrelb-storage-structure) *pkg-string*)
 (find-symbol (string-upcase jrelb))
 attrsb jrelb-project jrelb-key where key-value
 (find-symbol (string-upcase jrelb))))))
   (setf retrieve-function (find-symbol (concatenate 'string "RETRIEVE-" jrelb-implementation-type "-"
          jrelb-storage-structure) *pkg-string*)
 key-function (find-symbol (concatenate 'string "EXTRACT-KEY-" jrelb-storage-structure) *pkg-string*))
   (cond ((and (listp where)
       (equal (first where) 'equal)
       (or (and (member (setf attr1 (second where)) attrsa :test 'string-equal)
(member (setf attr2 (third where)) attrsb :test 'string-equal))
   (and (member (setf attr2 (second where)) attrsb :test 'string-equal)
(member (setf attr1 (third where)) attrsa :test 'string-equal))))
  (setf num1 (- (length attrsa) (length (member attr1 attrsa :test 'string-equal))))
  (setf num2 (- (length attrsb) (length (member attr2 attrsb :test 'string-equal))))
  (setf table (make-hash-table :test 'equal)
table1 (make-hash-table :test 'equal))
  (mapc #'(lambda (tuple &aux (key (nth num1 tuple)))
    (puthash key (cons tuple (gethash key table)) table))
(getp jrela 'entry-point))
  (mapc #'(lambda (tuple &aux (key (nth num2 tuple)) (temp nil))
    (cond ((setf temp1 (gethash key table))
   (mapc #'(lambda (temp-t)
      (setf temp (cons (append tuple temp-t) temp)))
  temp1)
   (puthash key (append temp (gethash key table1)) table1))))
(getp jrelb 'entry-point))
  (maphash #'(lambda (key tuple)
       key
        (setf join-insert-list (append tuple join-insert-list)))
   table1)
  (clrhash table)
  (clrhash table1))
 (t
  (mapt (function (lambda (tuplea)
    (cond ((equal where t)
    (setf tuples nil)
    (mapc (function (lambda (tuple)
        (setf tuples (cons (append tuplea tuple) tuples))))
   tupleb))
   (t
    (setf temp where)
    (cond ((equal temp '(t)) (setf temp t))
   (t
    (setf temp (process-where tuplea attrsa temp jrela jrelb attrsb))))
    (setf key-value (funcall key-function attrsb jrelb-key nil temp
        reader-package))
    (setf tuples nil)
    ;;
    ;;  For now it projects all of the attributes from jrelB, it should only project those which are
    ;; needed
    ;;
    (mapc (function (lambda (tupleb)
        (setf tuples (cons (append tuplea tupleb) tuples))))
   (funcall retrieve-function (find-symbol (string-upcase jrelb))
     attrsb jrelb-project jrelb-key temp key-value
     (find-symbol (string-upcase index-name))))))
    (if tuples
 (setf join-insert-list (append tuples join-insert-list)))))
(read-from-string (string-upcase jrela)))))
   join-insert-list)

(defun validate-relation-ops (keyword-list operation
      &aux attrsa attrsb attrsc print1 reader-package rel-exists? rela relb relc tempa
      tuples1 wherea whereb)
  (block validate-relation-ops
  (setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
 ((or (null keyword-list)(not (listp (car keyword-list)))) keyword-list)))
  (setf print1 (member 'print keyword-list))
  (if print1
      (setf print1 (cadr print1)))
  (setf keyword-list (get-keyword-value-prereq '(into from format dir doc key imp sto print tuples unique)
       keyword-list))
  ;;
  ;;  Return the attributes which are to participate in the difference operation as well as the selection critera for the tuples which will be
  ;; involved. The relation names and the attribute lists return-from validate-relation-ops stringified.
  ;;
  (multiple-value-setq (rela attrsa wherea relb attrsb whereb relc attrsc tuples1 reader-package)
    (pre-relation-ops keyword-list))
  (if (and (null tuples1) (null relc) (null print1) (null (member 'print  keyword-list)))
      (setf print1 t))
  ;;
  ;;  If two relations are not specified, generate an error message and exit
  ;;
  (cond ((or (null rela) (null relb))
 (if *provide-error-messages*
     (format *standard-output*
     "~%ERROR - Relations to participate in relational set operation have not been provided."))
 (return-from validate-relation-ops nil)))
  ;;
  ;;  Determine if the relations specified and the attributes specified are compatible for performing the difference operation
  ;;
  (if (null (set-compatibility rela relb attrsa attrsb))
      (return-from validate-relation-ops nil))
  (cond (relc
 (setf tempa  (get-relation relc '(attributes) nil))
 (setf relc (car tempa))
 ;;
 ;;  Relation C does not exist, create it.
 ;;
 (cond ((null (setf rel-exists? (caadr tempa)))
(if (not (set-create-relc rela relc keyword-list attrsa
    (format nil "Relation ~s of ~S and ~S" operation rela relb)))
    (return-from validate-relation-ops nil)))
       ;;
       ;;  Relation C already exists in the database, is it compatible with the other relations
       ;;
       (t
(if (null (set-compatibility rela relc attrsa attrsc))
    (return-from validate-relation-ops nil))))))
  (values rela attrsa wherea relb attrsb whereb relc attrsc tuples1 reader-package print1)))
 destroyed.
  RENAME-ATTRIBUTES    - List of list of OLD-NEW attribute names.
  IMPLEMENTATION-TYPE  - Name of the new implementation type.
  STORAGE-STRUCTURE    - Name of the new storage-structure.
  FORMAT               - List of new print-width values to be used for the attributes.
  KEY                  - List of attributes to form the new key for this relation.
  DOCUMENTATION        - New description of this relation.
  DIRECTORY            - New directory in which this relation is to be saved.  (MODIFY-RELATION RELATION &REST KEYWORD-LIST &KEY &OPTIONAL RELATION-NAME ADD-ATTRIBUTES DELETE-ATTRIBUTES RENAME-ATTRIBUTES IMPLEMENTATION-TYPE STORAGE-STRUCTURE FORMAT KEY DOCUMENTATION DIRECTORY &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄé	¿BÄ†¿√ÅADD-ATTRIBUTES¿CÇDELETE-ATTRIBUTESÄ¿CÇRENAME-ATTRIBUTESÄ¿BÄ£¿BÄ¨¿BÄﬂ¿BÄ¢¿BÄ°¿BÄ†¿BÄ≠“BÄﬂ“BÄé	íPA¡Pˇ€PPÅQPÇQPÉQ	PÑQ
PÖQPÜQPáQPàQP QPãQPäQJ∫@√ööAëÅQ@QîOÄ§	BÄ
	Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540804. :SYSTEM-TYPE :LOGICAL :VERSION 2. :TYPE "XLD" :NAME "RELATION-OPS" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760360567. :AUTHOR "REL3" :LENGTH-IN-BYTES 12012. :LENGTH-IN-BLOCKS 24. :BYTE-SIZE 16.)                               pp2Ä\Ä©ÅCOMPILE-DATA\ÄÏÄSW-MFG,ÅGODZILLAÜÄá§á–FÄFÄ\Äp¿,ÅCOMPILER,ÅVERSIONÄ\ÄFÄFÄp¿BÄ),ÇOPTIMIZE-SWITCHÄÜÄ©ÉQFASL-SOURCE-FILE-UNIQUE-IDÄ1Ä\Äp¿lÄFSÏÇMAKE-FASLOAD-PATHNAMEÄ\ÄÍÄQUOTEÄBÄ$\ÄBÄ8™ÄNILÄ\ÄBÄ8\Ä¨ÄRTMS\ÄBÄ8¨ÅRELATION-OPS\ÄBÄ8¨ÄLISP\ÄBÄ8FÄ©ÄBASEFÄ
ÈÄFONTSÄ\Ä©Å*CODE-FONT*ÄÈÅ*COMMENT-FONT*ÈÅ*STRING-FONT*Ä)ÅPACKAGEÄ©ÄRTMS©ÄMODE©ÅCOMMON-LISPÄÄ√ÅJOIN-INTERNALÄÄÎÄQÅSÜÄ‡Q@FÄ“¿$Ä¿BÄ:p¿¨ÄTICLÏÄART-QÄ]ÄFÄÄ:BÄ:BÄ:jÄTÄFÄp¿¨ÄSYSÄlÇDEBUG-INFO-STRUCTÄBÄP\ÄÉÅKEYWORD-LISTBÄ:\Ä!ÉÇRELA-ATTRIBUTES-USERÉÇRELB-ATTRIBUTES-USERÉUNKNOWN-ATTRIBUTES-USERÄÍÄPRINTÄCÅALL-ATTRSÄÉÅFROM-CLAUSEÄ√ÅJRELB-PROJECTÄÉÅJRELA-FORMATÉÅJRELB-FORMATÉÅJRELC-FORMATÉÄTEMPCÅJOIN-ATTRCÇJOIN-INSERT-LIST√ÄWHEREÄ√ÄATTRSA√ÄATTRSBÉÅA-JOIN-ATTRC√ÅREADER-PACKAGE√ÄJRELAÄ√ÄJRELBÄ√ÄJRELCÄÉÄIMPAÉÄSSAÄÅATTR-IMPCÉJRELB-IMPLEMENTATION-TYPEÄÉJRELB-STORAGE-STRUCTUREÄCÅJRELB-KEYÄBÄ`BÄ:BÄ:CÅATTRIBUTEÄBÄ:BÄ:\Ä)ÇMACROS-EXPANDEDÄ\Äp¿BÄTÏÇINHIBIT-STYLE-WARNINGSp¿¨ÄZLCÄ,ÅDO-NAMEDÍÄFOURTHÍÄFIFTHÄÍÄSIXTHÄÍÄTHIRDÄÍÄSECONDÍÄFIRSTÄp¿BÄTlÅCOND-EVERY™ÄPROG™ÄSETF¿ÜÄ° ÄÉÇ*DEFAULT-ANYP-WIDTH*—ÉÅ*PKG-STRING*—CÉ*PROVIDE-WARNING-MESSAGES*—É*PROVIDE-ERROR-MESSAGES*ëÇACTIVE-DATABASEÄ“ÍÄTERPRI“¨ÑERROR - No parameters passed to JOIN¿™ÅWRITE-STRING“BÄe¿\ÄÉÄINTOBÄoÉÄFROMÉÄIMPÄÉÄSTOÄÉÄKEYÄÍÄFORMATÉÄDIRÄÉÄDOCÄBÄe√ÄTUPLESÅPROJECTÄ√ÄUNIQUE¿ÉGET-KEYWORD-VALUE-PREREQ“\ÄBÄö¿CÇGET-KEYWORD-VALUEÄ“\ÄBÄo¿\ÄBÄ£¿\ÄBÄ¢¿\ÄBÄõ¿™ÄLIST“ÉÅVALIDATE-SYM“ÏãWARNING - More than two relations are provided for joining. The first two will be considered.Ä¿¨ÜERROR - The relations to be joined are not provided.¿lÄ*Ä¿ÅADD-DOTÄ“™ÅPACKAGE-NAME“ÜÄ¿CÇPARSE-FROM-CLAUSEÄ“ÏÄGLOBAL¿p¿BÄ\ÏÅSTRING-EQUAL*Ä“√ÄGLOBAL¿ÏàERROR - The FROM clause has not specified the relations to be joined.Ä¿\ÄlÅATTRIBUTES¨ÇIMPLEMENTATION-TYPEÄlÇSTORAGE-STRUCTUREÄ¨ÄKEYÄ¨ÅTUPLE-FORMAT¨ÅCARDINALITYÄ¿ÉÅGET-RELATION“lÇERROR - Relation Ä¿ÍÄPRIN1Ä“,Ç does not existÄ¿ÍÅSTRING-UPCASEÄ“™ÅFIND-SYMBOLÄ“CÇCONVERT-ATTRIBUTES“™ÅSTRING-EQUAL¿p¿BÄ\¨ÅMEMBER-TESTÄ“,ÅERROR - ¿lÑ is not an attribute of relation Ä¿,Ö is not an attribute of either relationÄ¿¨Ñ is an attribute of both relations: ¿ÏÄ and Ä¿eÄ.¿jÅWRITE-CHAR“¨Ü        It is unclear which attribute should be used¿FÄ–¿√ÇPARSE-JOIN-ATTRIBUTESÄ“p¿BÄ\,Å*APPENDÄ“*ÅREVERSEÄ“ÍÄSTRING¿lÄ.Ä¿™ÅCONCATENATEÄ“ÉÅPROJECT-LIST“FÄê¿CÅJOIN-INTOÄ“CÅJOIN-EVALÄ“ÇREMOVE-DOT-ATTRÄ“\ÄBÄ§¿√ÅUNIQUE-TUPLESÄ“,ÅINSERT-Ä¿lÄ-Ä¿\ÄBÄú¿\ÄBÄù¿p¿BÄTÏÅSTRING-APPENDÄ“ÇSYSTEM-RELATIONÄ¿√ÅRELATION-NAMEÄ¿BÄÿ“\ÄlÅMODIFIEDPÄ¨ÅCARDINALITYÄ¿ÇDELETE-OR-MODIFY“,ÇSYSTEM-RELATIONÄ¿\ÄlÅMODIFIEDPÄ¿\ÄBÄü¿CÇPRINTREL-INTERNAL*“ÉÄJOINÄÄÊRÄQ¸[S[¡‰[Sˇ5˙Á[QÄ¡‰ÄÊ‰	Ä
PàRR€K€P€S€PÄQãCC¡‰CWC¡PÄQíÄ¡PÄQíBT¡PÄQíB‚ˇ›M¡PÄQíBE¡PÄQíÊTÊCÊPÄÊC›PÄQíBJ¡‰J5ÊJQäJ¡J‰JSäR¡ÊR]€\›J‰JWäS¡ÊR]€\›JÊR€]€\›JÊS€]€\›JQ
‰‰	ÄPà]€¸ˇ€]¡\›R
ÊSÊEÊ‰	ÄPàRSÊRQS¡EÊRQPíSQPííE¡R‰RQ¸S‰SQùCäQ¡EQRQSQKQPQBQ@QAQPPAA¡@¡B¡P¡K¡S¡R¡E¡QÊRÚRQùCä¸SÚSQ˘˝PQ¡QQPêÊQQ &‰PQ¡RÊS
Ê‰	Ä!PàRSÊRQS¡RQ"Pˇ€#öN¡N
Ê‰	Ä$PàRQ%à&PàRSQ"Pˇ€#öO¡O
Ê‰	Ä$PàSQ%à&PàRNWBBOWBBy‰NQJ¡OQN¡JQO¡@QJ¡AQ@¡JQA¡NSR¡NWN¡NWU¡N[V¡NUBG¡NSN¡OSS¡OWO¡OWX¡O[Y¡OQBZ¡OUBH¡OSO¡R	ÚSRÚSQ'ä(äS¡
¸SSQ'ä(äS¡RQ'ä(äR¡@Q)ä@¡AQ)äA¡BQ)äB¡@Q\¡‰\S^√NQ*P+òÊ	‰	Ä,Pà^Q%à-PàRQ%àR\≈ÏÁAQ\¡‰\S^√OQ*P+òÊ	‰	Ä,Pà^Q%à-PàSQ%àR\≈ÏÁBQ\¡6‰\S^√NQ*P+òÊ^QOQ*P+ò
Ê‰	Ä,Pà^Q%à.PàR^QNQ*P+ò‰^QOQ*P+ò‰‰	Ä,Pà^Q%à/PàRQ%à0PàSQ%à1P2à	Ä3PàR\≈ ÁKQPQFQNQOQRQSQ4P5PAF¡P¡K¡]€]—NQ`¡_¡¸_QRQ`SíC_√¡`≈`ˆÁ]Q`€`—FQ]¡\¡¸\QSQ]SíC\√¡]≈]ˆÁ`Q6íD¡J€KQ7äK√^¡‰^S^U*P+ò	‰JQ8PSQ'ä9P^S'ä:¢¸JQ^S'ää6íJ¡^≈ÍÁJQ7äK¡GQHQ6íäDQPQ;öBI¡T‰W€TQKQPQÄQIQUQVQWQ<P=PAW¡T¡ÊRRQSQNQOQFQZQMQYQXQQQ
J>∫L¡RQ'äSQ'äê*‰D€PQ^¡‰^S^U*P+ò‰DQ^S?ä¸DQ^Sä6íD¡^≈ÁDQP¡NQ`€`—FQ]¡\¡¸\QSQ]SíC\√¡]≈]ˆÁ`Q6íD¡LQDQPQ;öL¡@PÄQí‰L‰LQAäL¡TM‰LK‰BPW‰8PWQB'äCPWUB¸8PDPÄQíB‚UQ'äCPEPÄQíB‚VQ'ä:¢FíP(í`¡TQKQLQW‰WWB¸KQTQ`©GPˇ›*PHPTQIä'äöJPˇ›W‰WWBB¸JLQäCˇaíK®GPˇ›*PHPLPöMPˇ›äK®PÄQí‰LCD‰T‰TQLQKQˇ€ˇ€ˇ€ˇ€W‰WWBB¸NPÄQíB‚IQˇ›ˇ›LQäCJO∏TC&‰PPLQKQˇ€ˇ€ˇ€ˇ€NPÄQíBäCKQäCx‰NPÄQíBPˇ€KQäCC6í¸NPÄQíBˇ›ˇ›LQäCJO∏STQ‚RˇOÄÙBÄPÄÄBÄÛÄÎÄÜÄ`8FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÛ\ÄÍÄ&RESTÄBÄ`™Ä&KEYBÄõBÄ˛jÅ&OPTIONALÄBÄ£BÄoBÄöjÅDIRECTORYÄÍÅDOCUMENTATIONÄBÄüÉÇIMPLEMENTATION-TYPEÄBÄûCÇSTORAGE-STRUCTUREÄBÄeBÄ¢BÄ§jÇ&ALLOW-OTHER-KEYSÄBÄ:\ÄBÄ`BÄõBÄ£BÄoBÄöBÄBÄBÄüBÄBÄûBÄBÄeBÄ¢BÄ§\ÄÈÅDOCUMENTATIONÄÏøÿThis function provides the capability to combine two relations into a new relation
   in which the tuples which are to participate in the operation are selected
   by a where clause.

   FROM                 - A list consisting of the relations to be joined.
   PROJECT              - This clause specifies the attributes that are to be in the resultant relation
                          and their associated names in that new relation. It should be of the form
                          (<[relation-name.]attribute-name>). The optional part relation-name can be
                          skipped if the attribute is unique in one of the two relations being joined.
                          If the keyword FROM is not specified, this clause should contain the names
                          of the relations to be joined. Also, if * is given instead of the attribute-name
                          it indicates that RTMS should use all the attributes in that relation.
   WHERE                - Can be used to perform theta-joins. It is a condition used in joining the relations.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   PRINT                - If NIL, the resultant relation will not be printed out.
   TUPLES               - If T, the resultant tuples will be returned.
   UNIQUE               - If T, only unique tuples will be part of the resultant relation.ÄÄ\Ä©ÄFROM)ÅPROJECTÄÈÄWHEREÄ©ÄINTOiÅDIRECTORYÄBÄÈÄFORMAT©ÇIMPLEMENTATION-TYPEÄ©ÄKEYÄiÇSTORAGE-STRUCTUREÄÈÄPRINTÄÈÄTUPLESÈÄUNIQUE¿p¿BÄ\ÏÅSTORE-KEYARGSÄ“BÄPí@‰@QPˇ›A—†@QåOÄBÄÛÄÄ√ÅPROCESS-WHEREÄÄÎÄUºÜÄAúFÄg¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ\Ä√ÄTUPLEABÄpCÅWHERE-EXPÄBÄtBÄuBÄqBÄ:\ÄCÅNEW-WHEREÄBÄlBÄ:BÄ:BÄ:BÄ:CÄXÄ\ÄBÄ\Äp¿BÄ\lÅXR-BQ-LISTBÄèBÄéBÄÖBÄÇÄjÄOR¿BÄ“BÄ¨“BÄ8¿BÄ≈“BÄ–¿p¿BÄ\lÅPOSITION*Ä“BÄﬂ“BÄ»¿BÄ “√ÅREMOVE-DOT-REL“BÄ∑“ÍÄEQUALÄ¿p¿BÄ\lÅMEMBER-EQL“BÄ÷íÇ5-‰B—ÇUD¡C¡¸CQDSˇ5>BCC√¡D≈DˆÁBQPD‰ÇSD€D—ÇUB¡E¡¸EQÄQÅQBSÉQÑQÖQ≤CE√¡B≈BÚÁDQ
CˇÇ5ÊÄQÅQÇQäÉQÑQÖQ≤B@¡@ÇS&‰ÇQ@¡@ÉQäÉ¡ÑQäÑ¡ÇQD¡_‰DSF¡F7&‰PFQä	ê!‰FQ
ääÅQPò‰FQääÉQê‰PFQ
ääÅQP	öÄQåCíA√@Qê‰FQääÑ+*Ê¸FQäÅQPò‰PFQäÅQP	öÄQåCíA¡@QAQ¸FF7‰PFQä	ê‰FQ
ääÖQPò
‰FQääÑQê‰@QFQ
ä¸@QFQäí@¡D≈°Á@OÄ2BÄÄÄÉÇPROCESS-SET-RELATIONÄÎÄ/qÜÄ@úFÄB¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ3\ÄBÄÈBÄ€BÄ:\ÄCÅATTRIBUTESÅDOMAINSÄBÄ:ÉÄATTÄBÄ:BÄ:ÉÄATTR\ÄBÄ\ÄBÄÇBÄéBÄÖBÄäBÄèÄÉÅ*ACTIVE-DB*Ä—BÄîë\ÄlÅATTRIBUTES,ÅDOMAINSÄ¿BÄ¡“BÄñ“¨ÅERROR - The ¿BÄò“BÄ√“lÖ relation is not defined in the database Ä¿BÄ¨“BÄ»¿BÄ “,ÅERROR - ¿ÏÑ is not an attribute in the relation Ä¿BÄ–¿BÄ—íÄQPˇ€ö@¡@Ê	‰ÄP	àÄQ
àP	àP
àÄ€E¸@SÄ¡@QBA¡@Q	B@¡Å;‰Å5ÊÅQäÅ¡ÅQB¡‰BSC√@QPòÊ‰ÄP	àCQ
àP	àÄQ
àPàÄ€B≈ÍÁB€B—ÅQE¡D¡¸DQESF¡@QäCFQ@QPöäCˇcAQåCCD√¡E≈EÌÁBQA¡ÅQ@¡ÄQ@QAQÉOÄKBÄ3ÄÄCÇSET-COMPATIBILITYÄÄÎÄ0pÜÄA0FÄ@¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄL\ÄÉÄRELAÉÄRELBBÄpBÄqBÄ:\ÄÉÅATTRIBUTESAÄÉÅATTRIBUTESBÄÅDOMAINSAÅDOMAINSBBÄ:BÄ:BÄ:BÄ:√ÄATTRAÄ√ÄATTRBÄÉÄDOMAÉÄDOMB\ÄBÄ\ÄBÄéBÄÖÄBÄîëFÄ–¿BÄ3“BÄñ“lÇERROR - Relations ¿BÄò“BÄ√“ÏÄ and Ä¿l  do not have the same number of attributes, thus they are not compatible.Ä¿lÇERROR - Attribute ¿ÏÅ of relation Ä¿,Ç and attribute Ä¿¨É are not compatible domainsÄÄÄQÇQPPAB¡@¡Ä¡ÊRÅQÉQPPAC¡A¡Å¡ÊR@QäCAQäC|Ê‰ÄPàÄQ	à
PàÅQ	àPàR@QAQBQCQG¡F¡E¡D¡&¸DSESFSGSK¡J¡I¡H¡JQK+Ê‰ÄPàHQ	àPàÄQ	àPàIQ	àPàÅQ	àPàRD≈E≈F≈G≈D‰E‰F‰G”ÁSOÄjBÄLÄÄÇSET-CREATE-RELCÄÄÎÄB¢ÜÄ·@FÄ`¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄk\ÄBÄUÉÄRELCBÄ`BÄpBÄ°BÄ:\ÄÉÇATTRIBUTE-DESCRIPTORBÄ=BÄ†ÉÄDOMÄBÄúBÄûÉÄMODPÉÅQTRIEVE-VARÄCÄSSÉÅTUPLE-FORMATBÄ:BÄ:BÄ:BÄ@BÄwBÄ=\ÄBÄ\Ä	BÄéBÄÖBÄÇBÄáBÄÜBÄ BÄäBÄãBÄè¿ÜÄê¢Ä√Ç*SYSTEM-RELATION-KEY*Ä—ÉÉ*SYSTEM-RELATION-ATTRIBUTES*ëBÄË¿\Ä	lÅMODIFIEDPÄÏÅSAVE-DIRECTORYlÅATTRIBUTES¨ÇIMPLEMENTATION-TYPEÄlÇSTORAGE-STRUCTUREÄ¨ÄKEYÄ¨ÅTUPLE-FORMAT¨ÄDOCÄ,ÅDOMAINSÄ¿BÄ»¿BÄÈ¿BÄ≈“BÄ¨“ÅQTRIEVEÄ“\ÄBÄ†¿BÄß“\ÄBÄú¿\ÄBÄù¿\ÄBÄ°¿\ÄBÄû¿\ÄBÄü¿BÄ “BÄw¿BÄ÷“BÄú¿BÄù¿BÄû¿BÄü¿BÄ°¿BÄ†¿√ÄDEFRELíPPPPP	PÄQ
äö™BG¡GSF¡PÇQíB‚GWB¡G[A¡PÇQíB‚GQBD¡PÇQíB‚GUBH¡PÇQíB‚ÑQÑ¡PÇQíBE¡PÇQíB‚JGQåCI¡JGQåCC¡É!‰É5ÊÉQäÉ¡J—ÉQL¡K¡¸KQLSM¡AQäCMQAQPöäCˇcCQåCCK√¡L≈LÌÁJQC¡ÉQA¡CQN¡AQO¡‰@QOSPNSííí@¡N≈O≈ıÁÅQ@QPDQPHQPEQPIQPÑQPBQJ∫úOÄìBÄkÄÄÉÇRELATION-DIFFERENCEÄÄÎÄ#\€ÜÄ‡#FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄî\ÄBÄ˝BÄ`BÄ˛BÄˇBÄõBÄöBÄBÄBÄüBÄBÄBÄûBÄeBÄ¢BÄ§BÄBÄ:\ÄBÄ`BÄõBÄöBÄBÄBÄüBÄBÄBÄûBÄeBÄ¢BÄ§√ÄTEMPAÄÉÅTEMP-TUPLESÄBÄl√ÄTABLEÄBÄUBÄVBÄt√ÄPRINT1ÅTUPLES1ÄBÄp√ÄWHEREABÄq√ÄWHEREB√ÄATTRSCBÄsBÄ:√ÄTUPLEÄBÄ:BÄ"\ÄBÄ\ÄBÄéBÄÖBÄèBÄÏø#Difference of the tuples in two relations.

   FROM                 - This clause specifies the relations to participate in the DIFFERENCE operation.
                          In addition, RTMS allows users to specify the attributes in these relations to
                          participate in the operation as well as a where-clause to specify the tuples.
                          It should be of the format: (RelA [(PROJECT <attrA> WHERE where-clause-A)] RelB
                          [(PROJECT <attrB> WHERE where-clause-B)]) where attrA indicates attributes in
                          relation relA and where-clause-A indicates a where-clause involving the attributes
                          in relation relA.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   PRINT                - If NIL, the resultant relation will not be printed out.
   TUPLES               - If T, the resultant tuples will be returned.
   UNIQUE               - If T, only unique tuples will be part of the resultant relation.¿ÜÄÄÉÇ*VALIDITY-CHECKING*Äë\ÄBÄ
BÄBÄBÄBÄBÄBÄBÄBÄBÄBÄ¿BÄ“ÇACTIVE-DATABASEP“lÅDIFFERENCE¿ÜÄ–¿√ÇVALIDATE-RELATION-OPSÄ“©ÄTEST¿BÄ/¿*ÇMAKE-HASH-TABLEÄ“BÄ≈“BÄ∆“BÄ£¿BÄ¢¿BÄ§¿\ÄBÄ§¿BÄß“BÄo¿ÅRETRIEVE“*ÅGETHASHÄ“p¿BÄT,ÅPUTHASHÄ“BÄ=¿√ÄINSERT“*ÅCLRHASHÄ“\ÄlÅATTRIBUTES¨ÅTUPLE-FORMAT¿BÄ¡“ÉÇUNCONVERT-ATTRIBUTES“BÄÚ“CÅDIFFERENCE¿\ÄBÄü¿BÄ€í@‰@QPˇ›A—†ÄÊR@QP	P
PAS¡Z¡T¡Y¡R¡X¡W¡Q¡V¡U¡P¡ÊRPPíO¡QQääPWQPˇ›PP@QíBPXQ	J∫N√[¡‰[S\√\Q\QOQí
COQò[≈ıÁPQääPˇ›PUQPP@QíBPVQ	J∫]¡
‰]S^√OQêÊ^QM]M¡]≈ˆÁM‰R‰PL¡⁄RQPMQPYQ®LQ¿OQàT‰MSC‰R‰RQPˇ€öBL¡RQMQYQ‚LSZQíˇ€ˇ€ˇ€ˇ€LWˇ›ˇ›MQäCJ∏RS&‰PQPˇ€öBL¡ PMQUQ‚LSZQíˇ€ˇ€ˇ€ˇ€!P@QíB	‚U‰LULSUQ"öB¸LWˇ›ˇ›MQäCJ∏SRQ‚PˇOÄ¿BÄîÄÄ√ÇRELATION-INTERSECTIONÄÄÎÄ#Y’ÜÄ‡#FÄ|¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ¡\ÄBÄ˝BÄ`BÄ˛BÄˇBÄõBÄöBÄBÄBÄüBÄBÄBÄûBÄeBÄ¢BÄ§BÄBÄ:\ÄBÄ`BÄõBÄöBÄBÄBÄüBÄBÄBÄûBÄeBÄ¢BÄ§BÄû√ÄTEMPBÄBÄlBÄ†BÄUBÄVBÄtBÄ¢BÄ°BÄpBÄ£BÄqBÄ§BÄ•BÄsBÄ:BÄ¶BÄ:√ÄTUPLEB\ÄBÄ\ÄBÄéBÄÖBÄèBÄÏø#Intersection of tuples in two relations.

   FROM                 - This clause specifies the relations to participate in the INTERSECTION operation.
                          In addition, RTMS allows users to specify the attributes in these relations to
                          participate in the operation as well as a where-clause to specify the tuples.
                          It should be of the format: (RelA [(PROJECT <attrA> WHERE where-clause-A)] RelB
                          [(PROJECT <attrB> WHERE where-clause-B)]) where attrA indicates attributes in
                          relation relA and where-clause-A indicates a where-clause involving the attributes
                          in relation relA.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   PRINT                - If NIL, the resultant relation will not be printed out.
   TUPLES               - If T, the resultant tuples will be returned.
   UNIQUE               - If T, only unique tuples will be part of the resultant relation.¿ÜÄÄBÄ´ë\ÄBÄ
BÄBÄBÄBÄBÄBÄBÄBÄBÄBÄ¿BÄ“BÄ≠“lÅDIFFERENCE¿ÜÄ–¿BÄ∞“BÄ¢¿BÄ£¿BÄo¿BÄ¥“BÄ±¿BÄ/¿BÄ≤“BÄµ“BÄ∑“BÄ≈“BÄ∆“BÄ§¿\ÄBÄ§¿BÄß“BÄ=¿BÄ∏“BÄπ“\ÄlÅATTRIBUTES¨ÅTUPLE-FORMAT¿BÄ¡“BÄΩ“BÄÚ“™ÅINTERSECTION¿\ÄBÄü¿BÄ€í@‰@QPˇ›A—†ÄÊR@QP	P
PAT¡Z¡S¡Y¡R¡X¡W¡Q¡V¡U¡P¡ÊRPQPˇ›PUQPVQJ∫N¡PPíO¡NQ[¡‰[S\√\Q\QOQí
COQò[≈ıÁQQääPˇ›PP@QíBPXQPWQ	J∫]¡
‰]S^√OQê‰^QM]M¡]≈ˆÁM‰R‰PL¡⁄RQPMQPYQ®LQ¿OQàS‰MTC‰R‰RQPˇ€öBL¡RQMQYQ‚LSZQíˇ€ˇ€ˇ€ˇ€LWˇ›ˇ›MQäCJ∏RT&‰PQPˇ€öBL¡ PMQUQ‚LSZQíˇ€ˇ€ˇ€ˇ€!P@QíB	‚U‰LULSUQ"öB¸LWˇ›ˇ›MQäCJ∏SRQ‚PˇOÄ⁄BÄ¡ÄÄ√ÅRELATION-UNIONÄÎÄ$ZÿÜÄ‡$FÄ~¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ€\ÄBÄ˝BÄ`BÄ˛BÄˇBÄõBÄöBÄBÄBÄüBÄBÄBÄûBÄeBÄ¢BÄ§BÄBÄ:\ÄBÄ`BÄõBÄöBÄBÄBÄüBÄBÄBÄûBÄeBÄ¢BÄ§BÄûBÄUBÄVBÄtBÄ†BÄ¢BÄ°BÄlCÅTEMP-UNIONBÄpBÄ£BÄqBÄ§BÄ•BÄsBÄ:BÄ¶BÄ:\ÄBÄ\ÄBÄéBÄÖBÄèBÄÏøUnion of tuples in two relations.

   FROM                 - This clause specifies the relations to participate in the UNION operation.
                          In addition, RTMS allows users to specify the attributes in these relations to
                          participate in the operation as well as a where-clause to specify the tuples.
                          It should be of the format: (RelA [(PROJECT <attrA> WHERE where-clause-A)] RelB
                          [(PROJECT <attrB> WHERE where-clause-B)]) where attrA indicates attributes in
                          relation relA and where-clause-A indicates a where-clause involving the attributes
                          in relation relA.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   PRINT                - If NIL, the resultant relation will not be printed out.
   TUPLES               - If T, the resultant tuples will be returned.
   UNIQUE               - If T, only unique tuples will be part of the resultant relation.¿ÜÄÄBÄ´ë\ÄBÄ
BÄBÄBÄBÄBÄBÄBÄBÄBÄBÄ¿BÄ“BÄ≠“lÅDIFFERENCE¿ÜÄ–¿BÄ∞“BÄ±¿BÄ/¿BÄ≤“BÄ≈“BÄ∆“BÄ§¿\ÄBÄ§¿BÄß“BÄ£¿BÄo¿BÄ¢¿BÄ¥“BÄµ“BÄ∑“BÄ=¿BÄ∏“BÄπ“BÄ÷“\ÄlÅATTRIBUTES¨ÅTUPLE-FORMAT¿BÄ¡“ÍÄUNIONÄ¿BÄΩ“\ÄBÄü¿BÄ€“BÄÚí@‰@QPˇ›A—†ÄÊR@QP	P
PAR¡Z¡Q¡Y¡O¡X¡W¡N¡V¡U¡M¡ÊRPPíP¡MQääPP@QíBPUQPVQPˇ›	J∫S√[¡‰[S\√\Q\QPQí
CPQò[≈ıÁO‰OQPSQPYQ®T€NQääPˇ›PP@QíBPWQPXQ	J∫]¡
‰]S\√PQêÊ\QT]T¡]≈ˆÁT‰O‰PL¡⁄OQPTQPYQ®LQ¿PQàQ‰SQTQîR5‰O‰OQääPYQòOR*‰MQPˇ€öBL¡PSQTQíUQ‚LSZQ íˇ€ˇ€ˇ€ˇ€!P@QíB	‚U‰LULSUQ"öB¸LWˇ›ˇ›SQTQíäCJ#∏SOQ‚MˇOÄÛBÄ€ÄÄ√ÅVALIDATE-WHEREÄÎÄ_–ÜÄAHFÄq¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÙ\ÄÉÅWHERE-CLAUSECÅRELA-NAMEÄCÅRELB-NAMEÄ√ÄATTSAÄ√ÄATTSBÄBÄ:\Ä√ÅTEST-ATTRIBUTE√ÅTEST-RELATIONÄ\ÄBÄ\ÄBÄèBÄ BÄäBÄãÄBÄC—BÄîëBÄñ“,ÅERROR - ¿BÄò“BÄ√“lÑ improperly formed where subclause¿BÄ.“BÄﬂ“BÄ»¿BÄ “ÏÉ is an unrecognized attributeÄ¿,ÅERROR -Ä¿lÉ is not a relation in the ¿lÅ databaseÄÄÄÊSÄQ±‰SÄQJô	ÊÄSˇ5ÊÄWˇ5ÊÄ[ˇ5
‰‰ÄPàÄQà	PàRÄW
äA¡ÄWä@¡AÊ@QÉQPòAÊ@QÑQPò<Ê‰ÄPà@QàPàRÅQA+‰@QÉQPò*Ê‰ÄPàÄWàPàRÇQA+‰@QÑQPòÊ‰ÄPàÄWàPàR‰ÄPàAQàPàPàPàRÄ[
äA¡Ä[ä@¡AÊ@QÉQPòÊ@QÑQPò	Ê‰ÄPà@QàPàRÅQA+‰@QÉQPò
Ê‰ÄPàÄ[àPàRSÇQA+‰@QÑQPò
Ê‰ÄPàÄ[àPàRS‰ÄPàÄ[
äàPàPàPàROÄBÄÙÄÄÇPRE-RELATION-OPSÄÎÄ9ÜÄ@pFÄF¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ\ÄBÄ`BÄ:\ÄBÄlBÄUBÄVBÄtBÄ¢BÄpBÄ£BÄqBÄ§BÄ•ÉÄVARÄBÄs\ÄBÄ\ÄBÄ BÄäBÄãBÄèÄ\ÄBÄõ¿BÄß“BÄ¨“BÄ≠“BÄ≤“\ÄBÄo¿\ÄBÄ£¿BÄ«“\ÄBÄö¿\ÄBÄ¢ÄPÄQíB@¡‰@5Ê@Qä@¡@SäA¡‰AQùCäK¡@WJ√ˇ5‰JQJô‰PJQíBF¡	PJQíB
äE¡AQ@Y
C@¡@WäB¡‰BQùCäK¡@[J√ˇ5‰JQJô‰PJQíBH¡	PJQíB
äG¡PÄQíBC¡PÄQíBD¡C5‰CWJ√ˇ5
‰JQJô‰	PJQíB
äI¡C5‰CS¸CQäC¡AQEQFQBQGQHQCQIQDQKQäOÄ BÄÄÄBÄ¥ÄÎÄX¿ÜÄBFÄh¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ¥\ÄBÄgBÄtBÄuBÄmBÄrBÄdBÄbBÄcBÄ:\ÄÉÅPERIOD-INDEX√ÇMYSTERY-RELATION-NAMEÄBÄ:BÄ@√ÄTEMP1Ä\ÄBÄ\ÄBÄèBÄéBÄÖÄBÄ¨“BÄ÷“BÄ–¿p¿BÄÉlÇSTRING-SEARCH-CHAR“BÄ.“BÄ≈“BÄ∑“BÄ≠“BÄÿ“ÍÄSUBSEQ“lÄ*Ä¿*ÇREAD-FROM-STRING“BÄﬂíÄQB¡§‰BSC¡D€C5.‰ÉQC‰CSä¸CQíÉ¡ÑQCWäíÑ¡PCWJˇ€ˇ›™@¡Q‰CWäD¡Å‰ÇJÊÅQäDQä	êDÊDQ
äÇ¡@¸CWˇ›
íääÅ¡9¸PCQJˇ€ˇ›™@¡"‰CQäD¡Å‰ÇÊÅQäDQä	ê
ÊDQ
äÇ¡¸CQˇ›
íääÅ¡ÉQCQäíÉ¡ÑQCQäíÑ¡¸ÉQCQäíÉ¡ÑQCQäíÑ¡ÖQCQäíÖ¡C5‰CWC¡@+‰CQä@kJ@aöP	ê ÊCQäJ@QöäA√ÅQ	ê‰ÜQCQääíÜ¡¸AQÇQ	ê‰áQCQääíá¡¸ÖQCQä¸@ÊÖQCQäíÖ¡B≈\ÁÄQÅQÇQÉQÑQÖQÜQáQàOÄ4BÄ¥ÄÄBÄ‘ÄÎÄoÜÄAËFÄÅ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ‘\ÄBÄmBÄrBÄhBÄpBÄqBÄtBÄuBÄ:\Ä
BÄlBÄ:BÄ@BÄ:BÄ:BÄ:BÄ:√ÄATTR1ÄBÄ:√ÄATTR2Ä\ÄBÄ\ÄBÄÇBÄéBÄÖBÄèÄBÄ≈“lÄ*Ä¿BÄ±“BÄ∑“BÄ–¿BÄ0“BÄ.“BÄﬂ“BÄ¨“BÄ÷“BÄ«“BÄÿ“BÄ»¿BÄ-“BÄ íÄQ@¡Ä€@QA¡C‰ASB¡B7‰BQäÖQPíäê‰ÄQÉQ/¸BB7‰BQäÜQPíäê‰ÄQÑQ ¸PBQJˇ€ˇ›®‰ÖQBQ	äêÊPBQJˇ€ˇ›®
‰ÜQBQ	äê‰ÄQBQ
ää¸ÄQBQäíÄ¡A≈ΩÁÅQä@¡Å€Ç€ÖQäÜQäê‰@QÖQPíÜQPííär‰ÉQÇ¡@QC¡v‰CSB√ÖQPíär‰ÅQD€D—ÉQF¡E¡¸EQFSG¡ÖQGQíäCE√¡F≈FÛÁDQT¸BQÜQPíär‰ÅQF€F—ÑQD¡H¡¸HQDSI¡ÜQIQíäCH√¡D≈DÛÁFQíÅ¡ÑQÇ¡5¸PBQäPò‰BQ
ääÑQPò‰ÇQBQ
äääíÇ¡ÅQBQ¸BQäÉQPò
‰ÖQBQíÅQPòÊÅQÖQ¸ÇQBQääíÇ¡ÅQÜQBQíääíÅ¡C≈äÁÄQÅQÇQÉOÄCBÄ‘ÄÄBÄ›ÄÎÄ$8îÜÄB$FÄ\¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ›\ÄBÄvBÄmBÄrBÄ`BÄkBÄwBÄxBÄyBÄ:\ÄÄlBÄ:BÄ:√ÄA-ATTRBÄ@ÉÄATTD\ÄBÄ\ÄÄ BÄäBÄãBÄéBÄÖBÄèÄ√Ç*SYSTEM-ATTRIBUTE-KEY*—√É*SYSTEM-ATTRIBUTE-ATTRIBUTES*Äë\ÄlÅATTRIBUTES¨ÇIMPLEMENTATION-TYPEÄlÇSTORAGE-STRUCTUREÄ¨ÄKEYÄ¨ÅCARDINALITYÄ¨ÅTUPLE-FORMAT¿BÄ¡“ÇSYSTEM-ATTRIBUTE¿\Ä,ÇDOMAIN-FUNCTIONÄÏÅDEFAULT-VALUEÄ¨ÄDOCÄ¿™ÄANDÄ¿BÄ»¿BÄÈ¿BÄ.“BÄÿ“BÄ¨“√ÅATTRIBUTE-NAME¿BÄﬂ“BÄã“BÄw¿p¿BÄT¨ÄDEFÄ¿BÄ°¿BÄ÷“BÄú¿\ÄBÄú¿BÄß“BÄù¿\ÄBÄù¿BÄû¿\ÄBÄû¿BÄü¿\ÄBÄü¿BÄ†¿\ÄBÄ†¿\ÄBÄ°¿¨Ä...Ä¿BÄííÄQPˇ€öá¡áeÊÅQ@¡Å€ÇQ@QB¡A¡)¸ASBSD¡C¡E€PPPP	P
PPCQääö
PPCQääöö™BE¡ÅQDQPESPEWPE[≤ííÅ¡A≈B≈A‰B‘ÁÄQÅQPPÉQíB‚ÖQPPÉQíB‚ÜQPPÉQíBPPÉQíB‚ÑQP PÉQíBP!PÉQíB‚"PJ∫#òÊˇ€¸ÄQáQÇáSÄ√áQÇOÄjBÄ›ÄÄBÄﬁÄÎÄ^€ÜÄ‚ÄFÄ}¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄﬁ\Ä
BÄtBÄuBÄpBÄqBÄhBÄ|BÄoBÄ{BÄzBÄsBÄ:\ÄBÄ:BÄ:p¿BÄ\lÇLEX-PARENT-ENV-REGp¿BÄ\ÏÅLEX-ENV-B-REGÄp¿BÄ\ÏÇLEX-CURRENT-VECTOR-REGp¿BÄ\¨ÇLEX-ALL-VECTORS-REGÄCÅKEY-VALUEÄBÄÃCÇRETRIEVE-FUNCTIONÄÉÅKEY-FUNCTIONBÄ¢CÅINDEX-NAMEBÄ>BÄ?ÉÄNUM1ÉÄNUM2BÄ†√ÄTABLE1BÄ,BÄlBÄnBÄ:BÄ:BÄ¶BÄûBÄlBÄ:√ÄTEMP-T\ÄÄ\ÄÄéBÄÖBÄ BÄäBÄãBÄè©ÇINTERNAL-FEF-OFFSETS\ÄFÄFÄiÑVARIABLES-USED-IN-LEXICAL-CLOSURES\ÄBÄnBÄlBÄBÄ¢BÄ~BÄ}BÄÃBÄ|BÄsBÄoBÄ|BÄhBÄqBÄpBÄuBÄt¿ÜÄEÄBÄíëÜÄ¿ÉÅEXTRACT-KEYÄ“BÄÿ¿lÅRETRIEVE-Ä¿lÄ-Ä¿BÄ⁄“BÄ∆“BÄ≈“¨ÅEXTRACT-KEY-¿BÄ/¿BÄ»¿BÄ “BÄ±¿BÄ≤“ÉÅENTRY-POINTÄ¿ÉÄGETP“BÄµ“BÄ∑“BÄ÷“\ÄFÄFÄFÄFÄFÄ
FÄ	FÄFÄFÄÜ¿˜ˇÜ¿˙ˇÜ¿ˇ˚ˇÜ¿¸ˇÜ¿˝ˇÜ¿˛ˇÜ¿ˇˇÜ¿Ä¿\Ä)ÅINTERNALBÄﬁÄ¿*ÅMAPHASHÄ“BÄπ“\ÄBÄßBÄﬁFÄ¿BÄ3“ÉÄMAPTíÅQÉQÖQˇ€áQÜQˇ€PPAÖ¡á¡F¡K¡ÜQ±‰PPàQ	PáQ
™PíU¡ÅQääÉQÑQÖQÜQFQÅQääJUªG¡PPàQ	PáQ
™PíH¡PPáQ
öPíI¡Ü5z‰ÜS&w‰ÜWL√ÇQPò‰Ü[M√ÉQPòÊÜWM√ÉQPòe‰Ü[L√ÇQPò_‰ÇQäCLQÇQPöäCˇcN¡ÉQäCMQÉQPöäCˇcO¡PPíP¡PPíQ¡ÄQPíV¡‰VSW¡NQWQåCX√WQXQPQí
CPQòV≈ÒÁÅQPíU¡"‰USW¡OQWQåCX¡Y€XQPQíR¡‰RQZ¡	‰ZS[¡WQ[QíY]Y¡Z≈˜ÁXQYQXQQQííQQòU≈ﬁÁPP”CQQêPQàQQàTPP”CÄQääêTOÄ≠BÄﬁÄÄBÄ¶ÄÎÄ
ÜÄ@åFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ¶\ÄBÄûBÄ¶BÄ:\ÄBÄ:BÄ:BÄu\ÄBÄ\ÄBÄèiÉLEXICAL-PARENT-DEBUG-INFOÄBÄpÄBÄ÷íÅQ¿Pí¿¬ˇOÄ∫BÄ¶ÄÄBÄ™ÄÎÄ%RÜÄ@\FÄ-¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ™\ÄBÄ"BÄ:\ÄBÄ:BÄ:BÄuBÄ:BÄ¶BÄ:BÄÃ\ÄBÄ\ÄBÄéBÄÖBÄèBÄπBÄpÄBÄ÷“\ÄBÄY¿BÄ“BÄ≈“BÄ∆í…P±‰√⁄∆PC¡:‰CSD¡ÄQDQí√\√¿C≈˜Á0¸…P¡¬*‰¡‹¸ÄQÕP¡PœPŒPÃP≤¡¿ÃP Pˇ€¡P»Pƒ™«¿√⁄≈PE¡ŒPääÃPÀP P¡P«P¬PääJ≈∫E¡	‰ESF¡ÄQFQí√\√¿E≈˜Á√‰√P¿Pí¿¬ˇROÄ«BÄ™ÄÄBÄ∞ÄÎÄ5|ÜÄ@∏FÄG¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ∞\ÄBÄ`CÅOPERATIONÄBÄ:\ÄBÄpBÄqBÄ•BÄ°BÄsÉÅREL-EXISTS?ÄBÄUBÄVBÄtBÄûBÄ¢BÄ£BÄ§BÄ`\ÄBÄ\ÄBÄéBÄèÄBÄîëBÄe¿\ÄBÄöBÄõBÄüBÄ†BÄ°BÄûBÄúBÄùBÄeBÄ¢BÄ§¿BÄ•“ÜÄê¿BÄ“BÄñ“¨äERROR - Relations to participate in relational set operation have not been provided.¿BÄò“BÄL“\ÄBÄ=¿BÄ¡“,ÉRelation ~s of ~S and ~S¿BÄü“BÄkíÄQ¸MSM¡‰MSˇ5˙ÁMQÄ¡PÄQãCC¡‰CWC¡PÄQíÄ√PPAD¡J¡B¡H¡L¡A¡G¡K¡@¡F¡JÊHÊCÊPÄÊC›F‰GÊ‰	Ä
PàRFQGQ@QAQ†ÊRH‰HQPˇ€öI¡ISH¡IQ	BE¡ÊFQHQÄQ@Qˇ€PÅQFQGQ™®ÊRFQHQ@QBQ†ÊRFQ@QKQGQAQLQHQBQJQDQCQãOÄ⁄BÄ∞Ä1Ä\Äp¿BÄ\,ÑFASL-RECORD-FILE-MACROS-EXPANDED\ÄBÄ8\Ä\ÄÍÄDEFUNÄÜÄ'\ÄBÄ*ÜÄ.Ÿã\ÄBÄèÜÄ[ÊÑ\ÄBÄéÜÄ=Ã#\ÄBÄ
ÜÄ∆9\ÄBÄãÜÄz(á\ÄBÄäÜÄ{öÕ\ÄBÄ ÜÄ:}n\ÄBÄàÜÄ{ƒ≤\ÄBÄáÜÄZiÛ\ÄBÄÜÜÄxıø\ÄBÄÖÜÄ*˝j\ÄBÄÇÜÄ(Ã¢ÄÄ where-clause-A)] RelB
                          [(PROJECT <attrB> WHERE where-clause-B)]) where attrA indicates attributes in
                          relation relA and where-clause-A indicates a where-clause involving the attributes
                          in relation relA.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relatLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540807. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "LISP" :NAME "RENAME" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :CHARACTERS T :NOT-BACKED-UP T :CREATION-DATE 2749846753. :AUTHOR "REL3" :LENGTH-IN-BYTES 20066. :LENGTH-IN-BLOCKS 20. :BYTE-SIZE 8.)

;;; -*- Mode:Common-Lisp; Package:RTMS; Fonts:(*CODE-FONT* *COMMENT-FONT* *STRING-FONT*); Base:10 -*-
;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved
;;; RENAME
;;;
;;; This file contains the following Explorer extensions to CommonLisp Standard as Indicated in the June 1985 Explorer Lisp
;;; Reference
;;;     firstn
;;;     copy-array-contents
;;;     deff
;;;
;;; The following function contains flavor references and thus are incompatable with CommonLisp. Their removal will not
;;; effect the functionality of RTMS.
;;;     rename-attribute-flavor
;;;     rename-relation-flavor
;;;

(deff rename-attr 'rename-attribute)

(defun rename-attribute (relation-name &rest attributes
 &aux relation-tuple pos pos-k sto rel-owner-id attribute-list key-list imp)
  "Use this function to rename attributes in a relation.

   RELATION-NAME  - Name of the relation whose attributes are to be renamed.
   ATTRIBUTES     - Specify old-attribute and new-attribute names.

   Example: (RENAME-ATTRIBUTE 'parts 'number 'id 'name 'description)."
    (block rename-attribute
  (if (not (active-database))
      (return-from rename-attribute nil))
  (if (null (setf relation-name (validate-sym relation-name t)))
 (return-from rename-attribute nil))
  (setf attributes  (do ((attr-l attributes (car attr-l)))
((not (listp (car attr-l))) attr-l) ()))
  (setf attributes (convert-attributes attributes))
  (cond ((member relation-name *system-relations*  :test 'string-equal)
 (if *provide-error-messages*
     (format *standard-output*
 "~%ERROR - The attributes cannot be renamed because ~s is a system relation." relation-name))
 (return-from rename-attribute nil)))
  (setf relation-tuple (cadr (get-relation relation-name '("OWNER-ID" "SAVE-DIRECTORY" "ATTRIBUTES" "KEY"
       "TUPLE-FORMAT" "IMPLEMENTATION-TYPE"
       "STORAGE-STRUCTURE" "DOC")
   nil)))
  (cond ((not relation-tuple)
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - The relation ~S is not defined in the database ~S"
      relation-name *active-db*))
 (return-from rename-attribute nil)))
  (setf rel-owner-id (first relation-tuple)
attribute-list (third relation-tuple)
key-list (fourth relation-tuple)
imp (sixth relation-tuple)
sto (seventh relation-tuple))
  (if (not
   (do ((attribute-name (first attributes) (first attributes))
(new-attribute-name (second attributes) (second attributes))
(attributes (cddr attributes) (cddr attributes)))
       ((null new-attribute-name) (return-from rename-attribute t))
     (if (null (setf attribute-name (validate-sym attribute-name t)))
 (return-from rename-attribute nil))
     (cond ((null (setf pos (position attribute-name attribute-list :test 'equal)))
    (if *provide-error-messages*
(format *standard-output* "~%ERROR - The attribute ~S is not defined in ~S."
attribute-name relation-name))
    (return-from rename-attribute nil))
   ((member new-attribute-name attribute-list  :test 'string-equal)
    (if *provide-error-messages*
(format *standard-output* "~%ERROR - The attribute ~S is already defined in the relation ~S."
new-attribute-name relation-name))
    (return-from rename-attribute nil))
   (t
    ;; define the new attribute-list of the relation
    (setf attribute-list (append (firstn pos attribute-list) (cons new-attribute-name
         (nthcdr (1+ pos) attribute-list))))
    ;; if the attribute is part of the key, rename it there too
    (cond ((setf pos-k (position attribute-name key-list :test 'equal))
   (setf key-list (append (firstn pos-k key-list) (cons new-attribute-name
      (nthcdr (1+ pos-k) key-list))))))))
      (if *provide-status-messages*
(format *standard-output* "~%The attribute ~S will be renamed to ~S."
attribute-name new-attribute-name))))
      (return-from rename-attribute nil))
  (funcall (find-symbol (concatenate 'string "RENAME-ATTRIBUTE-" imp) *pkg-string*) relation-name
   attribute-list key-list attributes relation-tuple)
  (if *provide-status-messages*
      (format *standard-output* "~%Renaming attributes completed."))
  (return-from rename-attribute relation-name)))

(defun rename-attribute-array  (relation-name attribute-list key-list attributes ignore )
  (rename-attribute-utility-array-list relation-name attribute-list key-list attributes))

(defun rename-attribute-flavor (relation-name attribute-list key-list ignore relation-tuple)
  (rename-attribute-utility-redef-rel relation-name attribute-list key-list relation-tuple))

(defun rename-attribute-list (relation-name attribute-list key-list attributes ignore )
  (rename-attribute-utility-array-list relation-name attribute-list key-list attributes))

(defun rename-attribute-struct (relation-name attribute-list key-list ignore relation-tuple)
  (rename-attribute-utility-redef-rel relation-name attribute-list key-list relation-tuple))

(defun rename-attribute-utility-array-list (relation-name attribute-list key-list attributes)
  (delete-or-modify 'system-relation t (list 'string-equal 'relation-name (string relation-name))
    '("MODIFIEDP" "ATTRIBUTES" "KEY") (list 't `(quote ,attribute-list) `(quote ,key-list)))
  (do ((attribute-name (first attributes) (first attributes))
       (new-attribute-name (second attributes) (second attributes))
       (attributes (cddr attributes) (cddr attributes)))
      ((null new-attribute-name) (return t))
    (delete-or-modify 'system-attribute t (list 'and (list 'string-equal 'relation-name (string relation-name))
   (list 'string-equal 'attribute-name (string attribute-name)))
      '("ATTRIBUTE-NAME") (list `(string (quote ,new-attribute-name)))))
  (save-system-relations))

(defun rename-attribute-utility-redef-rel (relation-name attribute-list key-list relation-tuple
     &aux system-attribute-list attr-des-pair tuple-list imp ss
     tuple-format-list doc dir)
  (setf system-attribute-list (get-system-attribute-list relation-name))
  (setf imp (sixth relation-tuple)
ss (seventh relation-tuple)
tuple-format-list (fifth relation-tuple)
doc (nth 7 relation-tuple)
dir (second relation-tuple))
  (setf attr-des-pair (create-attr-descriptor attribute-list system-attribute-list))
  (setf tuple-list (retrieve (read-from-string (string-upcase relation-name)) 'tuples t))
  (redefine-rel (read-from-string (string-upcase relation-name)) attr-des-pair imp ss key-list
tuple-format-list doc dir tuple-list)
  (save-system-relations)
  (save-relation (read-from-string (string-upcase relation-name))))

(deff rename-db 'rename-database)

(defun rename-database (&rest databases
&aux database-name new-database-name pathname path rel-name save-dir
relation-tuple-list)
  "Used to rename a database.

   DATABASES - Specify old-database-name and new-database-name.

   Example: (RENAME-DATABASE parts suppliers micro-parts micro-suppliers)."
    (block rename-database
  (if (not (active-database))
      (return-from rename-database nil))
  (setf databases (do ((db-l databases (car db-l)))
      ((not (listp (car db-l))) db-l) ()))
  (setf database-name (validate-sym (first databases) t))
  (setf new-database-name (validate-sym (second databases) t))
  (if (or (null database-name) (null new-database-name))
      (return-from rename-database nil))
  (cond ((not (string-equal database-name *active-db*))
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - The database to rename has to be the active database ~S."
       *active-db*))
 (return-from rename-database nil)))
  (cond ((string-equal new-database-name database-name)
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - The new database name ~S is identical to the actual name"
       new-database-name))
 (return-from rename-database nil)))
  (setf *active-db* new-database-name)
  (setf relation-tuple-list
(funcall (find-symbol (concatenate 'string "RETRIEVE-" *system-relation-base-implementation* "-"
       *system-relation-storage-structure*) *pkg-string*)
 'system-relation *system-relation-attributes* '("RELATION-NAME" "SAVE-DIRECTORY")
 *system-relation-key* t nil 'system-relation))
  ;;
  ;; if the database has been saved before,the file database-name.LISP exists In that case, the files need to be renamed.
  ;;
  (setf save-dir (get-save-directory))
  (cond ((setf path (or (probe-file (concatenate 'string save-dir database-name ".XLD"))
(probe-file (concatenate 'string save-dir database-name ".LISP"))
(probe-file (concatenate 'string save-dir database-name ".XFASL"))
(probe-file (concatenate 'string save-dir database-name ".QFASL"))))
 ;; the system relations have to be saved, even if there are not modified, because the database name and thus the file name
 ;; are changed. so, to be sure that save-relation will save them, modifiedp has to be true
 ;;
 (delete-or-modify 'system-relation t '(member relation-name *system-relations*  :test 'string-equal)
   '("MODIFIEDP") (list 't ))
 (save-system-relations)
 (delete-file path) ;because it is a rename and not a copy !
 ;; go through all the relation files of the database. the old system relation files are deleted because it is a rename and not a
 ;; copy the user relation files are renamed
 ;;
 (mapcar (function (lambda (rel-tuple)
     (setf rel-name (first rel-tuple)
    save-dir (second rel-tuple))
     (setf pathname (concatenate 'string save-dir database-name "-" rel-name "."))
     (setf path nil)
     (cond ((setf path (probe-file (concatenate 'string pathname "XLD")))
     (cond ((not (member rel-name *system-relations* :test 'string-equal))
     (rename-file path (concatenate 'string save-dir new-database-name
        "-" rel-name "." "XLD#>")))
    (t
     (delete-file (concatenate 'string pathname "XLD")))))
    ((setf path (probe-file (concatenate 'string pathname "QFASL")))
     (cond ((not (member rel-name *system-relations* :test 'string-equal))
     (rename-file path (concatenate 'string save-dir new-database-name
        "-" rel-name "." "XLD#>")))
    (t
     (delete-file (concatenate 'string pathname "QFASL")))))
    ((setf path (probe-file (concatenate 'string pathname "XFASL")))
     (cond ((not (member rel-name *system-relations* :test 'string-equal))
     (rename-file path (concatenate 'string save-dir new-database-name
        "-" rel-name "." "XLD#>")))
    (t
     (delete-file (concatenate 'string pathname "XFASL")))))
    ((setf path (probe-file (concatenate 'string pathname "LISP")))
     (cond ((not (member rel-name *system-relations* :test 'string-equal))
     (rename-file path (concatenate 'string save-dir new-database-name
        "-" rel-name "." "XLD#>")))
    (t
     (delete-file (concatenate 'string pathname "LISP"))))))))
 relation-tuple-list))
(t (save-database *active-db*)))
  (cond (*provide-status-messages*
 (format *standard-output* "~%The database ~S has been renamed to ~S." database-name new-database-name)
 (format *standard-output* "~%Renaming database completed.")))
  (return-from rename-database new-database-name)))

(deff rename-rel 'rename-relation)

(defun rename-relation (&rest relations
&aux last-new-relation-name pos relation-tuple rel-owner-id imp pathname path save-dir
(result nil) db-relations-list relation-tuple-list (call-save-db nil))
  "Rename relations in the active database.

   RELATIONS - Specify <old-rel-name new-rel-name>

   Example: (RENAME-RELATION rel1 new-rel1 rel2 new-rel2)"
  (block rename-relation
(if (not (active-database))
    (return-from rename-relation nil))
(setf relations (do ((rel-l relations (car rel-l)))
    ((or (not (listp (car rel-l))) (> (length rel-l) 1)) rel-l) ()))
(setf relation-tuple-list
      (funcall (find-symbol (concatenate 'string "RETRIEVE-" *system-relation-base-implementation* "-"
      *system-relation-storage-structure*) *pkg-string*)
       'system-relation *system-relation-attributes* '("RELATION-NAME" "OWNER-ID")
       *system-relation-key* t nil 'system-relation))
;;
;; Take the system relations away from the list relation-tuple-list, because the user is not allowed to rename the system relations.
(setf db-relations-list (do ((rel-l relation-tuple-list (cdr rel-l)))
     ((null rel-l) (reverse result))
   (if (not (member (caar rel-l) *system-relations* :test 'string-equal))
       (setf result (cons (caar rel-l) result)))))
(if (not (do ((relation-name (first relations) (first relations))
      (new-relation-name (second relations)(second relations))
      (relations (cddr relations)(cddr relations)))
     ((null new-relation-name)(return t))
   (setf relation-name (validate-sym relation-name))
   (setf new-relation-name (validate-sym new-relation-name))
   (if (or (null relation-name) (null new-relation-name))
       (return-from rename-relation nil))
   (cond ((null (setf pos (position (string-upcase relation-name) db-relations-list
       :test 'equal)))
  (if *provide-error-messages*
      (if (member (string-upcase relation-name) *system-relations* :test 'string-equal)
   (format *standard-output*
    "~%ERROR - The relation ~S cannot be renamed because it is a system relation."
    relation-name)
   (format *standard-output*
    "~%ERROR - The relation ~S is not defined in the database ~S."
    relation-name *active-db*)))
  (return-from rename-relation nil))
 ((or (member (string-upcase new-relation-name) db-relations-list :test 'string-equal)
      (member (string-upcase new-relation-name) *system-relations* :test 'string-equal))
  (if *provide-error-messages*
      (format *standard-output*
       "~%ERROR - The relation ~S is already defined in the database ~S."
       new-relation-name *active-db*))
  (return-from rename-relation nil))
 (t
  (setf db-relations-list (append (firstn pos db-relations-list)
      (cons (string-upcase new-relation-name)
     (nthcdr (1+ pos) db-relations-list))))))))
    (return-from rename-relation nil))
(if (not (do ((relation-name (validate-sym (first relations)) (first relations))
      (new-relation-name (validate-sym (second relations)) (second relations))
      (relations (cddr relations)(cddr relations)))
     ((null new-relation-name)(return-from rename-relation t))
   (setf relation-tuple (cadr (get-relation relation-name '("OWNER-ID" "SAVE-DIRECTORY"
          "ATTRIBUTES" "KEY" "TUPLE-FORMAT"
          "IMPLEMENTATION-TYPE"
          "STORAGE-STRUCTURE" "DOC")
       nil)))
   (setf rel-owner-id (first relation-tuple)
 imp (sixth relation-tuple))
   (funcall (find-symbol (concatenate 'string "RENAME-RELATION-" imp) *pkg-string*)
    relation-name new-relation-name relation-tuple)
   (setf save-dir (second relation-tuple))
   (setf pathname  (concatenate 'string save-dir *active-db* "-"
   (string relation-name) "."))
   (setf path nil)
   (cond ((setf path (probe-file (concatenate 'string pathname "XLD")))
  (rename-file path (concatenate 'string save-dir *active-db* "-"
     (string-upcase new-relation-name) "." "XLD#>")))
 ((setf path (probe-file (concatenate 'string pathname "XFASL")))
  (rename-file path (concatenate 'string save-dir *active-db* "-"
     (string-upcase new-relation-name) "." "XLD#>")))
 ((setf path (probe-file (concatenate 'string pathname "QFASL")))
  (rename-file path (concatenate 'string save-dir *active-db* "-"
     (string-upcase new-relation-name) "." "XLD#>")))
 ((setf path (probe-file (concatenate 'string pathname "LISP")))
  (rename-file path (concatenate 'string save-dir *active-db* "-"
     (string-upcase new-relation-name) "." "XLD#>")))
 (t
  (setf call-save-db t)))
   (cond (path
  (save-system-relations)))
   (if *provide-status-messages*
       (format *standard-output* "~%The relation ~S has been renamed to ~S."
       relation-name new-relation-name))
   (setf last-new-relation-name new-relation-name)))
    (return-from rename-relation nil))
(if (and *auto-save* call-save-db)
    (save-database *active-db*))
(if *provide-status-messages*
    (format *standard-output* "~%Renaming relations completed."))
(return-from rename-relation last-new-relation-name)))


(defun rename-relation-array (relation-name new-relation-name ignore
      &aux array-name new-array-name)
  (multiple-value-setq (array-name)
    (intern (read-from-string (concatenate 'string relation-name "ARRAY"))))
  (multiple-value-setq (new-array-name)
    (intern (read-from-string (concatenate 'string new-relation-name "ARRAY"))))
  (eval `(copy-array-contents ,array-name ,new-array-name))
  (rename-relation-utility-array-list relation-name new-relation-name))

(defun rename-relation-flavor (relation-name new-relation-name relation-tuple)
  (rename-relation-utility-redef-rel relation-name new-relation-name relation-tuple))

(defun rename-relation-list (relation-name new-relation-name ignore)
  (rename-relation-utility-array-list relation-name new-relation-name))

(defun rename-relation-struct (relation-name new-relation-name relation-tuple)
  (rename-relation-utility-redef-rel relation-name new-relation-name relation-tuple))

(defun rename-relation-utility-array-list (relation-name new-relation-name)
    (delete-or-modify 'system-relation t  (list 'string-equal 'relation-name (string-upcase relation-name))
       '("MODIFIEDP" "RELATION-NAME")
       (list 't (eval `(string-upcase (quote ,new-relation-name)))))
     (delete-or-modify 'system-attribute t (list 'string-equal 'relation-name (string-upcase relation-name))
       '("RELATION-NAME") (list (eval `(string-upcase (quote ,new-relation-name)))))
     (delete-or-modify 'system-index t (list 'string-equal 'relation-name (string-upcase relation-name))
       '("RELATION-NAME") (list (eval `(string-upcase (quote ,new-relation-name)))))
     (putp new-relation-name (getp relation-name 'entry-point) 'entry-point)
     (putp relation-name nil 'entry-point))

(defun rename-relation-utility-redef-rel (relation-name new-relation-name relation-tuple
    &aux system-attribute-list attr-des-pair tuple-list index-list
    imp ss tuple-format-list attribute-list key-list doc dir domains)
  (setf system-attribute-list (get-system-attribute-list relation-name))
  (setf imp (sixth relation-tuple)
ss (seventh relation-tuple)
tuple-format-list (fifth relation-tuple)
attribute-list (third relation-tuple)
key-list (fourth relation-tuple)
doc (nth 7 relation-tuple)
dir (second relation-tuple))
  (setf attr-des-pair (create-attr-descriptor attribute-list system-attribute-list))
  (setf tuple-list (retrieve relation-name 'tuples t))
  (define-relation new-relation-name attr-des-pair
    'imp imp 'modifiedp t 'sto ss 'key key-list'format tuple-format-list 'doc doc 'dir dir)
  (delete-or-modify 'system-index t (list 'string-equal 'relation-name (string relation-name))
     '("RELATION-NAME") (list (eval `(string-upcase (quote ,new-relation-name)))))
  (setf index-list (qtrieve 'system-index *system-index-attributes* '("INDEX-NAME" "INDEX-TYPE" "KEY")
    *system-index-key*
    (list 'string-equal 'relation-name (string-upcase new-relation-name))))
  (cond (index-list
 (setf domains (caar (qtrieve 'system-relation *system-relation-attributes* '("DOMAINS")
       *system-relation-key*
       (list 'string-equal 'relation-name (string-upcase new-relation-name)))))))
  (mapc (function (lambda (index-info)
    (funcall (find-symbol (concatenate 'string "DELETE-" imp "-" (second index-info))
    *pkg-string*)
     new-relation-name attribute-list (third index-info) nil t (first index-info))))
index-list)
  (if tuple-list
      (insert new-relation-name (list 'tuples tuple-list)))
  (destroy-relation relation-name))
	äêÊPBQJˇ€ˇ›®
‰ÜQBQ	äê‰ÄQBQ
ää¸ÄQBQäíÄ¡A≈ΩÁÅQä@¡Å€Ç€ÖQäÜQäê‰@QÖQPíÜQPííär‰ÉQÇ¡@QC¡v‰CSB√ÖQPíär‰ÅQD€D—ÉQF¡E¡¸EQFSG¡ÖQGQíäCE√¡F≈FÛÁDQT¸BQÜQPíär‰ÅQF€F—ÑQD¡H¡¸HQDSI¡ÜQIQíäCH√¡D≈DÛÁFQíÅ¡ÑQÇ¡5¸PBQäPò‰BQ
ääÑQPò‰ÇQBQ
äääíÇ¡ÅQBQ¸BQäÉQPò
‰ÖQBQíÅQPòÊÅQÖQ¸ÇQBQääíÇ¡ÅQÜQBQíääíÅ¡C≈äÁÄQÅQÇQÉOÄCBÄ‘ÄÄBÄ›ÄÎÄ$8îÜÄB$FÄ\¿$Ä¿BÄ:BÄV]ÄFÄÄLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540810. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "XLD" :NAME "RENAME" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760360779. :AUTHOR "REL3" :LENGTH-IN-BYTES 4902. :LENGTH-IN-BLOCKS 10. :BYTE-SIZE 16.)                                      pp2Ä\Ä©ÅCOMPILE-DATA\ÄÏÄSW-MFG,ÅGODZILLAÜÄá§\—FÄFÄ\Äp¿,ÅCOMPILER,ÅVERSIONÄ\ÄFÄFÄp¿BÄ),ÇOPTIMIZE-SWITCHÄÜÄ©ÉQFASL-SOURCE-FILE-UNIQUE-IDÄ1Ä\Äp¿lÄFSÏÇMAKE-FASLOAD-PATHNAMEÄ\ÄÍÄQUOTEÄBÄ$\ÄBÄ8™ÄNILÄ\ÄBÄ8\Ä¨ÄRTMS\ÄBÄ8ÏÄRENAME\ÄBÄ8¨ÄLISP\ÄBÄ8FÄ©ÄBASEFÄ
ÈÄFONTSÄ\Ä©Å*CODE-FONT*ÄÈÅ*COMMENT-FONT*ÈÅ*STRING-FONT*Ä)ÅPACKAGEÄ©ÄRTMS©ÄMODE©ÅCOMMON-LISPÄÇRENAME-ATTRIBUTEOÄPÉÅRENAME-ATTRÄÄBÄPÄÎÄ!T…ÜÄ`!xFÄu¿$Ä¿BÄ:p¿¨ÄTICLÏÄART-QÄ]ÄFÄÄ:BÄ:BÄ:jÄTÄFÄp¿¨ÄSYSÄlÇDEBUG-INFO-STRUCTÄBÄP\Ä√ÅRELATION-NAMEÄÍÄ&RESTÄCÅATTRIBUTESBÄ:\ÄBÄc√ÅRELATION-TUPLEÉÄPOSÄ√ÄPOS-KÄÉÄSTOÄÉÅREL-OWNER-ID√ÅATTRIBUTE-LISTÅKEY-LISTÉÄIMPÄ√ÄATTR-L√ÅATTRIBUTE-NAMECÇNEW-ATTRIBUTE-NAMEBÄcBÄ:\Ä)ÇMACROS-EXPANDEDÄ\ÄÍÄSECOND*ÅSEVENTHÄÍÄSIXTHÄÍÄFOURTHÍÄTHIRDÄÍÄFIRSTÄ™ÄPROG™ÄSETFÈÅDOCUMENTATIONÄÏ°Use this function to rename attributes in a relation.

   RELATION-NAME  - Name of the relation whose attributes are to be renamed.
   ATTRIBUTES     - Specify old-attribute and new-attribute names.

   Example: (RENAME-ATTRIBUTE 'parts 'number 'id 'name 'description).ÄÄÉÅ*PKG-STRING*—CÉ*PROVIDE-STATUS-MESSAGES*Ä—ÉÅ*ACTIVE-DB*Ä—É*PROVIDE-ERROR-MESSAGES*—CÇ*SYSTEM-RELATIONS*ëÇACTIVE-DATABASEÄ“ÉÅVALIDATE-SYM“CÇCONVERT-ATTRIBUTES“™ÅSTRING-EQUAL¿p¿BÄ]¨ÅMEMBER-TESTÄ“ÍÄTERPRI“lÜERROR - The attributes cannot be renamed because Ä¿™ÅWRITE-STRING“ÍÄPRIN1Ä“ÏÇ is a system relation.¿\Ä,ÅOWNER-IDÏÅSAVE-DIRECTORYlÅATTRIBUTES¨ÄKEYÄ¨ÅTUPLE-FORMAT¨ÇIMPLEMENTATION-TYPEÄlÇSTORAGE-STRUCTUREÄ¨ÄDOCÄ¿ÉÅGET-RELATION“ÏÇERROR - The relation Ä¿,Ñ is not defined in the database ¿ÍÄEQUALÄ¿p¿BÄ]lÅPOSITION*Ä“ÏÇERROR - The attribute ¿¨Ç is not defined in Ä¿eÄ.¿jÅWRITE-CHAR“¨Ñ is already defined in the relation ¿p¿BÄUÏÄFIRSTN“p¿BÄ],Å*APPENDÄ“ÏÅThe attribute ¿¨Ç will be renamed to ÄÄÊRÄQˇ›	íÄ¡ÊR@Q¸ISI¡ISˇ5˚ÁIQ@√
ä@¡ÄQPPò
‰‰ÄPàÄQàPàRÄQPˇ€öBA¡Ê	‰ÄPàÄQàPàPàRASE¡A[F¡AQBG¡AYBH¡AQBBD¡@S@W@YL¡K¡J¡Y¸JQˇ›	íJ¡ÊRJQFQPöB¡Ê‰ÄPàJQàPàÄQàPàRKQFQPò‰‰ÄPàKQàPàÄQàPàRBQFQíKQBkFQ
C
CíF¡JQGQPöC¡
‰CQGQíKQCkGQ
C
CíG¡‰ÄPàJQà PàKQàPàLSJ¡LWK¡L«K•ÁSOÄßBÄPÄÄ√ÇRENAME-ATTRIBUTE-ARRAYÄÎÄ
ÜÄ@FÄ¿$Ä¿BÄ:BÄW]ÄFÄÄ:BÄ:BÄ:BÄZFÄÄ_BÄ®\ÄBÄaBÄjBÄkBÄcÍÄIGNOREBÄ:BÄ:BÄ:ÄÉÑRENAME-ATTRIBUTE-UTILITY-ARRAY-LISTÄíÄQÅQÇQÉQ§OÄ≥BÄ®ÄÄÉRENAME-ATTRIBUTE-FLAVORÄÄÎÄ
ÜÄ@FÄ¿$Ä¿BÄ:BÄW]ÄFÄÄ:BÄ:BÄ:BÄZFÄÄ_BÄ¥\ÄBÄaBÄjBÄkBÄ±BÄeBÄ:BÄ:BÄ:ÄCÑRENAME-ATTRIBUTE-UTILITY-REDEF-RELíÄQÅQÇQÑQ§OÄæBÄ¥ÄÄ√ÇRENAME-ATTRIBUTE-LISTÄÄÎÄ
ÜÄ@FÄ¿$Ä¿BÄ:BÄW]ÄFÄÄ:BÄ:BÄ:BÄZFÄÄ_BÄø\ÄBÄaBÄjBÄkBÄcBÄ±BÄ:BÄ:BÄ:ÄBÄ≤íÄQÅQÇQÉQ§OÄ»BÄøÄÄÉRENAME-ATTRIBUTE-STRUCTÄÄÎÄ
ÜÄ@FÄ¿$Ä¿BÄ:BÄW]ÄFÄÄ:BÄ:BÄ:BÄZFÄÄ_BÄ…\ÄBÄaBÄjBÄkBÄ±BÄeBÄ:BÄ:BÄ:ÄBÄΩíÄQÅQÇQÑQ§OÄ“BÄ…ÄÄBÄ≤ÄÎÄGÜÄAFÄ,¿$Ä¿BÄ:BÄW]ÄFÄÄ:BÄ:BÄ:BÄZFÄÄ_BÄ≤\ÄBÄaBÄjBÄkBÄcBÄ:\ÄBÄnBÄoBÄc\ÄBÄq\ÄBÄsBÄxBÄyp¿BÄ]lÅXR-BQ-LISTÄÇSYSTEM-RELATIONÄ¿BÄÖ¿BÄa¿ÍÄSTRING“™ÄLIST“\ÄlÅMODIFIEDPÄlÅATTRIBUTES¨ÄKEYÄ¿BÄ8¿ÇDELETE-OR-MODIFY“ÇSYSTEM-ATTRIBUTE¿™ÄANDÄ¿BÄn¿\ÄÏÅATTRIBUTE-NAME¿BÄ·¿√ÇSAVE-SYSTEM-RELATIONSÄíPˇ›PPÄQäöPˇ›	PÅQí	PÇQíö
®ÉSÉWÉYB¡A¡@¡¸Pˇ›PPPÄQäöPP@QäööPP	PAQííä
®BS@¡BWA¡B«A„ÁÑOÄÌBÄ≤ÄÄBÄΩÄÎÄ<ÜÄA FÄ$¿$Ä¿BÄ:BÄW]ÄFÄÄ:BÄ:BÄ:BÄZFÄÄ_BÄΩ\ÄBÄaBÄjBÄkBÄeBÄ:\Ä√ÇSYSTEM-ATTRIBUTE-LISTÄ√ÅATTR-DES-PAIRÄCÅTUPLE-LISTBÄlCÄSSCÇTUPLE-FORMAT-LISTÄÉÄDOCÄÉÄDIRÄ\ÄBÄq\ÄBÄsÍÄFIFTHÄBÄtBÄuBÄzÄCÉGET-SYSTEM-ATTRIBUTE-LISTÄ“√ÇCREATE-ATTR-DESCRIPTOR“ÍÅSTRING-UPCASEÄ“*ÇREAD-FROM-STRING“√ÄTUPLES¿ÅRETRIEVE“ÉÅREDEFINE-REL“BÄÏ“√ÅSAVE-RELATIONÄíÄQä@¡ÉYBC¡ÉQBBD¡ÉUBE¡JÉQåCF¡ÉWG¡ÅQ@QíA¡ÄQääPˇ›öB¡ÄQääAQCQDQÇQEQFQGQBQ	J	∏
ÄÄQääåOÄ	BÄΩÄÇRENAME-DATABASEÄOÄ
CÅRENAME-DBÄÄBÄ
ÄÎÄ7y)ÜÄ`74FÄ∞¿$Ä¿BÄ:BÄW]ÄFÄÄ:BÄ:BÄ:BÄZFÄÄ_BÄ
\ÄBÄbCÅDATABASESÄBÄ:\ÄBÄ√ÅDATABASE-NAMEÄCÇNEW-DATABASE-NAMEÄ*ÅPATHNAMEÉÄPATHÅREL-NAMEÅSAVE-DIRÉÇRELATION-TUPLE-LISTÄÉÄDB-LBÄ:BÄ:BÄ:CÅREL-TUPLEÄ\ÄBÄq\Äp¿¨ÄZLCÄ,ÅDO-NAMEDp¿BÄUÏÇINHIBIT-STYLE-WARNINGSBÄsBÄxBÄyBÄzBÄ{,ïUsed to rename a database.

   DATABASES - Specify old-database-name and new-database-name.

   Example: (RENAME-DATABASE parts suppliers micro-parts micro-suppliers).ÄÄBÄ~—BÄÅ—√Ç*SYSTEM-RELATION-KEY*Ä—ÉÉ*SYSTEM-RELATION-ATTRIBUTES*—BÄ}—ÉÑ*SYSTEM-RELATION-STORAGE-STRUCTURE*Ä—√Ñ*SYSTEM-RELATION-BASE-IMPLEMENTATION*Ä—BÄÄ—BÄëBÄÇ“BÄÉ“p¿BÄ]ÏÅSTRING-EQUAL*Ä“BÄà“ÏáERROR - The database to rename has to be the active database Ä¿BÄä“BÄã“BÄû¿BÄü“ÏÉERROR - The new database name ¿,Ñ is identical to the actual name¿BÄ·¿lÅRETRIEVE-Ä¿lÄ-Ä¿™ÅCONCATENATEÄ“™ÅFIND-SYMBOLÄ“BÄ‡¿\ÄÏÅRELATION-NAMEÄÏÅSAVE-DIRECTORY¿CÇGET-SAVE-DIRECTORY“¨Ä.XLD¿jÅPROBE-FILE“ÏÄ.LISPÄ¿ÏÄ.XFASL¿ÏÄ.QFASL¿\ÄÍÄMEMBERBÄaBÄÅ©ÄTEST\ÄBÄ8BÄÖ¿\ÄlÅMODIFIEDPÄ¿BÄ‚“BÄÁ“BÄÏ“™ÅDELETE-FILEÄ“lÄ.Ä¿¨ÄXLDÄ¿BÄÖ¿BÄá“ÏÄXLD#>Ä¿™ÅRENAME-FILEÄ“ÏÄQFASLÄ¿ÏÄXFASLÄ¿¨ÄLISP¿√ÅSAVE-DATABASEÄ“ÏÅThe database Ä¿ÏÇ has been renamed to Ä¿¨ÉRenaming database completed.ÄÄÊR@Q¸HSH¡HSˇ5˚ÁHQ@¡@Sˇ›íA¡@Wˇ›íB¡A‰BÊRAQPê
Ê
‰ÄPàPàPàRBQAQê
‰
‰ÄPàBQàPàRBQ¿PP	PPP™PíI¡PPPPˇ›ˇ€PJIªG¡ÇF¡PFQAQP¢ ä‚PFQAQ!P¢ ä‚PFQAQ"P¢ ä‚PFQAQ#P¢ äD¡v‰Pˇ›$P%Pˇ›&ä'®(ÄDQ)àI€I—GQK¡J¡c¸JQKSL¡LSE¡LWF¡PFQAQPEQ*P≤C¡D€PCQ+Pö äD¡‰EQP,P-òÊDQPFQBQPEQ*P.PJ∫/í7¸PCQ+Pö)ä1¸PCQ0Pö äD¡	‰EQP,P-ò‚ÂPCQ0PÌ˝PCQ1Pö äD¡	‰EQP,P-ò“ÂPCQ1P›˝PCQ2Pö äD¡	‰EQP,P-ò¬ÂPCQ2PÕ˝ˇ€CJ√¡K≈KõÁ¸P3à‰Ä4PàAQà5PàBQàPàÄ6PàBOÄOBÄ
ÄÇRENAME-RELATIONÄOÄPCÅRENAME-RELÄBÄPÄÎÄ7Ç;ÜÄ‡7FÄπ¿$Ä¿BÄ:BÄW]ÄFÄÄ:BÄ:BÄ:BÄZFÄÄ_BÄP\ÄBÄbCÅRELATIONSÄBÄ:\ÄBÄZ√ÇLAST-NEW-RELATION-NAMEBÄfBÄeBÄiBÄlBÄBÄBÄ√ÄRESULTCÇDB-RELATIONS-LISTÄBÄÉÅCALL-SAVE-DB√ÄREL-LÄBÄ:BÄaCÇNEW-RELATION-NAMEÄBÄZ\ÄBÄq\ÄBÄuBÄsBÄxBÄyBÄzBÄ{,ìRename relations in the active database.

   RELATIONS - Specify <old-rel-name new-rel-name>

   Example: (RENAME-RELATION rel1 new-rel1 rel2 new-rel2)Ä¿ÜÄÄÉÅ*AUTO-SAVE*Ä—BÄ~—BÄ—BÄÄ—BÄÅ—BÄ'—BÄ(—BÄ}—BÄ)—BÄ*ëBÄÇ“BÄ·¿lÅRETRIEVE-Ä¿lÄ-Ä¿BÄ2“BÄ3“BÄ‡¿\ÄÏÅRELATION-NAMEÄ,ÅOWNER-ID¿BÄÖ¿BÄá“*ÅREVERSEÄ“BÄÉ“BÄ“BÄô¿BÄõ“BÄà“ÏÇERROR - The relation Ä¿BÄä“BÄã“¨Ü cannot be renamed because it is a system relation.Ä¿,Ñ is not defined in the database ¿BÄû¿BÄü“¨Ñ is already defined in the database ¿BÄ¢“BÄ§“\Ä,ÅOWNER-IDÏÅSAVE-DIRECTORYlÅATTRIBUTES¨ÄKEYÄ¨ÅTUPLE-FORMAT¨ÇIMPLEMENTATION-TYPEÄlÇSTORAGE-STRUCTUREÄ¨ÄDOCÄ¿BÄñ“,ÇRENAME-RELATION-¿BÄ·“lÄ.Ä¿¨ÄXLDÄ¿BÄ9“ÏÄXLD#>Ä¿BÄG“ÏÄXFASLÄ¿ÏÄQFASLÄ¿¨ÄLISP¿BÄÏ“ÏÅThe relation Ä¿ÏÇ has been renamed to ÄÄÄÊR@Q¸MSM¡MSˇ5‰MQJô˜ÂMQ@¡PPPPP™PíN¡P
PP	Pˇ›ˇ€PJNªK√M¡‰MQBPPòÊMQBI]I¡M≈ÙÁIQäJ¡@S@W@YQ¡P¡O¡X¸OQäO¡PQäP¡O‰PÊROQäJQPöB¡Ê‰OQäPPò‰ÄPàOQ à!PàRÄPàOQ à"PàP à#P$àRPQäJQPòÊPQäPPò‰‰ÄPàPQ à%PàP à#P$àRBQJQ&íPQäBkJQ
C
C'íJ¡QSO¡QWP¡Q«P¶Á@Sä@Wä@YQ¡P¡O¡c¸OQ(Pˇ€)öBC¡CSD¡CYBE¡P*PEQöPíN¡OQPQCQNôCWH¡PHQPPOQ+ä,P≤F¡G€PFQ-Pö.äG¡‰GQPHQPPPQä,P/PJ∫0ê¸PFQ1Pö.äG¡ÏÁPFQ2Pö.äG¡ÂÁPFQ3Pö.äG¡ﬁÁL›G‰4Ä‰Ä5PàOQ à6PàPQ à#P$àPQA¡QSO¡QWP¡Q«PõÁSOÄÉBÄPÄÄ√ÇRENAME-RELATION-ARRAYÄÄÎÄ"ÜÄ@»FÄ¿$Ä¿BÄ:BÄW]ÄFÄÄ:BÄ:BÄ:BÄZFÄÄ_BÄÑ\ÄBÄaBÄaBÄ±BÄ:\ÄCÅARRAY-NAME√ÅNEW-ARRAY-NAME\ÄBÄq\ÄBÄﬂÄBÄ·¿ÏÄARRAYÄ¿BÄ2“BÄ“ÍÄINTERN“p¿BÄU¨ÇCOPY-ARRAY-CONTENTSÄ¿BÄ‚“™ÄEVAL“CÑRENAME-RELATION-UTILITY-ARRAY-LISTíPÄQPöää@¡PÅQPöääA¡P@QAQ	ö
àÄQÅQîOÄòBÄÑÄÄ√ÇRENAME-RELATION-FLAVORÄÎÄFÄ¿FÄ¿$Ä¿BÄ:BÄW]ÄFÄÄ:BÄ:BÄ:BÄZFÄÄ_BÄô\ÄBÄaBÄaBÄeBÄ:BÄ:BÄ:ÄCÑRENAME-RELATION-UTILITY-REDEF-RELÄíÄQÅQÇQúOÄ£BÄôÄÄÉÇRENAME-RELATION-LISTÄÎÄFÄ¿FÄ¿$Ä¿BÄ:BÄW]ÄFÄÄ:BÄ:BÄ:BÄZFÄÄ_BÄ§\ÄBÄaBÄaBÄ±BÄ:BÄ:BÄ:ÄBÄóíÄQÅQîOÄ≠BÄ§ÄÄ√ÇRENAME-RELATION-STRUCTÄÎÄFÄ¿FÄ¿$Ä¿BÄ:BÄW]ÄFÄÄ:BÄ:BÄ:BÄZFÄÄ_BÄÆ\ÄBÄaBÄaBÄeBÄ:BÄ:BÄ:ÄBÄ¢íÄQÅQÇQúOÄ∑BÄÆÄÄBÄóÄÎÄOFÄÄFÄ1¿$Ä¿BÄ:BÄW]ÄFÄÄ:BÄ:BÄ:BÄZFÄÄ_BÄó\ÄBÄaBÄaBÄ:BÄ:\ÄBÄq\ÄBÄﬂÄBÄ‡¿BÄÖ¿BÄa¿BÄ“BÄ‚“\ÄlÅMODIFIEDPÄÏÅRELATION-NAMEÄ¿BÄ¿BÄ8¿BÄñ“BÄÁ“BÄË¿\ÄÏÅRELATION-NAMEÄ¿ÉÅSYSTEM-INDEX¿ÉÅENTRY-POINTÄ¿ÉÄGETP“ÉÄPUTPíPˇ›PPÄQäöPˇ›	P
PÅQííäí®Pˇ›PPÄQäöP	P
PÅQííää®Pˇ›PPÄQäöP	P
PÅQííää®ÅQÄQPíPòÄQˇ€PúOÄÀBÄóÄÄBÄ¢ÄÎÄ*>¶ÜÄ@*¸FÄh¿$Ä¿BÄ:BÄW]ÄFÄÄ:BÄ:BÄ:BÄZFÄÄ_BÄ¢\ÄBÄaBÄaBÄeBÄ:\ÄBÄ˜BÄ¯BÄ˘CÅINDEX-LISTBÄlBÄ˙BÄ˚BÄjBÄkBÄ¸BÄ˝ÅDOMAINSÄBÄ:CÅINDEX-INFOBÄ:\ÄBÄq\ÄBÄxBÄyBÄ#BÄﬂBÄsBÄvBÄwBÄBÄtBÄuBÄzÄBÄ}—BÄ'—BÄ(—CÇ*SYSTEM-INDEX-KEY*—CÉ*SYSTEM-INDEX-ATTRIBUTES*ÄëBÄ“BÄ“BÄ¿BÄ“BÄl¿CÅMODIFIEDPÄ¿BÄh¿ÉÄKEYÄ¿ÍÄFORMAT¿BÄ¸¿BÄ˝¿ÇDEFINE-RELATIONÄ“BÄ«¿BÄÖ¿BÄa¿BÄ·“BÄ‚“\ÄÏÅRELATION-NAMEÄ¿BÄ¿BÄ8¿BÄñ“BÄÁ“\ÄlÅINDEX-NAMElÅINDEX-TYPE¨ÄKEYÄ¿BÄ“ÅQTRIEVEÄ“BÄ‡¿\Ä,ÅDOMAINSÄ¿BÄ·¿,ÅDELETE-Ä¿lÄ-Ä¿BÄ2“BÄ3“√ÄINSERT“ÇDESTROY-RELATIONíÄQä@¡ÇYBD¡ÇQBBE¡ÇUBF¡Ç[G¡ÇQBH¡JÇQåCI¡ÇWJ¡GQ@Q	íA¡ÄQ
Pˇ›öB¡ÅQAQPDQPˇ›PEQPHQPFQPIQPJQJ∏Pˇ›PPÄQäöPPPÅQííää®PPPPPPÅQäö ™C¡‰!PP"PPPPÅQäö ™BK¡CQL¡‰LSM¡#P$PDQ%PMW&™P'íN¡ÅQGQM[ˇ€ˇ›MSN±L≈ÏÁB‰ÅQ
PBQí(êÄQ)åOÄÌBÄ¢Ä1Ä\Äp¿BÄ],ÑFASL-RECORD-FILE-MACROS-EXPANDED\ÄBÄ8\Ä\Äp¿BÄU¨ÄDEFFÜÄb\ÄÍÄDEFUNÄÜÄ'\ÄBÄ%ÜÄ(Ã¢\ÄBÄ#ÜÄ*˝j\ÄBÄÜÄZiÛ\ÄBÄﬂÜÄ.Ÿã\ÄBÄzÜÄ[ÊÑ\ÄBÄyÜÄ=Ã#\ÄBÄxÜÄz(á\ÄBÄwÜÄ:}n\ÄBÄvÜÄxıø\ÄBÄuÜÄ{ƒ≤\ÄBÄtÜÄ2ª=\ÄBÄsÜÄ{öÕÄÄdir new-database-name
        "-" rel-name "." "XLD#>")))
    (t
     (delete-file (concatenate 'string pathname "XLD")))))
    ((setf path (probe-file (concatenate 'string pathname "QFASL")))
     (cond ((not (member rel-name *system-relations* :test 'string-equal))
     (rename-file path (concatenate 'string save-dir new-database-name
        "-" rel-name "." "XLD#>")))
    (t
     (delete-file (concatLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540814. :SYSTEM-TYPE :LOGICAL :VERSION 2. :TYPE "LISP" :NAME "RESTORE" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :CHARACTERS T :NOT-BACKED-UP T :CREATION-DATE 2753699816. :AUTHOR "REL3" :LENGTH-IN-BYTES 16986. :LENGTH-IN-BLOCKS 17. :BYTE-SIZE 8.)

;;; -*- Mode:Common-Lisp; Package:RTMS; Base:10; Fonts:(*CODE-FONT* *COMMENT-FONT* *STRING-FONT*) -*-
;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved.
;;; RESTORE
;;;
;;; This file contains the following Explorer extensions to CommonLisp Standard as Indicated in the June 1985 Explorer Lisp
;;; Reference
;;;     fs:directory-list
;;;     errset
;;;     deff
;;;  Change History
;;;  04.01.87  MRR  Changed the way Load-database, Load-environment, and Load-relation probe for the presence of a directory.
;;;                     Now databases and relations can be loaded remotely.
;;;  04.03.87  MRR  Fixed Load-database to catch Unknown-host-name error condition (SPR #4426).

(defun get-relation (relation project-list manip? &optional (saving? nil))
  (let (qtrieve-var)
    (setf project-list (convert-attributes project-list))
    (setf qtrieve-var (car (qtrieve 'system-relation *system-relation-attributes*
     (append '("DISK" "SAVE-DIRECTORY") project-list) *system-relation-key*
     `(string-equal relation-name ,(string relation)))))
    ;;
    ;; The relation does not exist ... it could be a view, however views can not be modified in the current system, so do not retrieve
    ;; the view definition if the current operation would manipulate it.
    (cond ((and (null qtrieve-var) (null manip?))
   (setf qtrieve-var (caar (qtrieve 'system-view *system-view-attributes* '("VIEW-DEFINITION")
      *system-view-key*
      (list 'and (list 'string-equal 'view-name (string relation))
     (list 'string-equal 'owner-id user-id)))))
   ;;
   ;; A view is defined by evaluation of the view definition
   (if qtrieve-var
       (get-relation (setf relation (eval qtrieve-var)) project-list manip?)
       (list relation nil)))
  ((null qtrieve-var)
   (list relation nil))
  ((and qtrieve-var (car qtrieve-var)(not saving?))
   (load-relation relation 'dir (cadr qtrieve-var))
   (list relation (cddr qtrieve-var)))
  (t
   (list relation (cddr qtrieve-var))))))

(deff load-db 'load-database)

(defun load-database (dbname &rest keyword-list
      &key &optional directory
      &allow-other-keys
      &aux pathname temp-dir temp-status (dir-changed? nil))
  "A database saved on the disk can be loaded using this function.

   DBNAME    - Name of the database to be restored.
   DIRECTORY - Name of the directory in which it can be found."
  directory
  (block load-database
       (unwind-protect
   (progn
     ;;
     ;;  If there is an activedb, determine if any relation has been modified, if so do not do anything which would provoke
     ;; those relations, i.e. terminate the function.
     ;;
     (cond ((active-database 'restore)
    (cond ((car (funcall
   (find-symbol (concatenate 'string "RETRIEVE-"
        *system-relation-base-implementation*
          "-" *system-relation-storage-structure*)
         *pkg-string*)
   'system-relation *system-relation-attributes* '("RELATION-NAME")
   *system-relation-key* (list 'string-equal 'modifiedp "T") nil
   'system-relation))
     (when *provide-error-messages* ;mrr 04.03.87 - changed "if" to "when"
       (format *standard-output*
        "~%ERROR - ~s is the current database and it has modified relations"
        *active-db*)
       (format *standard-output*
        "~%          Please resolve this conflict by either saving or destroying this database")
       (format *standard-output*
        "~%          before restoring a saved database"))
   (return-from load-database nil)))))
     (cond ((null (setf dbname (validate-sym dbname t)))
    (return-from load-database nil)))
     (setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
     ((or (null keyword-list)(not (listp (car keyword-list)))) keyword-list)))
     (if (not keyword-list)
 (setf keyword-list (list 'dir *save-directory*)))
     (setf keyword-list (get-keyword-value-prereq '(dir) keyword-list))
     (setf pathname (concatenate 'string (setf temp-dir (get-directory keyword-list))
     dbname "-" "system-relation" "."))
     (cond ((errset (fs:directory-list temp-dir) nil) ;mrr 04.01.87
      (setf *save-directory* temp-dir)
      (setf *restore-operation* nil
     *donot-commit* t)
      (cond ((or
     (probe-file (setf pathname (concatenate 'string *save-directory* dbname ".XLD")))
     (probe-file (setf pathname (concatenate 'string *save-directory* dbname
          ".XFASL")))
     (probe-file (setf pathname (concatenate 'string *save-directory* dbname ".LISP"))))
   (if (active-databasep)
       (destroy-database *active-db*))
   (errset (load pathname :verbose (if *provide-status-messages*
         t
         nil))))
  (t
   (if *provide-error-messages*
       (format *standard-output*
        "~%ERROR - Database ~s does not exist in directory ~s"
        dbname *save-directory*))
   (setf *donot-commit* nil)
   (return-from load-database nil)))
    (setf *restore-operation* t
  *donot-commit* nil)
    ;;The DEFDB would have set the property COMMIT-TUPLES to a list of system-tuples which will be duplicated in
    ;; the following call. So, reset them.
    (mapcar (function (lambda (sys-rel)
  (putp sys-rel nil 'commit-tuples)))
    *system-relations*)
    (mapcar (function (lambda (relation)
  (load-relation relation 'dir temp-dir)
  (setf *restore-operation* t)))
    (reverse *system-relations*))
    (setf *restore-operation* nil)
    ;;We will get rid of all the tuples for relations which might have been saved without saving the relations
    ;; themselves. (No way to restore them !)
    (mapc (function (lambda (sys-tup &aux rel path)
  (setf rel (car (project-list (list sys-tup)
          *system-relation-attributes*
          '("RELATION-NAME" "SAVE-DIRECTORY"))))
  (setf path (cadr rel)
        rel (car rel))
  (if (not (member rel *system-relations* :test 'string-equal))
      (unless (condition-case () ;mrr 04.03.87
      (or (probe-file (concatenate 'string path dbname "-" rel
         ".XLD"))
          (probe-file (concatenate 'string path dbname "-" rel
         ".LISP"))
           (probe-file (concatenate 'string path dbname "-" rel
         ".XFASL"))
           (probe-file (concatenate 'string path dbname "-" rel
         ".QFASL")))
           ((sys:network-error
      sys:unknown-host-name) nil))
   (if (or (probe-file (concatenate 'string temp-dir dbname "-"
        rel ".XLD"))
    (probe-file (concatenate 'string temp-dir dbname "-"
        rel ".LISP"))
    (probe-file (concatenate 'string temp-dir dbname "-" rel
             ".XFASL"))
    (probe-file (concatenate 'string temp-dir dbname "-" rel
             ".QFASL")))
       ;; The files corresponding to this relation have been transferred to the
       ;; directory same as that of the database.
     (delete-or-modify 'system-relation t
           `(string-equal relation-name ,rel)
         '("SAVE-DIRECTORY") (list temp-dir))
     (progn
       (setf temp-status *provide-status-messages*)
       (setf *provide-status-messages* nil)
       (destroy-rel rel)
       (setf *provide-status-messages* temp-status))))
      (if (not (string-equal path temp-dir))
   (setf dir-changed? t)))))
    (qtrieve 'system-relation *system-relation-attributes* *system-relation-attributes*
      *system-relation-key* t))
    (delete-or-modify 'system-relation t t '("MODIFIEDP") '(nil))
    (delete-or-modify 'system-relation t '(not (equal 0 (search "SYSTEM" relation-name)))
       '("DISK") '(t))
    ;;In case the files have been transferred from one machine to another.
    (if dir-changed?
(delete-or-modify 'system-relation t '(equal 0 (search "SYSTEM" relation-name))
    '("SAVE-DIRECTORY") (list temp-dir)))
    (init-where-opt)
    (cond ((qtrieve 'system-storage-structure *system-storage-structure-attributes*
     '("STORAGE-STRUCTURE-NAME") *system-storage-structure-key*
     '(string-equal storage-structure-name "ISAM"))
   (modify-tuples 'system-storage-structure 'attr '("STORAGE-STRUCTURE-NAME")
    'where '(string-equal storage-structure-name "ISAM")
    'values '("AVL"))
   (delete-or-modify 'system-optfunc nil '(string-equal storage-structure-type "ISAM"))
   (insert 'system-optfunc 'tuples
    '(("=" "AVL" "OPT-AVL-EQUAL" "RTMS")
      ("<" "AVL" "OPT-AVL-LT" "RTMS")
      (">" "AVL" "OPT-AVL-GT" "RTMS")
      ("<=" "AVL" "OPT-AVL-LT" "RTMS")
      (">=" "AVL" "OPT-AVL-GT" "RTMS")
      ("AND" "AVL" "OPT-AVL-AND" "RTMS")
      ("EQUAL" "AVL" "OPT-AVL-EQUAL" "RTMS")
      ("LESSP" "AVL" "OPT-AVL-LT" "RTMS")
      ("STRING-LESSP" "AVL" "OPT-AVL-LT" "RTMS")
      ("GREATERP" "AVL" "OPT-AVL-GT" "RTMS")
      ("STRING-GREATERP" "AVL" "OPT-AVL-GT" "RTMS")
      ("OR" "AVL" "OPT-AVL-OR" "RTMS")
      ("STRING-EQUAL" "AVL" "OPT-AVL-EQUAL" "RTMS"))))))
   (t
    (if *provide-error-messages*
(format *standard-output* "~%ERROR - The Directory ~S does not exist" temp-dir))
    (return-from load-database nil)))
     (return-from load-database dbname))
 (setf *restore-operation* nil))))

(deff load-env 'load-environment)

(defun load-environment (envname &rest keyword-list
 &key &optional directory &allow-other-keys
 &aux pathname dir)
  "Load a saved environment.

   ENVNAME   - Name of the environment to be restored.
   DIRECTORY - Name of the directory in which it can be found."
  directory
  (block load-environment
  (if (not (setf envname (validate-sym envname t)))
      (return-from load-environment nil))
  (setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
 ((or (null keyword-list)(not (listp (car keyword-list)))) keyword-list)))
  (setf keyword-list (get-keyword-value-prereq '(dir) keyword-list))
  (setf pathname (concatenate 'string
   (if (errset (fs:directory-list (setf dir (get-directory keyword-list))) nil) ;mrr 04.01.87
       dir
       (progn
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR -The directory ~S does not exist." dir))
 (return-from load-environment nil)))
   "rtms-environment-" envname))
  (cond ((or (setf pathname (probe-file (concatenate 'string pathname ".XLD")))
     (setf pathname (probe-file (concatenate 'string pathname ".XFASL")))
     (setf pathname (probe-file (concatenate 'string pathname ".LISP"))))
 (load pathname :verbose (if *provide-status-messages*
   t
        nil))
 (return-from load-environment envname))
((string-equal *environment-name* envname)
 (if *provide-status-messages*
     (format *standard-output* "~%Environment ~s defined" envname))
 (return-from load-environment envname))
(t
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - Environment ~s does not exist in directory ~s"
     envname (get-directory keyword-list)))
 (return-from load-environment nil)))))

;;;
;;;  RTMS will now look first for xfasl extensions for relation names.  9/24/85 SMC
;;;
(defun load-relation (relation-name &rest keyword-list
      &key &optional directory &allow-other-keys
      &aux indices pathname xldf xfaslf qfaslf lispf relation-info temp1 temp2 dir)
  "Load a saved relation.

   RELATION-NAME    - Name of the relation to be restored.
   DIRECTORY        - Name of the directory in which it can be found."
  directory
  (block load-relation
(unwind-protect
    (progn
      (if (not (active-database))
  (return-from load-relation nil))
      (if (null (setf relation-name (validate-sym relation-name t)))
  (return-from load-relation nil))
      (if (and (not (member relation-name *system-relations* :test 'string-equal))
       (not (relationp relation-name)))
  (progn
    (if *provide-error-messages*
(format *standard-output* "~%ERROR -The relation ~S does not exist in the database ~S"
 relation-name *active-db*))
    (return-from load-relation nil)))
       ;;
      ;; Right now we will support only QFASL and LISP type formats. If a relation is stored as QFASL it will be used. Later using
      ;; the message ':modified-date on the file stream, we will use the latest relation file. set the restore key
      ;;
      (setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
      ((or (null keyword-list)(not (listp (car keyword-list)))) keyword-list)))
      (if (null (cadr keyword-list))
  (setf keyword-list (list 'dir (if (member relation-name *system-relations*
        :test 'string-equal)
       *save-directory*
       (caar (qtrieve 'system-relation *system-relation-attributes*
        '("SAVE-DIRECTORY") *system-relation-key*
        (list 'string-equal 'relation-name
       (string-upcase relation-name))))))))
      (setf keyword-list (get-keyword-value-prereq '(dir) keyword-list))
      (errset
(progn
  (setf *restore-operation* t)
  ;;
  ;;get the directory and form the pathname
  ;;
  (setf pathname (concatenate 'string
    (if (errset (fs:directory-list (setf dir (get-directory keyword-list))) nil) ;mrr 04.01.87
        dir
        (progn
   (if *provide-error-messages*
       (format *standard-output*
        "~%ERROR -The directory ~S does not exist."
        dir))
   (return-from load-relation nil)))
    *active-db* "-" relation-name))
  ;;Find out if there are both QFASL and LISP formats for this relation. If so, restore the latest.
  (setf xldf (concatenate 'string pathname ".XLD")
xfaslf (concatenate 'string pathname ".XFASL")
qfaslf (concatenate 'string pathname ".QFASL")
lispf (concatenate 'string pathname ".LISP"))
  (if (and (probe-file xldf) (probe-file xfaslf))
      (progn
(if (> (send (setf temp1 (open xldf)) ':creation-date)
       (send (setf temp2 (open xfaslf)) ':creation-date))
    (load xldf :verbose (if *provide-status-messages*
         t
         nil))
    (load xfaslf :verbose (if *provide-status-messages*
        t
        nil)))
(close temp1)
(close temp2))
      ;;see if the relation exists as QFASL.
      ;;
      (if (not (probe-file xldf))
  ;;
  ;;A QFASL does not exist for the relation. See if LISP type file exists for the relation.
  ;;
  (cond ((probe-file xfaslf)
  (load xfaslf :verbose (if *provide-status-messages*
       t
       nil)))
 ((probe-file qfaslf)
  (load qfaslf :verbose (if *provide-status-messages*
       t
       nil)))
 ((probe-file lispf)
  (load lispf :verbose (if *provide-status-messages*
      t
      nil)))
 ((string-equal relation-name "SYSTEM-INDEX")
  (define-system-index)
  (setf *system-relations* (cdr *system-relations*))
  (commit-system-relation)
  (delete-or-modify 'system-relation t
      `(string-equal relation-name "SYSTEM-INDEX")
      '("MODIFIEDP") '(t)))
 (t
  (setf *restore-operation* nil)
  (if *provide-error-messages*
      (format *standard-output*
       "~%ERROR -The relation ~S does not exist in the database ~S"
       relation-name *active-db*))
  (return-from load-relation nil)))
  ;;
  ;;if it does then load the relation file
  ;;
  (load xldf :verbose (if *provide-status-messages*
       t
       nil))))))
      ;;
      ;;reset the restore key
      ;;
      (delete-or-modify 'system-relation t `(string-equal relation-name ,(string-upcase relation-name))
 '("DISK") '(nil))
      ;;
      ;;  Need to determine if there are indexes defined on this relation, if so  they must also be defined.
      ;;
      (cond ((and (not (member relation-name *system-relations* :test 'string-equal))
  (setf indices (qtrieve 'system-index *system-index-attributes*
    '("INDEX-NAME" "KEY" "INDEX-TYPE")
    *system-index-key*
    `(string-equal relation-name ,relation-name))))
     (setf relation-info (car (qtrieve 'system-relation *system-relation-attributes*
          '("ATTRIBUTES" "IMPLEMENTATION-TYPE"
     "STORAGE-STRUCTURE")
          *system-relation-key*
          (list 'string-equal 'relation-name
         (string-upcase relation-name)))))
     (mapc (function (lambda (index-info)
        (create-index-relation relation-name (first index-info)
          (first relation-info)
          (second index-info) (third index-info)
          (second relation-info) (third relation-info))))
   indices)))
      (setf *restore-operation* nil)
      (return-from load-relation relation-name))
  (setf *restore-operation* nil))))

 (relation-name new-relation-name relation-tuple)
  (rename-relation-utility-redef-rel relation-name new-relation-name relation-tuple))

(defun rename-relation-list (relation-name new-relation-name ignore)
  (rename-relation-utility-array-list relation-name new-relation-name))

(defun rename-relation-struct (relation-name new-relation-name relation-tuple)
  (rename-relation-utility-redef-rel relation-name new-relation-LMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540817. :SYSTEM-TYPE :LOGICAL :VERSION 2. :TYPE "XLD" :NAME "RESTORE" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760360130. :AUTHOR "REL3" :LENGTH-IN-BYTES 4432. :LENGTH-IN-BLOCKS 9. :BYTE-SIZE 16.)                                      pp2Ä\Ä©ÅCOMPILE-DATA\ÄÏÄSW-MFG,ÅGODZILLAÜÄá§“ŒFÄFÄ\Äp¿,ÅCOMPILER,ÅVERSIONÄ\ÄFÄFÄp¿BÄ),ÇOPTIMIZE-SWITCHÄÜÄ©ÉQFASL-SOURCE-FILE-UNIQUE-IDÄ1Ä\Äp¿lÄFSÏÇMAKE-FASLOAD-PATHNAMEÄ\ÄÍÄQUOTEÄBÄ$\ÄBÄ8™ÄNILÄ\ÄBÄ8\Ä¨ÄRTMS\ÄBÄ8,ÅRESTOREÄ\ÄBÄ8¨ÄLISP\ÄBÄ8FÄÈÄFONTSÄ\Ä©Å*CODE-FONT*ÄÈÅ*COMMENT-FONT*ÈÅ*STRING-FONT*Ä©ÄBASEFÄ
)ÅPACKAGEÄ©ÄRTMS©ÄMODE©ÅCOMMON-LISPÄÄÉÅGET-RELATIONÄÎÄXÜÄÑƒFÄ9¿$Ä¿BÄ:p¿¨ÄTICLÏÄART-QÄ]ÄFÄÄ:BÄ:BÄ:jÄTÄFÄp¿¨ÄSYSÄlÇDEBUG-INFO-STRUCTÄBÄP\ÄÅRELATIONÉÅPROJECT-LIST√ÄMANIP?jÅ&OPTIONALÄ\ÄÅSAVING?ÄBÄ:BÄ:\ÄÉÅQTRIEVE-VARÄ\Ä)ÇMACROS-EXPANDEDÄ\Äp¿BÄ\lÅXR-BQ-LIST™ÄSETFÄp¿BÄT,ÅUSER-IDÄ—CÇ*SYSTEM-VIEW-KEY*Ä—É*SYSTEM-VIEW-ATTRIBUTES*—√Ç*SYSTEM-RELATION-KEY*Ä—ÉÉ*SYSTEM-RELATION-ATTRIBUTES*ëCÇCONVERT-ATTRIBUTES“ÇSYSTEM-RELATIONÄ¿\Ä¨ÄDISKÏÅSAVE-DIRECTORY¿p¿BÄ\,Å*APPENDÄ“™ÅSTRING-EQUAL¿√ÅRELATION-NAMEÄ¿ÍÄSTRING“™ÄLIST“ÅQTRIEVEÄ“ÉÅSYSTEM-VIEWÄ¿\Ä,ÇVIEW-DEFINITIONÄ¿™ÄANDÄ¿CÅVIEW-NAMEÄ¿ÅOWNER-ID¿™ÄEVAL“BÄP“ÉÄDIRÄ¿√ÅLOAD-RELATIONÄíÅQäÅ¡	PP
PÅQíPPPÄQäö™B@¡ ÊÇÊPPPPPPPÄQäöPPPöö™B@¡‰@QäÄ√ÅQÇQúÄQˇ€î@˚Â@‰ÉÊÄQP@WòÄQ@YîOÄ BÄPÄ√ÅLOAD-DATABASEÄOÄäÅLOAD-DBÄÄBÄäÄÎÄmÍAÜÄ‡m@FÄW¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄä\Ä√ÄDBNAMEÍÄ&RESTÄÉÅKEYWORD-LIST™Ä&KEYBÄcjÅDIRECTORYÄjÇ&ALLOW-OTHER-KEYSÄBÄ:\ÄBÄñBÄò*ÅPATHNAMEÅTEMP-DIRÉÅTEMP-STATUSÄÉÅDIR-CHANGED?BÄ:BÄñBÄ:BÄ:BÄ:p¿BÄ\¨ÄTEMÄBÄ`ÅSYS-TUPÄÉÄRELÄÉÄPATHBÄ:BÄ:p¿BÄ\¨Å.CASE.ITEM.Ä\ÄBÄi\ÄBÄlp¿BÄT¨ÄSENDp¿BÄ\¨ÅSELECT-MEMQÄp¿BÄTlÇCONDITION-CASE-IFÄp¿BÄTÏÅCONDITION-CASEÍÄUNLESSp¿¨ÄZLCÄ,ÅDO-NAMEDp¿BÄTÏÇINHIBIT-STYLE-WARNINGSp¿BÄTlÇCONDITION-BIND-IFÄp¿BÄTÏÅCONDITION-BINDp¿BÄTÏÇCATCH-CONTINUATION-IFÄp¿BÄTlÇCATCH-CONTINUATIONp¿BÄTÏÄERRSET™ÄPROGBÄm™ÄWHENÈÅDOCUMENTATIONÄ¨ñA database saved on the disk can be loaded using this function.

   DBNAME    - Name of the database to be restored.
   DIRECTORY - Name of the directory in which it can be found.Ä¿ÜÄì Äp¿lÄEH¨Ç*CONDITION-HANDLERS*—√É*SYSTEM-STORAGE-STRUCTURE-KEY*—√Ñ*SYSTEM-STORAGE-STRUCTURE-ATTRIBUTES*Ä—CÇ*SYSTEM-RELATIONS*—CÉ*PROVIDE-STATUS-MESSAGES*Ä—√Å*DONOT-COMMIT*—ÉÇ*RESTORE-OPERATION*Ä—Ç*SAVE-DIRECTORY*—ÉÅ*ACTIVE-DB*Ä—É*PROVIDE-ERROR-MESSAGES*—BÄr—BÄs—ÉÅ*PKG-STRING*—ÉÑ*SYSTEM-RELATION-STORAGE-STRUCTURE*Ä—√Ñ*SYSTEM-RELATION-BASE-IMPLEMENTATION*Äë\ÄiÅDIRECTORYÄ¿p¿BÄ\ÏÅSTORE-KEYARGSÄ“FÄß¿ÅRESTOREÄ¿ÇACTIVE-DATABASEÄ“BÄ}¿lÅRETRIEVE-Ä¿lÄ-Ä¿™ÅCONCATENATEÄ“™ÅFIND-SYMBOLÄ“BÄu¿\ÄÏÅRELATION-NAMEÄ¿BÄ{¿CÅMODIFIEDPÄ¿lÄTÄ¿BÄ~“ÍÄTERPRI“,ÅERROR - ¿™ÅWRITE-STRING“ÍÄPRIN1Ä“ÏÜ is the current database and it has modified relations¿¨ä          Please resolve this conflict by either saving or destroying this databaseÄ¿¨Ö          before restoring a saved databaseÄ¿ÉÅVALIDATE-SYM“BÄá¿\ÄBÄá¿ÉGET-KEYWORD-VALUE-PREREQ“√ÅGET-DIRECTORYÄ“,Çsystem-relationÄ¿lÄ.Ä¿p¿,ÄÏÄG3813Ä¿FÄP¿ÍÄERRORÄ¿p¿BÄ\ÏÅERRSET-HANDLER¿p¿BÄ4ÏÅDIRECTORY-LIST“¨Ä.XLD¿jÅPROBE-FILE“ÏÄ.XFASL¿ÏÄ.LISPÄ¿ÇACTIVE-DATABASEP“ÇDESTROY-DATABASE“p¿BÄÏÄG3821Ä¿FÄê¿)ÅVERBOSEÄ¿™ÄLOAD“lÇERROR - Database Ä¿ÏÉ does not exist in directory Ä¿√ÅCOMMIT-TUPLESÄ¿ÉÄPUTP“*ÅREVERSEÄ“BÄà“BÄ“\ÄÏÅRELATION-NAMEÄÏÅSAVE-DIRECTORY¿BÄa“p¿BÄ\¨ÅMEMBER-TESTÄ“p¿BÄÏÄG3856Ä¿FÄ¿\Äp¿¨ÄNETÄÏÅNETWORK-ERRORÄp¿BÄlÇUNKNOWN-HOST-NAMEÄ¿p¿BÄ\¨ÇCONDITION-CASE-THROW¿ÏÄ.QFASL¿)ÇCONDITION-NAMESÄ¿BÄ¿BÄ¿BÄ|¿\ÄÏÅSAVE-DIRECTORY¿ÇDELETE-OR-MODIFY“ÉÅDESTROY-RELÄ“p¿BÄ\ÏÅSTRING-EQUAL*Ä“\ÄlÅMODIFIEDPÄ¿\ÄBÄ:¿\Ä™ÄNOTÄ\ÄÍÄEQUALÄÄ\ÄÍÄSEARCHÏÄSYSTEMBÄ|¿\Ä¨ÄDISK¿\ÄBÄY¿\ÄBÄ'Ä\ÄBÄ*ÏÄSYSTEMBÄ|¿√ÅINIT-WHERE-OPT“ÉSYSTEM-STORAGE-STRUCTURE¿\ÄÏÇSTORAGE-STRUCTURE-NAME¿\ÄBÄ{√ÇSTORAGE-STRUCTURE-NAME¨ÄISAM¿ÉÄATTR¿√ÄWHEREÄ¿ÍÄVALUES¿\Ä¨ÄAVLÄ¿√ÅMODIFY-TUPLESÄ“√ÅSYSTEM-OPTFUNC¿\ÄBÄ{√ÇSTORAGE-STRUCTURE-TYPE¨ÄISAM¿√ÄTUPLES¿\Ä\ÄlÄ=Ä¨ÄAVLÄÏÅOPT-AVL-EQUALÄ¨ÄRTMS\ÄlÄ<Ä¨ÄAVLÄlÅOPT-AVL-LT¨ÄRTMS\ÄlÄ>Ä¨ÄAVLÄlÅOPT-AVL-GT¨ÄRTMS\ÄlÄ<=¨ÄAVLÄlÅOPT-AVL-LT¨ÄRTMS\ÄlÄ>=¨ÄAVLÄlÅOPT-AVL-GT¨ÄRTMS\Ä¨ÄANDÄ¨ÄAVLÄ¨ÅOPT-AVL-ANDÄ¨ÄRTMS\ÄÏÄEQUALÄ¨ÄAVLÄÏÅOPT-AVL-EQUALÄ¨ÄRTMS\ÄÏÄLESSPÄ¨ÄAVLÄlÅOPT-AVL-LT¨ÄRTMS\Ä¨ÅSTRING-LESSP¨ÄAVLÄlÅOPT-AVL-LT¨ÄRTMS\Ä,ÅGREATERP¨ÄAVLÄlÅOPT-AVL-GT¨ÄRTMS\Ä,ÇSTRING-GREATERPÄ¨ÄAVLÄlÅOPT-AVL-GT¨ÄRTMS\ÄlÄOR¨ÄAVLÄlÅOPT-AVL-OR¨ÄRTMS\Ä¨ÅSTRING-EQUAL¨ÄAVLÄÏÅOPT-AVL-EQUALÄ¨ÄRTMS¿√ÄINSERT“ÏÇERROR - The Directory ¿,Ç does not existÄÄ@‰@QPˇ›A—†ˇ›Pˇ€UPà,‰PPPPP™PíF¡PPPPP P!P"öˇ€PJFª‰‰#Ä$P%àP&à'P%à#Ä(P%à#Ä)P%àˇ€]¨ZˇÄQˇ›*íÄ¡ıÂ@Q¸GSG¡‰GSˇ5˙ÁGQ@¡Ê+PP"í@¡,P@Q-í@¡P@Q.äC√ÄQP/P0P≤B¡1P2PT3P4P1Pˇ€JCH√PJCI√÷CQ5ä"äJ!BJ!Bˇ\¸\ˇrïCQ¿
⁄	‹PPÄQ6P¢B√7àÊPPÄQ8P¢B√7àÊPPÄQ9P¢B√7à#‰:Ä‰P;à<P=PT3P4P<Pˇ›JCF√PJCJ√÷BQ>P‰ˇ›¸ˇ€?ö"àE\¸\K¡¸	‰#Ä@P%àÄQ&àAP%àP&à	⁄n˝
‹	⁄J€J—PH¡F¡	¸FQHSˇ€BPCöCF√¡H≈HıÁJ€J—PDäH¡F¡¸FQHSL√+PCQEò
‹ˇ›CF√¡H≈HÚÁ
⁄PPPPˇ›F™I¡ï‰ISM¡N€O€MQ"äPGPHöBN¡NWO¡NSN√PPIò{ÊJPKPTLPMPJPJCP√PJCQ√÷POQÄQPNQ6P≤7ä‚POQÄQPNQ9P≤7ä‚POQÄQPNQ8P≤7ä‚POQÄQPNQNP≤7äJ!BJ!Bˇ\
¸\Q¡OPQãR¡PPRÊQP¸<ÊPCQÄQPNQ6P≤7àÊPCQÄQPNQ9P≤7àÊPCQÄQPNQ8P≤7à	ÊPCQÄQPNQNP≤7à‰Pˇ›PRPNQ"öSPCQ"äT®¸PD¡⁄NQUàDQ¿¸OQCQVêÊE›I≈kÁPˇ›ˇ›WPXPT®Pˇ›YPZP[PT®E‰Pˇ›\PSPCQ"äT®]Ä^PP_PP`PF®‰^PaP_PbP`PcPdPJe∏fPˇ€gPTòfPhPiPjò¸r#ÄkP%àCQ&àlP~ÄQ]¨ZˇJ]¨ZP
⁄OÄäBÄäÄÇLOAD-ENVIRONMENTOÄãÅLOAD-ENVÄBÄãÄÎÄ&B™ÜÄ`&\FÄh¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄã\ÄÅENVNAMEÄBÄïBÄñBÄóBÄcBÄòBÄôBÄ:\ÄBÄñBÄòBÄõBÄáBÄñBÄ:BÄ:\ÄBÄi\ÄBÄ∑BÄπBÄªBÄΩBÄøBÄ¿BÄmBÄ¬,íLoad a saved environment.

   ENVNAME   - Name of the environment to be restored.
   DIRECTORY - Name of the directory in which it can be found.ÄBÄ«—CÇ*ENVIRONMENT-NAME*—BÄÀ—BÄ–ë\ÄBÄ’¿BÄ◊“BÄÍ“\ÄBÄá¿BÄÏ“BÄ}¿p¿BÄÏÄG3959Ä¿FÄÑ¿BÄÙ¿BÄˆ¿BÄÌ“BÄ¯“BÄ~“BÄ„“ÏÇERROR -The directory Ä¿BÄÂ“BÄÊ“,Ç does not exist.¿lÇrtms-environment-Ä¿BÄ›“¨Ä.XLD¿BÄ˙“ÏÄ.XFASL¿ÏÄ.LISPÄ¿BÄ¿BÄ“BÄ “¨ÅEnvironment ¿,Å defined¿¨ÇERROR - Environment ¿ÏÉ does not exist in directory ÄÄ@‰@QPˇ›A—†ÄQˇ›	íÄ¡ÊR@Q¸DSD¡‰DSˇ5˙ÁDQ@¡
P@Qí@¡PPPTPPPˇ€JCE√PJCF√÷@QäC√ääJ!BJ!Bˇ\¸\ˇ‰CQ
¸‰ÄPàCQàPàRPÄQ¢B¡PBQPöäB¡ÊPBQPöäB¡ÊPBQPöäB¡	‰BQP‰ˇ›¸ˇ€ òÄPÄQ!ê
‰‰Ä"PàÄQà#PàÄ
‰Ä$PàÄQà%Pà@QäàROÄ™BÄãÄÄBÄàÄÎÄJ•îÜÄ‡J@FÄÔ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄà\ÄBÄ|BÄïBÄñBÄóBÄcBÄòBÄôBÄ:\ÄBÄñBÄòÅINDICESÄBÄõÉÄXLDF√ÄXFASLF√ÄQFASLF√ÄLISPFÄ√ÅRELATION-INFOÄ√ÄTEMP1Ä√ÄTEMP2ÄBÄáBÄñBÄ:BÄ:BÄ:BÄ:BÄ†CÅINDEX-INFO\ÄBÄi\ÄÍÄTHIRDÄÍÄSECONDÍÄFIRSTÄBÄ≥BÄlBÄ©BÄ∑BÄπBÄªBÄΩBÄøBÄ¿BÄmBÄ¬,ìLoad a saved relation.

   RELATION-NAME    - Name of the relation to be restored.
   DIRECTORY        - Name of the directory in which it can be found.¿ÜÄì ÄBÄ«—CÇ*SYSTEM-INDEX-KEY*—CÉ*SYSTEM-INDEX-ATTRIBUTES*Ä—BÄÀ—BÄÕ—BÄr—BÄs—BÄŒ—BÄœ—BÄ–—BÄ ë\ÄBÄ’¿BÄ◊“FÄ◊¿BÄ⁄“BÄÍ“BÄ{¿BÄ“CÅRELATIONPÄ“BÄ„“¨ÇERROR -The relation ¿BÄÂ“BÄÊ“,Ñ does not exist in the database ¿BÄá¿BÄu¿\ÄÏÅSAVE-DIRECTORY¿BÄ|¿ÍÅSTRING-UPCASEÄ“BÄ~“BÄ“\ÄBÄá¿BÄÏ“p¿BÄÏÄG4015Ä¿FÄó¿BÄÙ¿BÄˆ¿BÄ}¿p¿BÄÏÄG4023Ä¿FÄ¿BÄÌ“BÄ¯“ÏÇERROR -The directory Ä¿,Ç does not exist.¿lÄ-Ä¿BÄ›“¨Ä.XLD¿ÏÄ.XFASL¿ÏÄ.QFASL¿ÏÄ.LISPÄ¿BÄ˙“ÈÅCREATION-DATEÄ¿™ÄOPEN“BÄ¿BÄ“ÍÄCLOSEÄ“¨ÅSYSTEM-INDEX¿BÄ “ÉÇDEFINE-SYSTEM-INDEXÄ“√ÇCOMMIT-SYSTEM-RELATION“\ÄBÄ{BÄ|¨ÅSYSTEM-INDEX¿\ÄlÅMODIFIEDPÄ¿\ÄBÄY¿BÄ“\Ä¨ÄDISK¿\ÄBÄ:¿ÉÅSYSTEM-INDEX¿\ÄlÅINDEX-NAME¨ÄKEYÄlÅINDEX-TYPE¿\ÄlÅATTRIBUTES¨ÇIMPLEMENTATION-TYPEÄlÇSTORAGE-STRUCTUREÄ¿√ÇCREATE-INDEX-RELATIONÄí@‰@QPˇ›A—†ˇ›Pˇ€UÄÊˇ€]‹ZˇÄQˇ›íÄ¡ıÂÄQPPòÊÄQàÊÎÂÄPàÄQàPàPà·˝@Q¸LSL¡‰LSˇ5˙ÁLQ@¡@ÊPÄQPPò‰P¸P
PP	PPPÄQ ä!ö"™B!í@¡#P@Q$í@¡%P&PT'P(P%Pˇ›JCM√PJCN√÷‹)P*P+PT'P(P*Pˇ€JCO√PJCP√÷@Q,äK√-ä!äJ!BJ!Bˇ\¸\ˇ‰KQ¸‰Ä.PàKQà/Pàˇ€\˝P0PÄQ1™C¡)PCQ2P1öD¡)PCQ3P1öE¡)PCQ4P1öF¡)PCQ5P1öG¡DQ6à‰EQ6à‰7PDQ8äI√ˇã7PEQ8äJ√ˇãy‰DQ¸EQ9P‰ˇ›¸ˇ€:òIQ;àJQ;ä5¸DQ6à*ÊEQ6à‰EQ&¸FQ6à‰FQ!¸GQ6à‰GQ¸ÄQ<P=ê
‰>Äƒ?ÄPˇ›@PAPBPC™¸⁄†ÂÄPàÄQàPàPàñ˝DQ9P‰ˇ›¸ˇ€:ö!àE\¸\Q¡Pˇ›PPÄQ ä!öDPEPC®ÄQPPò'ÊFPPGPPPPÄQ!ö"™B¡‰P
PHP	PPPÄQ ä!ö"™BH¡BQO¡‰OSR¡ÄQRSHSRWR[HWH[JI∏O≈ÛÁ⁄ÄQ]‹ZˇJ]‹ZP⁄OÄÙBÄàÄ1Ä\Äp¿BÄ\,ÑFASL-RECORD-FILE-MACROS-EXPANDED\ÄBÄ8\Ä\ÄÍÄDEFUNÄÜÄ'\Äp¿BÄT¨ÄDEFFÜÄb\ÄBÄ¡ÜÄz(á\ÄBÄ¿ÜÄ{öÕ\ÄBÄøÜÄ:}n\ÄBÄ¡ÜÄ,a\ÄBÄ¿ÜÄ=Ã#\ÄBÄøÜÄ-i\ÄBÄΩÜÄ~…z\ÄBÄªÜÄ<pë\ÄBÄπÜÄ`sN\ÄBÄ∑ÜÄ|ƒÙ\ÄBÄµÜÄ(Ã¢\ÄBÄ≥ÜÄ*˝j\ÄBÄ∞ÜÄ6\ÄBÄØÜÄ+≠ñ\ÄBÄ≠ÜÄjA¥\ÄBÄ´ÜÄ<ió\ÄBÄ©ÜÄaM*\ÄBÄmÜÄ[ÊÑ\ÄBÄlÜÄ.ŸãÄÄVL-AND" "RTMS")
      ("EQUAL" "AVL" "OPT-AVL-EQUAL" "RTMS")
      ("LESSP" "AVL" "OPT-AVL-LT" "RTMS")
      ("STRING-LESSP" "AVL" "OPT-AVL-LT" "RTMS")
      ("GREATERP" "AVL" "OPT-AVL-GT" "RTMS")
      ("STRING-GREATERP" "AVL" "OPT-AVL-GT" "RTMS")
      ("OR" "AVL" "OPT-AVL-OR" "RTMS")
      ("STRING-EQUAL" "AVL" "OPT-AVL-EQUAL"LMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540820. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "LISP" :NAME "RETRIEVE" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :CHARACTERS T :NOT-BACKED-UP T :CREATION-DATE 2749846786. :AUTHOR "REL3" :LENGTH-IN-BYTES 54403. :LENGTH-IN-BLOCKS 54. :BYTE-SIZE 8.)

;;; -*- Mode:Common-Lisp; Package:RTMS; Base:10; Fonts:(*CODE-FONT* *COMMENT-FONT* *STRING-FONT*) -*-
;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved.
;;; RETRIEVE
;;;
;;; This file contains the following Explorer extensions to CommonLisp Standard as Indicated in the June 1985 Explorer Lisp
;;; Reference
;;;     firstn
;;;     errset
;;;     deff
;;;
;;; The following function contains flavor references and thus are incompatable with CommonLisp. Their removal will not
;;; effect the functionality of RTMS.
;;;     retrieve-flavor-hash
;;;     retrieve-flavor-heap
;;;     retrieve-flavor-avl
;;;

(defun obtain-project-&-format (relation-name project-list tuple-format sort-list *current-attributes*
 *current-tuple-format*
 &aux a-p-l actual-project-list final-project-list items temp-tuple-format)
  ;;
  ;;  Make sure that the project-list and tuple-format values passed are in the proper initial form
  ;;
  (cond ((null project-list)
 (setf project-list *current-attributes*
       actual-project-list project-list
       final-project-list project-list)
 (cond ((null tuple-format)
(setf tuple-format *current-tuple-format*))
       ((> (setf items (- (length project-list) (length tuple-format))) 0)
(setf tuple-format (append tuple-format (nthcdr items *current-tuple-format*))))
       ((< items 0)
(setf tuple-format (firstn (length project-list) tuple-format)))))
(t
 ;;
 ;;  Do some error detection and corretion
 ;;
 (if (not (listp project-list))
     (setf project-list (list project-list)))
 (if (and tuple-format (not (listp tuple-format)))
     (setf tuple-format (list tuple-format)))
 ;;
 ;;  The PROJECT-LIST returned from PARSE-PROJECT contains all of the attributes whose values are needed.
 ;;  The ACTUAL-PROJECT-LIST is the user specified project-list will all of the illegal entries removed.
 ;;  The TEMP-TUPLE-FORMAT has the tuple format values for the actual attributes which will be output.
 ;;
 (multiple-value-setq (project-list actual-project-list temp-tuple-format)
   (parse-project relation-name *current-attributes* project-list *current-tuple-format* tuple-format))
 ;;
 ;;  Obtain the names of the attributes which are to be output and their format values
 ;;
 ;;  Form the list of attribute names as they will appear in the display. If the project element is a list, the first element of that
 ;; list will be the name of the attribute in the output.
 ;;
 (setf a-p-l (mapcar #'(lambda (attr)
  (if (not (listp attr))
      attr
      (car attr)))
     actual-project-list))
 ;;
 ;;  The TUPLE-FORMAT contains the values for the format of the attributes which will comprise the results. They will match up
 ;; with the final output attribute list
 ;;
 (if (< (length tuple-format) (length a-p-l))
     (setf tuple-format (append tuple-format (make-list (- (length a-p-l) (length tuple-format))
     ':initial-element 10))))
 ;;
 ;;  Form the final project and tuple format lists. If a sort is requested, determine if the sort attributes are contained in the
 ;; project list. If not put them there so that their values may be projected in order that the sort may be performed.
 ;;
 (cond (sort-list
(setf final-project-list project-list)
(mapcar (function (lambda (attribute)
     (cond ((and (not (member attribute project-list :test 'string-equal))
   (member attribute *current-attributes* :test 'string-equal))
     (setf project-list (append project-list (list attribute))
    actual-project-list (append actual-project-list
           (list attribute))))
    ((null attribute)
     (setf project-list (copy-list project-list))))))
sort-list)))
 (setf tuple-format temp-tuple-format)))
  (values project-list tuple-format actual-project-list final-project-list))

(defun obtain-sort (sort quick-sort)
  ;;
  ;;  Make sure that the sort value is a list of elements
  ;;
  (cond ((and sort (not (listp sort)))
 (setf sort (list sort)))
((and (listp sort) (null (car sort)))
 (setf sort nil))
((listp (car sort))
 (setf sort (car sort))))
  ;;
  ;;  Make sure that the quick-sort value is a list of elements
  ;;
  (cond ((and quick-sort (not (listp quick-sort)))
 (setf quick-sort (list quick-sort)))
((and (listp quick-sort) (null (car quick-sort)))
 (setf quick-sort nil))
((listp (car quick-sort))
 (setf quick-sort (car quick-sort))))
  ;;
  ;;  Validate that the names of the attributes were specified correctly
  ;;
  (cond (sort
 (setf sort (mapcar (function (lambda (attribute)
  (validate-sym attribute t)))
    sort))
 (cond ((member nil sort)
(if *provide-error-messages*
    (format *standard-output* "~%ERROR - Illegally specified sort clause"))
(setf sort 0))))
(quick-sort
 (setf quick-sort (mapcar (function (lambda (attribute)
        (validate-sym attribute t)))
   quick-sort))
 (cond ((member nil quick-sort)
(if *provide-error-messages*
    (format *standard-output* "~%ERROR - Illegally specified sort clause"))
(setf quick-sort 0)))))
  (values sort quick-sort))

(defun obtain-wide (widep number-per-line &aux (status t))
  (cond (widep
 ;;
 ;;  RTMS will now accept a numeric value to be specified with the widep keyword instead of having to specify both.
 ;;
 (cond ((and (numberp widep) (null number-per-line))
(setf number-per-line widep))
       ((null number-per-line)
(setf number-per-line -1)))
 (if (listp number-per-line)
     (setf number-per-line (car number-per-line)))
 ;;
 ;;  Validate the value of number-per-line
 ;;
 (cond ((or (not (numberp number-per-line)) (< number-per-line -1) (= 0 number-per-line))
(if *provide-error-messages*
    (format *standard-output* "~%ERROR - ~s is not a legal number specification"
    number-per-line))
(setf status nil))))
(t
 (setf number-per-line nil)))
  (values status number-per-line))

(defun process-quick-sort (tuple quick-sort actual-project-list
   &aux a-p-l)
  (setf a-p-l (mapcar #'(lambda (attr)
  (if (not (listp attr))
      (string attr)
      (string (car attr))))
      actual-project-list))
  (cond ((member quick-sort '(t (t)))
 (setf quick-sort (quick-sort-prereq t a-p-l)))
(t
 (setf quick-sort (quick-sort-prereq (mapcar #'(lambda (attr)
     (string attr))
        quick-sort)
       a-p-l))))
  (if quick-sort
      (quick-sort (copy-list tuple) quick-sort a-p-l)
      nil))

(defun process-sort (tuple sort project-list actual-project-list *current-attributes* *current-domains*
     &aux (domains nil) key-value attr-pos)
  ;;
  ;;  Before the tuples list can be sorted, the domains of the attributes must be determined
  ;;
  (do ((attr actual-project-list (cdr attr)))
      ((null attr) t)
    (cond ((setf key-value (- (length *current-attributes*)
      (length (member (if (not (listp (setf attr-pos (car attr))))
     attr-pos
     (setf attr-pos (car attr-pos)))
        *current-attributes* :test 'string-equal))))
   (setf domains (cons (if (member attr-pos *current-attributes* :test 'string-equal)
    (nth key-value *current-domains*)
    "ANYP")
       domains)))
  (t
   (setf domains (cons "ANYP" domains)))))
  (setf domains (reverse domains))
  ;;
  ;;  Sort the tuple list
  ;;
  (sort-list tuple sort project-list domains))

(defun convert-attributes (attribute-list)
  (cond ((null attribute-list)
 nil)
(t
 (if (not (listp attribute-list))
     (setf attribute-list (list attribute-list)))
 (mapcar #'(lambda (attribute)
     (cond ((symbolp attribute)
    (string attribute))
   ((stringp attribute)
    (string-upcase attribute))
   (t
    attribute)))
 attribute-list))))

(defun extract-key-heap (attribute-list key-list domains where-clause package-name)
  attribute-list key-list domains where-clause package-name
  nil)

(defun retrieve-internal (relation-name keyword-list
  &aux (tuple nil) current-attributes current-domains current-key
  current-implementation-type current-storage-structure actual-project-list
  current-tuple-format final-project-list key-value into project-list where-clause
  print qprint stream output-file-name tuple-format number-per-line return-tuples sort
  card retrieve-index-name quick-sort keyword-values status index-name
  (list-of-keywords '(into project where print output format num wide qprint tuple sort
        stream dir doc key imp sto unique quick-sort index-name)))
  (block retrieve-internal
  (cond (*parameter-checking*
 (if (or (not (active-database)) (null (setf relation-name (validate-sym relation-name))))
     (return-from retrieve-internal nil))))
  (setf tuple (get-relation relation-name '("ATTRIBUTES" "DOMAINS" "KEY" "IMPLEMENTATION-TYPE"
      "STORAGE-STRUCTURE" "TUPLE-FORMAT" "CARDINALITY") nil))
  (cond ((null (cadr tuple))
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - Relation ~s does not exist in the database ~s"
     relation-name *active-db*))
 (return-from retrieve-internal nil)))
  ;;
  ;;  Obtain the information which has been passed by the user and that which is known about the current relation
  ;;
  (setf relation-name (car tuple)
tuple (cadr tuple)
current-attributes (first tuple)
current-domains (second tuple)
current-key (third tuple)
current-implementation-type (fourth tuple)
current-storage-structure (fifth tuple)
current-tuple-format (sixth tuple)
card (seventh tuple))
  (if *parameter-checking*
      (setf keyword-list (get-keyword-value-prereq list-of-keywords keyword-list))
      (setf keyword-list (de-nest-keyword-list keyword-list)))
  (setf keyword-values (get-keyword-value list-of-keywords keyword-list))

  ;;
  ;;  Determine if the user requested a retrieve directly from an index. If so, must determine if the index requested exists in the database.
  ;;
  (cond ((and (setf retrieve-index-name (nth 19 keyword-values)) *parameter-checking*)
 (setf retrieve-index-name (caar (funcall
     (find-symbol
       (concatenate 'string "RETRIEVE-"
      *system-relation-base-implementation*
      "-" *system-relation-storage-structure*) *pkg-string*)
     'system-index *system-index-attributes* '("INDEX-NAME")
     *system-index-key*
     `(and (string-equal relation-name ,(string-upcase relation-name))
    (string-equal index-name ,(string-upcase retrieve-index-name)))
     nil 'system-index)))
 (cond ((null retrieve-index-name)
(if *provide-error-messages*
    (format *standard-output* "~%ERROR - ~s is not a defined index on the relation ~s"
    (nth 19 keyword-values) relation-name))
(return-from retrieve-internal nil)))))
  ;;
  ;;  Obtain the values of any other keywords which may be present
  ;;
  (setf where-clause (or (nth 2 keyword-values) t)
print (nth 3 keyword-values)
output-file-name (nth 4 keyword-values)
qprint (nth 8 keyword-values)
return-tuples (nth 9 keyword-values)
stream (nth 11 keyword-values))
  (if (null keyword-list)
      (setf print t))
  (if (and return-tuples (not (member 'print keyword-list)))
      (setf print nil))
  ;;
  ;;  Obtain the values from the wide and number keywords
  ;;
  (if (and (not return-tuples) print)
      (if (not (multiple-value-setq (status number-per-line)
     (obtain-wide (nth 7 keyword-values) (nth 6 keyword-values))))
  (return-from retrieve-internal nil)))
  ;;
  ;;  Obtain the values of the sort and quick-sort keywords. The sort keyword value is in the 10th position in the keyword-values list
   ;; while quick-sort is the 18th, based on 0 being the first position. An error condition is signaled if the value of sort or quick-sort is 0.
   ;; The attribute name lists are return-from retrieve-internaled validated.
  ;;
  (multiple-value-setq (sort quick-sort)
    (obtain-sort (nth 10 keyword-values) (nth 18 keyword-values)))
  (if (or (equal sort 0) (equal quick-sort 0))
      (return-from retrieve-internal nil))
  ;;
  ;;  Obtain the values of the project and format lists which are needed to proceed
  ;;
  ;;  PROJECT-LIST contains a list of the attributes whose values are needed to complete the retrieve processing.
  ;;  TUPLE-FORMAT contaion the format widths which will be used for the final display
  ;;  ACTUAL-PROJECT-LIST contains the project list as the user specified with all of the illegal elements removed
  ;;  FINAL-PROJECT-LIST contains the name of the attributes which are to be returned. These attributes may be attributes of the relation
   ;;                       or they may be calculated attributes. This list corresponds with the TUPLE-FORMAT list.
  ;;
  (multiple-value-setq (project-list tuple-format actual-project-list final-project-list)
    (obtain-project-&-format relation-name (nth 1 keyword-values) (nth 5 keyword-values) (or sort quick-sort)
     current-attributes current-tuple-format))
  (cond ((null *parameter-checking*))
((null (car project-list))
 (if *provide-error-messages*
     (format *standard-output*
     "~%ERROR - There are no legal attributes contained in the PROJECT clause"))
 (return-from retrieve-internal nil))
;;
;;  An error occured in OBTAIN-PROJECT-&-FORMAT.
;;
((member nil project-list)
 (return-from retrieve-internal nil)))
  ;;
  ;;  Determine if a key exists in the where clause for the current structure, if so utilize the key in the retrieval. Multiple keys may be
   ;; returned from extract-key-?. They will be handled properly in the individual retrieve functions.
  ;;
  (multiple-value-setq (index-name key-value current-storage-structure current-key)
    (extract-key relation-name current-attributes current-key current-domains current-storage-structure
 where-clause (nth 19 keyword-values)))
  (setf tuple (funcall (find-symbol (concatenate 'string "RETRIEVE-" current-implementation-type
      "-" current-storage-structure) *pkg-string*)
       relation-name current-attributes project-list current-key where-clause key-value
       index-name))
  (if (not (equal project-list actual-project-list))
      (multiple-value-setq (tuple project-list actual-project-list)
(calculate-attributes tuple project-list actual-project-list)))
  (if (and (nth 17 keyword-values) tuple)
      (setf tuple (unique-tuples tuple)))
  ;;
  ;;  Sort the results with either the quick-sort or the sort method before proceeding if so requested
  ;;
  (cond ((and sort tuple)
 (if (null (setf tuple (process-sort tuple sort project-list actual-project-list current-attributes
 current-domains)))
     (return-from retrieve-internal nil)))
((and quick-sort tuple)
 (if (null (setf tuple (process-quick-sort tuple quick-sort actual-project-list)))
     (return-from retrieve-internal nil))))
  (cond (tuple
 ;;
 ;;  If the final project list is not the same as the current project list, project the results a final time. This can occur if sort
 ;; attributes were not contained in the project list.
 ;;
 (setf final-project-list nil)
 (setf actual-project-list (or (convert-attributes (nth 1 keyword-values)) current-attributes))
 (if (not (listp actual-project-list))
     (setf actual-project-list (list actual-project-list)))
 (do ((project-list actual-project-list (cdr project-list)))
     ((null project-list) t)
   (cond ((and (listp (car project-list))
       (not (member (string-upcase (caar project-list)) current-attributes
     :test 'string-equal)))
  (setf final-project-list (append final-project-list
      (list (string-upcase (caar project-list))))))
 ((and (not (listp (car project-list)))
       (member (car project-list) current-attributes :test 'string-equal))
  (setf final-project-list (append final-project-list (list (car project-list)))))))
 (cond ((null (car project-list))
(if *provide-error-messages*
    (format *standard-output*
    "~%ERROR - There are no legal attributes contained in the PROJECT clause"))
(return-from retrieve-internal nil))
       ((not (equal project-list final-project-list))
(setf tuple (project-list tuple project-list final-project-list))
(setf project-list final-project-list)))))
  ;;
  ;;  Determine if the results of the retrieval should be piped into a relation. If so, do the proper things. The user specified value of the
   ;; keyword INTO is in the 0th position in the keyword-values list. This value is the name of the relation into which the results are placed.
  ;;
  (setf into (validate-sym (nth 0 keyword-values)))
  (cond (into
 (if (null (retrieve-into relation-name into tuple project-list actual-project-list current-key
   current-implementation-type current-storage-structure tuple-format
   keyword-list current-attributes))
     (return-from retrieve-internal nil)))
((nth 0 keyword-values)
 (return-from retrieve-internal nil)))
  ;;
  ;;Later, we will have to define a temporary relation and not print these tuples.
  ;;
  (cond (qprint
 (do ((tuple tuple (cdr tuple)))
     ((null tuple) t)
   (format *standard-output* "~%~s" (car tuple))))
((or print output-file-name stream)
 (setf project-list (unconvert-attributes project-list))
 (if into
     (setf tuple-format (or (nth 5 keyword-values) tuple-format)))
 (printrel-internal* (or into relation-name) tuple project-list number-per-line number-per-line stream
     output-file-name
     (if (<= (length project-list) (length tuple-format))
  tuple-format
  (append tuple-format (make-list (- (length project-list)
         (length tuple-format)) ':initial-element
      *default-anyp-width*)))
     t t
     (caar (qtrieve 'system-relation *system-relation-attributes* '("CARDINALITY")
      *system-relation-key*
      `(string-equal relation-name
       ,(string-upcase (or into relation-name)))))
     print return-tuples)))
  ;;
  ;; Return the resultant relation (either INTO or TEMPORARY) We want the option of having either a relation name returned or the tuples
   ;; returned. For now, if a into is provided, the relation name is returned, otherwise the tuples will be returned. This will have to be
   ;; modified in the future to handle the creation of a relation in which the user does not specify a name.
  ;;
  (cond ((and (null return-tuples) *provide-status-messages*)
 (cond ((car (errset (send *output-window* ':exposed-p) nil))
(send *output-window* ':append-item (format nil "~s tuple~:P retrieved" (length tuple)))
(send *output-window* ':append-item " "))
       (stream
(terpri stream)
(format stream "~%~s tuple~:P retrieved" (length tuple)))
       (t
(terpri)
(format *standard-output* "~%~s tuple~:P retrieved" (length tuple))))
 (return-from retrieve-internal (or into relation-name)))
(t
 (if return-tuples (return-from retrieve-internal tuple))))
  (return-from retrieve-internal relation-name)))


(defun retrieve-flavor-hash (relation-name attribute-list project-list key where-clause key-value-list
     index-name)
  (retrieve-hash relation-name attribute-list project-list key where-clause key-value-list "flavor" index-name))

(defun retrieve-flavor-heap (relation-name attribute-list project-list key where-clause heap-traversal
     index-name)
  heap-traversal
  (cond (index-name
 (qtrieve-flavor-heap index-name attribute-list project-list key where-clause))
(t
 (qtrieve-flavor-heap relation-name attribute-list project-list key where-clause))))

(defun retrieve-hash (relation-name attribute-list project-list key where-clause key-value imp index-name
      &aux retrieve-bucket hash-relation temp-attribute-list conv-attribute-list
      (tuple-list nil))
   imp key
   (block retrieve-hash
   (cond ((not (listp where-clause))
  (if (eval where-clause)
      (setf where-clause t)
      (return-from retrieve-hash nil))))
   (setf hash-relation (getp index-name 'entry-point))
   (cond ((null key-value)
  ;;
  ;;  There is not a key to use, therefore we have to look at the entire relation
  ;;
  (maphash (function (lambda (key-val tuples)
 key-val
 (setf tuple-list (append tuples tuple-list))))
    hash-relation))
 (t
  ;;
  ;; Ay least one key has been extracted from the where clause. Select the buckets indicated by the keys and evaluate them
  ;; with respect to the where clause
  ;;
   (do ((key-value% key-value (cdr key-value%)))
       ((null key-value%) t)
     (setf tuple-list (append tuple-list (gethash (car key-value%) hash-relation))))))

   ;;
   ;;  The eval-where and project-list functions were put into this loop in an attempt to perserve memory at the expense of speed. This
    ;; will not work for array-hash.
   ;;
   (cond ((string-equal (string-upcase imp) "FLAVOR")
  (setf conv-attribute-list (project-flavor-prereq attribute-list))
  (setf tuple-list (fast-project-flavor tuple-list conv-attribute-list)))
 ((string-equal (string-upcase imp) "STRUCT")
  (setf conv-attribute-list (unconvert-attributes (mapcar #'(lambda (attr)
           (concatenate 'string
          (string relation-name)
          attr))
       attribute-list)))
  (setf tuple-list (fast-project-struct tuple-list conv-attribute-list))))
   (cond ((not (equal where-clause t))
  (multiple-value-setq (where-clause  temp-attribute-list)
    (eval-where-prereq where-clause attribute-list relation-name))
  (setf tuple-list (fast-eval-where tuple-list where-clause temp-attribute-list))))
   (cond (tuple-list
  (setf tuple-list (project-list tuple-list attribute-list project-list))
  (setf retrieve-bucket (append tuple-list retrieve-bucket))))
   (return-from retrieve-hash retrieve-bucket)))

(defun fast-project-struct (tuples attribute-list)
  (mapcar (function (lambda (tuple)
      (mapcar (function (lambda (attr)
    (funcall attr tuple)))
    attribute-list)))
  tuples))

(defun retrieve-list-avl (relation-name attribute-list project-list key where-clause key-value-list index-name)
  (retrieve-avl relation-name attribute-list project-list key where-clause key-value-list "LIST" index-name))

(defun retrieve-flavor-avl (relation-name attribute-list project-list key where-clause key-value-list
    index-name)
  (retrieve-avl relation-name attribute-list project-list key where-clause key-value-list "FLAVOR" index-name))

(defun retrieve-struct-avl (relation-name attribute-list project-list key where-clause key-value-list
    index-name)
  (retrieve-avl relation-name attribute-list project-list key where-clause key-value-list "STRUCT" index-name))

(defun retrieve-list-hash (relation-name attribute-list project-list key where-clause key-value-list index-name)
  (retrieve-hash relation-name attribute-list project-list key where-clause key-value-list "LIST" index-name))

(defun retrieve-list-heap (relation-name attribute-list project-list key where-clause heap-traversal index-name)
  heap-traversal
  (cond (index-name
 (qtrieve-list-heap index-name attribute-list project-list key where-clause))
(t
 (qtrieve-list-heap relation-name attribute-list project-list key where-clause))))

(defun retrieve-struct-hash (relation-name attribute-list project-list key where-clause key-value-list
     index-name)
  (retrieve-hash relation-name attribute-list project-list key where-clause key-value-list "STRUCT" index-name))

(defun retrieve-struct-heap (relation-name attribute-list project-list key where-clause heap-traversal
     index-name)
  heap-traversal
  (if index-name
      (qtrieve-struct-heap relation-name attribute-list project-list key where-clause
   (getp index-name 'entry-point))
      (qtrieve-struct-heap relation-name attribute-list project-list key where-clause)))

(defun select-tuples (relation-name &rest keyword-list
      &key &optional directory documentation format implementation-type into key number output
      print qprint quick-sort sort stream storage-structure tuples unique where wide
      &allow-other-keys)
  "Same as Retrieve except that all attributes are retrieved.

   RELATION-NAME        - Name of the relation whose tuples are to be retrieved.
   WHERE                - Criterion to be used in selecting the tuples.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   WIDE                 - If T, result is printed out in Attribute: value format rather than as a table.
   NUMBER               - If WIDE is T, this keyword specifies the number of attributes per line.
   OUTPUT               - If the result is to be sent to a file, specify the file-name.
   PRINT                - If NIL, the result is not printed.
   QPRINT               - If T, the result is printed without formatting.
   QUICK-SORT           - Specifies the attributes to sort the result on.
   SORT                 - If any domain-specific, user-defined sort mechanism is to be used, this keyword
                          can be used.
   STREAM               - Specify the window to which the output is to be sent, if it is different than the
                          the *standard-output* or RTMS-interface.
   TUPLES               - If T, the resultant tuples are returned.
   UNIQUE               - If T, only unique tuples are retrieved."
  directory documentation format implementation-type into key number output print qprint quick-sort sort stream
  storage-structure tuples unique where wide
  (retrieve relation-name (append (list 'project nil) keyword-list)))

(defun sort-list (tuples sort-clause attribute-list domain-list
  &aux (avl-tree nil) domain (key-list nil) key-value-list new-element relation-name
  (sort-order nil) sort-element% (already-sorted-p nil))
  (block sort-list
;;
;;  Validate the sort-clause and form the insertion key
;;
(cond ((equal sort-clause t)
       (setf sort-clause nil)
        (setf key-list attribute-list))
      ((and (not (listp sort-clause)) sort-clause)
       (setf sort-clause (list sort-clause)))
      ((listp (car sort-clause))
       (setf sort-clause (car sort-clause))))
;;
;;  For version 0.0, ascending or decending order may only be specified for the tuples as a whole not on each attribute. This
;; restriction will go away soon.
;;
(if (null (do ((sort-element sort-clause (cdr sort-element)))
      ((null sort-element) t)
    (if (null (setf sort-element% (validate-sym (car sort-element) t)))
(return-from sort-list nil))
    (cond ((member sort-element% attribute-list :test 'string-equal)
   ;;
   ;;  Determine the domain for this attribute
   ;;
   (setf domain (nth (- (length domain-list)
   (length (member sort-element% attribute-list
     :test 'string-equal)))
       domain-list))
   (setf key-list (append key-list (list sort-element%)))
   (cond ((not (member domain '("NUMBERP" "STRINGP" "ATOM") :test 'string-equal))
   (return-from sort-list (setf already-sorted-p (quick-sort tuples sort-clause
           attribute-list))))))
  ((member sort-element% '("ASC" "GT" "GTE" "GE" "INCREASING" "DES" "DESC"
      "DECREASING" "LT" "LTE" "LE") :test 'string-equal)
   (setf sort-order (or sort-order sort-element%)))
  (t
   (cond (*provide-warning-messages*
   (format *standard-output*
    "~%WARNING - ~s is not an attribute nor a recognized sort keyword"
    sort-element%)
   (format *standard-output* "~%          This element will be ignored")))))))
    (return-from sort-list nil))
(if already-sorted-p
    (return-from sort-list already-sorted-p))
(cond ((null key-list)
       (cond (*provide-error-messages*
      (format *standard-output* "~%ERROR - No attributes specified in the sort clause --> ~s"
      sort-clause)
      (format *standard-output* "~%        Sort can not proceed")))
       (return-from sort-list nil)))
;;
;;  Loop through each tuple inserting each into the AVL tree based on the key
;;
(setf key-value-list (project-list tuples attribute-list key-list)
      domain-list (car (project-list (list domain-list) attribute-list key-list))
      relation-name (read-from-string (concatenate 'string *pkg-string* "-TEMP-"
       (string (gensym)))))
(do ((tuples tuples (cdr tuples))
     (key-value-list key-value-list (cdr key-value-list)))
    ((null tuples) t)
  (setf new-element (cons (list (car tuples)) (append (list 0) (list nil) (list nil)))
avl-tree (insert-avl-list new-element avl-tree (car key-value-list) key-list attribute-list
    domain-list nil relation-name)))
;;
;;  Convert from an AVL tree to a simply list-heap structure
;;
;;  Should determine if this relation might some how exist...later
;;
(putp relation-name avl-tree 'entry-point)
(setf tuples (retrieve-list-avl relation-name attribute-list attribute-list key-list t nil
  relation-name))
(putp relation-name nil 'entry-point)
;;
;;  Place the tuples in the final order
;;
(cond ((member sort-order '("DES" "DESC" "DECREASING" "LT" "LTE" "LE") :test 'string-equal)
       (setf tuples (reverse tuples))))
(return-from sort-list tuples)))

(defun maptuple (dbfunction relation)
  "Map a function on all the tuples in a relation using MAPCAR.

   DBFUNCTION  - Function to be applied to each and every tuple.
   RELATION    - Name of the relation."
  (block maptuple
(if (not (active-database))
    (return-from maptuple nil))
(cond ((null (car (errset (functionp dbfunction) t)))
       (if *provide-error-messages*
   (format *standard-output* "~%ERROR - Illegal function definition"))
       (return-from maptuple nil)))
(if (not (setf relation (validate-sym relation)))
    (return-from maptuple nil))
(return-from maptuple (mapcar (function (lambda (tuple)
     (funcall dbfunction tuple)))
(retrieve relation 'tuples t)))))

(defun mapt (dbfunction relation)
  "Map a function on all the tuples in a relation using MAPC.

   DBFUNCTION  - Function to be applied to each and every tuple.
   RELATION    - Name of the relation."
  (block mapt
(if (not (active-database))
    (return-from mapt nil))
(cond ((null (car (errset (functionp dbfunction) t)))
       (if *provide-error-messages*
   (format *standard-output* "~%ERROR - Illegal function definition"))
       (return-from mapt nil)))
(if (not (setf relation (validate-sym relation)))
    (return-from mapt nil))
(mapc (function (lambda (tuple)
  (funcall dbfunction tuple)))
      (retrieve relation 'tuples t))
(return-from mapt relation)))

(defun print-relation (relation &rest keyword-list
       &key &optional directory documentation format implementation-type index-name into key
       number output print qprint quick-sort sort stream storage-structure tuples unique wide
       &allow-other-keys)
  "Same as Retrieve without a where clause and all attributes are retrieved.

   RELATION-NAME        - Name of the relation whose tuples are to be retrieved.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   INDEX-NAME           - Name of the index to use in the retrieval.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   WIDE                 - If T, result is printed out in Attribute: value format rather than as a table.
   NUMBER               - If WIDE is T, this keyword specifies the number of attributes per line.
   OUTPUT               - If the result is to be sent to a file, specify the file-name.
   PRINT                - If NIL, the result is not printed.
   QPRINT               - If T, the result is printed without formatting.
   QUICK-SORT           - Specifies the attributes to sort the result on.
   SORT                 - If any domain-specific, user-defined sort mechanism is to be used, this keyword
                          can be used.
   STREAM               - Specify the window to which the output is to be sent, if it is different than the
                          the *standard-output* or RTMS-interface.
   TUPLES               - If T, the resultant tuples are returned.
   UNIQUE               - If T, only unique tuples are retrieved."
  directory documentation format implementation-type into key number output print qprint quick-sort sort stream
  storage-structure tuples unique wide index-name
  (retrieve relation keyword-list))

(deff printrel 'print-relation)

(defun project (relation-name &rest keyword-list
&key &optional directory documentation format implementation-type index-name into key number
output print project qprint quick-sort sort stream storage-structure tuples unique wide
&allow-other-keys)
  "Same as Retrieve except that all tuples are retrieved.

   RELATION-NAME        - Name of the relation whose tuples are to be retrieved.
   PROJECT              - List of attributes to be projected in the result.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   INDEX-NAME           - Name of the index to use in the retrieval.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   WIDE                 - If T, result is printed out in Attribute: value format rather than as a table.
   NUMBER               - If WIDE is T, this keyword specifies the number of attributes per line.
   OUTPUT               - If the result is to be sent to a file, specify the file-name.
   PRINT                - If NIL, the result is not printed.
   QPRINT               - If T, the result is printed without formatting.
   QUICK-SORT           - Specifies the attributes to sort the result on.
   SORT                 - If any domain-specific, user-defined sort mechanism is to be used, this keyword
                          can be used.
   STREAM               - Specify the window to which the output is to be sent, if it is different than the
                          the *standard-output* or RTMS-interface.
   TUPLES               - If T, the resultant tuples are returned.
   UNIQUE               - If T, only unique tuples are retrieved."
  directory documentation format implementation-type into key number output print project qprint quick-sort
  sort stream storage-structure tuples unique wide index-name
  (retrieve relation-name (append (list 'where t 'unique t) keyword-list)))

(defun attr-calc (calc-list attribute-list
  &aux new-calc-list new-calc-element result result-element)
  (cond ((and (functionp calc-list) (not (member (string-upcase calc-list) attribute-list :test 'string-equal)))
 (setf new-calc-list calc-list))
((or (symbolp calc-list) (stringp calc-list))
 (if (member (setf new-calc-element (string-upcase calc-list)) attribute-list :test 'string-equal)
     (setf result (push new-calc-element result)
   new-calc-list (read-from-string new-calc-element))
     (setf new-calc-list calc-list)))
((listp calc-list)
 (mapc #'(lambda (x)
   (multiple-value-setq (result-element new-calc-element)
     (attr-calc x attribute-list))
   (setf result (append result result-element)
 new-calc-list (append new-calc-list (list new-calc-element))))
       calc-list))
(t
 (setf new-calc-list calc-list)))
  (values result new-calc-list))

(defun quick-sort-prereq (quick-sort a-p-l &aux (result nil))
  (block quick-sort-prereq
  (maplist #'(lambda (attr &aux attr%)
       (cond ((null (setf attr% (validate-sym (car attr) t)))
      (setf result nil)
      (return-from quick-sort-prereq nil)))
       (cond ((member attr% '("LT" "LTE" "LE" "DECREASING" "DESC" "DES" "GT" "GTE" "GE" "INCREASING"
       "ASC") :test 'string-equal))
     ((and (member (validate-sym (cadr attr) t) '("LT" "LE" "LTE" "DECREASING" "DESC" "DES")
     :test 'string-equal)
   (member attr% a-p-l :test 'string-equal))
      (setf result (append result (list (list attr% 'dbgtp)))))
     ((and (member (validate-sym (cadr attr) t) '("GT" "GE" "GTE" "INCREASING" "ASC")
     :test 'string-equal)
   (member attr% a-p-l :test 'string-equal))
      (setf result (append result (list attr%))))
     ((and (or (not (listp attr%)) (equal (length attr%) 1))
   (member attr% a-p-l :test 'string-equal))
      (setf result (append result (list attr%))))
     (t (prog2
  (if *provide-warning-messages*
      (format *standard-output* "~%WARNING - ~S is neither a valid quick-sort keyword nor an attribute." attr%))
  nil))))
   quick-sort)
  (return-from quick-sort-prereq result)))

(defun retrieve (relation-name &rest keyword-list
 &key &optional directory documentation format implementation-type index-name into key number
 output print project qprint quick-sort sort stream storage-structure tuples unique where wide
 &allow-other-keys)
  "Retrieve some tuples from a relation satisying a where clause.

   RELATION-NAME        - Name of the relation whose tuples are to be retrieved.
   WHERE                - Criterion to be used in selecting the tuples.
   PROJECT              - List of attributes to be projected in the result.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   INDEX-NAME           - Name of the index to use in the retrieval.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   WIDE                 - If T, result is printed out in Attribute: value format rather than as a table.
   NUMBER               - If WIDE is T, this keyword specifies the number of attributes per line.
   OUTPUT               - If the result is to be sent to a file, specify the file-name.
   PRINT                - If NIL, the result is not printed.
   QPRINT               - If T, the result is printed without formatting.
   QUICK-SORT           - Specifies the attributes to sort the result on.
   SORT                 - If any domain-specific, user-defined sort mechanism is to be used, this keyword
                          can be used.
   STREAM               - Specify the window to which the output is to be sent, if it is different than the
                          the *standard-output* or RTMS-interface.
   TUPLES               - If T, the resultant tuples are returned.
   UNIQUE               - If T, only unique tuples are retrieved. "
  directory documentation format implementation-type into key number output print project qprint quick-sort
  sort stream storage-structure tuples unique where wide index-name
  (retrieve-internal relation-name keyword-list))


(defun parse-project (relation-name *current-attributes* project-list *current-tuple-format* tuple-format
      &aux a-p-l new-attr actual-project-list temp-tuple-format project-element new-attr-part2)
  ;;
  ;;We will have to process the project-list to make sure that the attributes specified are valid and to take care of the calculated
  ;; attributes.
  ;;
  (setf a-p-l (convert-attributes project-list)
project-list nil)
  (do ((a-p-l a-p-l (cdr a-p-l))
       (tuple-format tuple-format (cdr tuple-format)))
      ((null a-p-l) t)
    (setf new-attr (car a-p-l))
    (cond ((and (not (listp new-attr)) (member new-attr *current-attributes* :test 'string-equal))
   ;;
   ;;  The current element is an attribute of the current relation, no further processing is required, therefore the project and
   ;; tuple format lists can be completed.
   ;;
   (push new-attr actual-project-list)
   (push new-attr project-list)
   (push (cond ((null tuple-format)
(nth (- (length *current-attributes*) (length (member new-attr *current-attributes*
             :test 'string-equal)))
     *current-tuple-format*))
       (t
(car tuple-format)))
 temp-tuple-format))
  ((not (listp new-attr))
   ;;
   ;;  The element is not a list and it is not an attribute of the current relation therefore we have need to warn the user. This
    ;; element will not be included in the final solution
   ;;
   (if (and (validate-sym new-attr t) *provide-warning-messages*)
       (format *standard-output* "~%WARNING - ~s is not an attribute of the ~s relation"
       new-attr relation-name)))
  ;;
  ;;  If the second element of the new-attr list does not exist then this element is specified incorrectly
  ;;
  ((not (second new-attr))
   (if *provide-warning-messages*
       (format *standard-output* "~%WARNING - Improperly specified project element ~s."
       (first new-attr))))
  ;;
  ;;  The first element of the new-attr list must not be the name of an attribute in the current relation
  ;;
  ((member (string-upcase (first new-attr)) *current-attributes* :test 'string-equal)
   (cond (*provide-warning-messages*
  (format *standard-output* "~%WARNING - Improperly specified project element ~s." new-attr)
  (format *standard-output* "~%          ~s is an attribute of the ~s relation."
  (first new-attr) relation-name))))
  ((listp (second new-attr))
   ;;
   ;;  The second element of the new-attr is a list, indicating that this element is possibly true calculated attribute, further
   ;; checking is needed. In any case, it is not simply a rename
   ;;
   ;;
   ;; Determine the tuple format for the current calculated attribute
   ;;
   (push (cond ((null tuple-format)
(if (member (string-upcase (first new-attr)) *current-attributes* :test 'string-equal)
    (nth (+ 1 (- (length *current-attributes*)
   (length (member (string-upcase (first new-attr))
     *current-attributes* :test 'string-equal))))
  *current-tuple-format*)
    *default-anyp-width*))
       (t
(car tuple-format)))
 temp-tuple-format)
   (multiple-value-setq (project-element new-attr-part2)
     (attr-calc (second new-attr) *current-attributes*))
   (setf project-list (append project-element project-list))
   (push (append (list (first new-attr)) (list new-attr-part2)) actual-project-list))
  ;;
  ;;  The second element of new-attr is not a list, therefore it must either be a rename attribute request or a output attribute
  ;; with a constant value
  ;;
  (t
   ;;
   ;;  The second element of the current list is not a list therefore it must be either a renamed attribute or a new attribute
   ;; which is assigned a constant value.
   ;;
   (cond ((not (member (string-upcase (second new-attr)) *current-attributes* :test 'string-equal))
  ;;
  ;;  The second element is not an attribute from the current relation, nothing more to be done here except setting
  ;; the tuple format for this element
  ;;
  (push new-attr actual-project-list)
  (push (cond ((null tuple-format)
       *default-anyp-width*)
      (t
       (car tuple-format)))
temp-tuple-format))
 (t
  ;;
  ;;  The second element is an attribute in the current relation so the element indicates that the attribute is going to
  ;; be renamed. Set the appropriate lists.
  ;;
  (push (list (first new-attr) (second new-attr)) actual-project-list)
  ;;
  ;;  Modify the value of new-attr in the process so that the string conversion only has to be done once
  ;;
  (setf new-attr (string-upcase (second new-attr)))
  (push new-attr project-list)
  (push (cond ((null tuple-format)
       (if (member new-attr *current-attributes* :test 'string-equal)
    (nth (- (length *current-attributes*)
     (length (member new-attr *current-attributes* :test 'string-equal)))
         *current-tuple-format*)
    *default-anyp-width*))
      (t
       (car tuple-format)))
temp-tuple-format))))))
  (setf actual-project-list (reverse actual-project-list)
temp-tuple-format (reverse temp-tuple-format))
  ;;
  ;;Get rid of duplicate elements in project-list
  ;;
  (setf a-p-l project-list
project-list nil)
  (mapc (function (lambda (attr)
    (if (not (member attr project-list :test 'string-equal))
(push attr project-list))))
a-p-l)
  (values (convert-attributes project-list) actual-project-list temp-tuple-format))

(defun unconvert-attributes (attribute-list &optional (package-name *pkg-name*))
  (mapcar (function (lambda (attribute)
      (cond ((or (symbolp attribute) (stringp attribute))
     (read-from-string (concatenate 'string package-name ":" (string attribute))))
    (t
     attribute))))
  attribute-list))

(defun calculate-attributes (tuple project-list actual-project-list
     &aux result element)
  ;;
  ;;  If the project-list contains a aggregrate function, modify the value contained in the actual project list
  ;;
  (setf actual-project-list
(mapcar #'(lambda (x)
    (cond ((not (listp x))
   x)
  ((listp (second x))
   (list (car x) (third (parse-where (list 'equal 'x (second x))))))
  (t
   x)))
actual-project-list))
  (progv (unconvert-attributes project-list) nil
    (mapc (function (lambda (%tuple)
      ;;
      ;;  Take each attribute contained in the attribute list and make a variable out of it. The value which represents
      ;; that attribute in the current tuple is assigned to the appropriate variable.
      ;;
      (do ((att-list project-list (cdr att-list))
   (val-list %tuple (cdr val-list)))
  ((null att-list) t)
(set (read-from-string (car att-list)) (car val-list)))
      ;;
      ;; Substitute values for expressions
      ;;
      (setf result (cons (mapcar (function (lambda (attr)
         (cond ((not (listp attr))
         (symbol-value (read-from-string
           (string attr))))
        ((listp attr)
         ;;
         ;;  If the second element is a symbol and it is one of
         ;; the attributes from the relation, rid it of any
         ;; package indicators
         ;;
         (cond ((and (symbolp (second attr))
       (member (string-upcase
          (second attr))
        project-list
         :test 'string-equal))
         (setf element (read-from-string
           (string-upcase
               (second attr)))))
        (t
         (setf element (second attr))))
         (if (or (stringp (car attr))
          (symbolp (car attr)))
      (set (read-from-string
             (string (car attr)))
           (eval element))
      (eval element))))))
    actual-project-list)
   result))))
  tuple))
  (setf project-list (mapcar (function (lambda (x)
   (if (not (listp x))
       (string-upcase x)
     (string-upcase (car x)))))
     actual-project-list))
  (setf result (nreverse result))
  (values result project-list actual-project-list))

(defun retrieve-into (relation-name into tuple project-list actual-project-list current-key
      current-implementation-type current-storage-structure tuple-format keyword-list
      current-attributes
      &aux temp a-p-l old-values final-project-list)
  (block retrieve-into
(setf temp (car (qtrieve 'system-relation *system-relation-attributes* '("ATTRIBUTES")
  *system-relation-key*
  `(string-equal relation-name ,(string into)))))
(if (null temp)
    (progn
      ;;Form the attribute descriptor pair. Consider one attribute at a time and get the descriptor values from the
      ;; system-attribute relation. Also, if any of the key attributes are not part of the projected attributes, we will reset the
      ;; key to the entire list of projected attributes.
      (setf temp actual-project-list
    a-p-l nil)
      (mapc (function (lambda (attr &aux attd)
 (setf attd (car (qtrieve 'system-attribute *system-attribute-attributes*
     '("DOMAIN-FUNCTION" "DEFAULT-VALUE" "DOC")
     *system-attribute-key*
     (list 'and  (list 'string-equal 'relation-name
         (string relation-name))
           (list 'string-equal 'attribute-name
          (string-upcase (if (not (listp attr))
        attr
        (if (not (listp
            (cadr
              attr)))
            (cadr attr)
            nil))))))))
 (if attd
     (setf a-p-l (append a-p-l (list (if (not (listp attr))
      attr
      (car attr))
         (list 'dom (first attd) 'def (second attd)
        'doc (if (not (listp attr))
          (third attd)
          nil)))))
     (setf a-p-l (append a-p-l (list (if (not (listp attr))
      attr
      (car attr))
         (list 'dom 'anyp)))))
 (setf final-project-list (cons (if (listp attr)
        (string-upcase (car attr))
        attr) final-project-list))))
    temp)
      (let ((result nil))
(setf current-key (if (dolist (%attribute (or (car (get-keyword-value '(key) keyword-list))
          current-key) result)
  (if (member %attribute final-project-list :test 'string-equal)
      (setf result %attribute)
      (return-from retrieve-into nil)))
       (or (car (get-keyword-value '(key) keyword-list)) current-key)
       (list (car (reverse final-project-list))))))
      (if (null (defrel into a-p-l
  (list
    'imp (or (car (get-keyword-value '(imp) keyword-list))
      current-implementation-type)
    'sto (or (car (get-keyword-value '(sto) keyword-list)) current-storage-structure)
    'key current-key
    'format (or (car (get-keyword-value '(format) keyword-list)) tuple-format)
    'dir (car (get-keyword-value '(dir) keyword-list))
    'doc (car (get-keyword-value '(doc) keyword-list)))))
  (return-from retrieve-into nil)))
  (if (equal (length (setf temp (car temp))) (length actual-project-list))
      (mapc (function (lambda (attr-rel attr-into &aux temp-d)
 (if (listp attr-rel)
     (setf attr-rel (first attr-rel)))
 (cond ((not (member attr-rel current-attributes :test 'string-equal)))
       ;;It is a projected (calculated) attribute.
       ((not
   (or (equal (caar (qtrieve 'system-attribute
        *system-attribute-attributes*
        '("DOMAIN-FUNCTION")
        *system-attribute-key*
        (list 'and
       (list 'string-equal 'relation-name
             (string-upcase relation-name))
       (list 'string-equal 'attribute-name
             (string-upcase attr-rel)))))
       (setf temp-d (caar (qtrieve 'system-attribute
       *system-attribute-attributes*
       '("DOMAIN-FUNCTION")
       *system-attribute-key*
       (list 'and
             (list 'string-equal
            'relation-name
            (string-upcase
              into))
             (list 'string-equal
            'attribute-name
            (string-upcase
              (if (not (listp
             attr-into))
           attr-into
           (car
             attr-into)
           ))))))))
       (string-equal temp-d "ANYP")))
        (if *provide-error-messages*
     (format *standard-output*
      "~%ERROR - The attribute ~S in relation ~S and the attribute ~S in the output relation ~S have different domain predicates." attr-rel relation-name attr-into into))
        (return-from retrieve-into nil)))))
    actual-project-list temp)
      (progn
(if *provide-error-messages*
    (format *standard-output*
    "~%ERROR - The output relation ~S does not have all the attributes required to insert the retrieved tuples. ~S has ~S as attributes and the retrieve call requires ~S attributes in the relation ~S to be projected."
    into into temp project-list relation-name))
(return-from retrieve-into nil))))
(setf old-values (list *provide-error-messages* *validity-checking*))
(setf *provide-error-messages* nil
      *validity-checking* nil)
(insert into (list 'tuples tuple))
(setf *provide-error-messages* (car old-values)
      *validity-checking* (cadr old-values))
(return-from retrieve-into relation-name)))


(defun unique-tuples (tuples &aux result-table)
  (setf result-table (make-hash-table :test 'equal))
  (mapc #'(lambda (x &aux hash-bucket)
  (cond ((setf hash-bucket (gethash x result-table))
 (cond ((not (member x hash-bucket))
 (puthash x (cons x hash-bucket) result-table))))
(t
 (puthash x x result-table))))
 tuples)
  (setf tuples nil)
  (maphash #'(lambda (x y)
     y
     (setf tuples (cons x tuples)))
   result-table)
  tuples)
-TYPE  - Name of the new implementation type.
  STORAGE-STRUCTURE    - Name of the new storage-structure.
  FORMAT               - List of new print-width values to be used for the attributes.
  KEY                  - List of attributes to form the new key for this relation.
  DOCUMENTATION        - New description of this relation.
  DIRECTORY            - New directory in which this relation is to be saved.  (MODIFY-RELATION RELATION &REST KEYWORD-LIST &KEY &OPTIONAL RELATION-NAME ADD-ATTRIBUTES DELETE-ATTRIBUTES RENAME-ATTRIBUTES IMPLEMENTATION-TYPE STORAGE-STRUCTURE FORMAT KEY DOCUMENTATION DIRECTORY &ALLOW-OTHER-KEYS)ÄÄBÄõëBÄ‹¿lÄ~S¿BÄé	¿BÄ†¿√ÅADD-ATTRIBUTES¿CÇDELETE-ATTRIBUTESÄ¿CÇRENAME-ATTRIBUTESÄ¿BÄ£¿BÄ¨¿BÄﬂ¿BÄ¢¿BÄ°¿BÄ†¿BÄ≠“BÄﬂ“BÄé	íPA¡Pˇ€PPÅQPÇQPÉQ	PÑQ
PÖQPÜQPáQPàQP QPãQPäQJ∫@√ööAëÅQ@QîOÄ§	BÄ
	Ä1Ä\ÄBÄ‚\ÄBÄÂ\ÄBÄ8\ÄBÄLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540824. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "XLD" :NAME "RETRIEVE" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760360396. :AUTHOR "REL3" :LENGTH-IN-BYTES 13504. :LENGTH-IN-BLOCKS 27. :BYTE-SIZE 16.)                                   pp2Ä\Ä©ÅCOMPILE-DATA\ÄÏÄSW-MFG,ÅGODZILLAÜÄá§‹œFÄFÄ\Äp¿,ÅCOMPILER,ÅVERSIONÄ\ÄFÄFÄp¿BÄ),ÇOPTIMIZE-SWITCHÄÜÄ©ÉQFASL-SOURCE-FILE-UNIQUE-IDÄ1Ä\Äp¿lÄFSÏÇMAKE-FASLOAD-PATHNAMEÄ\ÄÍÄQUOTEÄBÄ$\ÄBÄ8™ÄNILÄ\ÄBÄ8\Ä¨ÄRTMS\ÄBÄ8,ÅRETRIEVE\ÄBÄ8¨ÄLISP\ÄBÄ8FÄÈÄFONTSÄ\Ä©Å*CODE-FONT*ÄÈÅ*COMMENT-FONT*ÈÅ*STRING-FONT*Ä©ÄBASEFÄ
)ÅPACKAGEÄ©ÄRTMS©ÄMODE©ÅCOMMON-LISPÄÄÉOBTAIN-PROJECT-&-FORMATÄÄÎÄIùÜÄA¨FÄT¿$Ä¿BÄ:p¿¨ÄTICLÏÄART-QÄ]ÄFÄÄ:BÄ:BÄ:jÄTÄFÄp¿¨ÄSYSÄlÇDEBUG-INFO-STRUCTÄBÄP\Ä√ÅRELATION-NAMEÄÉÅPROJECT-LISTÉÅTUPLE-FORMATCÅSORT-LISTÄÉÇ*CURRENT-ATTRIBUTES*√Ç*CURRENT-TUPLE-FORMAT*BÄ:\Ä√ÄA-P-LÄÉÇACTUAL-PROJECT-LISTÄCÇFINAL-PROJECT-LIST√ÄITEMSÄCÇTEMP-TUPLE-FORMATÄBÄ:BÄ:BÄ:ÉÄATTRBÄ:CÅATTRIBUTEÄ\Ä)ÇMACROS-EXPANDEDÄ\Ä™ÄPROGp¿¨ÄZLCÄ,ÅDO-NAMEDp¿BÄTÏÇINHIBIT-STYLE-WARNINGS™ÄSETFÄp¿BÄ\,Å*APPENDÄ“p¿BÄTÏÄFIRSTN“™ÄLIST“FÄ–¿√ÅPARSE-PROJECTÄ“™ÅSTRING-EQUAL¿p¿BÄ\¨ÅMEMBER-TESTÄ“jÅCOPY-LISTÄíÅÊÑQÅ√A¡ÅQB¡ÇÊÖQÅ¸ÅQäCÇQäCˇcC√v‰ÇQCQÖQ
Cís¸C?r‰ÅQäCÇQíl¸Å5ÊÅQäÅ¡Ç‰Ç5ÊÇQäÇ¡ÄQÑQÅQÖQÇQPPAD¡A¡Å¡E—AQG¡F¡¸FQGSH¡H5ÊHQ¸HSCF√¡G≈GÚÁEQ@¡ÇQäC@QäCx‰ÇQ
Jˇ€@QäCÇQäCˇcCíÇ¡É,‰ÅQB¡G€G—ÉQE¡I¡"¸IQESJ√ÅQP	òÊJQÑQP	ò‰ÅQJQäíÅ¡AQJQäíA√¸JÊÅQ
äÅ√¸ˇ€CI√¡E≈E‹ÁDQÇ¡ÅQÇQAQBQÑOÄÉBÄPÄÄÉÅOBTAIN-SORTÄÄÎÄ	1kÜÄ@	êFÄ:¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÑ\Ä™ÄSORTCÅQUICK-SORTBÄ:BÄ:\ÄBÄo\ÄBÄqBÄtBÄvBÄwÄÉ*PROVIDE-ERROR-MESSAGES*ëBÄ|“ÉÅVALIDATE-SYM“ÍÄTERPRI“,ÖERROR - Illegally specified sort clauseÄ¿™ÅWRITE-STRINGíÄ‰Ä5ÊÄQä
¸Ä5‰ÄÊÄ€¸ÄSˇ5‰ÄSÄ¡Å‰Å5ÊÅQä
¸Å5‰ÅÊÅ€¸ÅSˇ5‰ÅSÅ¡Ä‰@—ÄQB¡A¡¸AQBSˇ›íCA√¡B≈BˆÁ@QÄ¡ˇ€Ä$‰‰ÄPàÄﬂ¸Å‰B€B—ÅQ@¡C¡¸CQ@Sˇ›íCC√¡@≈@ˆÁBQÅ¡ˇ€Å‰‰ÄPàÅﬂÄQÅQÇOÄñBÄÑÄÄÉÅOBTAIN-WIDEÄÄÎÄ	1ÜÄ@	ÑFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄó\Ä√ÄWIDEPÄÇNUMBER-PER-LINEÄBÄ:\Ä√ÄSTATUS\ÄBÄo\ÄBÄwÄBÄëëBÄì“,ÅERROR - ¿BÄï“ÍÄPRIN1Ä“¨Ñ is not a legal number specificationÄ@›Ä ‰Ä1‰ÅÊÄQ¸ÅÊLÅ¡Å5‰ÅSÅ¡Å1‰ÅQˇÊÅÓ‰ÄPàÅQàPà@€¸Å€@QÅQÇOÄ©BÄóÄÄCÇPROCESS-QUICK-SORTÄÎÄ>ÜÄ@ÿFÄ#¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ™\Ä√ÄTUPLEÄBÄéBÄhBÄ:\ÄÄgBÄ:BÄ:BÄ:BÄlBÄ:\ÄBÄo\ÄBÄqBÄtBÄvBÄwÄÍÄSTRING“\ÄBÄY\ÄBÄY¿CÇQUICK-SORT-PREREQÄ“BÄÇ“BÄéíA—ÇQC¡B¡¸BQCSD¡D5ÊDQ¸DSäCB√¡C≈CÒÁAQ@¡ÅQ‰ˇ›¸C€C—ÅQA¡E¡¸EQASäCE√¡A≈A˜ÁCQ@QíÅ¡‰ÄQäÅQ@QúROÄªBÄ™ÄÄÉÅPROCESS-SORTÄÎÄ2ÜÄAêFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄº\ÄÄ≥BÄ
BÄaBÄhBÄdCÇ*CURRENT-DOMAINS*ÄBÄ:\ÄÅDOMAINSÄCÅKEY-VALUEÄÅATTR-POSBÄl\ÄBÄo\ÄBÄwBÄqÄBÄ¿BÄÅ“¨ÄANYP¿*ÅREVERSEÄ“BÄcíÉQC¡‰ÑQäCCSB√ˇ5ÊBQ¸BSB√ÑQPöäCˇcA¡	‰BQÑQPò‰AQÖQåC¸P@]@¡C≈·Á@Qä@¡ÄQÅQÇQ@Q§OÄŒBÄºÄÄCÇCONVERT-ATTRIBUTESÄÎÄ(ÜÄ@PFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄœ\Ä√ÅATTRIBUTE-LISTBÄ:\ÄBÄ:BÄ:BÄ:BÄm\ÄBÄo\ÄBÄqBÄtBÄvBÄwÄBÄ|“BÄ∑“ÍÅSTRING-UPCASEÄíÄÊRÄ5ÊÄQäÄ¡@—ÄQB¡A¡¸AQBSC¡ÚCQä¸C7‰CQä¸CQCA√¡B≈BÌÁ@OÄ›BÄœÄÄÇEXTRACT-KEY-HEAPÄÎÄÜÄ@FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄﬁ\ÄBÄÿÅKEY-LISTBÄ«ÉÅWHERE-CLAUSE™ÅPACKAGE-NAMEBÄ:BÄ:BÄ:ÄROÄÍBÄﬁÄÄCÇRETRIEVE-INTERNALÄÄÎÄTdÜÄ‡TÄFÄ\¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÎ\ÄBÄ`ÉÅKEYWORD-LISTBÄ:\Ä BÄ≥CÇCURRENT-ATTRIBUTESÇCURRENT-DOMAINSÄÉÅCURRENT-KEYÄÉÉCURRENT-IMPLEMENTATION-TYPEÄCÉCURRENT-STORAGE-STRUCTUREÄBÄhÉÇCURRENT-TUPLE-FORMATBÄiBÄ»ÉÄINTOBÄaBÄËÍÄPRINTÄ√ÄQPRINTÍÄSTREAMÇOUTPUT-FILE-NAMEBÄbBÄ°√ÅRETURN-TUPLESÄBÄ
ÉÄCARDÉÇRETRIEVE-INDEX-NAMEÄBÄé√ÅKEYWORD-VALUESBÄ£CÅINDEX-NAMEBÄ:BÄaBÄ≥BÄ:BÄ:\ÄBÄo\Äp¿BÄT¨ÄSENDp¿BÄTlÇCONDITION-BIND-IFÄp¿BÄTÏÅCONDITION-BINDp¿BÄTÏÇCATCH-CONTINUATION-IFÄp¿BÄTlÇCATCH-CONTINUATIONp¿BÄTÏÄERRSETBÄqp¿BÄ\lÅXR-BQ-LIST*ÅSEVENTHÄÍÄSIXTHÄÍÄFIFTHÄÍÄFOURTHÍÄTHIRDÄÍÄSECONDÍÄFIRSTÄBÄw¿ÜÄ AÄp¿lÄEH¨Ç*CONDITION-HANDLERS*—Ç*OUTPUT-WINDOW*Ä—CÉ*PROVIDE-STATUS-MESSAGES*Ä—√Ç*SYSTEM-RELATION-KEY*Ä—ÉÉ*SYSTEM-RELATION-ATTRIBUTES*—ÉÇ*DEFAULT-ANYP-WIDTH*—CÇ*SYSTEM-INDEX-KEY*—CÉ*SYSTEM-INDEX-ATTRIBUTES*Ä—ÉÅ*PKG-STRING*—ÉÑ*SYSTEM-RELATION-STORAGE-STRUCTURE*Ä—√Ñ*SYSTEM-RELATION-BASE-IMPLEMENTATION*Ä—ÉÅ*ACTIVE-DB*Ä—BÄë—ÉÇ*PARAMETER-CHECKING*ëÇACTIVE-DATABASEÄ“BÄí“\ÄlÅATTRIBUTES,ÅDOMAINSÄ¨ÄKEYÄ¨ÇIMPLEMENTATION-TYPEÄlÇSTORAGE-STRUCTUREÄ¨ÅTUPLE-FORMAT¨ÅCARDINALITYÄ¿ÉÅGET-RELATION“BÄì“lÇERROR - Relation Ä¿BÄï“BÄß“,Ñ does not exist in the database ¿\ÄBÄ¸ÅPROJECTÄ√ÄWHEREÄBÄ˝√ÄOUTPUTÍÄFORMATÉÄNUMÄÉÄWIDEBÄ˛BÄ≥BÄ
BÄˇÉÄDIRÄÉÄDOCÄÉÄKEYÄÉÄIMPÄÉÄSTOÄ√ÄUNIQUEBÄéBÄ¿ÉGET-KEYWORD-VALUE-PREREQ“ÉÇDE-NEST-KEYWORD-LIST“CÇGET-KEYWORD-VALUEÄ“BÄ∑¿lÅRETRIEVE-Ä¿lÄ-Ä¿™ÅCONCATENATEÄ“™ÅFIND-SYMBOLÄ“ÉÅSYSTEM-INDEX¿\ÄlÅINDEX-NAME¿™ÄANDÄ¿BÄ¿BÄ`¿BÄ‹“BÄ|“BÄ¿,ÅERROR - ¿,Ö is not a defined index on the relation ¿BÄ˝¿FÄê¿BÄó“BÄÑ“ÜÄ¿BÄP“ÏàERROR - There are no legal attributes contained in the PROJECT clauseÄ¿ÜÄ¿ÉÅEXTRACT-KEYÄ“FÄ–¿ÉÇCALCULATE-ATTRIBUTES“√ÅUNIQUE-TUPLESÄ“BÄº“BÄ™“BÄœ“BÄÅ“BÄy“BÄa“√ÅRETRIEVE-INTOÄ“ÉÇUNCONVERT-ATTRIBUTES“ÇSYSTEM-RELATIONÄ¿\Ä¨ÅCARDINALITYÄ¿ÅQTRIEVEÄ“CÇPRINTREL-INTERNAL*“p¿,ÄÏÄG7239Ä¿FÄá¿ÍÄERRORÄ¿p¿BÄ\ÏÅERRSET-HANDLER¿iÅEXPOSED-PÄ¿©ÅAPPEND-ITEMÄ¿ÏÇ~s tuple~:P retrievedÄ¿BÄ=“lÄ Ä¿,É~%~s tuple~:P retrievedÄ¿ÏÄ tuple¿eÄs¿jÅWRITE-CHAR“lÅ retrievedÄ‰Ä‰ÄQäÄ¡ÊRÄQPˇ€ö@¡@Ê	‰ÄPàÄQàPàPàR@SÄ¡@W@¡@SA¡@WB¡@[C¡@QBD¡@UBE¡@YBG¡@QBBU¡‰PÅQí¸ÅQäÅ¡PÅQíX¡JXQåCV¡0‰.‰P PP!PP"™P#í[¡$PP%P
P&P'P(PÄQ)ä*ö'P+PVQ)ä*ö*öˇ€$PJ[ªBV¡Ê‰Ä,PàJXQåCà-PàÄQàRX[‚ˇ›L¡XQBM¡JXQåCP¡JXQåCN¡	JXQåCS¡JXQåCO¡ÅÊM›S‰.PÅÊM€SÊM‰JXQåCJXQåC/P0PAR¡Y¡ÊR
JXQåCJXQåC/P1PAW¡T√ÊJW'‰RÄQXWJXQåCTQ‚WQAQGQ2P3PAH¡F¡Q¡K¡‰KÊ‰Ä4PàRˇ€K‰RÄQAQCQBQEQLQJXQåC5P6PAC¡E¡I¡Z¡P PDQ!PEQ"™P#í[¡ÄQAQKQCQLQIQZQJ[ª@¡KQF+	Ê@QKQFQ7P8PAF¡K¡@¡JXQå‰@‰@Q9ä@¡T‰@
‰@QTQKQFQAQBQ:≤@¡ÊRW‰@E‰@QWQFQ;öÙ˝@>‰H€XW<ä‚AQF¡F5ÊFQ*äF¡FQ\¡‰\Sˇ5‰\QB)äAQ'P=òÊHQ\QB)ä
¸\Sˇ5
Ê\SAQ'P=ò‰HQ\S*ä>íH¡\≈‚ÁKÊ‰Ä4PàRKQH+Ê@QKQHQ?ö@¡HQK¡XSäJ¡‰ÄQJQ@QKQFQCQDQEQQQÅQAQJ@∏ÊRX‰RN	‰@Q]¡D‰Ä]Sà]≈˚Á>¸MÊPÊO8‰KQAäK¡J‰JXQåC‚QQQ¡JQ‚ÄQ@QKQRQRQOQPQKQQQäCôÊQQ
¸QQ	Pˇ€KQäCQQäCˇcC>íˇ›ˇ›BPPCPP'P(PJQ‚ÄQ)ä*öD™BMQSQJE∏SHÊD‰FPGPTHPIPFPˇ€JC^√PJC_√÷JPä*äJ!BJ!B\‰P_¡KPˇ€LP@QäCMö_ëKPNPê¸O‰OQàOQOP@QäCMò¸ÄÄ@QäCàPPà@QäCÊQPRàSPàJQ‚ÄˇS‰@ÄOÄrBÄÎÄÄÉÇRETRIEVE-FLAVOR-HASHÄÎÄÜÄ¿FÄ
¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄs\ÄBÄ`BÄÿBÄaBÄBBÄË√ÅKEY-VALUE-LISTBÄBÄ:BÄ:BÄ:ÄÏÄflavor¿√ÅRETRIEVE-HASHÄíÄQÅQÇQÉQÑQÖQPÜQJºOÄBÄsÄÄÉÇRETRIEVE-FLAVOR-HEAPÄÎÄÜÄ¿FÄ	¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÄ\ÄBÄ`BÄÿBÄaBÄBBÄË√ÅHEAP-TRAVERSALBÄBÄ:BÄ:BÄ:ÄÉÇQTRIEVE-FLAVOR-HEAPÄíÜ‰ÜQ¸ÄQÅQÇQÉQÑQ¨OÄãBÄÄÄÄBÄ~ÄÎÄ2~ÜÄB<FÄL¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ~\ÄBÄ`BÄÿBÄaBÄBBÄËBÄ»BÄCBÄBÄ:\ÄBÄ:BÄ:p¿BÄ\lÇLEX-PARENT-ENV-REGp¿BÄ\ÏÅLEX-ENV-B-REGÄp¿BÄ\ÏÇLEX-CURRENT-VECTOR-REGp¿BÄ\¨ÇLEX-ALL-VECTORS-REGÄÇRETRIEVE-BUCKETÄ√ÅHASH-RELATIONÄÉÇTEMP-ATTRIBUTE-LISTÄÉÇCONV-ATTRIBUTE-LISTÄCÅTUPLE-LISTCÅKEY-VALUE%BÄ:BÄ:BÄ:\ÄÄo\ÄBÄtBÄvBÄqBÄw©ÇINTERNAL-FEF-OFFSETS\ÄFÄiÑVARIABLES-USED-IN-LEXICAL-CLOSURES\ÄBÄ°Ä™ÄEVAL“ÉÅENTRY-POINTÄ¿ÉÄGETP“\ÄFÄFÄ
¿\Ä)ÅINTERNALBÄ~Ä¿*ÅMAPHASHÄ“*ÅGETHASHÄ“BÄy“BÄ‹“ÏÄFLAVOR¿p¿BÄ\ÏÅSTRING-EQUAL*Ä“√ÇPROJECT-FLAVOR-PREREQÄ“ÉÇFAST-PROJECT-FLAVORÄ“ÏÄSTRUCT¿BÄ∑¿BÄ∑“BÄK“BÄ\“ÉÇFAST-PROJECT-STRUCTÄ“FÄê¿CÇEVAL-WHERE-PREREQÄ“ÇFAST-EVAL-WHEREÄ“BÄaíÑ5ÊÑQà‰Ñ›¸RáQPíG¡ÖÊPP”CGQê¸ÖQK¡‰JQKSGQ	í
íJ¡K≈¯ÁÜQäPê‰ÅQäI¡JQIQí¸ÜQäPê‰L—ÅQN¡M¡
¸MQPÄQäNSöCM√¡N≈NÙÁLQäI¡JQIQíJ¡ÑQ±ÊÑQÅQÄQPPAH¡Ñ¡JQÑQHQöJ¡J‰JQÅQÇQöJ√ˇ€
íF¡FOÄøBÄ~ÄÄBÄ∞ÄÎÄ
ÜÄ@åFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ∞\ÄÅKEY-VALÄ√ÄTUPLESBÄ:\ÄBÄ:BÄ:BÄñ\ÄBÄo\ÄBÄwiÉLEXICAL-PARENT-DEBUG-INFOÄBÄëÄBÄyíÅQ¿Pí¿¬ˇOÄŒBÄ∞ÄÄBÄªÄÎÄ#ÜÄ@úFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄª\ÄBÄ…BÄÿBÄ:\ÄBÄ:BÄ:BÄ:BÄ≥BÄ:BÄ:BÄ:\ÄBÄo\ÄBÄqBÄtBÄvÄ@—ÄQB¡A¡¸AQBSC¡D€D—ÅQF¡E¡¸EQCQFSˇãCE√¡F≈FˆÁDQCA√¡B≈BÊÁ@OÄ⁄BÄªÄÄCÇRETRIEVE-LIST-AVLÄÄÎÄÜÄ¿FÄ
¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ€\ÄBÄ`BÄÿBÄaBÄBBÄËBÄ|BÄBÄ:BÄ:BÄ:Ä¨ÄLIST¿ÉÅRETRIEVE-AVLíÄQÅQÇQÉQÑQÖQPÜQJºOÄÊBÄ€ÄÄÉÇRETRIEVE-FLAVOR-AVLÄÄÎÄÜÄ¿FÄ
¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÁ\ÄBÄ`BÄÿBÄaBÄBBÄËBÄ|BÄBÄ:BÄ:BÄ:ÄÏÄFLAVOR¿BÄÂíÄQÅQÇQÉQÑQÖQPÜQJºOÄÒBÄÁÄÄÉÇRETRIEVE-STRUCT-AVLÄÄÎÄÜÄ¿FÄ
¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÚ\ÄBÄ`BÄÿBÄaBÄBBÄËBÄ|BÄBÄ:BÄ:BÄ:ÄÏÄSTRUCT¿BÄÂíÄQÅQÇQÉQÑQÖQPÜQJºOÄ¸BÄÚÄÄCÇRETRIEVE-LIST-HASHÄÎÄÜÄ¿FÄ
¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ˝\ÄBÄ`BÄÿBÄaBÄBBÄËBÄ|BÄBÄ:BÄ:BÄ:Ä¨ÄLIST¿BÄ~íÄQÅQÇQÉQÑQÖQPÜQJºOÄBÄ˝ÄÄCÇRETRIEVE-LIST-HEAPÄÎÄÜÄ¿FÄ	¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ\ÄBÄ`BÄÿBÄaBÄBBÄËBÄ BÄBÄ:BÄ:BÄ:ÄCÇQTRIEVE-LIST-HEAPÄíÜ‰ÜQ¸ÄQÅQÇQÉQÑQ¨OÄBÄÄÄÉÇRETRIEVE-STRUCT-HASHÄÎÄÜÄ¿FÄ
¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ\ÄBÄ`BÄÿBÄaBÄBBÄËBÄ|BÄBÄ:BÄ:BÄ:ÄÏÄSTRUCT¿BÄ~íÄQÅQÇQÉQÑQÖQPÜQJºOÄBÄÄÄÉÇRETRIEVE-STRUCT-HEAPÄÎÄ	ÜÄ¿FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ\ÄBÄ`BÄÿBÄaBÄBBÄËBÄ BÄBÄ:BÄ:BÄ:ÄBÄ´¿BÄ¨“ÉÇQTRIEVE-STRUCT-HEAPÄíÜ	‰ÄQÅQÇQÉQÑQÜQPí¥ÄQÅQÇQÉQÑQ¨OÄ(BÄÄÄ√ÅSELECT-TUPLESÄÄÎÄ	ÜÄ‡	@FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ)\ÄBÄ`ÍÄ&RESTÄBÄÙ™Ä&KEYjÅ&OPTIONALÄjÅDIRECTORYÄÍÅDOCUMENTATIONÄBÄ=ÉÇIMPLEMENTATION-TYPEÄBÄ¸BÄBÍÄNUMBERBÄ<BÄ˝BÄ˛BÄéBÄ
BÄˇCÇSTORAGE-STRUCTUREÄBÄ…BÄEBÄ;BÄ?jÇ&ALLOW-OTHER-KEYSÄBÄ:\ÄBÄÙBÄ5BÄ6BÄ=BÄ7BÄ¸BÄBBÄ8BÄ<BÄ˝BÄ˛BÄéBÄ
BÄˇBÄ9BÄ…BÄEBÄ;BÄ?\ÄÈÅDOCUMENTATIONÄÏøáSame as Retrieve except that all attributes are retrieved.

   RELATION-NAME        - Name of the relation whose tuples are to be retrieved.
   WHERE                - Criterion to be used in selecting the tuples.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   WIDE                 - If T, result is printed out in Attribute: value format rather than as a table.
   NUMBER               - If WIDE is T, this keyword specifies the number of attributes per line.
   OUTPUT               - If the result is to be sent to a file, specify the file-name.
   PRINT                - If NIL, the result is not printed.
   QPRINT               - If T, the result is printed without formatting.
   QUICK-SORT           - Specifies the attributes to sort the result on.
   SORT                 - If any domain-specific, user-defined sort mechanism is to be used, this keyword
                          can be used.
   STREAM               - Specify the window to which the output is to be sent, if it is different than the
                          the *standard-output* or RTMS-interface.
   TUPLES               - If T, the resultant tuples are returned.
   UNIQUE               - If T, only unique tuples are retrieved.¿ÜÄì Ä\ÄiÅDIRECTORYÄBÄ=ÈÄFORMAT©ÇIMPLEMENTATION-TYPEÄ©ÄINTO©ÄKEYÄÈÄNUMBERÈÄOUTPUTÈÄPRINTÄÈÄQPRINTiÅQUICK-SORT©ÄSORTÈÄSTREAMiÇSTORAGE-STRUCTUREÄÈÄTUPLESÈÄUNIQUEÈÄWHEREÄ©ÄWIDE¿p¿BÄ\ÏÅSTORE-KEYARGSÄ“BÄ:¿ÍÄLIST*Ä“ÅRETRIEVEí@‰@QPˇ›A—†ÄQPˇ€@QöîOÄVBÄ)ÄÄBÄcÄÎÄ$X‘ÜÄA$0FÄ|¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄc\ÄBÄ…ÉÅSORT-CLAUSEÄBÄÿÉÅDOMAIN-LISTÄBÄ:\ÄÅAVL-TREE√ÄDOMAINBÄÁBÄ|ÉÅNEW-ELEMENTÄBÄ`CÅSORT-ORDER√ÅSORT-ELEMENT%ÄÇALREADY-SORTED-PÉÅSORT-ELEMENTBÄ…BÄ|\ÄBÄo\ÄBÄqBÄwÄBÄ(—BÄë—CÉ*PROVIDE-WARNING-MESSAGES*ëBÄ|“BÄí“BÄ¿BÄÅ“BÄy“\Ä,ÅNUMBERPÄ,ÅSTRINGPÄ¨ÄATOM¿BÄé“\Ä¨ÄASCÄlÄGT¨ÄGTEÄlÄGElÅINCREASING¨ÄDESÄ¨ÄDESClÅDECREASINGlÄLT¨ÄLTEÄlÄLE¿BÄì“lÅWARNING - ¿BÄï“BÄß“lÜ is not an attribute nor a recognized sort keyword¿ÏÑ          This element will be ignored¿,áERROR - No attributes specified in the sort clause --> Ä¿¨É        Sort can not proceed¿BÄa“BÄ∑¿ÏÄ-TEMP-¿ÍÄGENSYM“BÄ∑“BÄK“*ÇREAD-FROM-STRING“ÍÄAPPEND“ÇINSERT-AVL-LISTÄ“BÄ´¿ÉÄPUTP“BÄ€“\Ä¨ÄDESÄ¨ÄDESClÅDECREASINGlÄLT¨ÄLTEÄlÄLE¿BÄÕíÅQ±‰Å€ÇQB¡¸Å5ÊÅ‰ÅQä¸ÅSˇ5‰ÅSÅ¡ÅQI¡=‰ISˇ›íG¡ÊRGQÇQP	ò‰ÉQäCGQÇQP	öäCˇcÉQåCA¡BQGQä
íB¡AQPP	òÊÄQÅQÇQöˇGQPP	ò‰FQ‚GQF¡¸
‰ÄPàGQàPàÄPàI≈√ÁH‰HBÊ‰ÄPàÅQàÄPàRÄQÇQBQöC¡ÉQäÇQBQöBÉ¡PPPÇä¢äE¡ÄQJ¡CQK¡¸JSäJäˇ€äˇ€äö
CD√@QKSBQÇQÉQˇ€EQJ∫@¡J≈K≈JÁÁEQ@QP òEQÇQÇQBQˇ›ˇ€EQJ!∫Ä¡EQˇ€P òFQ"PP	ò‰ÄQ#äÄ¡ÄOÄéBÄcÄÄÅMAPTUPLEÄÎÄNÜÄ@êFÄ0¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄè\ÄCÅDBFUNCTIONÅRELATIONBÄ:BÄ:\ÄBÄo\Ä	BÄqBÄtBÄvBÄwBÄBÄBÄBÄBÄBÄ=ÏîMap a function on all the tuples in a relation using MAPCAR.

   DBFUNCTION  - Function to be applied to each and every tuple.
   RELATION    - Name of the relation.ÄÄBÄ —BÄëëBÄ-“p¿BÄbÏÄG7595Ä¿FÄ>¿BÄf¿BÄh¿jÅFUNCTIONPÄ“BÄ|“BÄì“¨ÑERROR - Illegal function definitionÄ¿BÄï“BÄí“BÄ…¿BÄUíÄÊRPPTP	PPˇ›JC@√PJCA√÷ÄQ
ääJ!BJ!B\Ê‰ÄPàRÅQäÅ¡ÊRA€A—ÅQPˇ›öC¡B¡¸BQCSÄãCB√¡C≈C˜ÁAOÄ¢BÄèÄÄÉÄMAPTÄÎÄFÜÄ@àFÄ,¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ£\ÄBÄòBÄôBÄ:BÄ:\ÄBÄo\ÄBÄqBÄtBÄwBÄBÄBÄBÄBÄBÄ=¨îMap a function on all the tuples in a relation using MAPC.

   DBFUNCTION  - Function to be applied to each and every tuple.
   RELATION    - Name of the relation.ÄÄBÄ —BÄëëBÄ-“p¿BÄbÏÄG7635Ä¿FÄ>¿BÄf¿BÄh¿BÄ†“BÄ|“BÄì“¨ÑERROR - Illegal function definitionÄ¿BÄï“BÄí“BÄ…¿BÄUíÄÊRPPTP	PPˇ›JC@√PJCA√÷ÄQ
ääJ!BJ!B\Ê‰ÄPàRÅQäÅ¡ÊRÅQPˇ›ö@¡‰@SÄ @≈¸ÁÅOÄ≥BÄ£ÄÄ√ÅPRINT-RELATIONÄÎÄÜÄ‡@FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ¥\ÄBÄôBÄ2BÄÙBÄ3BÄ4BÄ5BÄ6BÄ=BÄ7BÄBÄ¸BÄBBÄ8BÄ<BÄ˝BÄ˛BÄéBÄ
BÄˇBÄ9BÄ…BÄEBÄ?BÄ:BÄ:\ÄBÄÙBÄ5BÄ6BÄ=BÄ7BÄBÄ¸BÄBBÄ8BÄ<BÄ˝BÄ˛BÄéBÄ
BÄˇBÄ9BÄ…BÄEBÄ?\ÄBÄ=Ïø
Same as Retrieve without a where clause and all attributes are retrieved.

   RELATION-NAME        - Name of the relation whose tuples are to be retrieved.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   INDEX-NAME           - Name of the index to use in the retrieval.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   WIDE                 - If T, result is printed out in Attribute: value format rather than as a table.
   NUMBER               - If WIDE is T, this keyword specifies the number of attributes per line.
   OUTPUT               - If the result is to be sent to a file, specify the file-name.
   PRINT                - If NIL, the result is not printed.
   QPRINT               - If T, the result is printed without formatting.
   QUICK-SORT           - Specifies the attributes to sort the result on.
   SORT                 - If any domain-specific, user-defined sort mechanism is to be used, this keyword
                          can be used.
   STREAM               - Specify the window to which the output is to be sent, if it is different than the
                          the *standard-output* or RTMS-interface.
   TUPLES               - If T, the resultant tuples are returned.
   UNIQUE               - If T, only unique tuples are retrieved.¿ÜÄì Ä\ÄBÄABÄ=BÄBBÄCiÅINDEX-NAMEBÄDBÄEBÄFBÄGBÄHBÄIBÄJBÄKBÄLBÄMBÄNBÄOBÄQ¿BÄS“BÄUí@‰@QPˇ›A—†ÄQ@QîOÄ√BÄ¥ÄBÄ¥OÄ¥ÅPRINTRELÄBÄ:ÄÎÄ
ÜÄ‡
@FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ:\ÄBÄ`BÄ2BÄÙBÄ3BÄ4BÄ5BÄ6BÄ=BÄ7BÄBÄ¸BÄBBÄ8BÄ<BÄ˝BÄ:BÄ˛BÄéBÄ
BÄˇBÄ9BÄ…BÄEBÄ?BÄ:BÄ:\ÄBÄÙBÄ5BÄ6BÄ=BÄ7BÄBÄ¸BÄBBÄ8BÄ<BÄ˝BÄ:BÄ˛BÄéBÄ
BÄˇBÄ9BÄ…BÄEBÄ?\ÄBÄ=Ïø™Same as Retrieve except that all tuples are retrieved.

   RELATION-NAME        - Name of the relation whose tuples are to be retrieved.
   PROJECT              - List of attributes to be projected in the result.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   INDEX-NAME           - Name of the index to use in the retrieval.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   WIDE                 - If T, result is printed out in Attribute: value format rather than as a table.
   NUMBER               - If WIDE is T, this keyword specifies the number of attributes per line.
   OUTPUT               - If the result is to be sent to a file, specify the file-name.
   PRINT                - If NIL, the result is not printed.
   QPRINT               - If T, the result is printed without formatting.
   QUICK-SORT           - Specifies the attributes to sort the result on.
   SORT                 - If any domain-specific, user-defined sort mechanism is to be used, this keyword
                          can be used.
   STREAM               - Specify the window to which the output is to be sent, if it is different than the
                          the *standard-output* or RTMS-interface.
   TUPLES               - If T, the resultant tuples are returned.
   UNIQUE               - If T, only unique tuples are retrieved.Ä¿ÜÄî Ä\ÄBÄABÄ=BÄBBÄCBÄ¬BÄDBÄEBÄFBÄGBÄH)ÅPROJECTÄBÄIBÄJBÄKBÄLBÄMBÄNBÄOBÄQ¿BÄS“BÄ;¿BÄE¿BÄT“BÄUí@‰@QPˇ›A—†ÄQPˇ›Pˇ›@Q™	îOÄ”BÄ:ÄÄCÅATTR-CALCÄÄÎÄFÜÄ@òFÄ)¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ‘\ÄCÅCALC-LISTÄBÄÿBÄ:\Ä√ÅNEW-CALC-LISTÄÇNEW-CALC-ELEMENT√ÄRESULT√ÅRESULT-ELEMENTBÄ:CÄXÄ\ÄBÄo\ÄBÄqBÄt™ÄPUSHBÄwÄBÄ†“BÄ‹“BÄ¿BÄÅ“BÄÉ“FÄê¿BÄ‘“BÄy“BÄ|íÄQà‰ÄQäÅQPò+‰ÄÄ7‰ÄQäA√ÅQPò ‰AQCB√B¡AQä¸Ä5‰ÄQD¡‰DSE√ÅQP	PAA¡C¡BQCQ
íB¡@QAQä
í@¡D≈ÌÁ¸ÄQ@¡BQ@QÇOÄËBÄ‘ÄÄBÄ∫ÄÎÄ)dÜÄ@îFÄ;¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ∫\ÄBÄéBÄgBÄ:\ÄBÄ·BÄ:BÄ:BÄ:√ÄATTR%Ä\ÄBÄo\ÄÍÄPROG2ÄBÄwBÄqBÄtBÄvÄBÄkëBÄí“\ÄlÄLT¨ÄLTEÄlÄLElÅDECREASING¨ÄDESC¨ÄDESÄlÄGT¨ÄGTEÄlÄGElÅINCREASING¨ÄASCÄ¿BÄ¿BÄÅ“\ÄlÄLTlÄLE¨ÄLTEÄlÅDECREASING¨ÄDESC¨ÄDESÄ¿√ÄDBGTPÄ¿BÄ|“BÄy“\ÄlÄGTlÄGE¨ÄGTEÄlÅINCREASING¨ÄASCÄ¿BÄì“lÅWARNING - ¿BÄï“BÄß“,á is neither a valid quick-sort keyword nor an attribute.ÄA—ÄQC¡B¡J¸BQD€CSˇ›íD¡Ê@€RDQPPö8‚CWˇ›íPPò‰DQÅQPò‰@QDQ	P
í
äí@√$¸CWˇ›íPPò‰DQÅQPò‰@QDQÌ˝D5‰DQäC‰DQÅQPòÚÁ‰ÄPàDQàPàˇ€CB√¡C≈C¥Á@OÄBÄ∫ÄÄBÄUÄÎÄÜÄ‡@FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄU\ÄBÄ`BÄ2BÄÙBÄ3BÄ4BÄ5BÄ6BÄ=BÄ7BÄBÄ¸BÄBBÄ8BÄ<BÄ˝BÄ:BÄ˛BÄéBÄ
BÄˇBÄ9BÄ…BÄEBÄ;BÄ?BÄ:BÄ:\ÄBÄÙBÄ5BÄ6BÄ=BÄ7BÄBÄ¸BÄBBÄ8BÄ<BÄ˝BÄ:BÄ˛BÄéBÄ
BÄˇBÄ9BÄ…BÄEBÄ;BÄ?\ÄBÄ=Ïø“Retrieve some tuples from a relation satisying a where clause.

   RELATION-NAME        - Name of the relation whose tuples are to be retrieved.
   WHERE                - Criterion to be used in selecting the tuples.
   PROJECT              - List of attributes to be projected in the result.
   INTO                 - If the result is to be inserted in a relation, specify the name of that relation.
                          If the above relation is not defined, RTMS defines it with the following keywords.
   DIRECTORY            - Save directory for this relation.
   DOCUMENTATION        - Documentation for this relation.
   FORMAT               - List of print widths for the attributes in this relation.
   IMPLEMENTATION-TYPE  - Name of the implementation type to be used.
   INDEX-NAME           - Name of the index to use in the retrieval.
   KEY                  - List of the resultant attributes to form the key for this relation.
   STORAGE-STRUCTURE    - Name of the storage-structure.
   WIDE                 - If T, result is printed out in Attribute: value format rather than as a table.
   NUMBER               - If WIDE is T, this keyword specifies the number of attributes per line.
   OUTPUT               - If the result is to be sent to a file, specify the file-name.
   PRINT                - If NIL, the result is not printed.
   QPRINT               - If T, the result is printed without formatting.
   QUICK-SORT           - Specifies the attributes to sort the result on.
   SORT                 - If any domain-specific, user-defined sort mechanism is to be used, this keyword
                          can be used.
   STREAM               - Specify the window to which the output is to be sent, if it is different than the
                          the *standard-output* or RTMS-interface.
   TUPLES               - If T, the resultant tuples are returned.
   UNIQUE               - If T, only unique tuples are retrieved. ¿ÜÄï Ä\ÄBÄABÄ=BÄBBÄCBÄ¬BÄDBÄEBÄFBÄGBÄHBÄ“BÄIBÄJBÄKBÄLBÄMBÄNBÄOBÄPBÄQ¿BÄS“BÄÎí@‰@QPˇ›A—†ÄQ@QîOÄ BÄUÄÄBÄ~ÄÎÄiÌÜÄAhFÄÑ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ~\ÄBÄ`BÄdBÄaBÄeBÄbBÄ:\Ä
BÄgÅNEW-ATTRBÄhBÄkÇPROJECT-ELEMENTÄ√ÅNEW-ATTR-PART2BÄgBÄbBÄ:BÄl\ÄBÄo\ÄÄtBÄBÄBÄÊBÄqBÄwÄBÄ%—BÄkëBÄœ“BÄ¿BÄÅ“BÄí“BÄì“lÅWARNING - ¿BÄï“BÄß“¨É is not an attribute of the ¿lÅ relationÄ¿,ÜWARNING - Improperly specified project element Ä¿eÄ.¿BÄp“BÄ‹“lÅ          ¿,É is an attribute of the ¿lÅ relation.¿FÄê¿BÄ‘“BÄy“BÄ|“BÄÕíÇQä@¡Ç€@QF¡ÑQG¡™¸FSA¡A5"ÊAQÅQPò	‰AQB]B¡AQÇ]Ç¡Gá‰ì¸A5ÊAQˇ›êê‰é‰	Ä
PàAQàPàÄQàPàÇ¸A
Ê~‰	ÄPàASàPàv¸ASäÅQPò‰n‰	ÄPàAQàPà	ÄPàASàPàÄQàPà[¸AWˇ5*‰GÊASäÅQPò‰ÅQäCASäÅQPöäCˇcˇkÉQåC¸P¸GSC]C¡AWÅQPPAE¡D√ÇQíÇ¡ASEQíB]B¡.¸AWäÅQPòÊAQB]B¡G ÊP¸ASAWíB]B¡AWäA√Ç]Ç¡GÊAQÅQPò‰ÅQäCAQÅQPöäCˇcÉQåC¸P¸GSC]C¡F≈G≈FTÁBQäB¡CQäC¡ÇQ@¡Ç€@QH¡‰HSI√ÇQPòÊIQÇ]Ç¡H≈ıÁÇQäBQCQÉOÄ8BÄ~ÄÄBÄ\ÄÎÄ	)ÜÄÑ	PFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ\\ÄBÄÿBÄ4\ÄBÄÈCÅ*PKG-NAME*BÄ:\ÄBÄ:BÄ:BÄ:BÄm\ÄBÄo\ÄBÄqBÄtBÄvÄBÄBëBÄ∑¿lÄ:Ä¿BÄ∑“BÄK“BÄÉívÊPÅ¡@—ÄQB¡A¡¸AQBSC¡C7‰PÅQPCQä¢ä¸CQCA√¡B≈BÎÁ@OÄGBÄ\ÄÄBÄYÄÎÄL®ÜÄ@¯FÄ\¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄY\ÄBÄ≥BÄaBÄhBÄ:\ÄBÄ·ÅELEMENTÄBÄ:BÄ:BÄ:BÄ„BÄ:√Ä%TUPLEÅATT-LISTÅVAL-LISTBÄ:BÄ:BÄ:BÄl\ÄBÄo\ÄÍÄUNLESSBÄBÄBÄqBÄtBÄvBÄwÄÍÄEQUALÄ¿BÄ„¿BÄ|“ÉÅPARSE-WHEREÄ“BÄ\“jÅMAKUNBOUND“BÄÉ“BÄ∑“BÄ‹“BÄ¿BÄÅ“BÄ™“*ÅNREVERSEíB—ÇQD¡C¡¸CQDSE¡E5‰EWˇ5	‰ESPPEWöäBí¸EQCC√¡D≈DËÁBQÇ¡ÅQäD€C¡CC‰CSöCDSÄDÊCSàC≈D≈Û˝ÄQF¡F‰FSG¡ÅQGQI¡H¡‰HS	äIS»H≈I≈H¯ÁJ€J—ÇQL¡K¡*¸KQLSM¡M5ÊMQ
ä	äûC¸M
ÚMWäÅQPò‰MWä	ä¸MWA¡MSÅÊMÚMS
ä	äAQä»B¸AQäCK√¡L≈L‘ÁJQ@]@¡F≈∫ÁL€L—ÇQJ¡F¡¸FQJSE¡E5ÊEQ¸ESäCF√¡J≈JÒÁLQÅ¡@Qä@√ÅQÇQÉOÄ\BÄYÄÄBÄ[ÄÎÄ<öpÜÄB<¯FÄ÷¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ[\ÄBÄ`BÄ¸BÄ≥BÄaBÄhBÄ¯BÄ˘BÄ˙BÄbBÄÙBÄˆBÄ:\ÄÉÄTEMPBÄgCÅOLD-VALUESBÄiBÄ:BÄlÉÄATTDBÄ·CÅ%ATTRIBUTEBÄ:BÄ:ÅATTR-RELCÅATTR-INTOÄ√ÄTEMP-D\ÄBÄo\ÄBÄBÄBÄBÄqBÄtBÄBÄwÄÉÇ*VALIDITY-CHECKING*Ä—BÄë—√Ç*SYSTEM-ATTRIBUTE-KEY*—√É*SYSTEM-ATTRIBUTE-ATTRIBUTES*Ä—BÄ#—BÄ$ëBÄ]¿\ÄlÅATTRIBUTES¿BÄ¿BÄ`¿BÄ∑“BÄ|“BÄ`“ÇSYSTEM-ATTRIBUTE¿\Ä,ÇDOMAIN-FUNCTIONÄÏÅDEFAULT-VALUEÄ¨ÄDOCÄ¿BÄP¿√ÅATTRIBUTE-NAME¿BÄ‹“ÉÄDOMÄ¿p¿BÄT¨ÄDEFÄ¿BÄA¿ÉÄANYP¿BÄy“\ÄBÄB¿BÄH“BÄÅ“BÄÕ“BÄC¿\ÄBÄC¿BÄD¿\ÄBÄD¿BÄB¿BÄ=¿\ÄBÄ=¿BÄ@¿\ÄBÄ@¿\ÄBÄA¿√ÄDEFREL“\Ä,ÇDOMAIN-FUNCTIONÄ¿¨ÄANYP¿BÄ∑“BÄì“ÏÇERROR - The attribute ¿BÄï“BÄß“ÏÅ in relation Ä¿¨Ç and the attribute Ä¿,É in the output relation ¿lÑ have different domain predicates.¿¨ÉERROR - The output relation ¿¨  does not have all the attributes required to insert the retrieved tuples. Ä¿ÏÄ has Ä¿ÏÖ as attributes and the retrieve call requires ¿¨É attributes in the relation ¿lÇ to be projected.Ä¿BÄ…¿√ÄINSERTí	PP
PPPPÅQäö™B@¡ñÊÑQ@¡A€@QD¡H‰DSE¡F€PPPPPPPÄQäöPPE5ÊEQ¸EWˇ5ÊEW¸ˇ€äöö™BF¡‰AQE5ÊEQ¸ESPFSPFWPE5ÊF[¸ˇ€≤	¸AQE5ÊEQ¸ESPPíííA¡E5‰ESä¸EQC]C¡D≈∏ÁP QíB‚ÖQD√Hœ	‰HQCQPò‰HQG¡ˆ˝RG‰P QíB‚ÖQ¸CQäBäÖ¡ÅQAQPP QíB‚ÜQ P!P QíB‚áQ"PÖQ#P$P QíB‚àQ%P&P QíBP'P QíBJ∫(òÅÊR@S@√äCÑQäC|_‰ÑQ@QJ¡I¡V¸ISJSL¡K¡M€K5‰KSK¡KQäQPòE‰PP)PPPPPÄQäöPPKQäöö™BPP)PPPPPÅQäöPPL5ÊLQ¸LSäöö™BM√rÊMQ*P+êÊ‰,Ä-P.àKQ/à0P.àÄQ/à1P.àLQ/à2P.àÅQ/à3P.àRI≈J≈I‰JßÁ¸‰,Ä4P.àÅQ/à5P.àÅQ/à6P.à@Q/à7P.àÉQ/à8P.àÄQ/à9P.àRPPíB¡⁄⁄ÅQ:PÇQí;êBS¿BW¿ÄOÄîBÄ[ÄÄBÄZÄÎÄ0ÜÄ@hFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄZ\ÄBÄ…BÄ:\Ä
BÄ:BÄ:BÄñBÄòBÄöBÄúÉÅRESULT-TABLEBÄ:BÄ„ÉÅHASH-BUCKETÄ\ÄÄo\ÄBÄqBÄtBÄwBÄ•\ÄFÄ
BÄ®\ÄBÄ…Ä©ÄTEST¿BÄX¿*ÇMAKE-HASH-TABLEÄ“BÄ¥“p¿BÄ\lÅMEMBER-EQL“p¿BÄT,ÅPUTHASHÄ“\ÄFÄÜ¿¿\ÄBÄ±BÄZÄ¿BÄ≥íPPíF¡ÄQG¡‰GSH¡I€HQFQíI¡‰HQIQêÊHQHQI]¸HQHQFQòG≈ÍÁÄ€	P
P”CFQêÄOÄ∞BÄZÄÄBÄÆÄÎÄÜÄ@åFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÆ\ÄBÄ„CÄYÄBÄ:\ÄBÄ:BÄ:BÄñ\ÄBÄo\ÄBÄwBÄÕBÄöÄÄQ¿\¿¬ˇOÄΩBÄÆÄ1Ä\Äp¿BÄ\,ÑFASL-RECORD-FILE-MACROS-EXPANDED\ÄBÄ8\Ä\ÄÍÄDEFUNÄÜÄ'\Äp¿BÄT¨ÄDEFFÜÄb\ÄBÄWÜÄ6\ÄBÄıÜÄJ=¯\ÄBÄÊÜÄ•ò\ÄBÄÜÄz(á\ÄBÄÜÄ{öÕ\ÄBÄÜÄ:}n\ÄBÄÜÄxıø\ÄBÄÜÄZiÛ\ÄBÄÜÄ{ƒ≤\ÄBÄÜÄ2ª=\ÄBÄÜÄ.Ÿã\ÄBÄÜÄ-i\ÄBÄÜÄ~…z\ÄBÄÜÄ<pë\ÄBÄÜÄ`sN\ÄBÄÜÄ|ƒÙ\ÄBÄ	ÜÄaM*\ÄBÄwÜÄ[ÊÑ\ÄBÄvÜÄ(Ã¢\ÄBÄtÜÄ*˝j\ÄBÄqÜÄ=Ã#ÄÄy soon.
;;
(if (null (do ((sort-element sort-clause (cdr sort-element)))
      ((null sort-element) t)
    (if (null (setf sort-element% (validate-sym (car sort-element) t)))
(return-from sort-list nil))
    (cond ((member sort-element% attribute-list :test 'string-equal)
   ;;
   ;;  Determine the domain for this attribute
   ;;
   (setf domain (nth (- (length domain-list)
   (length (member sort-element% attribute-list
     :test 'string-equal)))
       domain-list))
   (setf key-list (append key-list (list sort-element%)))
   (cond ((not (member domain '("NUMBERP" "STRINGP" "ATOM") :tesLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540827. :SYSTEM-TYPE :LOGICAL :VERSION 2. :TYPE "LISP" :NAME "SAVE" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :CHARACTERS T :NOT-BACKED-UP T :CREATION-DATE 2753699838. :AUTHOR "REL3" :LENGTH-IN-BYTES 19595. :LENGTH-IN-BLOCKS 20. :BYTE-SIZE 8.)

;;; -*- Mode:Common-Lisp; Package:RTMS; Fonts:(*CODE-FONT* *COMMENT-FONT* *STRING-FONT*); Base:10 -*-
;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved
;;; SAVE
;;;
;;; This file contains the following Explorer extensions to CommonLisp Standard as Indicated in the June 1985 Explorer Lisp
;;; Reference
;;;     fs:directory-list
;;;     dump-forms-to-file
;;;     deff
;;;     errset
;;;
;;; The following function contains flavor references and thus are incompatable with CommonLisp. Their removal will not
;;; effect the functionality of RTMS.
;;;     save-flavor-hash
;;;     save-flavor-heap
;;;     save-flavor-avl
;;;
;;;   Change History
;;;  Change History
;;;   03.31.87   MRR   Fixed SAVE-RELATION for saving Command files.
;;;   04.02.87   MRR   Fixed several Save functions to allow saving to remote hosts.

(defun save-flavor-hash (relation pathname rel-definition)
   (dump-forms-to-file pathname `(,rel-definition
   (setf var1 ',(getp relation 'entry-point))
   (putp ',relation var1 'entry-point))))

(defun save-flavor-heap (relation pathname rel-definition)
  (dump-forms-to-file pathname `(,rel-definition
  (setf var1 ',(getp relation 'entry-point))
  (putp ',relation var1 'entry-point))))

(defun save-flavor-avl (relation pathname rel-definition)
  (dump-forms-to-file pathname `(,rel-definition
  (setf var1 ',(getp relation 'entry-point))
  (putp ',relation var1 'entry-point))))

(defun save-list-avl (relation pathname rel-definition)
  (dump-forms-to-file pathname `((setf var1 ',(getp relation 'entry-point))
  ,rel-definition
  (putp ',relation var1 'entry-point))))

(defun save-list-hash (relation pathname rel-definition)
   (dump-forms-to-file pathname `((setf var1 ',(getp relation 'entry-point))
   ,rel-definition
   (putp ',relation var1 'entry-point))))

(defun save-list-heap (relation pathname rel-definition)
  (dump-forms-to-file pathname `((setf var1 ',(getp relation 'entry-point))
  ,rel-definition
  (putp ',relation var1 'entry-point))))

(defun save-struct-avl (relation pathname rel-definition)
   (dump-forms-to-file pathname `(,rel-definition
   (setf var1 ',(getp relation 'entry-point))
   (putp ',relation var1 'entry-point))))

(defun save-struct-hash (relation pathname rel-definition)
   (dump-forms-to-file pathname `(,rel-definition
   (setf var1 ',(getp relation 'entry-point))
   (putp ',relation var1 'entry-point))))

(defun save-struct-heap (relation pathname rel-definition)
  (dump-forms-to-file pathname `(,rel-definition
  (setf var1 ',(getp relation 'entry-point))
  (putp ',relation var1 'entry-point))))

(deff save-db 'save-database)

(defun save-database (database-name &rest keyword-list
      &key &optional directory &allow-other-keys
      &aux keys temp-dir pathname temp-rel (error-flag nil))
  "Save all system relations and the user-defined, modified relations.

   DATABASE-NAME    - Name of the database to be saved.
   DIRECTORY         - Name of the directory in which it is to be saved."
  directory temp-rel
  (block save-database
  (setf keys (copy-list keyword-list))
  (if (not (active-database))
      (return-from save-database nil))
  (if (null (setf database-name (validate-sym database-name t)))
      (return-from save-database nil))
  (if *transaction-on*
      (progn
(setf *transaction-forms-postponed* (append *transaction-forms-postponed*
       (list `(save-database ',database-name ',keys))))
(return-from save-database database-name)))

  (cond ((not (equal *active-db* database-name))
 (cond (*provide-error-messages*
(format *standard-output* "~%ERROR - Only the current database may be (or needs to be) saved.")
(format *standard-output* "~%        The current database is ~s" *active-db*)))
 (return-from save-database nil)))
  (setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
 ((or (null keyword-list)(not (listp (car keyword-list)))) keyword-list)))
  ;;
  ;; Prepare the keyword-list for use
  ;; Note: Keywords need to be added to allow the user to specify the type of format in which the relations should be saved. This will be
   ;; done later...
  ;;
  (setf keyword-list (get-keyword-value-prereq '(dir) keyword-list))
  ;;form the path. If database-name is given it has to be the active database.
  (setf pathname (concatenate 'string (setf temp-dir (get-directory keyword-list)) database-name ".XLD"))
  ;;
  ;;  If the directory specified is not the save directory for this database, mark all relations as modified so that all of the relations will be
   ;; stored in the new directory
  ;;
  (cond ((not (equal *save-directory* temp-dir))
 (delete-or-modify 'system-relation t t '("MODIFIEDP") '(t))
 (delete-or-modify 'system-relation t t '("SAVE-DIRECTORY") (list temp-dir)) ;mrr 04.02.87
 (setf *save-directory* temp-dir)))
  ;;for each relation in the database call saverel It will be saverel-qfasl soon. Save the database definition.
  (cond ((errset (fs:directory-list temp-dir) nil)  ;mrr 04.02.87
 (dump-forms-to-file pathname (list (list 'setf  '*system-relation-base-implementation*
     *system-relation-base-implementation*)
      (list 'setf '*system-relation-storage-structure*
     *system-relation-storage-structure*)
      (list 'define-database* database-name
     (list 'dir temp-dir 'doc *database-documentation*
    'env *environment-name*)))))
(t
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - Directory ~s does not exist" temp-dir))
 (return-from save-database nil)))
  (mapt (function (lambda (tuple &aux temp-rel temp-dir)
    (setf temp-dir (car (project-list (list tuple) *system-relation-attributes*
         '("RELATION-NAME" "SAVE-DIRECTORY"))))
    (setf temp-rel (read-from-string (concatenate 'string *pkg-name* (car temp-dir)))
  temp-dir (cadr temp-dir))
    (unless (errset (fs:directory-list temp-dir) nil) ;mrr 04.02.87
      (if *provide-error-messages*
  (format *standard-output* "~%ERROR - Directory ~s does not exist" temp-dir))
      (setf error-flag t)
      (return-from save-database nil))
    (save-relation temp-rel 'dir temp-dir)))
'system-relation)
  (if *provide-status-messages*
      (format *standard-output* "~%The ~s database has been saved in ~s" database-name temp-dir))
  (if error-flag
      (return-from save-database nil)
    (return-from save-database database-name))))

(deff save-env 'save-environment)

(defun save-environment (envname &rest keyword-list
 &key &optional directory &allow-other-keys
 &aux pathname dir keys rel-imp rel-sto)
  "Save an environment.

   ENVNAME   - Name of the environment to be saved.
   DIRECTORY - Name of the directory in which it is to be saved."
  directory
  (block save-environment
  (if (not (setf envname (validate-sym envname t)))
      (return-from save-environment nil))
  (setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
 ((or (null keyword-list)(not (listp (car keyword-list)))) keyword-list)))
  (setf keys (copy-list keyword-list))
  (cond (*transaction-on*
 (setf *transaction-forms-postponed* (append *transaction-forms-postponed*
        (list `(save-environment ',envname ',keys))))
 (return-from save-environment envname)))
  (setf keyword-list (get-keyword-value-prereq '(dir) keyword-list))
  (setf pathname (concatenate 'string (setf dir (get-directory keyword-list)) "rtms-environment-" envname
      ".XLD"))
  (unless (errset (fs:directory-list dir) nil)  ;mrr 04.02.87
    (if *provide-error-messages*
        (format *standard-output* "~%ERROR - The ~s directory does not exist" dir))
    (return-from save-environment nil))
  (setf rel-imp (subseq *relation-implementation* 0 (search "-" *relation-implementation*))
rel-sto (subseq *relation-implementation* (+ (search "-" *relation-implementation*) 1)))
  (unwind-protect
      (dump-forms-to-file pathname (list (list 'define-environment `(quote ,envname) `(quote auto-save)
         `(quote ,*auto-save-relations*) `(quote directory)
         `(quote ,*save-directory*) `(quote errors)
         `(quote ,*provide-error-messages*) `(quote para)
         `(quote ,*parameter-checking*) `(quote rel-imp) `(quote ,rel-imp)
         `(quote rel-sto) `(quote ,rel-sto) `(quote status)
         `(quote ,*provide-status-messages*) `(quote sys-imp)
         `(quote ,*system-relation-base-implementation*) `(quote sys-sto)
         `(quote ,*system-relation-storage-structure*) `(quote validity)
         `(quote ,*validity-checking*) `(quote warnings)
         `(quote ,*provide-warning-messages*))))
    nil)
    (return-from save-environment envname)))

(deff save-rel 'save-relation)

(defun save-relation (relation-name &rest keyword-list
      &key &optional directory save type &allow-other-keys
      &aux insert-routine pathname templist keys on-disk? modp dir attributes imp ss
      temp-message key tuple-format doc temp qtrieve-var save-type always-save temp-dir)
   "Save a relation if it is modified.

    RELATION-NAME - Name of the relation to be saved.
    DIRECTORY     - Name of the directory in which it is to be saved.
    SAVE          - If T, saves the relation even if the relation is not modified.
    TYPE          - Two types of save are allowed: COMMAND and XLD. This keyword can be used to
                    specify the type."
   directory save type
   (block save-relation
 (if (not (active-database))
     (return-from save-relation nil))
 (if (null (setf relation-name (validate-sym relation-name t)))
     (return-from save-relation nil))
 (setf keys (copy-list keyword-list))
 (cond (*transaction-on*
(setf *transaction-forms-postponed* (append *transaction-forms-postponed*
       (list `(save-relation ',relation-name ',keys))))
(return-from save-relation relation-name)))
  (setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
 ((or (null keyword-list)(not (listp (car keyword-list)))) keyword-list)))
 (setf keyword-list (get-keyword-value-prereq '(type dir save) keyword-list))
  (setf qtrieve-var (cadr (get-relation relation-name '("RELATION-NAME" "MODIFIEDP" "SAVE-DIRECTORY"
    "ATTRIBUTES" "IMPLEMENTATION-TYPE" "STORAGE-STRUCTURE"
    "KEY" "TUPLE-FORMAT" "DOC" "DISK") t t)))
  (cond ((not qtrieve-var)
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - The relation ~S is not defined in the database ~S"
     relation-name *active-db*))
 (return-from save-relation nil)))
  ;;
  ;;The relation is defined and (if saved) also restored.
  ;;
  (setf modp (second qtrieve-var)
dir (third qtrieve-var)
attributes (fourth qtrieve-var)
imp (fifth qtrieve-var)
ss (sixth qtrieve-var)
key (seventh qtrieve-var)
tuple-format (nthcdr 7 qtrieve-var)
doc (second tuple-format)
on-disk? (third tuple-format)
tuple-format (first tuple-format))
  ;;
  ;;LATER.... If the keyword for save-format is QFASL or data save it in that fashion. check if the relation provided is a valid dbms object
  ;;See if the TYPE has been provided.
  ;;
 (setf save-type (or (car (get-keyword-value '(type) keyword-list)) 'xld)
       always-save (car (get-keyword-value '(save) keyword-list)))
  ;;
  ;;Dump-forms-to-file has a bug for hash tables. Until that is fixed we will have to avoid QFASL format for hash storage structure.
  ;;
  (cond ((equal save-type  'xld)
 (return-from save-relation (saverel-qfasl relation-name keyword-list)))
((not (equal save-type 'command))
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - ~s is an unrecognized save type ." save-type))
 (return-from save-relation nil)))
  ;;
  ;;create a pathname to access the file where this relation is stored
  ;;
  (setf temp-dir (get-directory keyword-list dir))
  (unless (errset (fs:directory-list temp-dir) nil)  ;mrr 04.02.87
   (if *provide-error-messages*
       (format *standard-output* "~%ERROR - Directory ~s does not exist" temp-dir))
 (return-from save-relation nil))
  (cond ((not (equal temp-dir dir))
 (delete-or-modify 'system-relation t
   (list 'string-equal (concatenate 'string *pkg-name* "RELATION-NAME")
  (string-upcase relation-name))
   '("SAVE-DIRECTORY") (list temp-dir))
 (delete-or-modify 'system-relation t
   '(string-equal (concatenate 'string *pkg-name* "RELATION-NAME") "SYSTEM-RELATION")
   '("MODIFIEDP") '(t))))
  (setf pathname (concatenate 'string temp-dir *active-db* "-" relation-name "." (string save-type))) ;mrr 03.31.87
  (setf templist nil)
  (cond ((and (not modp) (not always-save) (equal temp-dir dir))
 (if *provide-status-messages*
     (format *standard-output* "~%The relation ~S has not been modified" relation-name))
 (return-from save-relation relation-name)))
  (if on-disk?
      (load-relation relation-name 'dir dir))
  (unwind-protect
      (progn
(setf pathname (open pathname :direction :output)) ;mrr 03.31.87
(format pathname "~&(SETF *non-qfasl-restore* T)")
(if (not (member relation-name *system-relations* :test 'string-equal))
    (format pathname "~&(DEFREL-RESTORE ~S ~S ~S)"
    relation-name attributes (list 'imp imp 'modifiedp nil 'sto ss 'key key
      'tuple-format tuple-format 'doc doc 'dir dir)))

(if (and (not (member relation-name *system-relations* :test 'string-equal)) (string-equal ss "hash"))
    (format pathname "~&(PUTP '~S (make-hash-table :test 'equal) '~S)" relation-name 'entry-point))
(setf temp nil)
(mapt (function (lambda (%tuple)
  (setf temp (cons %tuple temp))))
      relation-name)
(if temp
    (progn
      (setf insert-routine (read-from-string (concatenate 'string "INSERT-" imp "-" ss)))
      (format pathname  "~&(~S '~S '~S '~S '~S)" insert-routine relation-name attributes (reverse temp)
      key relation-name)))
(format pathname "~&(SETF *non-qfasl-restore* NIL)")
(close pathname)
(delete-or-modify 'system-relation t
  (list 'string-equal (concatenate 'string *pkg-name* "RELATION-NAME")
 (string-upcase relation-name))
  '("MODIFIEDP") '(nil))
(cond ((not (member relation-name *system-relations* :test 'string-equal))
       (setf temp-message *provide-status-messages*
     *provide-status-messages* nil)
       (save-system-relations)
       (setf *provide-status-messages* temp-message)))
(if *provide-status-messages*
    (format *standard-output* "~%The relation ~s has been saved in the directory ~s"
    relation-name temp-dir)))
    nil)
 (return-from save-relation relation-name)))

(defun saverel-qfasl (relation keyword-list
      &aux keys rel-definition pathname temp-dir card mod dir attributes imp ss key tuple-format
      doc qtrieve-var temp-message on-disk?)
  (block saverel-qfasl
(setf keys (copy-list keyword-list))
(if *transaction-on*
    (progn
      (setf *transaction-forms-postponed* (append *transaction-forms-postponed*
      (list `(saverel-qfasl ',relation ',keys))))
      (return-from saverel-qfasl relation)))
        (setf keyword-list (get-keyword-value-prereq '(dir type save) keyword-list))
        (setf qtrieve-var (cadr (get-relation relation '("RELATION-NAME" "MODIFIEDP"  "SAVE-DIRECTORY"
     "ATTRIBUTES" "IMPLEMENTATION-TYPE" "STORAGE-STRUCTURE"
     "KEY" "TUPLE-FORMAT" "DOC" "CARDINALITY") t t)))
(cond ((not qtrieve-var)
       (if *provide-error-messages*
   (format *standard-output* "~%ERROR - The relation ~S is not defined in the database ~S"
   relation *active-db*))
       (return-from saverel-qfasl nil)))
;;The relation might be defined and restored.
(setf mod (second qtrieve-var) dir (third qtrieve-var)
      attributes (fourth qtrieve-var)
      imp (fifth qtrieve-var) ss (sixth qtrieve-var)
      key (seventh qtrieve-var)
      tuple-format (nthcdr 7 qtrieve-var) doc (second tuple-format)
      card (third tuple-format) on-disk? (fourth tuple-format) tuple-format (first tuple-format))
(setf temp-dir (get-directory keyword-list dir))
(unless (errset (fs:directory-list temp-dir) nil) ;mrr 04.02.87
       (if *provide-error-messages*
   (format *standard-output* "~%ERROR - The ~s directory does not exist" temp-dir))
       (return-from saverel-qfasl nil))
(cond ((not (equal temp-dir dir))
       (delete-or-modify 'system-relation t (list 'string-equal 'relation-name `(quote ,relation))
 '("SAVE-DIRECTORY") (list temp-dir))
       (delete-or-modify 'system-relation t '(string-equal relation-name "SYSTEM-RELATION")
 '("MODIFIEDP") '(t))))
;;check if it is modified
(cond ((and (not mod)(equal temp-dir dir))
       (if *provide-status-messages*
   (format *standard-output*
   "~%The relation ~S has not been modified and thus does not need to be saved"
   relation))
 (return-from saverel-qfasl relation)))
(setf pathname (concatenate 'string temp-dir *active-db* "-" relation ".XLD"))

;; Form the relation definition.
;;
;; In QFASL we have to save every bit of information (eg. cardinality) because the tuples are restored automatically
;; (without INSERT).
(setf rel-definition (list 'defrel-restore relation attributes
    (list 'imp imp 'cardinality card 'modifiedp nil 'sto ss 'key key
   'tuple-format tuple-format 'doc doc 'dir dir)))
(if on-disk?
    (load-relation relation 'dir dir))
(unwind-protect
    (progn
      (funcall (find-symbol (concatenate 'string "SAVE-" imp "-" ss) *pkg-string*) relation pathname
       rel-definition)
      (delete-or-modify 'system-relation t (list 'string-equal 'relation-name `(quote ,relation))
 '("MODIFIEDP") '(nil))
      (cond ((not (member relation *system-relations* :test 'string-equal))
     (setf temp-message *provide-status-messages*
   *provide-status-messages* nil)
     (save-system-relations)
     (setf *provide-status-messages* temp-message)))
      (if *provide-status-messages*
  (format *standard-output* "~%The relation ~s has been saved in the directory ~s"
  relation temp-dir)))
  nil)
(return-from saverel-qfasl relation)))


(defun save-system-relations (&rest ignore &aux pathname)
  (block save-system-relations
;;
;;If no system relation is modified, we do not want to save the database definition.
;;
(if (not (member '(t) (qtrieve 'system-relation *system-relation-attributes* '("MODIFIEDP")
   *system-relation-key* '(member relation-name *system-relations*
      :test 'string-equal))))
    (return-from save-system-relations t))
;;
;;For each system relation in the database call saverel
;;
(unless (errset (fs:directory-list *save-directory*) nil) ;mrr 04.02.87
  (if *provide-error-messages*
   (format *standard-output* "~%ERROR - The ~s directory does not exist" *save-directory*))
       (return-from save-system-relations nil))
(mapcar (function (lambda (sys-rel)
    (save-relation sys-rel 'dir *save-directory*)))
*system-relations*)
(setf pathname (concatenate 'string *save-directory* *active-db* ".XLD"))
(dump-forms-to-file pathname (list (list 'setf '*system-relation-base-implementation*
    *system-relation-base-implementation*)
     (list 'setf '*system-relation-storage-structure*
    *system-relation-storage-structure*)
     (list 'define-database* *active-db*
    (list 'dir *save-directory* 'doc *database-documentation*
          'env *environment-name*))))
(return-from save-system-relations t)))
G2ÄBÄwBÄqBÄtBÄvÄBÄkëBÄí“\ÄlÄLT¨ÄLTEÄlÄLElÅDECREASING¨ÄDESC¨ÄDESÄlÄGT¨ÄGTEÄlÄGElÅINCREASING¨ÄASCÄ¿BÄ¿BÄÅ“\ÄlÄLTlÄLE¨ÄLTEÄlÅDECREASING¨ÄDESC¨ÄDESÄ¿√ÄDBGTPÄ¿BÄ|“BÄy“\ÄlÄGTlÄGE¨ÄGTEÄlÅINCREASING¨ÄASCÄ¿BÄì“lÅWARNING - ¿BÄï“BÄß“,á is neither a valid quick-sort keyword nor an attribute.ÄA—ÄQC¡B¡J¸BQD€CSˇ›íD¡Ê@€RDQPPö8‚CWˇ›íPPò‰DQÅQPò‰@QDQ	P
í
äí@√$¸CWˇ›íPPò‰DQÅQPò‰@QDQÌ˝D5‰DQäC‰DQÅQPòÚÁ‰ÄPàDQàPàˇ€CB√¡C≈C¥Á@OÄBÄ∫ÄÄBÄUÄÎÄÜÄ‡@FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄU\ÄBÄ`BÄ2BÄÙBÄ3BÄ4BÄ5BÄ6BÄ=BÄ7BÄBÄ¸BÄBBÄ8BÄ<BÄ˝BÄ:BÄ˛BÄéBÄ
BÄˇBÄ9BÄ…BÄEBÄ;BÄ?BÄ:BÄ:\ÄBÄÙBÄ5BÄ6BÄ=BÄ7BÄBÄ¸BÄBBÄ8BÄ<BÄ˝BÄ:BÄ˛BÄéBÄ
BÄˇBÄ9BÄ…BÄEBÄ;BÄ?\ÄBÄ=Ïø“Retrieve some tuples from a relation satisying a where clause.

   RELATION-NAME        - Name of the reLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540830. :SYSTEM-TYPE :LOGICAL :VERSION 2. :TYPE "XLD" :NAME "SAVE" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760360526. :AUTHOR "REL3" :LENGTH-IN-BYTES 6527. :LENGTH-IN-BLOCKS 13. :BYTE-SIZE 16.)  pp2Ä\Ä©ÅCOMPILE-DATA\ÄÏÄSW-MFG,ÅGODZILLAÜÄá§^–FÄFÄ\Äp¿,ÅCOMPILER,ÅVERSIONÄ\ÄFÄFÄp¿BÄ),ÇOPTIMIZE-SWITCHÄÜÄ©ÉQFASL-SOURCE-FILE-UNIQUE-IDÄ1Ä\Äp¿lÄFSÏÇMAKE-FASLOAD-PATHNAMEÄ\ÄÍÄQUOTEÄBÄ$\ÄBÄ8™ÄNILÄ\ÄBÄ8\Ä¨ÄRTMS\ÄBÄ8¨ÄSAVE\ÄBÄ8¨ÄLISP\ÄBÄ8FÄ©ÄBASEFÄ
ÈÄFONTSÄ\Ä©Å*CODE-FONT*ÄÈÅ*COMMENT-FONT*ÈÅ*STRING-FONT*Ä)ÅPACKAGEÄ©ÄRTMS©ÄMODE©ÅCOMMON-LISPÄÄÇSAVE-FLAVOR-HASHÄÎÄ	FÄ¿FÄ¿$Ä¿BÄ:p¿¨ÄTICLÏÄART-QÄ]ÄFÄÄ:BÄ:BÄ:jÄTÄFÄp¿¨ÄSYSÄlÇDEBUG-INFO-STRUCTÄBÄP\ÄÅRELATION*ÅPATHNAME√ÅREL-DEFINITIONBÄ:BÄ:\Ä)ÇMACROS-EXPANDEDÄ\Äp¿BÄ\¨ÅXR-BQ-LIST*Äp¿BÄ\lÅXR-BQ-LISTÄ™ÄSETF¿ÉÄVAR1¿BÄ8¿ÉÅENTRY-POINTÄ¿ÉÄGETP“™ÄLIST“ÉÄPUTP¿\ÄBÄk\ÄBÄ8BÄl¿ÍÄLIST*Ä“p¿BÄTlÇDUMP-FORMS-TO-FILEíÅQÇQPPPÄQPííö	PPÄQí
PööîOÄuBÄPÄÄÇSAVE-FLAVOR-HEAPÄÎÄ	FÄ¿FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄv\ÄBÄ`BÄaBÄbBÄ:BÄ:\ÄBÄd\ÄBÄgBÄiÄBÄj¿BÄk¿BÄ8¿BÄl¿BÄm“BÄn“BÄo¿\ÄBÄk\ÄBÄ8BÄl¿BÄr“BÄtíÅQÇQPPPÄQPííö	PPÄQí
PööîOÄÉBÄvÄÄÇSAVE-FLAVOR-AVLÄÄÎÄ	FÄ¿FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÑ\ÄBÄ`BÄaBÄbBÄ:BÄ:\ÄBÄd\ÄBÄgBÄiÄBÄj¿BÄk¿BÄ8¿BÄl¿BÄm“BÄn“BÄo¿\ÄBÄk\ÄBÄ8BÄl¿BÄr“BÄtíÅQÇQPPPÄQPííö	PPÄQí
PööîOÄëBÄÑÄÄ√ÅSAVE-LIST-AVLÄÄÎÄ	FÄ¿FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄí\ÄBÄ`BÄaBÄbBÄ:BÄ:\ÄBÄd\ÄBÄgBÄiÄBÄj¿BÄk¿BÄ8¿BÄl¿BÄm“BÄn“BÄo¿\ÄBÄk\ÄBÄ8BÄl¿BÄr“BÄtíÅQPPPÄQPííöÇQ	PPÄQí
PööîOÄüBÄíÄÄ√ÅSAVE-LIST-HASHÄÎÄ	FÄ¿FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ†\ÄBÄ`BÄaBÄbBÄ:BÄ:\ÄBÄd\ÄBÄgBÄiÄBÄj¿BÄk¿BÄ8¿BÄl¿BÄm“BÄn“BÄo¿\ÄBÄk\ÄBÄ8BÄl¿BÄr“BÄtíÅQPPPÄQPííöÇQ	PPÄQí
PööîOÄ≠BÄ†ÄÄ√ÅSAVE-LIST-HEAPÄÎÄ	FÄ¿FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÆ\ÄBÄ`BÄaBÄbBÄ:BÄ:\ÄBÄd\ÄBÄgBÄiÄBÄj¿BÄk¿BÄ8¿BÄl¿BÄm“BÄn“BÄo¿\ÄBÄk\ÄBÄ8BÄl¿BÄr“BÄtíÅQPPPÄQPííöÇQ	PPÄQí
PööîOÄªBÄÆÄÄÇSAVE-STRUCT-AVLÄÄÎÄ	FÄ¿FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄº\ÄBÄ`BÄaBÄbBÄ:BÄ:\ÄBÄd\ÄBÄgBÄiÄBÄj¿BÄk¿BÄ8¿BÄl¿BÄm“BÄn“BÄo¿\ÄBÄk\ÄBÄ8BÄl¿BÄr“BÄtíÅQÇQPPPÄQPííö	PPÄQí
PööîOÄ…BÄºÄÄÇSAVE-STRUCT-HASHÄÎÄ	FÄ¿FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ \ÄBÄ`BÄaBÄbBÄ:BÄ:\ÄBÄd\ÄBÄgBÄiÄBÄj¿BÄk¿BÄ8¿BÄl¿BÄm“BÄn“BÄo¿\ÄBÄk\ÄBÄ8BÄl¿BÄr“BÄtíÅQÇQPPPÄQPííö	PPÄQí
PööîOÄ◊BÄ ÄÄÇSAVE-STRUCT-HEAPÄÎÄ	FÄ¿FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÿ\ÄBÄ`BÄaBÄbBÄ:BÄ:\ÄBÄd\ÄBÄgBÄiÄBÄj¿BÄk¿BÄ8¿BÄl¿BÄm“BÄn“BÄo¿\ÄBÄk\ÄBÄ8BÄl¿BÄr“BÄtíÅQÇQPPPÄQPííö	PPÄQí
PööîOÄÂBÄÿÄ√ÅSAVE-DATABASEÄOÄÊÅSAVE-DBÄÄBÄÊÄÎÄ<XÏÜÄ`<|FÄî¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÊ\Ä√ÅDATABASE-NAMEÄÍÄ&RESTÄÉÅKEYWORD-LIST™Ä&KEYjÅ&OPTIONALÄjÅDIRECTORYÄjÇ&ALLOW-OTHER-KEYSÄBÄ:\Ä\ÄBÄÚBÄ:p¿BÄ\lÇLEX-PARENT-ENV-REGp¿BÄ\ÏÅLEX-ENV-B-REGÄp¿BÄ\ÏÇLEX-CURRENT-VECTOR-REGp¿BÄ\¨ÇLEX-ALL-VECTORS-REGÄBÄıÉÄKEYSÅTEMP-DIRBÄaCÅERROR-FLAGBÄ:BÄÚBÄ:BÄ:\ÄBÄd\Äp¿BÄTlÇCONDITION-BIND-IFÄp¿BÄTÏÅCONDITION-BINDp¿BÄTÏÇCATCH-CONTINUATION-IFÄp¿BÄTlÇCATCH-CONTINUATIONp¿BÄTÏÄERRSET™ÄPROGBÄiBÄj©ÇINTERNAL-FEF-OFFSETS\ÄFÄ8iÑVARIABLES-USED-IN-LEXICAL-CLOSURES\ÄÉExit block SAVE-DATABASEBÄÈÅDOCUMENTATIONÄÏòSave all system relations and the user-defined, modified relations.

   DATABASE-NAME    - Name of the database to be saved.
   DIRECTORY         - Name of the directory in which it is to be saved.ÄÄp¿lÄEH¨Ç*CONDITION-HANDLERS*—CÉ*PROVIDE-STATUS-MESSAGES*Ä—CÇ*ENVIRONMENT-NAME*—É*DATABASE-DOCUMENTATION*—ÉÑ*SYSTEM-RELATION-STORAGE-STRUCTURE*Ä—√Ñ*SYSTEM-RELATION-BASE-IMPLEMENTATION*Ä—Ç*SAVE-DIRECTORY*—É*PROVIDE-ERROR-MESSAGES*—ÉÅ*ACTIVE-DB*Ä—√É*TRANSACTION-FORMS-POSTPONED*Ä—Ç*TRANSACTION-ON*ë\ÄiÅDIRECTORYÄ¿p¿BÄ\ÏÅSTORE-KEYARGSÄ“FÄ&¿jÅCOPY-LISTÄ“ÇACTIVE-DATABASEÄ“ÉÅVALIDATE-SYM“BÄÊ¿BÄ8¿BÄn“p¿BÄ\,Å*APPENDÄ“ÍÄTERPRI“,àERROR - Only the current database may be (or needs to be) saved.¿™ÅWRITE-STRING“,Ñ        The current database is ¿ÍÄPRIN1Ä“\ÄÉÄDIRÄ¿ÉGET-KEYWORD-VALUE-PREREQ“ÍÄSTRING¿√ÅGET-DIRECTORYÄ“¨Ä.XLD¿™ÅCONCATENATEÄ“ÇSYSTEM-RELATIONÄ¿\ÄlÅMODIFIEDPÄ¿\ÄBÄY¿ÇDELETE-OR-MODIFY“\ÄÏÅSAVE-DIRECTORY¿p¿,ÄÏÄG8924Ä¿FÄÔ¿ÍÄERRORÄ¿p¿BÄ\ÏÅERRSET-HANDLER¿p¿BÄ4ÏÅDIRECTORY-LIST“BÄj¿BÄ ¿BÄ¿ÇDEFINE-DATABASE*¿BÄ6¿ÉÄDOCÄ¿ÉÄENVÄ¿BÄt“lÇERROR - Directory ¿,Ç does not existÄ¿\ÄFÄÜÄÄFÄ
¿\Ä)ÅINTERNALBÄÊÄ¿ÉÄMAPT“¨ÄThe ¿¨É database has been saved in Ä@‰@QPˇ›F—†K—K√Pˇ€U@QäG¡ÄÊˇ€ö¸ÄQˇ›íÄ¡˘Â‰PPPÄQíPGQíöäí¿Ö¸PÄ+Ê
ÂÂÄPàÄPàPà‹˝@Q¸LSL¡‰LSˇ5˙ÁLQ@¡P@Qí@¡P@Q äH√ÄQ!P"¢I¡	PH+Ê#Pˇ›ˇ›$P%P&®#Pˇ›ˇ›'PHQä&®HQ	¿(P)PT*P+P(Pˇ€JCM√PJCN√÷HQ,ääJ!BJ!Bˇ\¸\ˇ‰IQ-P.PPö-P/PPö0PÄQ1PHQ2PP3PP≤öö4ê
¸
ÂÄ5PàHQà6Pàw˝7P8P”C#P9ê	‰Ä:PàÄQà;PàHQàJeÁÄQJ\POÄ[BÄÊÄÄBÄUÄÎÄWÜÄ@\FÄ8¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄU\Ä√ÄTUPLEÄBÄ:\ÄBÄ:BÄ:BÄ˙ÅTEMP-RELBÄBÄ:BÄ:\ÄBÄd\ÄBÄBÄ	BÄBÄBÄÍÄUNLESSBÄjiÉLEXICAL-PARENT-DEBUG-INFOÄBÄÌÄBÄ—BÄ"—CÅ*PKG-NAME*—ÉÉ*SYSTEM-RELATION-ATTRIBUTES*ëBÄn“\ÄÏÅRELATION-NAMEÄÏÅSAVE-DIRECTORY¿ÉÅPROJECT-LIST“BÄ8¿BÄ;“*ÇREAD-FROM-STRING“p¿BÄCÏÄG8933Ä¿FÄ[¿BÄG¿BÄI¿BÄK“BÄ0“lÇERROR - Directory ¿BÄ2“BÄ4“,Ç does not existÄ¿BÄ6¿√ÅSAVE-RELATIONÄíÄQäPP	öBD¡
PPDSöäC¡DWD¡PPTPPPˇ€JCE√PJCF√÷DQääJ!BJ!Bˇ\¸\ˇÊ‰ÄPàDQàPà¡‹¿Pˇ€XCQPDQúOÄxBÄUÄÇSAVE-ENVIRONMENTOÄyÅSAVE-ENVÄBÄyÄÎÄ:U‰ÜÄ`:hFÄè¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄy\ÄÅENVNAMEÄBÄÒBÄÚBÄÛBÄÙBÄıBÄˆBÄ:\Ä
BÄÚBÄıBÄaBÄ6BÄÅREL-IMPÄÅREL-STOÄBÄÚBÄ:BÄ:\ÄBÄd\Ä	BÄBÄ	BÄBÄBÄBÄiBÄiBÄBÄjBÄlëSave an environment.

   ENVNAME   - Name of the environment to be saved.
   DIRECTORY - Name of the directory in which it is to be saved.ÄBÄ—CÉ*PROVIDE-WARNING-MESSAGES*—ÉÇ*VALIDITY-CHECKING*Ä—BÄ—BÄ —BÄ—ÉÇ*PARAMETER-CHECKING*—BÄ!—√Ç*AUTO-SAVE-RELATIONS*Ä—CÉ*RELATION-IMPLEMENTATION*Ä—BÄ"—BÄ$—BÄ%ë\ÄBÄ'¿BÄ)“BÄ-“BÄ+“BÄy¿BÄ8¿BÄn“BÄ/“\ÄBÄ6¿BÄ7“BÄ8¿BÄ9“lÇrtms-environment-Ä¿¨Ä.XLD¿BÄ;“p¿BÄCÏÄG9006Ä¿FÄ√¿BÄG¿BÄI¿BÄK“BÄ0“¨ÅERROR - The ¿BÄ2“BÄ4“lÉ directory does not existÄ¿lÄ-Ä¿p¿BÄ\,ÅSEARCH*Ä“ÍÄSUBSEQ“FÄ¿CÇDEFINE-ENVIRONMENT¿\ÄBÄ8CÅAUTO-SAVEÄ¿\ÄBÄ8BÄı¿\ÄBÄ8√ÄERRORS¿\ÄBÄ8ÉÄPARA¿\ÄBÄ8BÄÖ¿\ÄBÄ8BÄÜ¿\ÄBÄ8√ÄSTATUS¿\ÄBÄ8ÅSYS-IMPÄ¿\ÄBÄ8ÅSYS-STOÄ¿\ÄBÄ8ÅVALIDITY¿\ÄBÄ8ÅWARNINGS¿BÄtí@‰@QPˇ›A—†ÄQˇ›íÄ¡ÊR@Q¸GSG¡‰GSˇ5˙ÁGQ@√äD¡‰PPPÄQíPDQíöäí¿ÄP@Qí@¡P@QäC√PÄQP™B¡P PT!P"PPˇ€JCH√PJCI√÷CQ#ääJ!BJ!Bˇ\¸\ˇ
Ê‰$Ä%P&àCQ'à(P&àRPJ)PP*í+öE¡P)PP*íˇk+íF¡ˇ›,PJUBQ-PPÄQí.PPPí/PP
Pí0PPPí1PP	Pí2PPEQí3PPFQí4PPPí5PPPí6PPPí7PPPí8PPPíJ∫ä9ê]ZÄOÄ±BÄyÄBÄwOÄwÅSAVE-RELÄBÄwÄÎÄ^∞æÜÄ‡^@FÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄw\Ä	√ÅRELATION-NAMEÄBÄÒBÄÚBÄÛBÄÙBÄıÉÄSAVE™ÄTYPEBÄˆBÄ:\Ä\ÄBÄÚBÄ:BÄ˙BÄ¸BÄ˛BÄBÄıBÄºBÄΩ√ÅINSERT-ROUTINEBÄaÅTEMPLISTBÄÅON-DISK?ÉÄMODPBÄ6CÅATTRIBUTESÉÄIMPÄCÄSSÉÅTEMP-MESSAGEÉÄKEYÄÉÅTUPLE-FORMATBÄMÉÄTEMPÉÅQTRIEVE-VARÄCÅSAVE-TYPEÄÉÅALWAYS-SAVEÄBÄBÄÚBÄ:BÄ:\ÄBÄd\ÄBÄBÄ	BÄBÄBÄBÄiÍÄFIRSTÄ*ÅSEVENTHÄÍÄSIXTHÄÍÄFIFTHÄÍÄFOURTHÍÄTHIRDÄÍÄSECONDBÄBÄiBÄjBÄ\ÄFÄSBÄ\ÄBÄ BÄ,ØSave a relation if it is modified.

    RELATION-NAME - Name of the relation to be saved.
    DIRECTORY     - Name of the directory in which it is to be saved.
    SAVE          - If T, saves the relation even if the relation is not modified.
    TYPE          - Two types of save are allowed: COMMAND and XLD. This keyword can be used to
                    specify the type.¿ÜÄü ÄBÄ—CÇ*SYSTEM-RELATIONS*—BÄ—BÄk—BÄ#—BÄ"—BÄ$—BÄ%ë\ÄBÄ'©ÄSAVE©ÄTYPE¿BÄ)“BÄ,“BÄ-“BÄ+“BÄw¿BÄ8¿BÄn“BÄ/“\ÄBÄΩBÄ6BÄº¿BÄ7“\Ä
ÏÅRELATION-NAMEÄlÅMODIFIEDPÄÏÅSAVE-DIRECTORYlÅATTRIBUTES¨ÇIMPLEMENTATION-TYPEÄlÇSTORAGE-STRUCTUREÄ¨ÄKEYÄ¨ÅTUPLE-FORMAT¨ÄDOCÄ¨ÄDISK¿ÉÅGET-RELATION“BÄ0“ÏÇERROR - The relation Ä¿BÄ2“BÄ4“,Ñ is not defined in the database ¿\ÄBÄΩ¿CÇGET-KEYWORD-VALUEÄ“ÉÄXLDÄ¿\ÄBÄº¿√ÅSAVEREL-QFASLÄ“ÅCOMMANDÄ¿,ÅERROR - ¿,Ñ is an unrecognized save type .Ä¿BÄ9“p¿BÄCÏÄG9052Ä¿FÄX¿BÄG¿BÄI¿BÄK“lÇERROR - Directory ¿,Ç does not existÄ¿BÄ<¿™ÅSTRING-EQUAL¿BÄ8¿ÏÅRELATION-NAMEÄ¿BÄ;“ÍÅSTRING-UPCASEÄ“\ÄÏÅSAVE-DIRECTORY¿BÄ@“\ÄBÄ¸\ÄBÄ;\ÄBÄ8BÄ8BÄkÏÅRELATION-NAMEÄ,ÇSYSTEM-RELATIONÄ¿\ÄlÅMODIFIEDPÄ¿\ÄBÄY¿lÄ-Ä¿lÄ.Ä¿BÄ8“ÏÅThe relation Ä¿ÏÇ has not been modified¿BÄ6¿√ÅLOAD-RELATIONÄ“FÄ¿iÅDIRECTIONÄ¿ÈÄOUTPUT¿™ÄOPEN“ÏÉ~&(SETF *non-qfasl-restore* T)¿ÍÄFORMAT“p¿BÄ\¨ÅMEMBER-TESTÄ“¨É~&(DEFREL-RESTORE ~S ~S ~S)Ä¿BÄ≈¿CÅMODIFIEDPÄ¿ÉÄSTOÄ¿BÄ»¿BÄ…¿BÄM¿¨Ähash¿p¿BÄ\ÏÅSTRING-EQUAL*Ä“,Ü~&(PUTP '~S (make-hash-table :test 'equal) '~S)Ä¿BÄl¿\ÄFÄFÄ¿\ÄBÄVBÄwÄ¿BÄX“,ÅINSERT-Ä¿BÄq“ÏÇ~&(~S '~S '~S '~S '~S)¿*ÅREVERSEÄ“,Ñ~&(SETF *non-qfasl-restore* NIL)¿ÍÄCLOSEÄ“\ÄBÄ:¿√ÇSAVE-SYSTEM-RELATIONSÄ“lÑ has been saved in the directory ÄÄ@‰@QPˇ›F—†ÄÊRÄQˇ›íÄ¡ÊR@QäL¡‰
PPPÄQíPLQíöäí
¿Ä@Q¸\S\¡‰\Sˇ5˙Á\Q@¡P@Qí@¡ÄQPˇ›ˇ›¢BX¡Ê		‰ÄPàÄQàPàPàRXWN¡X[O¡XQBP¡XUBQ¡XYBR¡XQBBT¡JXQ
CU¡UWV¡U[M¡USU¡P@QíB‚ PY¡!P@QíBZ¡YQ &‰ÄQ@Q"îYQ#&
Ê	‰Ä$PàYQà%PàR@QOQ&í[¡'P(PT)P*P'Pˇ€JC]√PJC^√÷[Q+ääJ!BJ!Bˇ\¸\ˇ
Ê	‰Ä,Pà[Qà-PàR[QO+Ê.Pˇ›/P0PP1P2öÄQ3äö4P[Qä5®.Pˇ›6P7P8P5®0P[QP9PÄQ:PYQ;äJ2∫J¡K€NÊZÊ[QO+
‰‰Ä<PàÄQà=PàÄM‰ÄQ>POQ?òˇ›@PJUJQAPBPCöJ√DPEêÄQP/PFòÊJQGPÄQPQHPQQIPˇ€JPRQKPTQLPUQMPVQ>POQJ∫E®ÄQP/PFò	ÊRQNPOê‰JQPPÄQQPE†W€RPSP”CÄQTêW‰0PUPQQ9PRQ2™VäI¡JQWPIQÄQPQWQXäTQÄQJE∏JQYPEêJQZà.Pˇ›/P0PP1P2öÄQ3äö7P[P5®ÄQP/PFòÊPS¡⁄\ÄSQ¿	‰Ä<PàÄQà]Pà[Qà]ZÄOÄ*BÄwÄÄBÄ ÄÎÄÜÄ@LFÄ¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ \Ä√Ä%TUPLEBÄ:\ÄBÄ:BÄ:BÄ˙\ÄBÄd\ÄBÄjBÄjBÄ∏ÄÄQ¿\¿¬ˇOÄ7BÄ ÄÄBÄÛÄÎÄAz5ÜÄ‡AÄFÄª¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÛ\ÄBÄ`BÄÚBÄ:\ÄBÄBÄbBÄaBÄÉÄCARD™ÄMODÄBÄ6BÄƒBÄ≈BÄ∆BÄ»BÄ…BÄMBÄÀBÄ«BÄ¬BÄ:BÄ:\ÄBÄd\ÄBÄBÄ	BÄBÄBÄBÄiBÄ–BÄ—BÄ“BÄ”BÄ‘BÄ’BÄ÷BÄiBÄj¿ÜÄAÄBÄ—BÄ‹—ÉÅ*PKG-STRING*—BÄ—BÄ#—BÄ"—BÄ$—BÄ%ëBÄ+“BÄÛ¿BÄ8¿BÄn“BÄ/“\ÄBÄ6BÄΩBÄº¿BÄ7“\Ä
ÏÅRELATION-NAMEÄlÅMODIFIEDPÄÏÅSAVE-DIRECTORYlÅATTRIBUTES¨ÇIMPLEMENTATION-TYPEÄlÇSTORAGE-STRUCTUREÄ¨ÄKEYÄ¨ÅTUPLE-FORMAT¨ÄDOCÄ¨ÅCARDINALITYÄ¿BÄÏ“BÄ0“ÏÇERROR - The relation Ä¿BÄ2“BÄ4“,Ñ is not defined in the database ¿BÄ9“p¿BÄCÏÄG9151Ä¿FÄË¿BÄG¿BÄI¿BÄK“¨ÅERROR - The ¿lÉ directory does not existÄ¿BÄ<¿BÄ¸¿BÄª¿\ÄÏÅSAVE-DIRECTORY¿BÄ@“\ÄBÄ¸BÄª,ÇSYSTEM-RELATIONÄ¿\ÄlÅMODIFIEDPÄ¿\ÄBÄY¿ÏÅThe relation Ä¿lá has not been modified and thus does not need to be savedÄ¿BÄ8¿lÄ-Ä¿¨Ä.XLD¿BÄ;“√ÅDEFREL-RESTORE¿BÄ≈¿ÉÅCARDINALITYÄ¿BÄ¿BÄ¿BÄ»¿BÄ…¿BÄM¿BÄ6¿BÄ“FÄp¿ÏÄSAVE-Ä¿™ÅFIND-SYMBOLÄ“\ÄBÄ:¿BÄ“BÄ(“lÑ has been saved in the directory ÄÄÅQä@¡‰
PPPÄQíP@Qíöäí
¿ÄPÅQíÅ¡ÄQPˇ›ˇ›¢BM¡Ê		‰ÄPàÄQàPàPàRMWE¡M[F¡MQBG¡MUBH¡MYBI¡MQBBJ¡JMQ
CK¡KWL¡K[D¡KQBO¡KSK¡ÅQFQíC¡PPTPPPˇ€JCP√PJCQ√÷CQääJ!BJ!Bˇ\¸\ˇ
Ê	‰Ä PàCQà!PàRCQF+Ê"Pˇ›#P$PPÄQíö%PCQä&®"Pˇ›'P(P)P&®EÊCQF+
‰‰Ä*PàÄQà+PàÄ,PCQP-PÄQ.P/≤B¡0PÄQGQ1PHQ2PDQ3Pˇ€4PIQ5PJQ6PKQ7PLQ8PFQJ∫¢A¡O‰ÄQ8PFQ9òˇ›:PJU,P;PHQ-PIQ/™P<íQ¡ÄQBQAQQô"Pˇ›#P$PPÄQíö(P=P&®ÄQP#P>òÊPN¡⁄?ÄNQ¿	‰Ä*PàÄQà@PàCQà]uZÄOÄlBÄÛÄÄBÄ(ÄÎÄ*/àÜÄ`*FÄY¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄ(\ÄBÄÒÍÄIGNOREBÄ:\ÄÄuBÄaBÄ:BÄ:BÄ:BÄ:\ÄBÄd\Ä
BÄjBÄp¿¨ÄZLCÄ,ÅDO-NAMEDp¿BÄTÏÇINHIBIT-STYLE-WARNINGSBÄBÄ	BÄBÄBÄBÄiÄBÄ—BÄ—BÄ—BÄ—BÄ —BÄ#—BÄ‹—BÄ"—BÄ!—√Ç*SYSTEM-RELATION-KEY*Ä—BÄlë\ÄBÄY¿BÄ<¿\ÄlÅMODIFIEDPÄ¿\ÄÍÄMEMBERBÄªBÄ‹©ÄTEST\ÄBÄ8BÄ¸¿ÅQTRIEVEÄ“p¿BÄCÏÄG9212Ä¿FÄx¿BÄG¿BÄI¿BÄK“BÄn“BÄ0“¨ÅERROR - The ¿BÄ2“BÄ4“lÉ directory does not existÄ¿BÄ6¿BÄw“BÄ8¿¨Ä.XLD¿BÄ;“BÄj¿BÄ ¿BÄ¿BÄL¿BÄM¿BÄN¿BÄtíPPPPPP™ãÊSPPTPPPˇ€JCB√PJCC√÷PääJ!BJ!Bˇ\¸\ˇ
Ê
‰ÄPàPàPàRC€C—	PE¡D¡	¸DQESPPöCD√¡E≈EıÁ PPP!P"¢A√#P$PPö#P%PPö&PPPP'PP(PP≤öö)êSOÄ
BÄ(Ä1Ä\Äp¿BÄ\,ÑFASL-RECORD-FILE-MACROS-EXPANDED\ÄBÄ8\Ä\ÄÍÄDEFUNÄÜÄ'\Äp¿BÄT¨ÄDEFFÜÄb\ÄBÄ}ÜÄ(Ã¢\ÄBÄ{ÜÄ*˝j\ÄBÄ÷ÜÄ{öÕ\ÄBÄ’ÜÄ:}n\ÄBÄ‘ÜÄxıø\ÄBÄ”ÜÄZiÛ\ÄBÄ“ÜÄ{ƒ≤\ÄBÄ—ÜÄ2ª=\ÄBÄ–ÜÄz(á\ÄBÄiÜÄ6\ÄBÄjÜÄ[ÊÑ\ÄBÄÜÄ=Ã#\ÄBÄÜÄ-i\ÄBÄÜÄ~…z\ÄBÄÜÄ<pë\ÄBÄ	ÜÄ`sN\ÄBÄÜÄ|ƒÙ\ÄBÄiÜÄ.Ÿã\ÄBÄgÜÄN¶™ÄÄogn
(setf pathname (open pathname :direction :output)) ;mrr 03.31.87
(format pathname "~&(SETF *non-qfasl-restore* T)")
(if (not (member relation-name *system-relations* :test 'string-equal))
    (format pathname "~&(DEFREL-RESTORE ~S ~S ~S)"
    relatLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540834. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "LISP" :NAME "STARTER-KIT" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :CHARACTERS T :NOT-BACKED-UP T :CREATION-DATE 2749846954. :AUTHOR "REL3" :LENGTH-IN-BYTES 10293. :LENGTH-IN-BLOCKS 11. :BYTE-SIZE 8.)

;;; -*- Mode:LISP; Package:RTMS; Base:10 -*-;


;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (b)(3)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151
;;;
;;; Copyright (c) 1985, Texas Instruments Incorporated.  All Rights Reserved.
;;; Copyright (c) 1986, Texas Instruments Incorporated.  All Rights Reserved.


;
(PKG-GOTO 'RTMS)

;
;  Define an environment which will silence any messages
;
(DEFINE-ENVIRONMENT 'silent-status 'status NIL 'validity 'NIL)
;
;  Define the database
;
(DEFINE-DATABASE 'micro-parts
 'doc "Contains all of the parts which are available from RTMS Micro"
 'dir "rtms;")

;
;  Define the parts relation
;
(DEFINE-RELATION 'parts
 '(number (dom numberp
     format 6
     doc "The RTMS micro part number")
   name (dom stringp
     format 25
     doc "The name of the part")
   qoh (dom numberp
    format 8
    doc "Quantity of parts On Hand")
   price (dom numberp
      format 10
      doc "The cost to the user of the part"))
                 'doc "A complete listing of the parts available from RTMS micro")

;
;  Define the SUPPLIERS relation
;
(DEFINE-RELATION 'suppliers
 '(number (dom numberp
     format 6
     doc "The RTMS micro part number")
   supplier-number (dom numberp
         format 16
         doc "The suppliers part number")
   supplier-name (dom stringp
       format 29
       doc "The name of the supplier")
   primary-source (dom anyp
        format 14
                    doc "T or NIL if this supplier is the primary source of the part")
   address (dom stringp
        format 35
        doc "The address of the supplier")
   cost-function (dom listp
       format 10
       doc "The individual suppliers cost function"))
  'doc "A listing of the suppliers of the parts which are available from RTMS Micro")


;
;  Insert the tuples into the parts relation
;
(SETQ parts-tuples '((3 "System Unit" 30 2200)
(4 "System Power Cable" 55 22)
(12 "Keyboard" 33 259)
(7 "Keyboard Cable" 42 18)
(1 "Monochrome Monitor" 15 389)
(2 "Color Monitor" 32 545)
(5 "Monitor Power Cable" 48 15)
(6 "Monitor to System Cable" 32 20)
(13 "Floppy Disk Drive" 65 249)
(15 "Hard Disk Drive" 20 1500)
(14 "Streaming Tape Drive" 8 895)
(11 "Printer" 35 525)
(9 "Printer Cable" 36 24)
(10 "Optical Mouse System" 27 295)
(8 "Optical Mouse Cable" 30 8)))
(INSERT 'parts 'tuple (NREVERSE parts-tuples))

;
;  Insert the Suppliers tuples
;
(SETQ suppliers-tuples '((3 3 "RTMS Micro" Yes "1486 Micro Drive, Dallas, TX"
    (lambda (price quantity) (* price quantity)))
          (4 4 "RTMS Micro" No "1486 Micro Drive, Dallas, TX"
    (lambda (price quantity) (* price quantity)))
          (12 12 "RTMS Micro" Yes "1486 Micro Drive, Dallas, TX"
    (lambda (price quantity) (* price quantity)))
          (7 7 "RTMS Micro" No "1486 Micro Drive, Dallas, TX"
    (lambda (price quantity) (* price quantity)))
          (1 1 "RTMS Micro" Yes "1486 Micro Drive, Dallas, TX"
    (lambda (price quantity) (* price quantity)))
          (2 2 "RTMS Micro" Yes "1486 Micro Drive, Dallas, TX"
    (lambda (price quantity) (* price quantity)))
          (5 5 "RTMS Micro" No "1486 Micro Drive, Dallas, TX"
    (lambda (price quantity) (* price quantity)))
          (6 6 "RTMS Micro" No "1486 Micro Drive, Dallas, TX"
    (lambda (price quantity) (* price quantity)))
          (13 13 "RTMS Micro" No "1486 Micro Drive, Dallas, TX"
    (lambda (price quantity) (* price quantity)))
          (15 15 "RTMS Micro" No "1486 Micro Drive, Dallas, TX"
    (lambda (price quantity) (* price quantity)))
          (14 14 "RTMS Micro" No "1486 Micro Drive, Dallas, TX"
    (lambda (price quantity) (* price quantity)))
          (11 11 "RTMS Micro" Yes "1486 Micro Drive, Dallas, TX"
    (lambda (price quantity) (* price quantity)))
          (9 9 "RTMS Micro" No "1486 Micro Drive, Dallas, TX"
    (lambda (price quantity) (* price quantity)))
          (10 10 "RTMS Micro" No "1486 Micro Drive, Dallas, TX"
    (lambda (price quantity) (* price quantity)))
          (8 8 "RTMS Micro" No "1486 Micro Drive, Dallas, TX"
    (lambda (price quantity) (* price quantity)))
 (3 1 "Chaparral Computer Systems" No
       "1212 Runner Road, Denver, CO"
(lambda (price quantity)
  (cond ((< quantity 10)
  (* price quantity))
 ((< quantity 100)
  (* price quantity .90))
 ((< quantity 1000)
  (* price quantity .75))
 (T (* price quantity .50))))
  )
 (1 2 "Chaparral Computer Systems" No
       "1212 Runner Road, Denver, CO"
(lambda (price quantity)
  (cond ((< quantity 10)
  (* price quantity))
 ((< quantity 100)
  (* price quantity .90))
 ((< quantity 1000)
  (* price quantity .75))
 (T (* price quantity .50))))
  )
 (2 3 "Chaparral Computer Systems" No
       "1212 Runner Road, Denver, CO"
(lambda (price quantity)
  (cond ((< quantity 10)
  (* price quantity))
 ((< quantity 100)
  (* price quantity .90))
 ((< quantity 1000)
  (* price quantity .75))
 (T (* price quantity .50))))
  )
 (13 4 "Chaparral Computer Systems" Yes
       "1212 Runner Road, Denver, CO"
(lambda (price quantity)
  (cond ((< quantity 10)
  (* price quantity))
 ((< quantity 100)
  (* price quantity .90))
 ((< quantity 1000)
  (* price quantity .75))
 (T (* price quantity .50))))
  )
 (14 5 "Chaparral Computer Systems" Yes
       "1212 Runner Road, Denver, CO"
(lambda (price quantity)
  (cond ((< quantity 10)
  (* price quantity))
 ((< quantity 100)
  (* price quantity .90))
 ((< quantity 1000)
  (* price quantity .75))
 (T (* price quantity .50))))
  )
 (15 6 "Chaparral Computer Systems" Yes
       "1212 Runner Road, Denver, CO"
(lambda (price quantity)
  (cond ((< quantity 10)
  (* price quantity))
 ((< quantity 100)
  (* price quantity .90))
 ((< quantity 1000)
  (* price quantity .75))
 (T (* price quantity .50))))
  )
 (4 1 "Raven Micro Systems" Yes
  "26 Cable Springs, Boston, MA"
(lambda (price quantity)
  (cond ((< quantity 10)
  (* price quantity .95))
 ((< quantity 100)
  (* price quantity .90))
 ((< quantity 1000)
  (* price quantity .80))
 (T (* price quantity .70))))
  )
 (5 2 "Raven Micro Systems" Yes
  "26 Cable Springs, Boston, MA"
(lambda (price quantity)
  (cond ((< quantity 10)
  (* price quantity .95))
 ((< quantity 100)
  (* price quantity .90))
 ((< quantity 1000)
  (* price quantity .80))
 (T (* price quantity .70))))
  )
 (6 3 "Raven Micro Systems" Yes
  "26 Cable Springs, Boston, MA"
(lambda (price quantity)
  (cond ((< quantity 10)
  (* price quantity .95))
 ((< quantity 100)
  (* price quantity .90))
 ((< quantity 1000)
  (* price quantity .80))
 (T (* price quantity .70))))
  )
 (7 4 "Raven Micro Systems" Yes
  "26 Cable Springs, Boston, MA"
(lambda (price quantity)
  (cond ((< quantity 10)
  (* price quantity .95))
 ((< quantity 100)
  (* price quantity .90))
 ((< quantity 1000)
  (* price quantity .80))
 (T (* price quantity .70))))
  )
 (8 5 "Raven Micro Systems" Yes
  "26 Cable Springs, Boston, MA"
(lambda (price quantity)
  (cond ((< quantity 10)
  (* price quantity .95))
 ((< quantity 100)
  (* price quantity .90))
 ((< quantity 1000)
  (* price quantity .80))
 (T (* price quantity .70))))
  )
 (9 6 "Raven Micro Systems" Yes
  "26 Cable Springs, Boston, MA"
(lambda (price quantity)
  (cond ((< quantity 10)
  (* price quantity .95))
 ((< quantity 100)
  (* price quantity .90))
 ((< quantity 1000)
  (* price quantity .80))
 (T (* price quantity .70))))
  )
   (1 10 "Peripheral Products" No "86 South Lane, Atlanta, GA"
    (lambda (price quantity)
      (cond ((< quantity 2000)
     (* quantity (* (- 1.0 (* (quotient quantity 2000.0) * 0.5)) price)))
    (T (* quantity price .5)))))
 (2 20 "Peripheral Products" No "86 South Lane, Atlanta, GA"
    (lambda (price quantity)
      (cond ((< quantity 2000)
     (* quantity (* (- 1.0 (* (quotient quantity 2000.0) * 0.5)) price)))
    (T (* quantity price .5)))))
 (10 30 "Peripheral Products" Yes "86 South Lane, Atlanta, GA"
    (lambda (price quantity)
      (cond ((< quantity 2000)
     (* quantity (* (- 1.0 (* (quotient quantity 2000.0) * 0.5)) price)))
    (T (* quantity price .5)))))
 (11 40 "Peripheral Products" No "86 South Lane, Atlanta, GA"
    (lambda (price quantity)
      (cond ((< quantity 2000)
     (* quantity (* (- 1.0 (* (quotient quantity 2000.0) * 0.5)) price)))
    (T (* quantity price .5)))))
 (12 50 "Peripheral Products" No "86 South Lane, Atlanta, GA"
    (lambda (price quantity)
      (cond ((< quantity 2000)
     (* quantity (* (- 1.0 (* (quotient quantity 2000.0) * 0.5)) price)))
    (T (* quantity price .5)))))
 (13 60 "Peripheral Products" No "86 South Lane, Atlanta, GA"
    (lambda (price quantity)
      (cond ((< quantity 2000)
     (* quantity (* (- 1.0 (* (quotient quantity 2000.0) * 0.5)) price)))
    (T (* quantity price .5)))))
 (14 70 "Peripheral Products" No "86 South Lane, Atlanta, GA"
    (lambda (price quantity)
      (cond ((< quantity 2000)
     (* quantity (* (- 1.0 (* (quotient quantity 2000.0) * 0.5)) price)))
    (T (* quantity price .5)))))
 (15 80 "Peripheral Products" No "86 South Lane, Atlanta, GA"
    (lambda (price quantity)
      (cond ((< quantity 2000)
     (* quantity (* (- 1.0 (* (quotient quantity 2000.0) * 0.5)) price)))
    (T (* quantity price .5)))))
 ))
(INSERT 'suppliers 'tuple (NREVERSE suppliers-tuples) )
;
;  Turn back on validity and status message output
;
(DEFINE-ENVIRONMENT 'Micro-Parts 'status T 'validity T)
BÄjBÄjBÄ∏ÄÄQ¿\¿¬ˇOÄ7BÄ ÄÄBÄÛÄÎÄAz5ÜÄ‡AÄFÄª¿$Ä¿BÄ:BÄV]ÄFÄÄ:BÄ:BÄ:BÄYFÄÄ^BÄÛ\ÄBÄ`BÄÚBÄ:\ÄBÄBÄbBÄaBÄÉÄCARD™ÄMODÄBÄ6BÄƒBÄ≈BÄ∆BÄ»BÄ…BÄMBÄÀBÄ«BÄ¬BÄ:BÄ:\ÄBÄd\ÄBÄBÄ	BÄBÄBÄBÄiBÄ–BÄ—BÄ“BÄ”BÄ‘BÄ’BÄ÷BÄiBÄj¿ÜÄAÄBÄ—BÄ‹—ÉÅ*PKG-STRING*—BÄ—BÄ#—BÄ"—BÄ$—BÄ%ëBÄ+“BÄÛ¿BÄ8¿BÄn“BÄ/“\ÄBÄ6BÄΩBÄº¿BÄ7“\Ä
ÏÅRELATION-NAMEÄlÅMODIFIEDPÄÏÅSAVE-DIRECTORYlÅATTRIBUTES¨ÇIMPLEMENTATION-TYPEÄlÇSTORAGE-STRUCTUREÄ¨ÄKEYÄ¨ÅTUPLE-FORMAT¨ÄDOCÄ¨ÅCARDINALITYÄ¿BÄÏ“BÄ0“ÏÇERROR - The relation Ä¿BÄ2“BÄ4“,Ñ is not defined in the database ¿BÄ9“p¿BÄCÏÄG9151Ä¿FÄË¿BÄG¿BÄI¿BÄK“¨ÅERROR - The ¿lÉ directory does not existÄ¿BÄ<¿BÄ¸¿BÄª¿\ÄÏÅSAVE-DIRECTORY¿BÄ@“\ÄBÄ¸BÄª,ÇSYSTEM-RELATIONÄ¿\ÄlÅMODIFIEDPÄ¿\ÄBÄY¿ÏÅThe relation Ä¿lá has not been modified and thus does not need to be savedÄ¿BÄ8¿lÄ-Ä¿¨Ä.XLD¿BÄ;“√ÅDEFREL-RESTORE¿BÄ≈¿ÉÅCARDINALLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540837. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "LISP" :NAME "STARTER-KIT-DESTROY" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :CHARACTERS T :NOT-BACKED-UP T :CREATION-DATE 2749846960. :AUTHOR "REL3" :LENGTH-IN-BYTES 669. :LENGTH-IN-BLOCKS 1. :BYTE-SIZE 8.)

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (b)(3)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151
;;;
;;; Copyright (C) 1985, Texas Instruments Incorporated. All rights reserved.


(DEFINE-ENVIRONMENT 'rtms-micro 'status nil)
(DESTROY-DATABASE (ACTIVE-DATABASE))
(DEFINE-ENVIRONMENT 'rtms-micro 'status T)
             LMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540840. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "LISP" :NAME "STARTER-KIT-INSERT" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :CHARACTERS T :NOT-BACKED-UP T :CREATION-DATE 2749846965. :AUTHOR "REL3" :LENGTH-IN-BYTES 691. :LENGTH-IN-BLOCKS 1. :BYTE-SIZE 8.)

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (b)(3)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151
;;;
;;; Copyright (C) 1985, Texas Instruments Incorporated. All rights reserved.
;;; Copyright (C) 1896, Texas Instruments Incorporated. All rights reserved.

((20 "Basic Manual" 32 18)
 (21 "How to Operate the Computer" 27 20))
                             LMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540843. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "LISP" :NAME "STARTER-KIT-PL" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :CHARACTERS T :NOT-BACKED-UP T :CREATION-DATE 2749846970. :AUTHOR "REL3" :LENGTH-IN-BYTES 1815. :LENGTH-IN-BLOCKS 2. :BYTE-SIZE 8.)

;;; -*- Mode:LISP; Package:RTMS; Base:10 -*-;

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (b)(3)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151
;;;
;;; Copyright (C) 1985, Texas Instruments Incorporated. All rights reserved.
;;; Copyright (C) 1896, Texas Instruments Incorporated. All rights reserved.
(define-relation 'price-list '(number (dom numberp
        doc "The RTMS Micro part number"
        format 6)
       s-number (dom numberp
   doc "The suppliers part number"
   format 8)
       supplier-name (dom stringp
        doc "The name of the supplier for this part"
        format 30)
       price (dom numberp
       doc "The cost of the part per 100"
       format 10)))
(insert 'price-list 'tuples '((3 100 "RTMS Micro" 1295)
      (3 101 "RTMS Micro" 1595)
      (3 102 "RTMS Micro" 1795)
      (3 1 "Chaparral Computer Systems" 1395)
      (3 2 "Chaparral Computer Systems" 1695)
      (1 103 "RTMS Micro" 250)
      (1 104 "RTMS Micro" 300)
      (1 10 "Chaparral Computer Systems" 200)
      (1 20 "Chaparral Computer Systems" 275)
      (1 100 "Peripheral Products" 185)
      (1 102 "Peripheral Products" 200)
      (1 104 "Peripheral Products" 250)
      (13 200 "RTMS Micro" 100)
      (13 201 "RTMS Micro" 150)
      (13 400 "Chaparral Computer Systems" 125)
      (13 402 "Chaparral Computer Systems" 175)
      (13 500 "Peripheral Products" 110)))
                   doc "T or NIL if this supplier is the primary source of the part")
   address (dom stringp
        format 35
        doc "The address of the supplier")
   cost-function (dom listp
       format 10
     LMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540846. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "LISP" :NAME "STARTER-KIT-SET" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :CHARACTERS T :NOT-BACKED-UP T :CREATION-DATE 2749846974. :AUTHOR "REL3" :LENGTH-IN-BYTES 787. :LENGTH-IN-BLOCKS 1. :BYTE-SIZE 8.)

;;;                           RESTRICTED RIGHTS LEGEND

;;;Use, duplication, or disclosure by the Government is subject to
;;;restrictions as set forth in subdivision (b)(3)(ii) of the Rights in
;;;Technical Data and Computer Software clause at 52.227-7013.
;;;
;;;                     TEXAS INSTRUMENTS INCORPORATED.
;;;                              P.O. BOX 2909
;;;                           AUSTIN, TEXAS 78769
;;;                                 MS 2151
;;;
;;; Copyright (C) 1985, Texas Instruments Incorporated. All rights reserved.
;;; Copyright (C) 1896, Texas Instruments Incorporated. All rights reserved.

(RETRIEVE 'parts 'into 'set-rel-2 'project '(number name qoh) 'where '(<= number 10))
(RETRIEVE 'parts 'into 'set-rel-1 'project '(number name) 'where '(>= number 7))
