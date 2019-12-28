
;;; -*- Mode:Common-Lisp; Package:RTMS; Base:10; Fonts:(*CODE-FONT* *COMMENT-FONT* *STRING-FONT*) -*-
;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved.
;;; DELETE-MODIFY
;;;
;;; This file contains the following Explorer extensions to CommonLisp Standard as Indicated in the June 1985 Explorer Lisp
;;; Reference
;;;     firstn
;;;     errset
;;;     deff
;;;
;;; The following function contains flavor references and thus are incompatable with CommonLisp. Their removal will not
;;; effect the functionality of RTMS.
;;;     delete-flavor-heap
;;;     delete-flavor-hash
;;;     modify-flavor-heap
;;;     modify-flavor-hash
;;;     flavor-bucket
;;;

(defun form-alist (tuple attributes &aux (alist nil))
  (do ((att-list attributes (cdr att-list))
       (val-list tuple (cdr val-list)))
      ((null att-list) t)
    (setf alist (append (list (cons (car att-list) (car val-list))) alist)))
  alist)

(defun quote-tuple (tuple)
  (mapcar (function (lambda (value)
      `(quote ,value)))
  tuple))

(defun dom-check (value attr dom-def &aux temp)
   ;;This is necessary when we define database.
  (if (and *validity-checking* (not (and *restore-operation* (null dom-def))))
      (cond ((null (funcall (intern (string-upcase
       (second (setf temp (assoc (string attr) dom-def :test 'string-equal))))
     *pkg-string*)
    value))
     (if *provide-warning-messages*
 (format *standard-output*
 "~%WARNING - ~s is not a legal value for the ~s attribute whose domain is ~s"
 value attr (second temp)))
     nil)
    (t t))
      t))

(defun modify-tuple (attribute-list modify-attributes tuple values dom-def relation-name temp-attribute-list
     &aux tempval (temp-result nil))
  (do ((attribute attribute-list (cdr attribute))
       (tuple-value tuple (cdr tuple-value)))
      ((null attribute) t)
    (cond ((member (car attribute) modify-attributes :test 'string-equal)
   (setf tempval
 ;;
 ;;  It is legal to allow the user to specify attributes from the relation when computing the
 ;;  new value of the attribute. This is why the entire tuple is formed in the a-list.
 ;;
 (modify-eval-where tuple (nth (position (car attribute) modify-attributes :test 'equal)
   values) temp-attribute-list))
   (if (or (member relation-name *system-relations* :test 'string-equal) (not *parameter-checking*)
   (not *validity-checking*) (dom-check tempval (car attribute) dom-def))
       (setf temp-result (cons tempval temp-result))
       (setf temp-result (cons (car tuple-value) temp-result))))
  (t
   (setf temp-result (cons (car tuple-value) temp-result)))))
   (reverse temp-result))

(defun modify-eval-where (val-list where-clause temp-attribute-list)
  (mapc #'(lambda (x z)
    (set x z))
temp-attribute-list
val-list)
  (eval where-clause))

(defun delete-tuples (relation &rest keyword-list
      &key &optional where &allow-other-keys
      &aux where-clause qtrieve-var ss key-attributes attributes card imp (num-deleted 0)
      domains)
  "Deletes the tuples which satisfy the WHERE clause from the specified relation.

   RELATION - Name of the relation from which the tuples are to be deleted.
   WHERE    - Selection criterion to be used."
  where
  (block delete-tuples
(setf keyword-list (de-nest-keyword-list keyword-list))
(cond (*parameter-checking*
       (if (or (not (active-database)) (not (setf relation (validate-sym relation))))
   (return-from delete-tuples nil))
       (setf keyword-list (get-keyword-value-prereq '(where) keyword-list))))
(setf where-clause (car (get-keyword-value '(where) keyword-list)))
;;
;;  Obtain some revelant information about the current relation
;;
(setf qtrieve-var (get-relation relation '("IMPLEMENTATION-TYPE" "STORAGE-STRUCTURE" "KEY" "ATTRIBUTES"
      "CARDINALITY" "DOMAINS") t))
(setf relation (car qtrieve-var)
      qtrieve-var (cadr qtrieve-var))
(cond ((null qtrieve-var)
       (if *provide-error-messages*
   (format *standard-output* "~%ERROR - Relation ~s does not exist in the database ~s"
   (read-from-string (string-upcase relation)) *active-db*))
       (return-from delete-tuples nil)))
;;
;;  If the user does not specify a where clause, it defaults to NIL and thus no tuples will be deleted.
;;
(setf imp (first qtrieve-var)
      ss (second qtrieve-var)
      key-attributes (third qtrieve-var)
      attributes (fourth qtrieve-var)
      card (fifth qtrieve-var)
      domains (sixth qtrieve-var)
      num-deleted 0)
(cond ((null where-clause)
       (if *provide-error-messages*
   (format *standard-output* "~%ERROR - No where clause specified, no tuples deleted"))
       (return-from delete-tuples nil))
      ((not (listp where-clause))
       (cond ((eval where-clause)
      (setf where-clause t))
     (t
      (return-from delete-tuples 0)))))
;;
;;  Perform the actual deletion of the requested tuples
;;
(setf num-deleted (funcall (find-symbol (concatenate 'string "DELETE-" imp "-" ss) *pkg-string*)
    relation attributes key-attributes domains where-clause relation))
(cond ((> num-deleted 0)
       ;;
       ;;  Tuples were deleted, so now we must determine if there are indices defined on this relation. If so the tuples must
       ;; be deleted from each of the index structures.
       ;;
       (mapc (function (lambda (index-info)
  (funcall (find-symbol (concatenate 'string "DELETE-" imp "-"
         (second index-info)) *pkg-string*)
    relation attributes (third index-info) domains where-clause
    (first index-info))))
     (qtrieve 'system-index *system-index-attributes*
   '("INDEX-NAME" "INDEX-TYPE" "KEY") *system-index-key*
   `(string-equal relation-name ,(string relation))))
       ;;
       ;;  Indicate that the system-relation relation has been updated.
       ;;
       (funcall (find-symbol (concatenate 'string "MODIFY-" *system-relation-base-implementation*
     "-" *system-relation-storage-structure*) *pkg-string*)
'system-relation *system-relation-attributes* *system-relation-key* '("MODIFIEDP")
'(t) `(equal relation-name "SYSTEM-RELATION") nil nil)
       ;;
       ;;  Update the system-relation relation to reflect the number tuples deleted
       ;;
       (funcall (find-symbol (concatenate 'string "MODIFY-" *system-relation-base-implementation* "-"
       *system-relation-storage-structure*) *pkg-string*)
'system-relation *system-relation-attributes* *system-relation-key*
'("MODIFIEDP" "CARDINALITY") (list t (- card num-deleted))
`(equal relation-name ,(string relation)) nil nil)
       (if *provide-status-messages*
   (format *standard-output* "~%~S tuple~:P deleted." num-deleted)))
      (t
       (if *provide-status-messages*
   (format *standard-output* "~%No tuples deleted"))))
(return-from delete-tuples relation num-deleted)))

(deff modify-tuples 'modify)

(defun delete-or-modify (relation del-or-mod where-clause
 &optional attributes values)
  ;;for old-times sake.
  (if del-or-mod
      (if (member (string-upcase relation) *system-relations* :test 'string-equal)
  (funcall
    (find-symbol (concatenate 'string "MODIFY-" *system-relation-base-implementation* "-"
  *system-relation-storage-structure*) *pkg-string*)
    relation
    (symbol-value
      (read-from-string (concatenate 'string *pkg-name* "*" (string-upcase relation) "-attributes*")))
    (symbol-value (read-from-string (concatenate 'string *pkg-name* "*" (string-upcase relation)
     "-key*")))
    attributes values where-clause nil nil)
(modify relation 'where where-clause 'attributes attributes 'values values))
    (if (member (string-upcase relation) *system-relations* :test 'string-equal)
  (funcall
    (find-symbol (concatenate 'string "DELETE-" *system-relation-base-implementation* "-"
  *system-relation-storage-structure*) *pkg-string*)
    relation
    (symbol-value
      (read-from-string (concatenate 'string *pkg-name* "*" (string-upcase relation) "-attributes*")))
    (symbol-value (read-from-string (concatenate 'string *pkg-name* "*" (string-upcase relation)
     "-key*")))
    nil where-clause relation)
(delete-tuples relation 'where where-clause))))

(defun modify (relation &rest keyword-list
       &key &optional attribute value where &allow-other-keys
       &aux attributes values where-clause ss key attr card imp (num-modified 0) qtrieve-var dom-def
       tuples domains indices)
  "The values of the tuples in a relation can be modified using this function.

   RELATION  - Name of the relation whose tuples are to be modified.
   ATTRIBUTE - List of attributes which are to be modified.
   VALUE     - Corresponding list of values to be used in modifying the above attributes.
   WHERE     - Selection criterion to be used."
  attribute value where
  (block modify
(cond (*parameter-checking*
       (if (or (not (active-database)) (not (setf relation (validate-sym relation))))
   (return-from modify nil))
       (setf keyword-list (get-keyword-value-prereq '(where attr value)
        (de-nest-keyword-list keyword-list))))
      (t
       (setf keyword-list (de-nest-keyword-list keyword-list))))
   (setf attributes (car (get-keyword-value '(attr) keyword-list))
 values (car (get-keyword-value '(value) keyword-list))
 where-clause (or (car (get-keyword-value '(where) keyword-list)) t))
   (cond ((not attributes)
  (if *provide-error-messages*
      (format *standard-output* "~%ERROR - The attribute list was not specified correctly"))
  (return-from modify nil))
 ((not values)
  (if *provide-error-messages*
      (format *standard-output* "~%ERROR - The value list was not specified correctly"))
  (return-from modify nil)))
   (setf qtrieve-var (get-relation relation
    '("IMPLEMENTATION-TYPE" "STORAGE-STRUCTURE" "KEY" "ATTRIBUTES" "CARDINALITY")
    t))
   (setf relation (car qtrieve-var)
 qtrieve-var (cadr qtrieve-var))
   (cond ((null qtrieve-var)
  (if *provide-error-messages*
      (format *standard-output*  "~%ERROR - Relation ~s does not exist in the database ~s"
      relation *active-db*))
 (return-from modify nil)))
   (setf ss (cadr qtrieve-var)
 key (caddr qtrieve-var)
 attr (cadddr qtrieve-var)
 card (fifth qtrieve-var)
 imp (car qtrieve-var))
   ;;
   ;;If only one attribute is provided then it is quite possible that it is an atom and a list.
   ;;
   (if (not (listp  attributes))
       (setf attributes (list attributes)))
   (cond (*parameter-checking*
  (setf attributes (mapcar #'(lambda (attr)
        (validate-sym attr t))
    attributes))
  (if (member nil attributes)
      (return-from modify nil)))
 (t
  (setf attributes (convert-attributes attributes))))
  ;;
  ;;The same is true for values.
  ;;
  (if (not (listp  values))
      (setf values (list values)))
  ;;
  ;;Make sure that the attributes provided are actually attributes (attr) of the relation.
  ;;
   (cond (*parameter-checking*
  (mapc (function (lambda (%attr)
    (cond ((not (member %attr attr :test 'string-equal))
    (if *provide-error-messages*
        (format *standard-output*
         "~%ERROR - ~S is not an attribute in the relation ~S."
         %attr relation))
    (return-from modify nil)))))
attributes)
  (cond ((not (equal (length values) (length attributes)))
 (if *provide-error-messages*
     (format *standard-output*
     "~%ERROR - Number of attributes ~S is not the same as the number of values ~S"
     attributes values))
 (return-from modify nil)))
   ;;
   ;;  DOM-DEF is used to contain the information needed to determine if the modified value is within the domain of the
   ;; attribute. This does not need to be performed if validity checking is NIL
   ;;
  (setf dom-def (funcall (find-symbol (concatenate 'string "RETRIEVE-"
       *system-relation-base-implementation*
       "-" *system-relation-storage-structure*)
        *pkg-string*)
  'system-attribute *system-attribute-attributes*
  '("ATTRIBUTE-NAME" "DOMAIN-FUNCTION" "DEFAULT-VALUE") *system-attribute-key*
  (list 'string-equal 'relation-name (string-upcase relation)) nil
  'system-attribute))
  (setf domains (mapcar (function (lambda (domain)
      (second domain)))
 dom-def))
  (setf dom-def
(mapcar #'(lambda (dom)
    (list (first dom) (read-from-string (concatenate 'string *pkg-name* (second dom)))
   (third dom)))
dom-def))))
   ;;
   ;;  Must determine if there are any indices defined on this relation. If there are, the tuples must be deleted from the indices before they
   ;; are modified. After the modification is complete, the tuples modified are sent back. These must then be inserted.
   ;;
   (cond ((setf indices (qtrieve 'system-index *system-index-attributes* '("INDEX-NAME" "KEY" "INDEX-TYPE")
  *system-index-key* `(string-equal relation-name ,(string relation))))
  (mapc (function (lambda (index-element)
    (funcall (find-symbol (concatenate 'string "DELETE-" imp "-" (third index-element))
     *pkg-string*)
      relation attr (second index-element) domains t
      (first index-element))))
indices)))
   ;;
   ;;  Perform the modification on the tuples specified.
   ;;
   (multiple-value-setq (num-modified tuples)
 (funcall (find-symbol (concatenate 'string "MODIFY-" imp "-" ss) *pkg-string*) relation attr key
  attributes values where-clause dom-def indices))
   (cond (indices
  (setf tuples (funcall (find-symbol (concatenate 'string "OBTAIN-TUPLES-" ss) *pkg-string*) relation))
  (mapc (function (lambda (index-element)
    (funcall (find-symbol (concatenate 'string "INDEX-INSERT-" imp "-"
           (third index-element)) *pkg-string*)
      (first index-element) tuples attr (second index-element) relation)))
indices)))
   ;;
   ;;  If at least a single tuple was modified, reflect this fact in the system-relation relation.
   ;;
   (if (> num-modified 0)
       (progn
 (funcall (find-symbol (concatenate 'string "MODIFY-" *system-relation-base-implementation*
      "-" *system-relation-storage-structure*) *pkg-string*)
  'system-relation *system-relation-attributes* *system-relation-key* '("MODIFIEDP") '(t)
  (list 'or '(string-equal relation-name "SYSTEM-RELATION")
`(string-equal relation-name ,(string relation)))
  nil nil)
 (if *provide-status-messages*
     (format *standard-output* "~%~S tuple~:P modified." num-modified)))
     (if *provide-status-messages*
 (format *standard-output* "~%No tuples modified.")))
   (return-from modify relation num-modified)))

(defun delete-flavor-heap (relation attribute-list rel-key domains where index-name
   &aux beginning-tuple-length conv-attribute-list (data (getp index-name 'entry-point))
   num-deleted temp-attribute-list tuples)
  rel-key domains index-name
  (cond ((or (equal where t)(equal where '(t)))
 (putp index-name nil 'entry-point)
 (setf num-deleted (caar (qtrieve 'system-relation *system-relation-attributes* '("CARDINALITY")
    *system-relation-key*
    `(string-equal relation-name ,(string relation))))))
(t
 (multiple-value-setq (where temp-attribute-list)
   (eval-where-prereq where attribute-list relation))
 (setf beginning-tuple-length (length data))
 (setf conv-attribute-list (project-flavor-prereq temp-attribute-list))
 (progv temp-attribute-list nil
   (setf tuples (delete-if (function (lambda (tuple)
         (mapc #'(lambda (attribute attribute-value)
     (set attribute attribute-value))
        temp-attribute-list
        (car (fast-project-flavor (list tuple)
             conv-attribute-list)))
         (if (eval where)
      tuple)))
    data)
 num-deleted (- beginning-tuple-length (length tuples))))
 (if (> num-deleted 0)
     (putp index-name tuples 'entry-point))))
  num-deleted)

(defun delete-struct-heap (relation attribute-list rel-key domains where index-name
   &aux beginning-tuple-length conv-attribute-list (data (getp index-name 'entry-point))
   num-deleted temp-attribute-list tuples (string-relation-name (string relation)))
  rel-key domains
  (cond ((or (equal where t)(equal where '(t)))
 (putp index-name nil 'entry-point)
 (setf num-deleted (caar (qtrieve 'system-relation *system-relation-attributes* '("CARDINALITY")
    *system-relation-key*
    `(string-equal relation-name ,string-relation-name)))))
(t
 (multiple-value-setq (where temp-attribute-list)
   (eval-where-prereq where attribute-list relation))
 (setf beginning-tuple-length (length data))
 (setf conv-attribute-list (unconvert-attributes
      (mapcar #'(lambda (attr)
    (concatenate 'string string-relation-name attr))
       attribute-list)))
 (progv temp-attribute-list nil
   (setf tuples (delete-if (function (lambda (tuple)
         (mapc #'(lambda (attribute attribute-value)
     (set attribute attribute-value))
        temp-attribute-list
        (car (fast-project-struct (list tuple)
             conv-attribute-list)))
         (if (eval where)
      tuple)))
    data)
 num-deleted (- beginning-tuple-length (length tuples))))
 (if (> num-deleted 0)
     (putp index-name tuples 'entry-point))))
  num-deleted)

(defun delete-heap (relation rel-attributes rel-key domains where imp  index-name
    &aux beginning-tuple-length (data (getp index-name 'entry-point))
    (num-deleted 0) temp-attribute-list tuples)
  rel-key domains imp
  (cond ((or (equal where t)(equal where '(t)))
 (putp index-name nil 'entry-point)
 (setf num-deleted (caar (qtrieve 'system-relation *system-relation-attributes* '("CARDINALITY")
    *system-relation-key*
    `(string-equal relation-name ,(string relation))))))
(t
 (multiple-value-setq (where temp-attribute-list)
   (eval-where-prereq where rel-attributes relation))
 (setf beginning-tuple-length (length data))
 (progv temp-attribute-list nil
   (setf tuples (delete-if (function (lambda (tuple)
         (mapc #'(lambda (x z)
     (set x z))
        temp-attribute-list tuple)
         (eval where)))
    data)
 num-deleted (- beginning-tuple-length (length tuples))))))
  (if (> num-deleted 0)
      (putp index-name tuples 'entry-point))
  num-deleted)


(defun delete-list-heap (relation rel-attributes rel-key domains where index-name)
  (delete-heap relation rel-attributes rel-key domains where "list" index-name))

(defun delete-flavor-hash (relation rel-attributes rel-key domains where index-name)
  (delete-hash relation rel-attributes rel-key domains where "flavor" index-name))

(defun delete-struct-hash (relation rel-attributes rel-key domains where index-name)
  (delete-hash relation rel-attributes rel-key domains where "struct" index-name))

(defun delete-list-hash (relation rel-attributes rel-key domains where index-name)
  (delete-hash relation rel-attributes rel-key domains where "list" index-name))

(defun delete-hash (relation rel-attributes rel-key domains where imp index-name
    &aux (bucket nil) (sec-keys nil) (num-deleted 0) package-name
    (table (getp index-name 'entry-point)) temp-attribute-list conv-attribute-list tuples
    (string-relation-name (string relation)))
  domains
 (block delete-hash
  (cond ((or (equal where t)(equal where '(t)))
 (clrhash (getp index-name 'entry-point))
 (return-from delete-hash (caar (retrieve 'system-relation 'project '("CARDINALITY")
  'where `(string-equal relation-name ,(string-upcase relation)) 'tuples t)))))
  (cond ((symbolp relation)
 (setf package-name (package-name (symbol-package relation))))
(t
 (setf package-name *pkg-string*)))
  (setf sec-keys (extract-key-hash rel-attributes rel-key nil where package-name))
  (multiple-value-setq (where temp-attribute-list)
    (eval-where-prereq where rel-attributes relation))
  (cond ((string-equal (string-upcase imp) "FLAVOR")
 (setf conv-attribute-list (project-flavor-prereq rel-attributes)))
((string-equal (string-upcase imp) "STRUCT")
 (setf conv-attribute-list (unconvert-attributes (mapcar #'(lambda (attr)
          (concatenate 'string string-relation-name
         attr))
      rel-attributes)))))
  (setf bucket (mapcar (function (lambda (sec-key)
    (gethash sec-key table)))
       sec-keys))
  (if bucket
      (mapc (function (lambda (%bucket sec-key
       &aux (not-deleted nil) (del? nil))
   (cond ((string-equal (string-upcase imp) "FLAVOR")
  (setf tuples (fast-project-flavor %bucket conv-attribute-list)))
 ((string-equal (string-upcase imp) "STRUCT")
  (setf tuples (fast-project-struct %bucket conv-attribute-list)))
 (t
  (setf tuples %bucket)))
   (mapc #'(lambda (tup %tup)
     (if (or (equal where t) (fast-eval-where (list tup) where temp-attribute-list))
  (setf del? t
        not-deleted (if not-deleted
          (remove %tup not-deleted :test 'equal)
          (remove %tup %bucket :test 'equal)))))
 tuples %bucket)
   (if del?
       (progn
 (setf num-deleted (+ (- (length %bucket) (length not-deleted)) num-deleted))
 (if not-deleted
     (setf (gethash sec-key table) not-deleted)
     (remhash sec-key table))))))
    bucket sec-keys)
    (maphash (function (lambda (hash-key %bucket &aux (not-deleted nil) (del? nil))
  (cond ((string-equal (string-upcase imp) "FLAVOR")
  (setf tuples (fast-project-flavor %bucket conv-attribute-list)))
 ((string-equal (string-upcase imp) "STRUCT")
  (setf tuples (fast-project-struct %bucket conv-attribute-list)))
 (t
  (setf tuples %bucket)))
                  (mapc #'(lambda (tup %tup)
    (if (or (equal where t) (fast-eval-where (list tup) where temp-attribute-list))
 (setf del? t
       not-deleted (if not-deleted
         (remove %tup not-deleted :test 'equal)
         (remove %tup %bucket :test 'equal)))))
     tuples %bucket)
  (if del?
      (progn
(setf num-deleted (+ (- (length %bucket) (length not-deleted)) num-deleted))
(if not-deleted
    (setf (gethash hash-key table) not-deleted)
    (remhash hash-key table))))))
     table))
  (return-from delete-hash num-deleted)))


(defun modify-flavor-heap (relation rel-attributes rel-key attributes values where dom-def indices
   &aux (num-modified 0) new-heap-list delormod? (tuples nil) atom-rel-attributes
   atom-attributes flavor-package temp-attribute-list data conv-rel-attributes)
  rel-key
  (cond ((not (listp where))
 (if (eval where)
     (setf where t)
     (values 0 nil))))
  (multiple-value-setq (where temp-attribute-list)
    (eval-where-prereq where rel-attributes relation))
  (setf flavor-package (package-name (symbol-package (typep (car (getp relation 'entry-point))))))
  (setf atom-rel-attributes (unconvert-attributes rel-attributes flavor-package)
atom-attributes (unconvert-attributes attributes flavor-package))
  (setf conv-rel-attributes (project-flavor-prereq rel-attributes))
  (setf data (fast-project-flavor (reverse (getp relation 'entry-point)) conv-rel-attributes))
  (cond (indices
 (mapcar (function (lambda (tuple a-tuple)
     (setf delormod? nil)
     (cond ((or (equal where t)
  (fast-eval-where (list a-tuple) where temp-attribute-list))
     (mapcar
       (function
  (lambda (attr val &aux tempval)
    (setf tempval
   (eval (sublis
    (form-alist (quote-tuple a-tuple) atom-rel-attributes)
    val)))
    (cond ((or (member (string-upcase relation) *system-relations*
         :test 'string-equal)
        (not *validity-checking*)
        (dom-check tempval attr dom-def))
    (setf delormod? t)
    (set-in-instance tuple attr tempval)))))
       atom-attributes values)
     (setf new-heap-list (cons tuple new-heap-list))
     (if delormod?
  (setf num-modified (+ 1 num-modified)))
     (setf tuples (cons tuple tuples)))
    (t
     (setf new-heap-list (cons tuple new-heap-list))))))
 (reverse (getp relation 'entry-point))
 data))
(t
 (mapc (function (lambda (tuple a-tuple)
   (setf delormod? nil)
   (cond ((or (equal where t)(fast-eval-where (list a-tuple) where temp-attribute-list))
   (mapcar
     (function
       (lambda (attr val &aux tempval)
  (setf tempval
        (eval (sublis
         (form-alist (quote-tuple a-tuple) atom-rel-attributes)
         val)))
  (cond ((or (member (string-upcase relation) *system-relations*
        :test 'string-equal)
      (not *validity-checking*)
      (dom-check tempval attr dom-def))
         (setf delormod? t)
         (set-in-instance tuple attr tempval)))))
     atom-attributes values)
   (setf new-heap-list (cons tuple new-heap-list))
   (if delormod?
       (setf num-modified (+ 1 num-modified))))
  (t (setf new-heap-list (cons tuple new-heap-list))))))
       (reverse (getp relation 'entry-point))
       data)))
  (putp relation new-heap-list 'entry-point)
  (values num-modified tuples))

(defun modify-struct-heap (relation rel-attributes rel-key attributes values where dom-def indices
   &aux (num-modified 0) new-heap-list temp-struct delormod?
   atom-rel-attributes atom-attributes temp-attribute-list struct-rel-attributes
   struct-attributes (modified-tuples nil) (string-relation-name (string relation)))
  rel-key
  (cond ((not (listp where))
 (if (eval where)
     (setf where t)
     (values 0 nil))))
  (multiple-value-setq (where temp-attribute-list)
    (eval-where-prereq where rel-attributes relation))
  (setf struct-rel-attributes (unconvert-attributes (mapcar #'(lambda (attr)
        (concatenate 'string string-relation-name attr))
    rel-attributes))
struct-attributes (unconvert-attributes (mapcar #'(lambda (attr)
        (concatenate 'string string-relation-name attr))
    attributes)))
  (setf atom-rel-attributes (unconvert-attributes rel-attributes)
atom-attributes (unconvert-attributes attributes))
  (cond (indices
 (mapcar (function (lambda (struct-tuple)
     (setf delormod? nil)
     (setf temp-struct (mapcar (function (lambda (attr)
        (funcall attr struct-tuple)))
          struct-rel-attributes))
     (cond ((or (equal where t)
  (fast-eval-where (list temp-struct) where temp-attribute-list))
     (mapc (function (lambda (attr val struct-attr &aux tempval)
         (setf tempval (eval (sublis
          (form-alist
            (quote-tuple temp-struct)
            atom-rel-attributes)
          val)))
         (if (or (member (string-upcase relation)
           *system-relations* :test 'string-equal)
          (not *validity-checking*)
          (dom-check tempval attr dom-def))
      (progn
        (setf delormod? t)
        (eval `(setf (,struct-attr ,struct-tuple)
       ',tempval))))))
    atom-attributes values struct-attributes)
     (setf new-heap-list (cons struct-tuple new-heap-list))
     (if delormod?
  (setf num-modified (+ 1 num-modified)))
     (setf modified-tuples (cons struct-tuple modified-tuples)))
    (t
     (setf new-heap-list (cons struct-tuple new-heap-list))))))
 (reverse (getp relation 'entry-point))))
(t
 (mapcar (function (lambda (struct-tuple)
     (setf delormod? nil)
     (setf temp-struct (mapcar (function (lambda (attr)
        (funcall attr struct-tuple)))
          struct-rel-attributes))
     (cond ((or (equal where t)
  (fast-eval-where (list temp-struct) where temp-attribute-list))
     (mapc (function (lambda (attr val struct-attr &aux tempval)
         (setf tempval (eval (sublis
          (form-alist
            (quote-tuple temp-struct)
            atom-rel-attributes)
          val)))
         (if (or (member (string-upcase relation)
           *system-relations* :test 'string-equal)
          (not *validity-checking*)
          (dom-check tempval attr dom-def))
      (progn
        (setf delormod? t)
        (eval `(setf (,struct-attr ,struct-tuple)
       ',tempval))))))
    atom-attributes values struct-attributes)
     (setf new-heap-list (cons struct-tuple new-heap-list))
     (if delormod?
  (setf num-modified (+ 1 num-modified))))
    (t
     (setf new-heap-list (cons struct-tuple new-heap-list))))))
 (reverse (getp relation 'entry-point)))))
  (putp relation new-heap-list 'entry-point)
  (values num-modified modified-tuples))

(defun modify-list-heap (relation attribute-list rel-key attributes values where dom-def indices
 &aux modify-list (num-modified 0) new-heap-list delormod?
 temp-attribute-list (modified-tuples nil))
  rel-key
  (cond ((not (listp where))
 (if (eval where)
     (setf where t)
     (values 0 nil))))
  (multiple-value-setq (where temp-attribute-list)
    (eval-where-prereq where attribute-list relation))
  (do ((modify-value values (cdr modify-value)))
      ((null modify-value) t)
    (if (and (listp (car modify-value)) (not (equal (car modify-value) t)))
(setf modify-list (cons (parse-where (car modify-value)) modify-list))
(setf modify-list (cons (car modify-value) modify-list))))
  (setf modify-list (reverse modify-list))
  (progv temp-attribute-list nil
    (cond (indices
   (mapc (function (lambda (tuple &aux new-tuple)
       (setf delormod? nil)
       (cond ((or (equal where t)
    (super-fast-eval-where (list tuple) temp-attribute-list where))
       (setf new-heap-list (cons (setf new-tuple
           (modify-tuple attribute-list
      attributes tuple
      modify-list dom-def
      relation temp-attribute-list))
     new-heap-list))
       (if (not (equal tuple new-tuple))
    (setf num-modified (+ 1 num-modified)))
       (setf modified-tuples (cons new-tuple modified-tuples)))
      (t
       (setf new-heap-list (cons tuple new-heap-list))))))
   (reverse (getp relation 'entry-point))))
  (t
   (mapc (function (lambda (tuple &aux new-tuple)
     (setf delormod? nil)
     (cond ((or (equal where t)
  (super-fast-eval-where (list tuple) temp-attribute-list where))
     (setf new-heap-list (cons (setf new-tuple
         (modify-tuple attribute-list
           attributes tuple modify-list dom-def
           relation temp-attribute-list))
          new-heap-list))
     (if (not (equal tuple new-tuple))
  (setf num-modified (+ 1 num-modified))))
    (t
     (setf new-heap-list (cons tuple new-heap-list))))))
 (reverse (getp relation 'entry-point))))))
  (putp relation new-heap-list 'entry-point)
  (values num-modified modified-tuples))

(defun modify-flavor-hash (relation rel-attributes rel-key attributes values where dom-def indices)
  (modify-hash relation where rel-key rel-attributes attributes values "FLAVOR" dom-def indices))

(defun modify-struct-hash (relation rel-attributes rel-key attributes values where dom-def indices)
  (modify-hash relation where rel-key rel-attributes attributes values "STRUCT" dom-def indices))

(defun modify-list-hash (relation rel-attributes rel-key attributes values where dom-def indices)
  (modify-hash relation where rel-key rel-attributes attributes values "LIST" dom-def indices))

(defun modify-hash (relation where-clause key attribute-list attributes values imp dom-def indices
    &aux (bucket nil) (sec-keys nil) (num-modified 0) (result nil) hash-tuples
    num-bucket-modified (table (getp relation 'entry-point)) modify-list temp-attribute-list
    atom-attributes uncon-attr conv-attribute-list struct-rel-attributes package-name
    struct-attributes modify-clause (modified-tuples nil) (modify-tuples nil)
    (string-relation-name (string relation)))
  indices
  (setf imp (find-symbol (concatenate 'string imp "-BUCKET") *pkg-string*))
  (cond ((symbolp relation)
 (setf package-name (package-name (symbol-package relation))))
(t
 (setf package-name *pkg-string*)))
  (if (or (equal where-clause t)(equal where-clause nil))
      (setf sec-keys nil)
      (setf sec-keys (extract-key-hash attribute-list key nil where-clause package-name)))
  (setf bucket (mapcar (function (lambda (sec-key)
     (gethash sec-key table)))
sec-keys))
  (multiple-value-setq (where-clause temp-attribute-list)
    (eval-where-prereq where-clause attribute-list relation))
  (do ((modify-value values (cdr modify-value)))
      ((null modify-value) t)
    (cond ((listp (car modify-value))
   (multiple-value-setq (modify-clause temp-attribute-list)
     (eval-where-prereq (car modify-value) attribute-list relation))
   (setf modify-list (append (list modify-clause) modify-list)))
  (t
   (setf modify-list (append (list (car modify-value)) modify-list)))))
  (setf modify-list (reverse modify-list))
  (setf struct-rel-attributes (unconvert-attributes (mapcar #'(lambda (attr)
     (concatenate 'string string-relation-name
           (string-upcase attr)))
        attribute-list))
struct-attributes (unconvert-attributes (mapcar #'(lambda (attr)
        (concatenate 'string string-relation-name
       (string-upcase attr)))
    attributes)))
  (setf atom-attributes (unconvert-attributes attribute-list))
  (setf uncon-attr (unconvert-attributes attributes))
  (setf conv-attribute-list (project-flavor-prereq attribute-list))
  (progv temp-attribute-list nil
    (cond (bucket
   (mapc (function (lambda (%bucket sec-key)
     (multiple-value-setq (num-bucket-modified modify-tuples)
       (funcall imp relation where-clause sec-key result attributes values %bucket
  attribute-list key dom-def temp-attribute-list atom-attributes
  uncon-attr table conv-attribute-list struct-rel-attributes
  struct-attributes))
     (setf num-modified (+ num-bucket-modified num-modified))
     (cond ((and modify-tuples (car modify-tuples))
     (setf modified-tuples (append modify-tuples modified-tuples))))))
 bucket sec-keys))
  (t
   (setf hash-tuples nil)
   (maphash (function (lambda (hash-key tuple-list)
 (setf hash-tuples (cons (list hash-key tuple-list) hash-tuples))))
    table)
   (mapc (function (lambda (x &aux mod-bucket)
     ;;Some of the bucket entries might be due to the modified tuples !
     (setf result (firstn (- (length (setf mod-bucket (gethash (car x) table)))
        (length (cadr x))) mod-bucket))
     (multiple-value-setq (num-bucket-modified modify-tuples)
       (funcall imp relation where-clause (car x) result attributes values (cadr x)
  attribute-list key dom-def temp-attribute-list atom-attributes
  uncon-attr table conv-attribute-list struct-rel-attributes
  struct-attributes))
     (setf num-modified (+ num-bucket-modified num-modified))
     (cond ((and modify-tuples (car modify-tuples))
     (setf modified-tuples (append modify-tuples modified-tuples))))))
 hash-tuples))))
  (values num-modified modified-tuples))


(defun flavor-bucket (relation where-clause key result attributes values bucket attribute-list key-attr dom-def
      temp-attribute-list atom-attributes uncon-attr table conv-attribute-list
      struct-rel-attributes struct-attributes
      &aux new-key key-modp (num-modified 0) delormod? data)
  struct-rel-attributes struct-attributes attribute-list
  (cond ((not (listp where-clause))
 (if (eval where-clause)
     (setf where-clause t)
     (values 0 nil))))
  (setf data (fast-project-flavor bucket conv-attribute-list))
  (mapc (function (lambda (tuple a-tuple &aux c-tuple)
    (setf new-key nil
  delormod? nil)
     (cond ((or (equal where-clause t)
       (super-fast-eval-where (list a-tuple) temp-attribute-list where-clause))
   (mapcar
     (function (lambda (attr val &aux tempval)
   (setf tempval (eval (sublis
           (form-alist (quote-tuple a-tuple)
         atom-attributes)
           val)))
   (setf c-tuple (append c-tuple (list tempval)))
   (if (or (member (string-upcase relation) *system-relations*
     :test 'string-equal)
    (not *validity-checking*)
    (dom-check tempval attr dom-def))
       (progn
         (setf delormod? t)
         (set-in-instance tuple attr tempval)))))
     uncon-attr values)
   (setf key-modp nil)
   ;;key-attr --- the actual key attributes for this relation.
   ;;key      --- the key value for this bucket.
   ;;c-tuple  --- the modification values which are evaluated.
   (mapcar
     (function
       (lambda (attr val &aux temp)
  (if (and (member attr attributes :test 'string-equal)
    (car (errset
    (setf temp (nth (position attr attributes :test 'equal)
      c-tuple))
    nil))
    (not (equal temp val)))
      (progn
        (setf key-modp t)
        (setf new-key (cons temp new-key)))
      (setf new-key (cons val new-key)))))
     (reverse key-attr)
     (reverse key))
   (if key-modp
       (setf (gethash new-key table) (cons tuple (gethash new-key table)))
       (setf result (cons tuple result)))
   (if delormod?
       (setf num-modified (+ 1 num-modified))))
  (t
   (setf result (cons tuple result))))))
bucket data)
  (if result
      (setf (gethash key table) result)
    (remhash key table))
  (values num-modified result))

(defun struct-bucket (relation where-clause key result attributes values bucket attribute-list key-attr dom-def
      temp-attribute-list atom-attributes uncon-attr table conv-attribute-list
      struct-rel-attributes struct-attributes
      &aux key-modp new-key temp-struct (num-modified 0) delormod? tuples)
  conv-attribute-list attribute-list
  (cond ((not (listp where-clause))
 (if (eval where-clause)
     (setf where-clause t)
     (values 0 nil))))
  (mapc (function (lambda (struct-tuple &aux c-tuple)
    (setf new-key nil delormod? nil)
    (setf temp-struct (mapcar (function (lambda (struct-attr)
      (funcall struct-attr struct-tuple)))
        struct-rel-attributes))
    (cond ((or (equal where-clause t)
       (super-fast-eval-where (list temp-struct) temp-attribute-list where-clause))
   (mapc (function (lambda (struct-attr struct-val rel-attr &aux tempval)
       (setf tempval (eval (sublis
        (form-alist (quote-tuple temp-struct)
             atom-attributes)
        struct-val)))
      (setf c-tuple (append c-tuple (list tempval)))
      (if (or (member (string-upcase relation) *system-relations*
        :test 'string-equal)
       (not *validity-checking*)
       (dom-check tempval struct-attr dom-def))
   (progn
     (setf delormod? t)
     (eval `(setf (,rel-attr ,struct-tuple) ',tempval))))))
  uncon-attr values struct-attributes)
   (setf key-modp nil)
   (mapc (function (lambda (attr val &aux temp)
       (if (and (member attr attributes :test 'string-equal)
         (setf temp (car(errset (nth
             (- (length attributes)
         (length (member
            attr
            attributes
            :test
            'string-equal)))
           c-tuple)
         nil)))
         (not (equal temp val)))
    (progn
      (setf key-modp t)
      (setf new-key (cons temp new-key)))
    (setf new-key (cons val new-key)))))
  (reverse key-attr)
  (reverse key))
   (if key-modp
       (setf (gethash new-key table) (cons struct-tuple (gethash new-key table)))
       (setf result (cons struct-tuple result)))
   (if delormod?
       (setf num-modified (+ 1 num-modified))))
  (t (setf result (cons struct-tuple result))))
    (setf tuples (append struct-tuple tuples))))
bucket)
  (if result
      (setf (gethash key table) result)
    (remhash key table))
  (values num-modified tuples))


(defun list-bucket (relation where-clause key result attributes values bucket attribute-list key-attr dom-def
    temp-attribute-list atom-attributes uncon-attr table conv-attribute-list
    struct-rel-attributes struct-attributes
    &aux new-key key-modp (num-modified 0) delormod? (tuples nil))
  relation conv-attribute-list struct-rel-attributes struct-attributes attribute-list
  (cond ((not (listp where-clause))
 (if (eval where-clause)
     (setf where-clause t)
     (values 0 nil))))
  (setf tuples (mapcar
 (function (lambda (tuple)
     (setf new-key nil
    delormod? nil)
     (cond ((or (equal where-clause t)
  (super-fast-eval-where (list tuple) temp-attribute-list
           where-clause))
     (setf key-modp nil)
     (mapc
       (function (lambda (attr val &aux temp)
     (if (and (member attr attributes :test 'string-equal)
       (setf temp
      (eval
        (sublis
          (form-alist
            (quote-tuple tuple)
            atom-attributes)
          (car (errset
          (nth (position attr attributes
           :test 'equal) values)
          nil)))))
       (not (equal temp val)))
         (progn
    (setf key-modp t)
    (setf new-key (cons temp new-key)))
         (setf new-key (cons val new-key)))))
       (reverse key-attr) (reverse key))
     (cond (key-modp
     (setf key-modp (modify-tuple atom-attributes uncon-attr tuple values
      dom-def relation temp-attribute-list))
     (setf (gethash new-key table)
    (cons key-modp (gethash new-key table))))
    (t
     (setf result
    (cons (setf key-modp
         (modify-tuple atom-attributes uncon-attr tuple
         values dom-def relation
         temp-attribute-list))
          result))))
     (if (not (equal key-modp tuple))
  (setf num-modified (+ 1 num-modified))))
    (t (setf result (cons tuple result))))
     key-modp))
 bucket))
  (if result
      (setf (gethash key table) result)
      (remhash key table))
  (values num-modified tuples))

;; fragments
al temp-key temp-val)
  ;;
  ;; Loop until all of the sub-clauses of the where-clause have been processed.
  ;;
  (do ((where-clause (cdr where-clause) (cdr where-clause)))
      ((or (null where-clause) (string-equal (car result) "n")) result)
    (cond ((listp (car where-clause))
   ;;
   ;;  The element is a list, must be a function call. Call the appropriate optimization function if one exists. Add the results from
   ;; the function to the current results. The optimization functions which are defined by RTMS initially are used as selections in
   ;; the following COND clause to improve the speed of calling these functions. If the function in the sub-clause is not from this
   ;; group, the function name must be formed before it can be invoked.
   ;;
((string-equal (string-upcase imp) "STRUCT")
 (setf conv-attribute-list (unconvert-attributes (mapcar #'(lambda (attr)
          (concatenate 'string string-relation-name
         attr))
      rel-attributes)))))
