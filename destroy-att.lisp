
;;; -*- Mode:Common-Lisp; Package:RTMS; Base:10; Fonts:(*CODE-FONT* *COMMENT-FONT* *STRING-FONT*) -*-
;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved.
;;; DESTROY-ATT
;;;
;;; This file contains the following Explorer extensions to CommonLisp Standard as Indicated in the June 1985 Explorer Lisp
;;; Reference
;;;     firstn
;;;     deff
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

(deff destroy-attr 'destroy-attribute)

(defun destroy-attribute (relation-name
  &rest keyword-list
  &key &optional attribute key &allow-other-keys
  &aux (relation-tuple nil) key-list init-attribute-list delete-attribute-list indices
  attribute-list rel-owner-id rel-implementation-type new-key key-value save-dir
  rel-storage-structure  pos)
  "Attributes in a relation can be deleted using this function.

   RELATION-NAME - Name of the relation from which the attributes are to be deleted.
   ATTRIBUTE     - List of attributes to be destroyed.
   KEY           - List of attributes to form the new key, if so desired."
  attribute key
  (block destroy-attribute
(if *parameter-checking*
    (if (or (not (active-database)) (not (setf relation-name (validate-sym relation-name))))
(return-from destroy-attribute nil)))
  (setf keyword-list (de-nest-keyword-list keyword-list))
  (if *parameter-checking*
      (setf keyword-list (get-keyword-value-prereq '(att key) keyword-list)))
  (setf delete-attribute-list (car (get-keyword-value '(att) keyword-list)))
  (cond ((and (listp delete-attribute-list) (listp (car delete-attribute-list)))
 (setf delete-attribute-list (car delete-attribute-list)))
((and delete-attribute-list (not (listp delete-attribute-list)))
 (setf delete-attribute-list (list delete-attribute-list))))
  (setf relation-tuple (cadr (get-relation relation-name '("OWNER-ID" "TUPLE-FORMAT" "ATTRIBUTES" "DOMAINS"
       "KEY" "SAVE-DIRECTORY" "DOC" "IMPLEMENTATION-TYPE"
       "STORAGE-STRUCTURE") nil)))
  (cond ((not relation-tuple)
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - The relation ~S is not defined in the database ~S"
     relation-name *active-db*))
 (return-from destroy-attribute nil)))
  ;;
  ;; later, verify here that the user owns this relation
  ;;
  (setf rel-owner-id (first relation-tuple)
save-dir (sixth relation-tuple)
init-attribute-list (third relation-tuple)
key-list (fifth relation-tuple)
new-key nil)
  ;;
  ;; Check if the attributes of delete-attribute-list are o.k.
  ;;
  (let ((temp-delete-list nil))
    (mapc (function (lambda (attr)
      (setf attr (validate-sym attr t))
      (cond ((member attr temp-delete-list :test 'string-equal)
     (if *provide-warning-messages*
  (format *standard-output*
   "~%WARNING - The attribute ~S is already in the list." attr)))
    ((not (member attr init-attribute-list :test 'string-equal))
     (if *provide-warning-messages*
  (format *standard-output*
   "~%WARNING - The attribute ~S is not defined in the relation ~S."
   attr relation-name)))
    (t
     (setf temp-delete-list (append (list attr) temp-delete-list))
     ;;
     ;; If attribute is part of the key of the relation
     ;;
     (cond ((member attr key-list :test 'string-equal)
     (setf new-key t)
     (setf pos  (position attr key-list :test 'string-equal))
     (setf key-list (append (firstn pos key-list) (nthcdr (1+ pos) key-list)))
     ))))))
  delete-attribute-list)
    (if temp-delete-list
(setf delete-attribute-list (nreverse temp-delete-list))
(return-from destroy-attribute nil)))
  ;;
  ;; Not allowed to destroy all the attributes of the relation
  ;;
  (cond ((= (length delete-attribute-list)(length init-attribute-list))
 (if *provide-error-messages*
     (format *standard-output*
     "~%ERROR - Destroying all the attributes of the relation has to be done invoking DESTROY-REL"))
 (return-from destroy-attribute nil)))
  (cond ((or (setf key-value (car (get-keyword-value '(key) keyword-list))) new-key)
 (cond (key-value
;;
;; check the key-value to see if there is any of the attribute to be destroyed in it
;;
(mapc (function (lambda (attr-key)
  (setf attr-key (validate-sym attr-key t))
  (cond ((member attr-key delete-attribute-list :test 'string-equal)
  (if *provide-error-messages*
      (format *standard-output*
       "~%ERROR - The attribute ~S cannot be part of the key as it is to be destroyed." attr-key))
  (return-from destroy-attribute nil))
        ((not (member attr-key init-attribute-list :test 'string-equal))
  (if *provide-error-messages*
      (format *standard-output*
       "~%ERROR - ~s is not an attribute of the ~s relation"
       attr-key relation-name))
  (return-from destroy-attribute nil)))))
key-value))
       ;;
       ;; new key because 1.some (or 2.all) attributes which are part of the key have to be destroyed. In case 2., key-value is
       ;; set to T, and will be set to the default key value of the relation in DESTROY-ATT-UTILITY-REDEF-REL
       ;;
       (t
(setf key-value (if key-list key-list t))))
 (setf rel-implementation-type (nth 7 relation-tuple)
       rel-storage-structure (nth 8 relation-tuple))
 (setf attribute-list (destroy-att-utility init-attribute-list init-attribute-list
      delete-attribute-list))
 (modify-index-key delete-attribute-list attribute-list indices)
 (if (not (funcall (find-symbol (concatenate 'string "DESTROY-ATT-UTILITY-REDEF-REL-"
          rel-implementation-type "-" rel-storage-structure)
  *pkg-string*)
   relation-name delete-attribute-list relation-tuple key-value))
     (return-from destroy-attribute nil)))
(t
 (setf rel-implementation-type (nth 7 relation-tuple)
       rel-storage-structure (nth 8 relation-tuple))
 (setf attribute-list (destroy-att-utility init-attribute-list init-attribute-list
      delete-attribute-list))
 (modify-index-key delete-attribute-list attribute-list indices)
 (if (not (funcall (find-symbol (concatenate 'string "DESTROY-ATT-" rel-implementation-type
    "-" rel-storage-structure) *pkg-string*)
   relation-name delete-attribute-list relation-tuple))
     (return-from destroy-attribute nil))))
  (cond (*auto-save*
    (save-system-relations)
    (save-relation relation-name (list 'dir save-dir))))
  (if *provide-status-messages*
      (format *standard-output* "~%Destroy attribute complete"))
  (return-from destroy-attribute relation-name)))

(defun modify-index-key (delete-attribute-list attribute-list indices)
  (mapc (function (lambda (index-element &aux key-list new-key)
    (setf key-list (second index-element)
  new-key nil)
    (mapc (function (lambda (attribute &aux pos)
       (cond ((member attribute key-list :test 'string-equal)
       (setf new-key t)
       (setf pos (position attribute key-list :test 'equal))
       (setf key-list (append (firstn pos key-list)
         (nthcdr (1+ pos) key-list)))))))
  delete-attribute-list)
    (cond (new-key
   (cond ((null key-list)
   (setf key-list (list `(quote ,(list (car attribute-list))))))
  (t
   (setf key-list (list `(quote ,key-list)))))
   (modify-tuples 'system-index 'attr '(key) 'values key-list
    'where `(string-equal index-name ,(first index-element)))))))
indices))

(defun destroy-att-array-hash (relation-name delete-attr-list relation-tuple
       &aux array-name init-attribute-list)
  (if (destroy-att-utility-array-list relation-name delete-attr-list relation-tuple)
      (progn
(setf init-attribute-list (third relation-tuple))
(multiple-value-setq (array-name)
  (read-from-string (concatenate 'string relation-name "ARRAY")))
(maphash (function (lambda (key bucket)
     key
     (mapcar (function (lambda (row)
    (setf (aref (symbol-value array-name) row)
          (destroy-att-utility (aref (symbol-value array-name)
         row)
          init-attribute-list
          delete-attr-list))))
      bucket)))
 (getp relation-name 'entry-point))
relation-name)
      nil))

(defun destroy-att-array-heap (relation-name delete-attr-list relation-tuple
  &aux array-name init-attribute-list)
  (if (destroy-att-utility-array-list relation-name delete-attr-list relation-tuple)
      (progn
(setf init-attribute-list (third relation-tuple))
(multiple-value-setq (array-name)
  (read-from-string (concatenate 'string relation-name "ARRAY")))
(mapcar (function (lambda (row)
    (setf (aref (symbol-value array-name) row)
   (destroy-att-utility (aref (symbol-value array-name) row) init-attribute-list
          delete-attr-list))))
(getp relation-name 'entry-point))
relation-name)
      nil))

(defun destroy-att-flavor-hash (relation-name delete-attr-list relation-tuple)
  (destroy-att-utility-redef-rel relation-name delete-attr-list relation-tuple  nil))

(defun destroy-att-flavor-heap (relation-name delete-attr-list relation-tuple)
  (destroy-att-utility-redef-rel relation-name delete-attr-list relation-tuple nil))

(defun destroy-att-flavor-avl (relation-name delete-attr-list relation-tuple)
  (destroy-att-utility-redef-rel relation-name delete-attr-list relation-tuple nil))

(defun destroy-att-list-hash (relation-name delete-attr-list relation-tuple)
  (destroy-att-utility-redef-rel relation-name delete-attr-list relation-tuple  nil))

(defun destroy-att-list-heap (relation-name delete-attr-list relation-tuple)
  (destroy-att-utility-redef-rel relation-name delete-attr-list relation-tuple nil))

(defun destroy-att-list-avl (relation-name delete-attr-list relation-tuple)
  (destroy-att-utility-redef-rel relation-name delete-attr-list relation-tuple nil))

(defun destroy-att-struct-hash (relation-name delete-attr-list relation-tuple)
  (destroy-att-utility-redef-rel relation-name delete-attr-list relation-tuple nil))

(defun destroy-att-struct-heap (relation-name delete-attr-list relation-tuple)
 (destroy-att-utility-redef-rel relation-name delete-attr-list relation-tuple nil))

(defun destroy-att-struct-avl (relation-name delete-attr-list relation-tuple)
 (destroy-att-utility-redef-rel relation-name delete-attr-list relation-tuple nil))


(defun destroy-att-utility (val-list init-attribute-list delete-attr-list
    &aux (result nil))
  (do ((attr-l init-attribute-list (cdr attr-l))
       (val-l val-list (cdr val-l)))
      ((or (null attr-l)(null val-l)) (nreverse result))
    (if (not (member (car attr-l) delete-attr-list :test 'string-equal))
(setf result (cons (car val-l) result)))))

(defun destroy-att-utility-array-list (relation-name delete-attr-list relation-tuple
        &aux attribute-list domain-list tuple-format-list where-c
        init-attribute-list)
  ;;
  ;; (i) system-attribute relation
  ;;     delete the tuples which describe the attributes to be deleted
  (mapcar (function (lambda (attr)
      (delete-or-modify 'system-attribute nil    ;delete
  (list 'and (list 'string-equal 'relation-name (string relation-name))
        (list 'string-equal 'attribute-name (string attr))))))
  delete-attr-list)
  ;;
  ;;  (ii) modify the system-relation tuple which defines this relation
  ;;
  (setf init-attribute-list (third relation-tuple))
  (setf domain-list  (destroy-att-utility (fourth relation-tuple) init-attribute-list delete-attr-list))
  (setf tuple-format-list (destroy-att-utility (second relation-tuple) init-attribute-list
         delete-attr-list))
  (setf attribute-list (destroy-att-utility init-attribute-list init-attribute-list delete-attr-list))
  (setf where-c (list 'string-equal 'relation-name (string relation-name)))
  (delete-or-modify 'system-relation t where-c '("MODIFIEDP" "TUPLE-FORMAT" "ATTRIBUTES" "DOMAINS")
    (list 't `(quote ,tuple-format-list ) `(quote ,attribute-list) `(quote ,domain-list)))
  relation-name)

(defun destroy-att-utility-redef-rel (relation-name delete-attr-list relation-tuple new-key
       &aux system-attribute-list attribute-list tuple-format-list tuple-list
       dir imp ss key doc attr-des-pair init-attribute-list)
  (setf system-attribute-list (get-system-attribute-list relation-name))
  (setf init-attribute-list (third relation-tuple))
   (setf tuple-format-list (destroy-att-utility (second relation-tuple) init-attribute-list delete-attr-list))
   (setf attribute-list (destroy-att-utility init-attribute-list init-attribute-list delete-attr-list))
   (setf dir (nth 5 relation-tuple)
 imp (nth 7 relation-tuple)
 ss (nth 8 relation-tuple)
 key (cond
       ;; all the attributes which were part of the key have been destroyed,
       ;; the new key is a list of the first attribute of the relation (default)
       ((equal new-key t)
(list (car attribute-list)))
       ;; the key has been modified (either with the keyword KEY, either by destroying some attributes which were part of the
       ;; key)
       (new-key
new-key)
       ;; the key is not modified
       (t
(nth 4 relation-tuple)))
 doc (nth 6 relation-tuple))
   ;;
   ;; Create the attribute-descriptor list to redefine the relation
  (setf attr-des-pair (create-attr-descriptor attribute-list system-attribute-list))
  (setf tuple-list nil)
  (maptuple (function (lambda (tuple)
      (setf tuple-list (append (list (destroy-att-utility tuple init-attribute-list
        delete-attr-list)) tuple-list))))
   relation-name)
  (redefine-rel relation-name attr-des-pair imp ss key tuple-format-list doc dir (nreverse tuple-list))
  relation-name)

(defun destroy-att-utility-redef-rel-list-heap (relation-name attribute-descriptor relation-tuple key-value)
  (destroy-att-utility-redef-rel relation-name attribute-descriptor relation-tuple key-value))

(defun destroy-att-utility-redef-rel-list-hash (relation-name attribute-descriptor relation-tuple key-value)
  (destroy-att-utility-redef-rel relation-name attribute-descriptor relation-tuple key-value))

(defun destroy-att-utility-redef-rel-list-avl (relation-name attribute-descriptor relation-tuple key-value)
  (destroy-att-utility-redef-rel relation-name attribute-descriptor relation-tuple key-value))

(defun destroy-att-utility-redef-rel-list-list (relation-name attribute-descriptor relation-tuple key-value)
  (destroy-att-utility-redef-rel relation-name attribute-descriptor relation-tuple key-value))

(defun destroy-att-utility-redef-rel-flavor-heap (relation-name attribute-descriptor relation-tuple key-value)
  (destroy-att-utility-redef-rel relation-name attribute-descriptor relation-tuple key-value))

(defun destroy-att-utility-redef-rel-flavor-hash (relation-name attribute-descriptor relation-tuple key-value)
  (destroy-att-utility-redef-rel relation-name attribute-descriptor relation-tuple key-value))

(defun destroy-att-utility-redef-rel-flavor-avl (relation-name attribute-descriptor relation-tuple key-value)
  (destroy-att-utility-redef-rel relation-name attribute-descriptor relation-tuple key-value))

(defun destroy-att-utility-redef-rel-struct-heap (relation-name attribute-descriptor relation-tuple key-value)
  (destroy-att-utility-redef-rel relation-name attribute-descriptor relation-tuple key-value))

(defun destroy-att-utility-redef-rel-struct-avl (relation-name attribute-descriptor relation-tuple key-value)
  (destroy-att-utility-redef-rel relation-name attribute-descriptor relation-tuple key-value))

(defun destroy-att-utility-redef-rel-struct-hash (relation-name attribute-descriptor relation-tuple key-value)
  (destroy-att-utility-redef-rel relation-name attribute-descriptor relation-tuple key-value))


;; fragments
(defun destroy-att-list-heap (relation-name delete-attr-list relation-tuple)
  (destroy-att-utility-redef-rel relation-name delete-attr-list relation-tuple nil))

(defun destroy-att-list-avl (relation-name delete-attr-list relation-tuple)
  (destroy-att-utility-redef-rel relation-name delete-attr-list relation-tuple nil))

(defun des
