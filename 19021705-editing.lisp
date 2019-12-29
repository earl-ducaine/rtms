
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
€oB€àB€ş¬°Define a new database.

   DB-NALMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540781. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "XLD" :NAME "MODIFY-AVL" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760360011. :AUTHOR "REL3" :LENGTH-IN-BYTES 6865. :LENGTH-IN-BLOCKS 14. :BYTE-SIZE 16.)                                  pp2€\€©COMPILE-DATA\€ì€SW-MFG,GODZILLA†€‡¤[ÎF€F€\€pÀ,COMPILER,VERSION€\€F€F€pÀB€),‚OPTIMIZE-SWITCH€†€©ƒQFASL-SOURCE-FILE-UNIQUE-ID€1€\€pÀl€FSì‚MAKE-FASLOAD-PATHNAME€\€ê€QUOTE€B€$\€B€8ª€NIL€\€B€8\€¬€RTMS\€B€8lMODIFY-AVL\€B€8¬€LISP\€B€8F€©€BASEF€
é€FONTS€\€©*CODE-FONT*€é*COMMENT-FONT*é*STRING-FONT*€)PACKAGE€©€RTMS©€MODE©COMMON-LISP€€‚MODIFY-LIST-AVL€€ë€(p†€â(F€˜À$€ÀB€:pÀ¬€TICLì€ART-Q€]€F€€:B€:B€:j€T€F€pÀ¬€SYS€l‚DEBUG-INFO-STRUCT€B€P\€ÃRELATION-NAME€ÃATTRIBUTE-LISTÃKEY-ATTRIBUTESC‚MODIFY-ATTRIBUTES€ÃMODIFY-VALUES€ƒWHERE-CLAUSEDOM-DEF€INDICES€B€:\€DOMAINS€‚KEY-DOMAIN-LIST€CKEY-VALUE€ÃINSERT-TUPLES€ƒ€MODE‚MODIFIED-TUPLES€‚NUMBER-MODIFIED€ªPACKAGE-NAMECREBALANCEPƒ‚TEMP-ATTRIBUTE-LIST€Ã‚TERMINATION-CONDITION€ƒ‚TOTAL-INSERT-TUPLES€Ã‚TOTAL-NUMBER-MODIFIED€ƒ€TREEÃ€TUPLESÃDOMAIN-ELEMENTÃ‚STRING-ATTRIBUTE-LIST€ƒSTRING-MODIFY-ATTRIBUTESƒ€KEY%‚BEGINNING-VALUE%ƒ‚TERMINATION-CLAUSE%€B€:B€:‚BEGINNING-VALUE€Ã€TUPLE€\€)‚MACROS-EXPANDED€\€ê€UNLESSê€THIRD€ê€FIRST€ê€SECONDª€PROGª€SETFÀ†€€ƒ*PKG-STRING*ÑC‚*SYSTEM-RELATIONS*‘êSTRING-UPCASE€ÒªSTRING-EQUALÀpÀB€\¬MEMBER-TEST€Òê€STRINGÀl€:*Àì-KEY-DOMAINS*€ÀªCONCATENATE€Ò*‚READ-FROM-STRINGÒª€EVALÒ*REVERSE€Òª€LISTÒƒPROJECT-LISTÒC‚CONVERT-ATTRIBUTESÒB€pÒ‚EXTRACT-KEY-AVL€ÒƒENTRY-POINT€Àƒ€GETPÒ\€\€B€YÀB€YÀƒ€LEP€ÀF€ÀC‚EVAL-WHERE-PREREQ€ÒjMAKUNBOUNDÒlTERMINATE€À\€Ã€BOGUS€B€Àì€LOCATEÀ†€Àƒ‚LIST-AVL-KEY-MODIFY€ÒpÀB€\,*APPEND€Ò,FINISHEDÀpÀB€\ìSTRING-EQUAL*€Òƒ€PUTPÒƒMODIFY-TUPLEÒ‚INSERT-LIST-AVL€’FßLß€QŠPP˜
ä	PP
P€QŠPªŠŠü†QOÁäOQ
B@]@ÁOÅúç@QŠŠQ‚QšBŠAÁ€ò€QCŠüPGÁQ‚QAQ…QGQªBÁ€QP’MÁBæ‚QŠPPšBÁQŠƒQŠQÁPÁBSBWB[TÁSÁRÁ`äTQB&äJİüPRQBTQBšJÁ…QQ€QPPAIÁ…ÁIQVÛUÁCUäUSšCVS€VæUSˆUÅVÅóıPDÁJFÁPCÁSSWÁ"üFßMQPQ‚QAQWQJQP€Q…QHQFQÿÛ„QQQIQ†Q P!PACÁWÁFÁHÁDÁMÁCQKQ"’KÁLQFaLÁHÛJF'æDQ#P$æCÕçRÅSÅTÅR ç€QMQP%˜K*äIQVÛUÁCUäUSšCVS€VæUSˆUÅVÅóıNÛKQXÁäQƒQXS„Q†Q€QIQJ&ºE]EÁXÅóç€QQEQ‚Q€Q'ªKÁLQKQ‚O€±B€P€€C‚MODIFY-FLAVOR-AVL€€ë€(m†€â(F€•À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€²\€B€`B€aB€bB€cB€dB€eB€fB€gB€:\€B€iB€jB€kB€lB€mB€nB€oB€pB€qB€rB€sB€tB€uB€vB€wB€xB€{B€|B€}B€:B€:B€~B€\€B€\€€ƒB€„B€…B€†B€‡B€ˆéDOCUMENTATION€ì¿öThis function will is the driver for the function which will modify the tuples of the specified list avl
   represented relation. A count of the total number of tuples modified is returned.

   RELATION-NAME     - The name of the relation whose tuples will be modified.
   ATTRIBUTE-LIST    - A list of all of the attributes in the relation in string form.
   KEY-ATTRIBUTES    - A list of the attributes which make form the key of the relation.
   MODIFY-ATTRIBUTES - A list of the attributes to be modified.
   MODIFY-VALUES     - A list of the expressions by which the attributes will be modified.
   WHERE-CLAUSE      - An s-expression which is used as a predicate to select the tuples to be modified.
   DOM-DEF           - A list of elements. Each element is a list containing the name of the attribute, the
                       domain of the element and the default value of the attribute.
   INDICES           - A boolean value which indicates of there are any indices defined on this relation.À†€€B€ŠÑB€‹‘B€ŒÒB€
ÀB€ÒB€Àl€:*Àì-KEY-DOMAINS*€ÀB€“ÒB€”ÒB€•ÒB€–ÒB€—ÒB€˜ÒB€pÒB€šÒB€›ÀB€œÒ\€\€B€YÀB€YÀB€ŸÀF€ÀB€¡ÒB€¢ÒlTERMINATE€À\€B€¥B€ÀB€™Òì€LOCATEÀ†€ÀÃ‚FLAVOR-AVL-KEY-MODIFY€ÒB€ªÒ,FINISHEDÀB€­ÒB€®ÒB€¯ÒC‚INSERT-FLAVOR-AVL€’FßLß€QŠPP˜
ä	PP
P€QŠPªŠŠü†QOÁäOQ
B@]@ÁOÅúç@QŠŠQ‚QšBAÁ€ò€QCŠüPGÁQ‚QAQ…QGQªBÁ€QP’MÁBæ‚QŠPPšBÁBSBWB[RÁQÁPÁaäRQB&äJİüPPQBRQBšJÁ…QQ€QPPAIÁ…ÁIQTÛSÁCSäSSšCTS€TæSSˆSÅTÅóıPDÁJFÁPCÁQSUÁ#üFßMQQŠ‚QAQUQJQP€Q…QHQFQÿÛ„QƒQIQ†Q P!PACÁUÁFÁHÁDÁMÁCQKQ"’KÁLQFaLÁHÛJF'æDQ#P$æCÔçPÅQÅRÅPŸç€QMQP%˜K*äIQTÛSÁCSäSSšCTS€TæSSˆSÅTÅóıNÛKQVÁäQƒQVS„Q†Q€QIQJ&ºE]EÁVÅóç€QQEQ‚Q€Q'ªKÁLQKQ‚O€ÍB€²€€C‚MODIFY-STRUCT-AVL€€ë€(m†€â(F€•À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€Î\€B€`B€aB€bB€cB€dB€eB€fB€gB€:\€B€iB€jB€kB€lB€mB€nB€oB€pB€qB€rB€sB€tB€uB€vB€wB€xB€{B€|B€}B€:B€:B€~B€\€B€\€€ƒB€„B€…B€†B€‡B€ˆB€¾ì¿ôThis function will is the driver for the function which will modify the tuples of the specified list avl
   represented relation. A count of the total number of tuples modified is returned.

   RELATION-NAME     - The name of the relation whose tuples will be modified.
   ATTRIBUTE-LIST    - A list of all of the attributes in the relation in string form.
   KEY-ATTRIBUTES    - A list of the attributes which make form the key of the relation.
   MODIFY-ATTRIBUTES - A list of the attributes to be modified.
   MODIFY-VALUES     - A list of the expressions by which the attributes will be modified.
   WHERE-CLAUSE      - An s-expression which is used as a predicate to select the tuples to be modified.
   DOM-DEF           - A list of elements. Each element is a list containing the name of the attribute, the
                       domain of the element and the default value of the attribute.
   INDICES           - A boolean value which indicates if there are indices defined on the relation.€À†€€B€ŠÑB€‹‘B€ŒÒB€
ÀB€ÒB€Àl€:*Àì-KEY-DOMAINS*€ÀB€“ÒB€”ÒB€•ÒB€–ÒB€—ÒB€˜ÒB€pÒB€šÒB€›ÀB€œÒ\€\€B€YÀB€YÀB€ŸÀF€ÀB€¡ÒB€¢ÒlTERMINATE€À\€B€¥B€ÀB€™Òì€LOCATEÀ†€ÀÃ‚STRUCT-AVL-KEY-MODIFY€ÒB€ªÒ,FINISHEDÀB€­ÒB€®ÒB€¯ÒC‚INSERT-STRUCT-AVL€’FßLß€QŠPP˜
ä	PP
P€QŠPªŠŠü†QOÁäOQ
B@]@ÁOÅúç@QŠŠQ‚QšBAÁ€ò€QCŠüPGÁQ‚QAQ…QGQªBÁ€QP’MÁBæ‚QŠPPšBÁBSBWB[RÁQÁPÁaäRQB&äJİüPPQBRQBšJÁ…QQ€QPPAIÁ…ÁIQTÛSÁCSäSSšCTS€TæSSˆSÅTÅóıPDÁJFÁPCÁQSUÁ#üFßMQQŠ‚QAQUQJQP€Q…QHQFQÿÛ„QƒQIQ†Q P!PACÁUÁFÁHÁDÁMÁCQKQ"’KÁLQFaLÁHÛJF'æDQ#P$æCÔçPÅQÅRÅPŸç€QMQP%˜K*äIQTÛSÁCSäSSšCTS€TæSSˆSÅTÅóıNÛKQVÁäQƒQVS„Q†Q€QIQJ&ºE]EÁVÅóç€QQEQ‚Q€Q'ªKÁLQKQ‚O€èB€Î€€ƒ‚MODIFY-FLAVOR-TUPLES€ë€*h†€âF€>À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€é\€RELATIONB€aB€cB€dÃ€WHERE€B€fB€wB€rB€:\€ƒ‚ATOM-ATTRIBUTE-LIST€Ã‚ATOM-MODIFY-ATTRIBUTESÃFLAVOR-PACKAGEƒ€DATAƒ‚CONV-ATTRIBUTE-LIST€B€:B€:B€:B€:B€A-TUPLE€B€:B€:ƒ€ATTRƒ€VAL€TEMPVAL€\€B€\€B€‡pÀ¬€ZLC€,DO-NAMEDpÀB€Tì‚INHIBIT-STYLE-WARNINGSB€ˆÀ†€€ƒ‚*VALIDITY-CHECKING*€ÑB€‹‘ê€TYPEP€ÒB€pÒƒ‚UNCONVERT-ATTRIBUTESÒÃ‚PROJECT-FLAVOR-PREREQ€Òƒ‚FAST-PROJECT-FLAVOR€ÒƒQUOTE-TUPLE€ÒCFORM-ALISTÒpÀB€\,SUBLIS*€ÒB€•ÒB€ŒÒB€
ÀB€ÒCDOM-CHECK€ÒpÀB€T,‚SET-IN-INSTANCE€’†SŠCŠBÁQBQ’@Á‚QBQ’AÁQ	ŠDÁ†QDQ
’CÁEÑ†QCQHÁGÁFÁ4üFQGSHSJÁIÁAQƒQLÁKÁ!üKSLSNÁMÁOÛJQŠ@Q’NQ’ŠOÁ€QŠPP˜æäOQMQ…Q˜äIQMQOQ˜KÅLÅKäLÜçIQCFÃÁGÅHÅGäHÈçEO€B€é€€ƒ‚MODIFY-STRUCT-TUPLES€ë€L®†€âF€bÀ$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€\€B€òB€aB€cB€dB€óB€fB€wB€rB€:\€B€õB€öCDELORMOD?€Ã‚STRUCT-ATTRIBUTE-LIST€ƒNUM-MODIFIEDƒTEMP-STRUCT€ƒSTRUCT-MODIFY-ATTRIBUTESƒ‚STRING-RELATION-NAMEB€:B€:B€:B€:ƒSTRUCT-TUPLEB€:B€:B€:B€:B€ûB€üƒSTRUCT-ATTR€B€ı\€B€\€pÀB€\lXR-BQ-LISTB€‡B€B€B€ˆÀ†€€B€ÑB€‹‘B€ÒB€ÀB€“ÒB€ÒB€—ÒÃ‚SUPER-FAST-EVAL-WHERE€ÒB€ÒB€ÒB€ÒB€•ÒB€ŒÒB€
ÀB€ÒB€ÒB€ˆÀB€8€Dß€QŠGÁHÑQJÁIÁ	üIQPGQJSšCIÃÁJÅJõçHQ	ŠCÁJÛJÑ‚QHÁKÁ	üKQPGQHSšCKÃÁHÅHõçJQ	ŠFÁQ	Š@Á‚Q	ŠAÁ†QJÁbäJSLÁBÛIÛIÑCQNÁMÁüMQLQNSÿ‹CMÃÁNÅNöçIQEÁ„Q±æEQ
Š‡Q„Q˜BäNÛNÑAQƒQFQIÁHÁPÁOÁ/üOQPSHSISSÁRÁQÁTÛEQŠ@Q’RQ’ŠTÁ€QŠPP˜æäTQQQ…Q˜äBİPSQLQ
’PTQ
’
šŠüÿÛCOÃÁPÅHÅIÅPäHäIËçBäDÉJÅçDO€+B€€€Ã€MODAVL€ë€-†€@ÄF€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€,\€B€vB€qCTEMP-TREE€B€:\€MODTREE€\€B€\€B€„B€ˆê€FOURTH€F€ĞÀB€,ÒF€ÀBALANCE2’€Qä€QBQ‚QPPA‚ÁÁ@Á€QB@QÀä€QQPPAÁ€Áü‚Q€SÀ€[€Áİ€QQ‚QƒO€>B€,€€B€¨€ë€†€àF€™À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€¨\€B€vB€aB€bB€iB€~C‚TERMINATION-CLAUSEB€mB€`B€eB€qB€oB€lB€dB€cB€rB€fB€:\€ƒ‚COMPARISON-OPERATOR€Ã‚CURRENT-NODE-KEY-VALUEMOD-TREE\€B€\€B€„B€ˆÀ†€€Ã€LOCATEÀB€­ÒÃLOCATE-STAGE-2ÀìDELETE-SEARCH€ÀB€—ÒB€˜ÒB€YÀCLESS-THAN€ÀƒNODE-COMPAREÒ†€ÀB€¨ÒƒGREATER-THANÀF€ÀB€=ÒlTERMINATE€À,RESTART€Àê€EQUAL€ÀìLOCATE-STAGE-2Àì€LOCATEÀBALANCE1Ò,FINISHEDÀƒPROCESS-LIST-AVL-MODIFY€’€Öä†QPæ†QPæ†QPÈä€QBŠQ‚Q	šBAÁ„S
&äPü„QAQƒQš@Ã&ä†QPä€[Q‚QƒQ„Q…Q†Q‡QˆQ QŠQ‹QŒQ
QQQPPA‹Á„ÁŠÁ Á†ÁBÁ]ü@Q&1ä†QP-ä€QBQ‚QƒQ„Q…Q†Q‡QˆQ QŠQ‹QŒQ
QQQPPA‹Á„ÁŠÁ Á†ÁBÁ€QBBQÀ sä€Q QPPA Á€Á äPgüPeü@QPbäP†Á€[Q‚QƒQ„Q…Q†Q‡QˆQ QŠQ‹QŒQ
QQQPPA‹Á„ÁŠÁ Á†ÁBÁ†QPæ†QPäP†Á€YBQÀ ä€Q QPPA Á€Á äPüP†Á†QP'æ†QP#æ†QPæ€QQˆQŠQ Q†Q„Q‡Q…Q‚QƒQ‹QŒQ
QQQPPA‹Á„ÁŠÁ Á†ÁBÃ€Áü€æP†Á†QPä€QQ‚QƒQ„Q…QP‡QˆQ QŠQ‹QŒQ
QQQPPA‹Á„ÁŠÁ Á†Á€Á€Q†Q QŠQ„Q‹Q†O€_B€¨€€B€Ê€ë€†€àF€™À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€Ê\€B€vB€aB€bB€iB€~B€GB€mB€`B€eB€qB€oB€lB€dB€cB€rB€fB€:\€B€IB€JB€K\€B€\€B€„B€ˆÀ†€€B€OÀB€­ÒB€PÀìDELETE-SEARCH€ÀB€—ÒÃPROJECT-FLAVORÒB€YÀB€RÀB€SÒ†€ÀB€ÊÒB€UÀF€ÀB€=ÒlTERMINATE€À,RESTART€ÀB€YÀìLOCATE-STAGE-2Àì€LOCATEÀB€\Ò,FINISHEDÀCƒPROCESS-FLAVOR-AVL-MODIFY€’€Öä†QPæ†QPæ†QPÈä€QBŠQ‚Q	šBAÁ„S
&äPü„QAQƒQš@Ã&ä†QPä€[Q‚QƒQ„Q…Q†Q‡QˆQ QŠQ‹QŒQ
QQQPPA‹Á„ÁŠÁ Á†ÁBÁ]ü@Q&1ä†QP-ä€QBQ‚QƒQ„Q…Q†Q‡QˆQ QŠQ‹QŒQ
QQQPPA‹Á„ÁŠÁ Á†ÁBÁ€QBBQÀ sä€Q QPPA Á€Á äPgüPeü@QPbäP†Á€[Q‚QƒQ„Q…Q†Q‡QˆQ QŠQ‹QŒQ
QQQPPA‹Á„ÁŠÁ Á†ÁBÁ†QPæ†QPäP†Á€YBQÀ ä€Q QPPA Á€Á äPüP†Á†QP'æ†QP#æ†QPæ€QQˆQŠQ Q†Q„Q‡Q…Q‚QƒQ‹QŒQ
QQQPPA‹Á„ÁŠÁ Á†ÁBÃ€Áü€æP†Á†QPä€QQ‚QƒQ„Q…QP‡QˆQ QŠQ‹QŒQ
QQQPPA‹Á„ÁŠÁ Á†Á€Á€Q†Q QŠQ„Q‹Q†O€vB€Ê€€B€å€ë€€†€àF€šÀ$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€å\€B€vB€aB€bB€iB€~B€GB€mB€`B€eB€qB€oB€lB€dB€cB€rB€fB€:\€B€IB€JB€K\€B€\€B€„B€ˆÀ†€€B€OÀB€­ÒB€PÀìDELETE-SEARCH€ÀB€—ÒÃPROJECT-STRUCTÒB€YÀB€RÀB€SÒ†€ÀB€åÒB€UÀF€ÀB€=ÒlTERMINATE€À,RESTART€ÀB€YÀìLOCATE-STAGE-2Àì€LOCATEÀB€\Ò,FINISHEDÀCƒPROCESS-STRUCT-AVL-MODIFY€’€×ä†QPæ†QPæ†QPÉä€QBŠQ‚Q‡Q	¢BAÁ„S
&äPü„QAQƒQš@Ã&ä†QPä€[Q‚QƒQ„Q…Q†Q‡QˆQ QŠQ‹QŒQ
QQQPPA‹Á„ÁŠÁ Á†ÁBÁ]ü@Q&1ä†QP-ä€QBQ‚QƒQ„Q…Q†Q‡QˆQ QŠQ‹QŒQ
QQQPPA‹Á„ÁŠÁ Á†ÁBÁ€QBBQÀ sä€Q QPPA Á€Á äPgüPeü@QPbäP†Á€[Q‚QƒQ„Q…Q†Q‡QˆQ QŠQ‹QŒQ
QQQPPA‹Á„ÁŠÁ Á†ÁBÁ†QPæ†QPäP†Á€YBQÀ ä€Q QPPA Á€Á äPüP†Á†QP'æ†QP#æ†QPæ€QQˆQŠQ Q†Q„Q‡Q…Q‚QƒQ‹QŒQ
QQQPPA‹Á„ÁŠÁ Á†ÁBÃ€Áü€æP†Á†QPä€QQ‚QƒQ„Q…QP‡QˆQ QŠQ‹QŒQ
QQQPPA‹Á„ÁŠÁ Á†Á€Á€Q†Q QŠQ„Q‹Q†O€
B€å€€B€^€ë€`×†€à(F€wÀ$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€^\€B€vB€aB€eB€oB€qB€mB€~B€`B€GB€bB€iB€lB€dB€cB€rB€fB€:\€
ÃDELETE-TUPLES€ƒKEY-MODIFIEDB€KNEW-NODEB€5B€wB€{B€:CNODE-TUPLEÃ€TUPLE%\€B€\€€„B€:B€B€…B€‡B€ˆÀ†€
€B€
ÀB€ÒB€*ÒB€ªÒpÀB€\lMEMBER-EQLÒB€—ÒB€˜ÒlTERMINATE€ÀF€ĞÀB€,ÒF€ÀB€\ÒB€¯ÒB€­Òì€LOCATEÀ†€ÀB€¨ÒB€=Ò,FINISHED€ QFÁüFS
QPšAÁFÅäA÷åAVä€SQ‚Qš@ÃJ™kä@Q‹Q’‹Á€SGÁ
äGSHÃ‹QæHQE]EÁGÅöç@QŠCƒaƒÁE4æ€QB	ŠQ Q
šB†Á€Q€QB	ŠÀP…Á€QDÁDUæD[€Á„İ<üDQæDQB÷ı€[„QDQPPADÁ„ÁBÁDYBQÀ„(ä€Q„QPPA„Á€Á ü€QEQüCÛ€SIÁäIS	ŠQ‚Q˜äƒÉQ
QISŒQQ‡QQJºüISC]CÁIÅêç€QCQÀ…QP4æ€SQˆQ˜*ä€QBQ QŠQ†QˆQP‡Q‚Q„QƒQ‹QŒQ
QQQPPA‹Á†ÁƒÁ„Á…ÁBÁ€QBBQÀ„ä€Q„QPPA„Á€ÁP…Áü€äP…Á†Û€Q…Q„QƒQ†Q‹Q†O€§B€^€€B€u€ë€hç†€à0F€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€u\€B€vB€aB€eB€oB€qB€mB€~B€`B€GB€bB€iB€lB€dB€cB€rB€fB€:\€B€—ƒ‚DELETE-FLAVOR-TUPLE€B€˜B€KB€5B€wB€{B€:B€:ƒFLAVOR-TUPLECLIST-TUPLEB€š\€B€\€€„B€:B€…B€B€‡B€ˆÀ†€€B€
ÀB€ÒB€mÒB€—ÒB€*ÒB€ªÒB€ ÒlTERMINATE€ÀF€ĞÀB€,ÒF€ÀB€\ÒB€éÒB€­Òì€LOCATEÀ†€ÀB€ÊÒB€=Ò,FINISHED€ QFÁüFS
QPšBÁFÅäB÷å€S€SQQšHÁGÁüGSHSJÁIÁJQŠQ‚Q˜äJQ@]@ÁIQA]AÁGÅHÅGäHêçBSä@QJ™]ä@Q‹Q	’‹Á€SGÁ
äGSKÃAQ
æKQE]EÁGÅöç@QŠCƒaƒÁE4æ€QBŠQ QšB†Á€Q€QBŠÀP…Á€QDÁDUæD[€Á„İ.üDQæDQB÷ı€[„QDQPPADÁ„ÁCÁDYCQÀ„ä€Q„QPPA„Á€Áü€QEQÀüAQŠCƒaƒÁ‡QQ
QŒQˆQQAQQJ¸…QP7æ€SQQšQˆQ˜*ä€QBQ QŠQ†QˆQP‡Q‚Q„QƒQ‹QŒQ
QQQPPA‹Á†ÁƒÁ„Á…ÁCÁ€QBCQÀ„ä€Q„QPPA„Á€ÁP…Áü€äP…Á†Û€Q…Q„QƒQ†Q‹Q†O€½B€u€€B€Œ€ë€jì†€à4F€‚À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€Œ\€B€vB€aB€eB€oB€qB€mB€~B€`B€GB€bB€iB€lB€dB€cB€rB€fB€:\€B€—ƒ‚DELETE-STRUCT-TUPLE€B€˜B€KB€5B€wÃ‚STRING-TEMP-ATTRIBUTESB€{B€:B€:B€#B€³B€š\€B€\€€„B€:B€…B€B€‡B€ˆÀ†€€B€™ÒB€
ÀB€ÒB€„ÒB€—ÒB€*ÒB€ªÒB€ ÒlTERMINATE€ÀF€ĞÀB€,ÒF€ÀB€\ÒB€ÒB€­Òì€LOCATEÀ†€ÀB€åÒB€=Ò,FINISHED€QŠFÁ QGÁüGS
QPšBÁGÅäB÷å€S€SFQFQ‡Q¢IÁHÁüHSISKÁJÁKQŠQ‚Q	˜äKQ@]@ÁJQA]AÁHÅIÅHäIêçBTä@QJ™]ä@Q‹Q
’‹Á€SHÁ
äHSLÃAQæLQE]EÁHÅöç@QŠCƒaƒÁE5æ€QBŠQ Q‡Q¢B†Á€Q€QBŠÀP…Á€QDÁDUæD[€Á„İ-üDQæDQB÷ı€[„QDQPPADÁ„ÁCÁDYCQÀ„ä€Q„QPPA„Á€Áü€QEQÀüƒQ‡QQ
QŒQˆQQAQQJºÿaƒÁ…QP8æ€SFQFQ‡Q¢QˆQ	˜*ä€QBQ QŠQ†QˆQP‡Q‚Q„QƒQ‹QŒQ
QQQPPA‹Á†ÁƒÁ„Á…ÁCÁ€QBCQÀ„ä€Q„QPPA„Á€ÁP…Áü€äP…Á†Û€Q…Q„QƒQ†Q‹Q†O€ÒB€Œ€1€\€pÀB€\,„FASL-RECORD-FILE-MACROS-EXPANDED\€B€8\€\€ê€DEFUN€†€'\€B€:†€xõ¿\€B€(†€.Ù‹\€B€†€(Ì¢\€B€†€*ıj\€B€ˆ†€[æ„\€B€‡†€=Ì#\€B€††€{šÍ\€B€…†€z(‡\€B€„†€:}n\€B€ƒ†€6€€tribute-list dom-def))
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
        (concatenate 'string string-relatLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540787. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "XLD" :NAME "MODIFY-REL" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760360646. :AUTHOR "REL3" :LENGTH-IN-BYTES 4758. :LENGTH-IN-BLOCKS 10. :BYTE-SIZE 16.)                                  pp2€\€©COMPILE-DATA\€ì€SW-MFG,GODZILLA†€‡¤ÖĞF€F€\€pÀ,COMPILER,VERSION€\€F€F€pÀB€),‚OPTIMIZE-SWITCH€†€©ƒQFASL-SOURCE-FILE-UNIQUE-ID€1€\€pÀl€FSì‚MAKE-FASLOAD-PATHNAME€\€ê€QUOTE€B€$\€B€8ª€NIL€\€B€8\€¬€RTMS\€B€8lMODIFY-REL\€B€8¬€LISP\€B€8F€©€BASEF€
é€FONTS€\€©*CODE-FONT*€é*COMMENT-FONT*é*STRING-FONT*€)PACKAGE€©€RTMS©€MODE©COMMON-LISP€€Ã‚CREATE-ATTR-DESCRIPTOR€ë€,f†€@´F€:À$€ÀB€:pÀ¬€TICLì€ART-Q€]€F€€:B€:B€:j€T€F€pÀ¬€SYS€l‚DEBUG-INFO-STRUCT€B€P\€ÃATTRIBUTE-LISTÃ‚SYSTEM-ATTRIBUTE-LIST€B€:\€ÃATTR-DES-PAIR€ATTR-DEFATTR-DOCƒ€DOM€CFOUND-ATTRCATTR-TUPLEB€:B€:B€:CATTR-NAME€Ã€TUPLESB€:B€:\€)‚MACROS-EXPANDED€\€ê€THIRD€ê€FIFTH€ê€FOURTHê€SECONDpÀB€Tl‚CONDITION-BIND-IF€pÀB€TìCONDITION-BINDpÀB€Tì‚CATCH-CONTINUATION-IF€pÀB€Tl‚CATCH-CONTINUATIONpÀB€Tì€ERRSETª€PROGpÀ¬€ZLC€,DO-NAMEDpÀB€Tì‚INHIBIT-STYLE-WARNINGSª€SETF€pÀl€EH¬‚*CONDITION-HANDLERS*‘pÀ,€ì€G0360€ÀF€BÀê€ERROR€ÀpÀB€\ìERRSET-HANDLERÀpÀB€\ìSTRING-EQUAL*€Òª€LISTÒB€fÀpÀB€T¬€DEF€Àƒ€DOC€ÀpÀB€\,*APPEND€’FÑ€QHÁGÁOüGQHSIÁDÛQJÁ3üJSEÁPPTPPPÿÛJCKÃPJCLÃÖEWIQ’	ŠJ!BJ!B\äDİEQBAÁEUBBÁE[CÁ@QEW
PCQPAQPBQ	²	’’@ÁJÅDæJÉçD
æ@QI5äIQüIQ	Š’@ÃüÿÛCGÃÁHÅH¯ç@O€•B€P€€CƒGET-SYSTEM-ATTRIBUTE-LIST€€ë€(†€@DF€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€–\€ÃRELATION-NAME€B€:B€:B€:€Ã‚*SYSTEM-ATTRIBUTE-KEY*ÑÃƒ*SYSTEM-ATTRIBUTE-ATTRIBUTES*€Ñƒ*PKG-STRING*Ñƒ„*SYSTEM-RELATION-STORAGE-STRUCTURE*€ÑÃ„*SYSTEM-RELATION-BASE-IMPLEMENTATION*€‘ê€STRINGÀlRETRIEVE-€Àl€-€ÀªCONCATENATE€ÒªFIND-SYMBOL€Ò‚SYSTEM-ATTRIBUTEÀªSTRING-EQUALÀB€ŸÀêSTRING-UPCASE€ÒB€’P	PP
PPªP’@ÁPPPPPP€QŠšÿÛPJ@½O€­B€–€€ƒREDEFINE-REL€ë€"a†€BHF€?À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€®\€	B€ŸB€cƒ€IMP€C€SSƒ€KEY€C‚TUPLE-FORMAT-LIST€B€’ƒ€DIR€CTUPLE-LISTB€:\€TEMP-RELSTATUS?€\€B€l\€pÀB€\lXR-BQ-LISTB€‚€Cƒ*PROVIDE-STATUS-MESSAGES*€‘ê€GENSYMÒB€¥Ò*‚READ-FROM-STRINGÒB€·Àƒ€STO€ÀB€¹Àê€FORMATÀB€’ÀB€»À‚DEFINE-RELATION€ÒƒSYSTEM-INDEXÀB€«ÀB€ŸÀB€Ò\€ìRELATION-NAME€ÀB€¬ÀB€8Àª€EVALÒ‚DELETE-OR-MODIFYÒCRELATIONP€Ò‚DESTROY-RELATIONÒƒ‚DELETE-INDEX-TUPLES€Ò‚RENAME-RELATION€ÒB€jÀÃ€INSERT’PAÁ‚ŠŠ@ÁÚ@QQP‚QPƒQ	P„Q
P…QP†QP‡QJ¸æAQÀRPÿİPP€QŠšPPP@Q’’ŠŠ¨€Qˆä€Qˆ@Q‚Q@Q€QŠŠßåˆä€QŠŠPˆQ˜AQÀ€O€ÔB€®€€‚MODIFY-RELATION€€ë€UÖ†€àU@F€+À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€Õ\€RELATIONê€&REST€ƒKEYWORD-LISTª€&KEYj&OPTIONAL€B€ŸÃADD-ATTRIBUTESC‚DELETE-ATTRIBUTES€C‚RENAME-ATTRIBUTES€ƒ‚IMPLEMENTATION-TYPE€C‚STORAGE-STRUCTURE€B€ÈB€¹êDOCUMENTATION€jDIRECTORY€j‚&ALLOW-OTHER-KEYS€B€:\€#B€àB€ŸB€ãB€äB€åB€æB€çB€ÈB€¹B€èB€éƒ€REL€ƒ€ATTRB€·B€ÇFORMAT1€ƒ€KEY1B€’B€»MOD-ATTRMOD-VALSƒRENAME-ATTRSCNEW-ATTRS€C‚CURRENT-ATTRIBUTESB€jƒDELETE-ATTRSOLD-VALSB€àB€:B€:ƒ€ATT€B€:B€:DOM-DEF€B€:\€B€l\€ê€FIRST€B€B€qB€nB€p*SEVENTH€ê€SIXTH€B€oB€B€ÃpÀB€TlCOND-EVERYB€|B€‚éDOCUMENTATION€ì¿‡Modify various features of a relation.

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
  DIRECTORY            - New directory in which this relation is to be saved.€À†€£ €C*PKG-NAME*ÑÃ‚*SYSTEM-RELATION-KEY*€Ñƒƒ*SYSTEM-RELATION-ATTRIBUTES*ÑB€¢ÑB€£ÑB€¤ÑB€ÄÑƒ‚*VALIDITY-CHECKING*€Ñƒ*ACTIVE-DB*€Ñƒ*PROVIDE-ERROR-MESSAGES*Ñƒ‚*PARAMETER-CHECKING*‘\€
éRELATION-NAME€éADD-ATTRIBUTESi‚DELETE-ATTRIBUTES€i‚RENAME-ATTRIBUTES€©‚IMPLEMENTATION-TYPE€i‚STORAGE-STRUCTURE€é€FORMAT©€KEY€B€iDIRECTORY€ÀpÀB€\ìSTORE-KEYARGS€Ò‚ACTIVE-DATABASE€ÒƒVALIDATE-SYMÒ\€
B€ìADD-ATTRƒDELETE-ATTR€ƒRENAME-ATTR€B€·B€ÇB€ÈB€¹B€’B€»ÀƒGET-KEYWORD-VALUE-PREREQÒ\€lATTRIBUTESÀƒGET-RELATIONÒê€TERPRIÒl‚ERROR - Relation €ÀªWRITE-STRINGÒê€PRIN1€Ò,„ does not exist in the database À\€B€ÈÀC‚GET-KEYWORD-VALUE€Ò\€¬TUPLE-FORMATÀB€”ÒB€8ÀB€Ò\€B€»À\€ìSAVE-DIRECTORYÀ\€B€’À\€ìDOCUMENTATION€À\€B€¹À\€¬€KEY€À\€B€·À\€B€ÇÀB€jÀRETRIEVEÒ\€lATTRIBUTESìSAVE-DIRECTORY¬€DOC€¬TUPLE-FORMAT¬‚IMPLEMENTATION-TYPE€l‚STORAGE-STRUCTURE€¬€KEY€ÀjCOPY-LIST€Ò\€B€ÀC‚CONVERT-ATTRIBUTESÒB€«ÀpÀB€\¬MEMBER-TEST€ÒpÀB€\,DELETE*€Ò,ERROR - Àì„ is not an attribute in the relation €À,ƒ. It can not be deleted.ÀƒPROJECT-LISTÒB€–ÒB€PÒ\€B€ÀpÀB€\lSUBST-EQL€Ò,ƒ. It can not be renamed.À\€B€À\€B€ìÀB€®ÒB€¥À,MODIFY-€Àl€-€ÀB€¨ÒB€©Ò‚SYSTEM-RELATION€ÀB€ŸÀB€¥ÒB€ªÀPROJECT€À\€ÃATTRIBUTE-NAME‚DOMAIN-FUNCTION€ÀÃ€WHERE€ÀB€¬ÒB€ÆÒ\€lATTRIBUTES¬‚IMPLEMENTATION-TYPE€l‚STORAGE-STRUCTURE€¬€KEY€À‚RENAME-ATTRIBUTEÒ‚DEFINE-ATTRIBUTEÒCATTRIBUTE€ÀC‚DESTROY-ATTRIBUTE€ÒÃDELETE-TUPLES€ÒÃINSERT-TUPLES€ÒB€Ò’@ä@QPÿİAÑ ä€æR€QŠ€ÁæR@Qü[S[Áä[Sÿ5úç[Q@ÁP@Q’@Á€QPÿİš	BWÁæ	ä€Pˆ€QˆPˆPˆRP@Q’BOÁäÿÛP’SÁÿÛ POQ!’!Š’TÃ]Á\İ"P@Q’BRÁäSQ#P’SÁTQ PRQ!’!Š’TÃ]Á\İ$P@Q’BQÁäSQ%P’SÁTQ PQQ!’!Š’TÃ]Á\İ&P@Q’BPÁäSQ'P’SÁTQ PPQ!’!Š’TÃ]Á\İ(P@Q’BMÁ)P@Q’BNÁMæN§ä€Q*Pÿİ+šXÁ€Q,PÿİšBWÁWS-ŠLÁ.P@Q’&ä.P@Q’B/Š\Áä\S^ÃLQ0P1˜ä^QLQ2’LÁüä€3Pˆ^Qˆ4Pˆ€Qˆ5Pˆ\ÅæçXQWSLQ6šXÁLQ€Q7Š8’LÁ9P@Q’)ä9P@Q’B/Š\Á"ä\S^Á^5ä^SLQ0P1˜ä^W^SLQ:šLÁüä€3Pˆ^5ä^Sü^Qˆ4Pˆ€Qˆ;Pˆ\ÅŞç<P@Q’äLQ<P@Q’B’LÁP
P!’ZÁÚ
Ú=P@Q’Bâ€QKÃLQMQâWUBNQâWYBPQâWQBBOQâWQBQQâW[RQâWWXQ	J>¸æRZSÀZW
ÀKS9ä?P@P	PAPPBªPC’bÁDPPPSQTQ0PEP€QFŠ!š]Û]ÑGPHPIP*PÿİJP0PEP€QKŠ!šJ+º`Á_Áü_Q`SaÁaS?PPaWBšLŠ!’C_ÃÁ`Å`ğç]QÿÛJb¹€QMPÿİšBWÁ9P@Q’BUÁ<P@Q’BVÁ.P@Q’BYÁUä€QUQNVä€QVQOYä€QPPYQQ˜Pä€Q*Pÿİ+šXÁ€QJPÿİR˜€Q*PXQS˜=P@Q’BKÁä€QKQTSO€aB€Õ€€‚MODIFY-DATABASE€€ë€-v†€``F€IÀ$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€b\€	DATABASEB€ßB€àB€áB€âÃDATABASE-NAME€B€éB€èB€êB€:\€B€àB€lB€éB€èƒ€TEMPB€àB€:B€:\€B€l\€B€ÿB€|B€‚B€l¢Modify various features of the active database.

  DATABASE      - Name of the database to be modified.
  DATABASE-NAME - New name for this database.
  DIRECTORY     - New directory in which this database is to be saved.
  DOCUMENTATION - New description for this database.€€B€ÑB€ÑB€ÑB€	‘\€éDATABASE-NAME€B€B€ÀB€ÒB€ÒB€ÒB€ÒB€ Òì‡ERROR - The database to modify has to be the active database €ÀB€"ÒB€#Ò\€B€kB€»B€’ÀB€Ò\€B€»ÀB€&ÒB€¥À,‚*SAVE-DIRECTORY*ÀB€¨ÒB€ÆÒ\€B€’À,ƒ*DATABASE-DOCUMENTATION*À\€B€kÀ‚RENAME-DATABASE€’@ä@QPÿİAÑ ä	€æR€Qÿİ
’€ÁæR€QPæä€PˆPˆR@QüESEÁäESÿ5úçEQ@ÁP@Q’@ÁP@Q’BDÁ	äPPPšŠDQÈBGÁFİP@Q’BDÁ	äPPPšŠDQÈBGÁFİP@Q’BDÁä€QDQ’GÁFİDQâ€ÿO€|B€b€€‚MODIFY-ATTRIBUTE€ë€<t$†€à<€F€°À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€}\€B€ŞB€]B€ßB€àB€áB€âB€SÃDEFAULT-VALUE€B€èB€ÈB€êB€:\€B€àB€SB€†B€èB€ÈB€nCATTRIBUTESƒTUPLE-FORMATƒ€NUM€B€àB€:B€:B€:B€:B€:B€ø\€B€l\€B€qB€ûB€ÃB€B€B€ÿB€|B€‚B€¬³Modify various features of an attribute in a given relation.

  RELATION       - Name of the relation in which the attribute to be modified exists.
  ATTRIBUTE      - Name of the attribute to be modified.
  ATTRIBUTE-NAME - New name for this attribute.
  DEFAULT-VALUE  - New default value for this attribute.
  DOCUMENTATION  - New description.
  FORMAT         - New print width to be used for this attribute.€À†€A€Cƒ*PROVIDE-WARNING-MESSAGES*ÑB€ÑB€ÑB€ÑB€	‘\€éATTRIBUTE-NAMEéDEFAULT-VALUE€B€B€ÀB€ÒB€ÒB€Ò\€lATTRIBUTES¬TUPLE-FORMATÀB€ÒB€ Òl‚ERROR - Relation €ÀB€"ÒB€#Ò,„ does not exist in the database ÀB€?ÒB€«ÀB€AÒ,ERROR - Àì„ is not an attribute in the relation €À\€B€íB€‘B€’B€ÈÀB€Ò\€B€‘ÀB€&ÒB€ªÀB€QÀ\€B€SB€TÀB€jÀB€UÀB€ŸÀB€¬ÒB€ÒB€4ÒB€¥ÀB€¨ÒB€ÆÒCDOM-CHECK€Òª€AND€ÀB€SÀ\€ìDEFAULT-VALUE€ÀB€ÎÒ\€B€ÈÀlWARNING - Àìƒ is not a valid format value.€Àê€EQUAL€ÀpÀB€\lPOSITION*€ÒpÀB€Tì€FIRSTNÒê€APPENDÒB€PÀ\€¬TUPLE-FORMATÀB€8À\€B€’À\€¬€DOC€À\€B€íÀB€[’@ä@Q	PÿİAÑ
 ä€æR€QŠ€ÁæR€QPÿÛšBGÁGæ	ä€Pˆ€QˆPˆPˆRQŠBÃGSP˜æ	ä€PˆQˆPˆ€QˆR@QüISIÁäISÿ5úçIQ@ÁP@Q’@ÁP@Q’BEÁ<äEQQLÑPPP Pÿİ!PP"P€Q#Š$šJ%ºNÁMÁüMQNSOÁOS&PPOW'š(Š$’CMÃÁNÅNğçLQ)˜äPÿİ*PP"P€Q#Š$šP+PQ#Š$š$š,PEQ$Š-ªüÿÛKÁJİ.P@Q’BEÁ.äE1æä€/PˆEQˆ0PˆKÛ üGSFÁGWGÁQFQ1P2šHÃGQ3’EQ$ŠHkGQ
C4šGÁ5PÿİP"P€Q#Š$š6P7PGQ$’$Š-ªKÁJİ8P@Q’BEÁäPÿİ*PP"P€Q#Š$šP+PQ#Š$š$š9PEQ$Š-ªKÁJİ:P@Q’BEÁä€QQEQ;šKÁJİEQâÿO€°B€}€1€\€pÀB€\,„FASL-RECORD-FILE-MACROS-EXPANDED\€B€8\€\€ê€DEFUN€†€'\€B€ÿ†€Æ9\€B€ı†€{Ä²\€B€ü†€2»=\€B€û†€z(‡\€B€Ã†€.Ù‹\€B€‚†€[æ„\€B€†€(Ì¢\€B€†€*ıj\€B€|†€=Ì#\€B€{†€-i\€B€y†€~Éz\€B€w†€<p‘\€B€u†€`sN\€B€s†€|Äô\€B€q†€{šÍ\€B€p†€xõ¿\€B€o†€Zió\€B€n†€:}n€€     (setf new-attrs (car (get-keyword-value '(add-attr) keyword-list)))
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
PPPPPP€QŠšÿÛPJ@½O€­B€–€€ƒREDEFINE-REL€ë€"a†€BHF€?À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€®\€	B€ŸB€cƒ€IMP€C€SSƒ€KEY€C‚TUPLE-FORMAT-LIST€B€’ƒ€DIR€CTUPLE-LISTB€:\€TEMP-RELSTATUS?€\€B€l\€pÀB€\lXR-BQ-LISTB€‚€Cƒ*PROVIDE-STATUS-MESSAGES*€‘ê€GENSYMÒB€LMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540793. :SYSTEM-TYPE :LOGICAL :VERSION 2. :TYPE "LISP" :NAME "PRINT" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :CHARACTERS T :NOT-BACKED-UP T :CREATION-DATE 2755785296. :AUTHOR "REL3" :LENGTH-IN-BYTES 13236. :LENGTH-IN-BLOCKS 13. :BYTE-SIZE 8.)

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
 (format *standLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540797. :SYSTEM-TYPE :LOGICAL :VERSION 2. :TYPE "XLD" :NAME "PRINT" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760360237. :AUTHOR "REL3" :LENGTH-IN-BYTES 3284. :LENGTH-IN-BLOCKS 7. :BYTE-SIZE 16.)  pp2€\€©COMPILE-DATA\€ì€SW-MFG,GODZILLA†€‡¤>ÏF€F€\€pÀ,COMPILER,VERSION€\€F€F€pÀB€),‚OPTIMIZE-SWITCH€†€©ƒQFASL-SOURCE-FILE-UNIQUE-ID€1€\€pÀl€FSì‚MAKE-FASLOAD-PATHNAME€\€ê€QUOTE€B€$\€B€8ª€NIL€\€B€8\€¬€RTMS\€B€8ì€PRINT€\€B€8¬€LISP\€B€8F€©€BASEF€
é€FONTS€\€©*CODE-FONT*€é*COMMENT-FONT*é*STRING-FONT*€)PACKAGE€©€RTMS©€MODE©COMMON-LISP€€ƒPRINT-TUPLE€€ë€C–†€A´F€SÀ$€ÀB€:pÀ¬€TICLì€ART-Q€]€F€€:B€:B€:j€T€F€pÀ¬€SYS€l‚DEBUG-INFO-STRUCT€B€P\€Ã€TUPLE€ƒTUPLE-FORMAT*PATHNAME‚OUTPUT-TO-WINDOWÃ€BLANKSê€STREAMB€:\€CATTR-LIST€ƒ€LEN€B€:B€:B€:Ã€%TUPLEB€:B€:ƒ€TUP€Ã€TUPFMTC€X€B€:B€:\€)‚MACROS-EXPANDED€\€pÀB€TlCOND-EVERYê‚WITH-OUTPUT-TO-STRING€ª€SETFª€PROGpÀ¬€ZLC€,DO-NAMEDpÀB€Tì‚INHIBIT-STYLE-WARNINGS€ƒPRINT-TUPLE*Òl€|€Àª€LISTÒjƒMAKE-STRING-OUTPUT-STREAM€Òê€PRIN1€Ò*ƒGET-OUTPUT-STREAM-STRINGÒê€STRINGÀê€SUBSEQÒl€@€ÀªCONCATENATE€Òê€APPENDÒê€PRINC€Òê€TERPRI’ƒæ€QQ‚Q„Q…Q¬BÑ€QDÁCÁvüCQDSEÁPŠ@ÁEQQGÁFÁ+üFSGSIÁHÁ‚JÁHQJQJQŠHÃŠCAÁ@QAQI#ä	PHQJIm
šPü	PHQ„QJIQAc
ššHQ’ŠPŠš@ÁFÅGÅFäGÒç@QFÁ'äFSJÁJ5äKÛLÛ‚äJS‚Q’LÁKİ…äJS…Q’LÁKİüLÛKÛ‚äJQ‚Q’KÁLİ…äJQ…Q’KÁLİFÅÙçLÛKÛ‚ä‚QŠKÁLİ…ä…QŠKÁLİ@QCCÃÁDÅDˆçBO€‡B€P€€B€z€ë€@Œ†€AhF€LÀ$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€z\€B€`B€aB€bB€dB€eB€:\€
B€hB€:B€iB€:B€:B€:B€jB€kB€:B€:\€B€n\€B€sB€qB€tB€wB€y€l€|€ÀB€…Òl€~SÀê€FORMATÒB€€ÀB€Òl€@€ÀB€ƒÒB€†’€QAÁ|äASBÁCÛDÛ‚äP‚Q’DÁCİ„äP„Q’DÁCİBQQCÁEÁRüESCSGÁFÁÿÛPFQšFÃŠC@ÃG#äPFQJGmš	P
šFÁHÛIÛ‚äFQ‚Q’IÁHİ„äFQ„Q’IÁHİüPFQƒQJGQ@cš
šFÁIÛHÛ‚äFQ‚Q’HÁIİ„äFQ„Q’HÁIİIÛHÛ‚äP‚Q’HÁIİ„äP„Q’HÁIİEÅCÅEäC«çIÛHÛ‚ä‚QŠHÁIİ„ä„QŠHÁIİAÅ„ç€O€—B€z€€‚PRINT-TUPLE-WIDE€ë€eá†€á€F€|À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€˜\€Ã€TUPLESC‚PROJECT-ATTRIBUTES‚NUMBER-PER-LINE€B€cB€bB€eB€:\€Ã€ITEMS€ATT-STS€Ã€SCREENB€:B€:B€:ƒ€ATTRB€`CLINE-ITEM€ƒLINE-LENGTH€ÃNUMBER-IN-LINEB€:B€:B€:ƒ€VAL€Ã€AT-ST€Ã€VAL-STDUM-VAL€B€:B€:B€l\€B€n\€B€rB€qB€tB€wB€ypÀB€T¬€SENDB€sÀ†€Ã€pÀl€TVìDEFAULT-SCREEN‘é€HEIGHTÀé€WIDTH€Àl€~SÀB€•ÒC‚PRINT-TUPLE-WIDE*€ÒB€†Òé€ITEM1€ÀCATTRIBUTE€ÀB€|Òl€: ÀB€}ÒB€~ÒB€Òl€  ÀpÀB€\,*APPEND€ÒB€…Òl€ €À*REVERSE€’PŠPŠyäUJünJBÁCÑQEÁDÁüDQESFÁÿÛPFQšŠCCDÃÁEÅEòçCQAÁƒ	æ€QQ‚Q„Q…QAQBQJ	¼€QDÁšäDSGÁHÛIßJßQGQAQMÁLÁKÁoüKSLSMSOÁNÁFÁPÛQÛÿÛPNQšŠCPÁ‚QJ+æJIaOaPaB#äRÛSÛ„ä„Q
ŠSÁRİ…ä…Q
ŠSÁRİJßIßHQ@]@ÁHÛHQPFQPšPN7ä‚TÁNQTQTQŠüNQQÃQQ’P¢’HÁSÛRÛ„äFQ„QP„QNQ„QP„Q’RÁSİ…äFQ…QP…QNQ…QP…Q’RÁSİJÉJIaOaPaIÁKÅLÅMÅKäLäMŒçHäHQ@]@ÁP@]@ÁSÛRÛ„ä„Q
ŠRÁSİ…ä…Q
ˆ…Q
ŠRÁSİDÅfç@QŒO€ÄB€˜€€B€»€ë€
=„†€A
øF€GÀ$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€»\€B€¡B€¢B€£B€bB€eB€¦B€§B€:\€B€:B€`B€©B€ªB€«B€:B€:B€:B€¨B€¬B€­B€®B€:B€:\€B€n\€B€qB€sB€tB€wB€y€l€~SÀB€•ÒB€†ÒB€~Òl€: ÀB€…Òl€  €€Q@Ávä@SAÁBÛCßDßQAQ…QGÁFÁEÁSüESFSGSJÁIÁHÁKÛÿÛPIQšŠCKÁ‚QD+æJCaJaKa†#äLÛMÛƒäƒQŠMÁLİ„ä„QŠMÁLİDßCßBÛMÛLÛƒäHQƒQPƒQIQƒQ	PƒQ’LÁMİ„äHQ„QP„QIQ„Q	P„Q’LÁMİDÉJCaJaKaCÁEÅFÅGÅEäFäG¨çMÛLÛƒäƒQŠLÁMİ„ä„Qˆ„QŠLÁMİ@ÅŠç€O€ÓB€»€€C‚PRINT-WIDE-FORMAT€€ë€*m†€BˆF€CÀ$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€Ô\€
RELATIONB€¢B€£B€cB€bCITEM-LIST€ÃLIST-OF-TUPLESCATTRIBUTESƒ€CARDB€eB€:\€CNEW-ITEMS€ƒ€TEMP\€B€n\€B€³B€s€‚*OUTPUT-WINDOW*€Ñƒ*ACTIVE-DB*€‘¬Relation:  €ÀB€¼ÀB€İÀB€|Òì  Database:  €ÀDATABASEÀ,‚  Cardinality:  Àé€ITEMS€Àé€INDEX€Àƒ€PUTPÒl€ €ÀB€†Ò¬…Relation: ~S  Database:  ~S  Cardinality: ~SÀB€•Òì…~%Relation: ~S  Database:  ~S  Cardinality: ~SÀB€ÃÒB€˜ÒB€„Òê€CLOSE€ÒiSET-ITEMS€€ƒäPP€QPš	PPP
PšPˆQ²C@Á€QPŠŠCAÁäAQüJP˜PŠ@]@Áü Qˆ QP€QPˆQ¨ Qˆ Qˆ„ä„Qˆ„QP€QPˆQ¨„Qˆ„Qˆ†ä…Q@QŠ†QQ‚QƒQ„Q Q²š…Á„ä„QˆƒäP…Q€O€õB€Ô€€C‚PRINTREL-INTERNAL*€ë€CG†€êCÀF€EÀ$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€ö\€B€İB€ßB€¢B€£ƒWIDE-FORMAT€B€eÃOUTPUT-TO-FILEB€aÃ€HEADERƒ€TAILB€áj&OPTIONAL€\€Ã€PRINT?B€Y\€ÃRETURN-TUPLES€B€:B€:\€B€bB€cROWLINE€B€gB€ŞƒQTRIEVE-VAR€ÃNEW-ITEM-LIST€B€dê€FIRST€ª€LASTB€:B€:B€:B€½B€:B€:B€¨ƒ€FORMB€jB€kB€hB€lC€Y€B€:\€B€n\€B€qB€³pÀB€Tl‚CONDITION-BIND-IF€pÀB€TìCONDITION-BINDpÀB€Tì‚CATCH-CONTINUATION-IF€pÀB€Tl‚CATCH-CONTINUATIONpÀB€Tì€ERRSETB€tB€wB€yB€sÀ†€1˜f€pÀl€EH¬‚*CONDITION-HANDLERS*ÑB€èÑj‚*STANDARD-OUTPUT*€Ñƒ*PROVIDE-ERROR-MESSAGES*ÑB€ç‘êSTRING-UPCASE€Ò*‚READ-FROM-STRINGÒpÀ,€ì€G5649€ÀF€´Àê€ERROR€ÀpÀB€\ìERRSET-HANDLERÀiEXPOSED-P€ÀB€|ÒpÀB€$ì€G5657€ÀF€ØÀ\€ª€OPENÀ\€iDIRECTION€é€OUTPUTÀB€„Òª€EVALÒpÀB€$ì€G5665€ÀF€îÀB€2ÀB€3ÀB€0ÒB€†Ò,ERROR - ÀªWRITE-STRINGÒB€~Ò,‚ is a bad file.€ÀB€íÀpÀB€TlLISTARRAY€Ò‚SCROLL-TO-BOTTOMÒ¬€~%~sÀB€•ÒB€óÒB€ÔÒl€~SÀj€+€ÒpÀB€\l‚SIMPLE-MAKE-ARRAY€Òl€ €À¬Relation :  ÀB€¼ÀB€İÀ,‚    Database :  ÀB€ëÀ¬‚    Cardinality :  €ÀB€îÀB€ïÒì†Relation :  ~S    Database :  ~S    Cardinality :  ~S€Àì†~%Relation :  ~S    Database :  ~S   Cardinality :  ~SÀl€|€Àl€~sÀB€€ÀB€Òl€@€ÀB€ƒÒB€½ÀB€…ÒB€ÃÒB€ÁÒB€PÒB€ôÀB€¥€væ‹İ€7æJÑ‚QLÁKÁ	üKQLSMÃ	Š
ŠCKÃÁLÅLõçJQ‚ÁPPTPPPÿÛJCJÃPJCNÃÖPŠŠJ!BJ!B\BAÁ‹æAÛ†@ä†5äPPTPPPÿİJCKÃPJCOÃÖP†QPšŠŠJ!BJ!BüPPTPPPÿÛJCLÃPJCJÃÖ†Q@ÃPPšäı\B@Áæ
ä€Pˆ†Qˆ Pˆü†Q@ÁAä!PŠ"ŠDÁAä#€…æAæ‹äP…ÁŒä…ä…Q$PQ%˜@ä@Q$PQ%˜@Q&ˆS„äƒæLƒÁ€Q	Š‚QƒQAQ@QDQQÿÛŠQ…Q
J'¼OÛOÑ‚Q‡QLÁKÁJÁüJQKSLSQÁPÁQäQQüÿÛ(PPQ%šŠCCJÃÁKÅLÅKäLéçOQ‡Á‚QŠCÿk‡Q)PEÿa	JÿÛÿÛ-J*ªBÁ‚QŠCÿk‡Q)PEÿa	JÿÛÿÛ J*ªGÁˆãäA$ä+PCFÁ,P-P€Q	Š.Pš/P-PP0Pš1PŠQ²F]FÁBQF]FÁ€Q!PŠŠCEÁäEQüJ2P3˜JEaHÁ…ä…Qˆ…Q4P€Q	ŠPŠQ%¨…Qˆ…QBQ%…Qˆ@ä@Q5P€Q	ŠPŠQ%¨@Qˆ@QBQ%@Qˆ6PŠCÁOÛOÑ‚Q‡QLÁKÁJÁ/üJQKSLSSÁRÁTÛÿÛ7PRQ%šRÃŠCTÁCQ-PTQS#ä8PRQJSm9š:Pü8PRQGQJSQTc9š;šRQ’<PšŠ6PŠšCÃCJÃÁKÅLÅKäLÍçAäCQF]FÁOÛOÑCQLÁKÁ9üKQLSUÁVÛU5äJÛWÛ@äUWVÃÿ5äVSüVQ@Q=’WÁJİ…äUWVÃÿ5äVSüVQ…Q=’WÁJİWQüWÛJÛ@äUQ@Q=’JÁWİ…äUQ…Q=’JÁWİJQCKÃÁLÅLÅç@ä@Qˆ@QBQ%@QˆAäBQF]FÁ…ä…Qˆ…QBQ%…QˆDQFQ>Š?’DÁ
äDQQ‡Q@QAQGQ…Q@²?’DÁDQŠCÿmIÁ@ä@QBQ%@Q&ˆ…ä…QBQ% äAäDQBQŠ?’DÁA	äAPDQ€QHQIQ’BP3˜€O€LB€ö€1€\€pÀB€\,„FASL-RECORD-FILE-MACROS-EXPANDED\€B€8\€\€ê€DEFUN€†€'\€B€†€-i\€B€†€~Éz\€B€†€<p‘\€B€†€`sN\€B€†€|Äô\€B€³†€aM*\€B€y†€(Ì¢\€B€w†€*ıj\€B€t†€=Ì#\€B€s†€[æ„\€B€r†€5%“\€B€q†€Æ9€€))
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
  DIRECTORY            - New directory in which this relation is to be saved.  (MODIFY-RELATION RELATION &REST KEYWORD-LIST &KEY &OPTIONAL RELATION-NAME ADD-ATTRIBUTES DELETE-ATTRIBUTES RENAME-ATTRIBUTES IMPLEMENTATION-TYPE STORAGE-STRUCTURE FORMAT KEY DOCUMENTATION DIRECTORY &ALLOW-OTHER-KEYS)€€B€›‘B€ÜÀl€~SÀB€	ÀB€ ÀÃADD-ATTRIBUTESÀC‚DELETE-ATTRIBUTES€ÀC‚RENAME-ATTRIBUTES€ÀB€£ÀB€¬ÀB€ßÀB€¢ÀB€¡ÀB€ ÀB€­ÒB€ßÒB€	’PAÁPÿÛPPQP‚QPƒQ	P„Q
P…QP†QP‡QPˆQP QP‹QPŠQJº@ÃššA‘Q@Q”O€¤	B€
	€1€\€B€â\€B€å\€B€8\€B€LMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540804. :SYSTEM-TYPE :LOGICAL :VERSION 2. :TYPE "XLD" :NAME "RELATION-OPS" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760360567. :AUTHOR "REL3" :LENGTH-IN-BYTES 12012. :LENGTH-IN-BLOCKS 24. :BYTE-SIZE 16.)                               pp2€\€©COMPILE-DATA\€ì€SW-MFG,GODZILLA†€‡¤‡ĞF€F€\€pÀ,COMPILER,VERSION€\€F€F€pÀB€),‚OPTIMIZE-SWITCH€†€©ƒQFASL-SOURCE-FILE-UNIQUE-ID€1€\€pÀl€FSì‚MAKE-FASLOAD-PATHNAME€\€ê€QUOTE€B€$\€B€8ª€NIL€\€B€8\€¬€RTMS\€B€8¬RELATION-OPS\€B€8¬€LISP\€B€8F€©€BASEF€
é€FONTS€\€©*CODE-FONT*€é*COMMENT-FONT*é*STRING-FONT*€)PACKAGE€©€RTMS©€MODE©COMMON-LISP€€ÃJOIN-INTERNAL€€ë€QS†€àQ@F€ÒÀ$€ÀB€:pÀ¬€TICLì€ART-Q€]€F€€:B€:B€:j€T€F€pÀ¬€SYS€l‚DEBUG-INFO-STRUCT€B€P\€ƒKEYWORD-LISTB€:\€!ƒ‚RELA-ATTRIBUTES-USERƒ‚RELB-ATTRIBUTES-USERƒUNKNOWN-ATTRIBUTES-USER€ê€PRINT€CALL-ATTRS€ƒFROM-CLAUSE€ÃJRELB-PROJECT€ƒJRELA-FORMATƒJRELB-FORMATƒJRELC-FORMATƒ€TEMPCJOIN-ATTRC‚JOIN-INSERT-LISTÃ€WHERE€Ã€ATTRSAÃ€ATTRSBƒA-JOIN-ATTRCÃREADER-PACKAGEÃ€JRELA€Ã€JRELB€Ã€JRELC€ƒ€IMPAƒ€SSA€ATTR-IMPCƒJRELB-IMPLEMENTATION-TYPE€ƒJRELB-STORAGE-STRUCTURE€CJRELB-KEY€B€`B€:B€:CATTRIBUTE€B€:B€:\€)‚MACROS-EXPANDED€\€pÀB€Tì‚INHIBIT-STYLE-WARNINGSpÀ¬€ZLC€,DO-NAMEDê€FOURTHê€FIFTH€ê€SIXTH€ê€THIRD€ê€SECONDê€FIRST€pÀB€TlCOND-EVERYª€PROGª€SETFÀ†€¡ €ƒ‚*DEFAULT-ANYP-WIDTH*Ñƒ*PKG-STRING*ÑCƒ*PROVIDE-WARNING-MESSAGES*Ñƒ*PROVIDE-ERROR-MESSAGES*‘‚ACTIVE-DATABASE€Òê€TERPRIÒ¬„ERROR - No parameters passed to JOINÀªWRITE-STRINGÒB€eÀ\€ƒ€INTOB€oƒ€FROMƒ€IMP€ƒ€STO€ƒ€KEY€ê€FORMATƒ€DIR€ƒ€DOC€B€eÃ€TUPLESPROJECT€Ã€UNIQUEÀƒGET-KEYWORD-VALUE-PREREQÒ\€B€šÀC‚GET-KEYWORD-VALUE€Ò\€B€oÀ\€B€£À\€B€¢À\€B€›Àª€LISTÒƒVALIDATE-SYMÒì‹WARNING - More than two relations are provided for joining. The first two will be considered.€À¬†ERROR - The relations to be joined are not provided.Àl€*€ÀADD-DOT€ÒªPACKAGE-NAMEÒ†€ÀC‚PARSE-FROM-CLAUSE€Òì€GLOBALÀpÀB€\ìSTRING-EQUAL*€ÒÃ€GLOBALÀìˆERROR - The FROM clause has not specified the relations to be joined.€À\€lATTRIBUTES¬‚IMPLEMENTATION-TYPE€l‚STORAGE-STRUCTURE€¬€KEY€¬TUPLE-FORMAT¬CARDINALITY€ÀƒGET-RELATIONÒl‚ERROR - Relation €Àê€PRIN1€Ò,‚ does not exist€ÀêSTRING-UPCASE€ÒªFIND-SYMBOL€ÒC‚CONVERT-ATTRIBUTESÒªSTRING-EQUALÀpÀB€\¬MEMBER-TEST€Ò,ERROR - Àl„ is not an attribute of relation €À,… is not an attribute of either relation€À¬„ is an attribute of both relations: Àì€ and €Àe€.ÀjWRITE-CHARÒ¬†        It is unclear which attribute should be usedÀF€ĞÀÃ‚PARSE-JOIN-ATTRIBUTES€ÒpÀB€\,*APPEND€Ò*REVERSE€Òê€STRINGÀl€.€ÀªCONCATENATE€ÒƒPROJECT-LISTÒF€ÀCJOIN-INTO€ÒCJOIN-EVAL€Ò‚REMOVE-DOT-ATTR€Ò\€B€¤ÀÃUNIQUE-TUPLES€Ò,INSERT-€Àl€-€À\€B€œÀ\€B€ÀpÀB€TìSTRING-APPEND€Ò‚SYSTEM-RELATION€ÀÃRELATION-NAME€ÀB€ØÒ\€lMODIFIEDP€¬CARDINALITY€À‚DELETE-OR-MODIFYÒ,‚SYSTEM-RELATION€À\€lMODIFIEDP€À\€B€ŸÀC‚PRINTREL-INTERNAL*Òƒ€JOIN€€æR€Qü[S[Áä[Sÿ5úç[Q€Áä€æä	€
PˆRRÛKÛPÛSÛP€Q‹CCÁäCWCÁP€Q’€ÁP€Q’BTÁP€Q’BâÿİMÁP€Q’BEÁP€Q’æTæCæP€æCİP€Q’BJÁäJ5æJQŠJÁJäJSŠRÁæR]Û\İJäJWŠSÁæR]Û\İJæRÛ]Û\İJæSÛ]Û\İJQ
ää	€Pˆ]ÛüÿÛ]Á\İR
æSæEæä	€PˆRSæRQSÁEæRQP’SQP’’EÁRäRQüSäSQCŠQÁEQRQSQKQPQBQ@QAQPPAAÁ@ÁBÁPÁKÁSÁRÁEÁQæRòRQCŠüSòSQùıPQÁQQPæQQ &äPQÁRæS
æä	€!PˆRSæRQSÁRQ"PÿÛ#šNÁN
æä	€$PˆRQ%ˆ&PˆRSQ"PÿÛ#šOÁO
æä	€$PˆSQ%ˆ&PˆRNWBBOWBByäNQJÁOQNÁJQOÁ@QJÁAQ@ÁJQAÁNSRÁNWNÁNWUÁN[VÁNUBGÁNSNÁOSSÁOWOÁOWXÁO[YÁOQBZÁOUBHÁOSOÁR	òSğRòSQ'Š(ŠSÁ
üSğSQ'Š(ŠSÁRQ'Š(ŠRÁ@Q)Š@ÁAQ)ŠAÁBQ)ŠBÁ@Q\Áä\S^ÃNQ*P+˜æ	ä	€,Pˆ^Q%ˆ-PˆRQ%ˆR\ÅìçAQ\Áä\S^ÃOQ*P+˜æ	ä	€,Pˆ^Q%ˆ-PˆSQ%ˆR\ÅìçBQ\Á6ä\S^ÃNQ*P+˜æ^QOQ*P+˜
æä	€,Pˆ^Q%ˆ.PˆR^QNQ*P+˜ä^QOQ*P+˜ää	€,Pˆ^Q%ˆ/PˆRQ%ˆ0PˆSQ%ˆ1P2ˆ	€3PˆR\ÅÊçKQPQFQNQOQRQSQ4P5PAFÁPÁKÁ]Û]ÑNQ`Á_Áü_QRQ`S’C_ÃÁ`Å`öç]Q`Û`ÑFQ]Á\Áü\QSQ]S’C\ÃÁ]Å]öç`Q6’DÁJÛKQ7ŠKÃ^Áä^S^U*P+˜	äJQ8PSQ'Š9P^S'Š:¢üJQ^S'ŠŠ6’JÁ^ÅêçJQ7ŠKÁGQHQ6’ŠDQPQ;šBIÁTäWÛTQKQPQ€QIQUQVQWQ<P=PAWÁTÁæRRQSQNQOQFQZQMQYQXQQQ
J>ºLÁRQ'ŠSQ'Š*äDÛPQ^Áä^S^U*P+˜äDQ^S?ŠüDQ^SŠ6’DÁ^ÅğçDQPÁNQ`Û`ÑFQ]Á\Áü\QSQ]S’C\ÃÁ]Å]öç`Q6’DÁLQDQPQ;šLÁ@P€Q’äLäLQAŠLÁTMäLKäBPWä8PWQB'ŠCPWUBü8PDP€Q’BâUQ'ŠCPEP€Q’BâVQ'Š:¢F’P(’`ÁTQKQLQWäWWBüKQTQ`©GPÿİ*PHPTQIŠ'ŠšJPÿİWäWWBBüJLQŠCÿa’K¨GPÿİ*PHPLPšMPÿİŠK¨P€Q’äLCDäTäTQLQKQÿÛÿÛÿÛÿÛWäWWBBüNP€Q’BâIQÿİÿİLQŠCJO¸TC&äPPLQKQÿÛÿÛÿÛÿÛNP€Q’BŠCKQŠCxäNP€Q’BPÿÛKQŠCC6’üNP€Q’BÿİÿİLQŠCJO¸STQâRÿO€ôB€P€€B€ó€ë€†€`8F€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€ó\€ê€&REST€B€`ª€&KEYB€›B€şj&OPTIONAL€B€£B€oB€šjDIRECTORY€êDOCUMENTATION€B€Ÿƒ‚IMPLEMENTATION-TYPE€B€C‚STORAGE-STRUCTURE€B€eB€¢B€¤j‚&ALLOW-OTHER-KEYS€B€:\€B€`B€›B€£B€oB€šB€B€B€ŸB€B€B€B€eB€¢B€¤\€éDOCUMENTATION€ì¿ØThis function provides the capability to combine two relations into a new relation
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
   UNIQUE               - If T, only unique tuples will be part of the resultant relation.€€\€©€FROM)PROJECT€é€WHERE€©€INTOiDIRECTORY€B€é€FORMAT©‚IMPLEMENTATION-TYPE€©€KEY€i‚STORAGE-STRUCTURE€é€PRINT€é€TUPLESé€UNIQUEÀpÀB€\ìSTORE-KEYARGS€ÒB€P’@ä@QPÿİAÑ @QŒO€B€ó€€ÃPROCESS-WHERE€€ë€U¼†€AœF€gÀ$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€\€Ã€TUPLEAB€pCWHERE-EXP€B€tB€uB€qB€:\€CNEW-WHERE€B€lB€:B€:B€:B€:C€X€\€B€\€pÀB€\lXR-BQ-LISTB€B€B€…B€‚€j€ORÀB€ÒB€¬ÒB€8ÀB€ÅÒB€ĞÀpÀB€\lPOSITION*€ÒB€ßÒB€ÈÀB€ÊÒÃREMOVE-DOT-RELÒB€·Òê€EQUAL€ÀpÀB€\lMEMBER-EQLÒB€Ö’‚5-äBÑ‚UDÁCÁüCQDSÿ5>BCCÃÁDÅDöçBQPDä‚SDÛDÑ‚UBÁEÁüEQ€QQBSƒQ„Q…Q²CEÃÁBÅBòçDQ
Cÿ‚5æ€QQ‚QŠƒQ„Q…Q²B@Á@‚S&ä‚Q@Á@ƒQŠƒÁ„QŠ„Á‚QDÁ_äDSFÁğF7&äPFQŠ	!äFQ
ŠŠQP˜äFQŠŠƒQäPFQ
ŠŠQP	š€QŒC’AÃ@QäFQŠŠ„+*æüFQŠQP˜äPFQŠQP	š€QŒC’AÁ@QAQüFğF7äPFQŠ	äFQ
ŠŠ…QP˜
äFQŠŠ„Qä@QFQ
Šü@QFQŠ’@ÁDÅ¡ç@O€2B€€€ƒ‚PROCESS-SET-RELATION€ë€/q†€@œF€BÀ$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€3\€B€éB€ÛB€:\€CATTRIBUTESDOMAINS€B€:ƒ€ATT€B€:B€:ƒ€ATTR\€B€\€B€‚B€B€…B€ŠB€€ƒ*ACTIVE-DB*€ÑB€”‘\€lATTRIBUTES,DOMAINS€ÀB€ÁÒB€–Ò¬ERROR - The ÀB€˜ÒB€ÃÒl… relation is not defined in the database €ÀB€¬ÒB€ÈÀB€ÊÒ,ERROR - Àì„ is not an attribute in the relation €ÀB€ĞÀB€Ñ’€QPÿÛš@Á@æ	ä€P	ˆ€Q
ˆP	ˆP
ˆ€ÛEü@S€Á@QBAÁ@Q	B@Á;ä5æQŠÁQBÁäBSCÃ@QP˜æä€P	ˆCQ
ˆP	ˆ€Q
ˆPˆ€ÛBÅêçBÛBÑQEÁDÁüDQESFÁ@QŠCFQ@QPšŠCÿcAQŒCCDÃÁEÅEíçBQAÁQ@Á€Q@QAQƒO€KB€3€€C‚SET-COMPATIBILITY€€ë€0p†€A0F€@À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€L\€ƒ€RELAƒ€RELBB€pB€qB€:\€ƒATTRIBUTESA€ƒATTRIBUTESB€DOMAINSADOMAINSBB€:B€:B€:B€:Ã€ATTRA€Ã€ATTRB€ƒ€DOMAƒ€DOMB\€B€\€B€B€…€B€”‘F€ĞÀB€3ÒB€–Òl‚ERROR - Relations ÀB€˜ÒB€ÃÒì€ and €Àl  do not have the same number of attributes, thus they are not compatible.€Àl‚ERROR - Attribute Àì of relation €À,‚ and attribute €À¬ƒ are not compatible domains€€€Q‚QPPABÁ@Á€ÁæRQƒQPPACÁAÁÁæR@QŠCAQŠC|æä€Pˆ€Q	ˆ
PˆQ	ˆPˆR@QAQBQCQGÁFÁEÁDÁ&üDSESFSGSKÁJÁIÁHÁJQK+æä€PˆHQ	ˆPˆ€Q	ˆPˆIQ	ˆPˆQ	ˆPˆRDÅEÅFÅGÅDäEäFäGÓçSO€jB€L€€‚SET-CREATE-RELC€€ë€B¢†€á@F€`À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€k\€B€Uƒ€RELCB€`B€pB€¡B€:\€ƒ‚ATTRIBUTE-DESCRIPTORB€=B€ ƒ€DOM€B€œB€ƒ€MODPƒQTRIEVE-VAR€C€SSƒTUPLE-FORMATB€:B€:B€:B€@B€wB€=\€B€\€	B€B€…B€‚B€‡B€†B€ B€ŠB€‹B€À†€¢€Ã‚*SYSTEM-RELATION-KEY*€Ñƒƒ*SYSTEM-RELATION-ATTRIBUTES*‘B€èÀ\€	lMODIFIEDP€ìSAVE-DIRECTORYlATTRIBUTES¬‚IMPLEMENTATION-TYPE€l‚STORAGE-STRUCTURE€¬€KEY€¬TUPLE-FORMAT¬€DOC€,DOMAINS€ÀB€ÈÀB€éÀB€ÅÒB€¬ÒQTRIEVE€Ò\€B€ ÀB€§Ò\€B€œÀ\€B€À\€B€¡À\€B€À\€B€ŸÀB€ÊÒB€wÀB€ÖÒB€œÀB€ÀB€ÀB€ŸÀB€¡ÀB€ ÀÃ€DEFREL’PPPPP	P€Q
ŠšªBGÁGSFÁP‚Q’BâGWBÁG[AÁP‚Q’BâGQBDÁP‚Q’BâGUBHÁP‚Q’Bâ„Q„ÁP‚Q’BEÁP‚Q’BâJGQŒCIÁJGQŒCCÁƒ!äƒ5æƒQŠƒÁJÑƒQLÁKÁüKQLSMÁAQŠCMQAQPšŠCÿcCQŒCCKÃÁLÅLíçJQCÁƒQAÁCQNÁAQOÁä@QOSPNS’’’@ÁNÅOÅõçQ@QPDQPHQPEQPIQP„QPBQJºœO€“B€k€€ƒ‚RELATION-DIFFERENCE€€ë€#\Û†€à#F€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€”\€B€ıB€`B€şB€ÿB€›B€šB€B€B€ŸB€B€B€B€eB€¢B€¤B€B€:\€B€`B€›B€šB€B€B€ŸB€B€B€B€eB€¢B€¤Ã€TEMPA€ƒTEMP-TUPLES€B€lÃ€TABLE€B€UB€VB€tÃ€PRINT1TUPLES1€B€pÃ€WHEREAB€qÃ€WHEREBÃ€ATTRSCB€sB€:Ã€TUPLE€B€:B€"\€B€\€B€B€…B€B€ì¿#Difference of the tuples in two relations.

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
   UNIQUE               - If T, only unique tuples will be part of the resultant relation.À†€€ƒ‚*VALIDITY-CHECKING*€‘\€B€
B€B€B€B€B€B€B€B€B€B€ÀB€Ò‚ACTIVE-DATABASEPÒlDIFFERENCEÀ†€ĞÀÃ‚VALIDATE-RELATION-OPS€Ò©€TESTÀB€/À*‚MAKE-HASH-TABLE€ÒB€ÅÒB€ÆÒB€£ÀB€¢ÀB€¤À\€B€¤ÀB€§ÒB€oÀRETRIEVEÒ*GETHASH€ÒpÀB€T,PUTHASH€ÒB€=ÀÃ€INSERTÒ*CLRHASH€Ò\€lATTRIBUTES¬TUPLE-FORMATÀB€ÁÒƒ‚UNCONVERT-ATTRIBUTESÒB€òÒCDIFFERENCEÀ\€B€ŸÀB€Û’@ä@QPÿİAÑ €æR@QP	P
PASÁZÁTÁYÁRÁXÁWÁQÁVÁUÁPÁæRPP’OÁQQŠŠPWQPÿİPP@Q’BPXQ	JºNÃ[Áä[S\Ã\Q\QOQ’
COQ˜[ÅõçPQŠŠPÿİPUQPP@Q’BPVQ	Jº]Á
ä]S^ÃOQæ^QM]MÁ]ÅöçMäRäPLÁÚRQPMQPYQ¨LQÀOQˆTäMSCäRäRQPÿÛšBLÁRQMQYQâLSZQ’ÿÛÿÛÿÛÿÛLWÿİÿİMQŠCJ¸RS&äPQPÿÛšBLÁ PMQUQâLSZQ’ÿÛÿÛÿÛÿÛ!P@Q’B	âUäLULSUQ"šBüLWÿİÿİMQŠCJ¸SRQâPÿO€ÀB€”€€Ã‚RELATION-INTERSECTION€€ë€#YÕ†€à#F€|À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€Á\€B€ıB€`B€şB€ÿB€›B€šB€B€B€ŸB€B€B€B€eB€¢B€¤B€B€:\€B€`B€›B€šB€B€B€ŸB€B€B€B€eB€¢B€¤B€Ã€TEMPB€B€lB€ B€UB€VB€tB€¢B€¡B€pB€£B€qB€¤B€¥B€sB€:B€¦B€:Ã€TUPLEB\€B€\€B€B€…B€B€ì¿#Intersection of tuples in two relations.

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
   UNIQUE               - If T, only unique tuples will be part of the resultant relation.À†€€B€«‘\€B€
B€B€B€B€B€B€B€B€B€B€ÀB€ÒB€­ÒlDIFFERENCEÀ†€ĞÀB€°ÒB€¢ÀB€£ÀB€oÀB€´ÒB€±ÀB€/ÀB€²ÒB€µÒB€·ÒB€ÅÒB€ÆÒB€¤À\€B€¤ÀB€§ÒB€=ÀB€¸ÒB€¹Ò\€lATTRIBUTES¬TUPLE-FORMATÀB€ÁÒB€½ÒB€òÒªINTERSECTIONÀ\€B€ŸÀB€Û’@ä@QPÿİAÑ €æR@QP	P
PATÁZÁSÁYÁRÁXÁWÁQÁVÁUÁPÁæRPQPÿİPUQPVQJºNÁPP’OÁNQ[Áä[S\Ã\Q\QOQ’
COQ˜[ÅõçQQŠŠPÿİPP@Q’BPXQPWQ	Jº]Á
ä]S^ÃOQä^QM]MÁ]ÅöçMäRäPLÁÚRQPMQPYQ¨LQÀOQˆSäMTCäRäRQPÿÛšBLÁRQMQYQâLSZQ’ÿÛÿÛÿÛÿÛLWÿİÿİMQŠCJ¸RT&äPQPÿÛšBLÁ PMQUQâLSZQ’ÿÛÿÛÿÛÿÛ!P@Q’B	âUäLULSUQ"šBüLWÿİÿİMQŠCJ¸SRQâPÿO€ÚB€Á€€ÃRELATION-UNION€ë€$ZØ†€à$F€~À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€Û\€B€ıB€`B€şB€ÿB€›B€šB€B€B€ŸB€B€B€B€eB€¢B€¤B€B€:\€B€`B€›B€šB€B€B€ŸB€B€B€B€eB€¢B€¤B€B€UB€VB€tB€ B€¢B€¡B€lCTEMP-UNIONB€pB€£B€qB€¤B€¥B€sB€:B€¦B€:\€B€\€B€B€…B€B€ì¿Union of tuples in two relations.

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
   UNIQUE               - If T, only unique tuples will be part of the resultant relation.À†€€B€«‘\€B€
B€B€B€B€B€B€B€B€B€B€ÀB€ÒB€­ÒlDIFFERENCEÀ†€ĞÀB€°ÒB€±ÀB€/ÀB€²ÒB€ÅÒB€ÆÒB€¤À\€B€¤ÀB€§ÒB€£ÀB€oÀB€¢ÀB€´ÒB€µÒB€·ÒB€=ÀB€¸ÒB€¹ÒB€ÖÒ\€lATTRIBUTES¬TUPLE-FORMATÀB€ÁÒê€UNION€ÀB€½Ò\€B€ŸÀB€ÛÒB€ò’@ä@QPÿİAÑ €æR@QP	P
PARÁZÁQÁYÁOÁXÁWÁNÁVÁUÁMÁæRPP’PÁMQŠŠPP@Q’BPUQPVQPÿİ	JºSÃ[Áä[S\Ã\Q\QPQ’
CPQ˜[ÅõçOäOQPSQPYQ¨TÛNQŠŠPÿİPP@Q’BPWQPXQ	Jº]Á
ä]S\ÃPQæ\QT]TÁ]ÅöçTäOäPLÁÚOQPTQPYQ¨LQÀPQˆQäSQTQ”R5äOäOQŠŠPYQ˜OR*äMQPÿÛšBLÁPSQTQ’UQâLSZQ ’ÿÛÿÛÿÛÿÛ!P@Q’B	âUäLULSUQ"šBüLWÿİÿİSQTQ’ŠCJ#¸SOQâMÿO€óB€Û€€ÃVALIDATE-WHERE€ë€_Ğ†€AHF€qÀ$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€ô\€ƒWHERE-CLAUSECRELA-NAME€CRELB-NAME€Ã€ATTSA€Ã€ATTSB€B€:\€ÃTEST-ATTRIBUTEÃTEST-RELATION€\€B€\€B€B€ B€ŠB€‹€B€CÑB€”‘B€–Ò,ERROR - ÀB€˜ÒB€ÃÒl„ improperly formed where subclauseÀB€.ÒB€ßÒB€ÈÀB€ÊÒìƒ is an unrecognized attribute€À,ERROR -€Àlƒ is not a relation in the Àl database€€€æS€Q±äS€QJ™	æ€Sÿ5æ€Wÿ5æ€[ÿ5
ää€Pˆ€Qˆ	PˆR€W
ŠAÁ€WŠ@ÁAæ@QƒQP˜Aæ@Q„QP˜<æä€Pˆ@QˆPˆRQA+ä@QƒQP˜*æä€Pˆ€WˆPˆR‚QA+ä@Q„QP˜æä€Pˆ€WˆPˆRä€PˆAQˆPˆPˆPˆR€[
ŠAÁ€[Š@ÁAæ@QƒQP˜æ@Q„QP˜	æä€Pˆ@QˆPˆRQA+ä@QƒQP˜
æä€Pˆ€[ˆPˆRS‚QA+ä@Q„QP˜
æä€Pˆ€[ˆPˆRSä€Pˆ€[
ŠˆPˆPˆPˆRO€B€ô€€‚PRE-RELATION-OPS€ë€9†€@pF€FÀ$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€\€B€`B€:\€B€lB€UB€VB€tB€¢B€pB€£B€qB€¤B€¥ƒ€VAR€B€s\€B€\€B€ B€ŠB€‹B€€\€B€›ÀB€§ÒB€¬ÒB€­ÒB€²Ò\€B€oÀ\€B€£ÀB€ÇÒ\€B€šÀ\€B€¢€P€Q’B@Áä@5æ@QŠ@Á@SŠAÁäAQCŠKÁ@WJÃÿ5äJQJ™äPJQ’BFÁ	PJQ’B
ŠEÁAQ@Y
C@Á@WŠBÁäBQCŠKÁ@[JÃÿ5äJQJ™äPJQ’BHÁ	PJQ’B
ŠGÁP€Q’BCÁP€Q’BDÁC5äCWJÃÿ5
äJQJ™ä	PJQ’B
ŠIÁC5äCSüCQŠCÁAQEQFQBQGQHQCQIQDQKQŠO€ B€€€B€´€ë€XÀ†€BF€hÀ$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€´\€B€gB€tB€uB€mB€rB€dB€bB€cB€:\€ƒPERIOD-INDEXÃ‚MYSTERY-RELATION-NAME€B€:B€@Ã€TEMP1€\€B€\€B€B€B€…€B€¬ÒB€ÖÒB€ĞÀpÀB€ƒl‚STRING-SEARCH-CHARÒB€.ÒB€ÅÒB€·ÒB€­ÒB€ØÒê€SUBSEQÒl€*€À*‚READ-FROM-STRINGÒB€ß’€QBÁ¤äBSCÁDÛC5.äƒQCäCSŠüCQ’ƒÁ„QCWŠ’„ÁPCWJÿÛÿİª@ÁQäCWŠDÁä‚JæQŠDQŠ	DæDQ
Š‚Á@üCWÿİ
’ŠŠÁ9üPCQJÿÛÿİª@Á"äCQŠDÁä‚æQŠDQŠ	
æDQ
Š‚ÁüCQÿİ
’ŠŠÁƒQCQŠ’ƒÁ„QCQŠ’„ÁüƒQCQŠ’ƒÁ„QCQŠ’„Á…QCQŠ’…ÁC5äCWCÁ@+äCQŠ@kJ@ašP	 æCQŠJ@QšŠAÃQ	ä†QCQŠŠ’†ÁüAQ‚Q	ä‡QCQŠŠ’‡Áü…QCQŠü@æ…QCQŠ’…ÁBÅ\ç€QQ‚QƒQ„Q…Q†Q‡QˆO€4B€´€€B€Ô€ë€oğ†€AèF€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€Ô\€B€mB€rB€hB€pB€qB€tB€uB€:\€
B€lB€:B€@B€:B€:B€:B€:Ã€ATTR1€B€:Ã€ATTR2€\€B€\€B€‚B€B€…B€€B€ÅÒl€*€ÀB€±ÒB€·ÒB€ĞÀB€0ÒB€.ÒB€ßÒB€¬ÒB€ÖÒB€ÇÒB€ØÒB€ÈÀB€-ÒB€Ê’€Q@Á€Û@QAÁCäASBÁğB7äBQŠ…QP’Šä€QƒQ/üBğB7äBQŠ†QP’Šä€Q„Q üPBQJÿÛÿİ¨ä…QBQ	ŠæPBQJÿÛÿİ¨
ä†QBQ	Šä€QBQ
ŠŠü€QBQŠ’€ÁAÅ½çQŠ@ÁÛ‚Û…QŠ†QŠä@Q…QP’†QP’’ŠräƒQ‚Á@QCÁväCSBÃ…QP’ŠräQDÛDÑƒQFÁEÁüEQFSGÁ…QGQ’ŠCEÃÁFÅFóçDQTüBQ†QP’ŠräQFÛFÑ„QDÁHÁüHQDSIÁ†QIQ’ŠCHÃÁDÅDóçFQ’Á„Q‚Á5üPBQŠP˜äBQ
ŠŠ„QP˜ä‚QBQ
ŠŠŠ’‚ÁQBQüBQŠƒQP˜
ä…QBQ’QP˜æQ…Qü‚QBQŠŠ’‚ÁQ†QBQ’ŠŠ’ÁCÅŠç€QQ‚QƒO€CB€Ô€€B€İ€ë€$8”†€B$F€\À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€İ\€B€vB€mB€rB€`B€kB€wB€xB€yB€:\€€lB€:B€:Ã€A-ATTRB€@ƒ€ATTD\€B€\€€ B€ŠB€‹B€B€…B€€Ã‚*SYSTEM-ATTRIBUTE-KEY*ÑÃƒ*SYSTEM-ATTRIBUTE-ATTRIBUTES*€‘\€lATTRIBUTES¬‚IMPLEMENTATION-TYPE€l‚STORAGE-STRUCTURE€¬€KEY€¬CARDINALITY€¬TUPLE-FORMATÀB€ÁÒ‚SYSTEM-ATTRIBUTEÀ\€,‚DOMAIN-FUNCTION€ìDEFAULT-VALUE€¬€DOC€Àª€AND€ÀB€ÈÀB€éÀB€.ÒB€ØÒB€¬ÒÃATTRIBUTE-NAMEÀB€ßÒB€‹ÒB€wÀpÀB€T¬€DEF€ÀB€¡ÀB€ÖÒB€œÀ\€B€œÀB€§ÒB€À\€B€ÀB€À\€B€ÀB€ŸÀ\€B€ŸÀB€ À\€B€ À\€B€¡À¬€...€ÀB€’’€QPÿÛš‡Á‡eæQ@ÁÛ‚Q@QBÁAÁ)üASBSDÁCÁEÛPPPP	P
PPCQŠŠš
PPCQŠŠššªBEÁQDQPESPEWPE[²’’ÁAÅBÅAäBÔç€QQPPƒQ’Bâ…QPPƒQ’Bâ†QPPƒQ’BPPƒQ’Bâ„QP PƒQ’BP!PƒQ’Bâ"PJº#˜æÿÛü€Q‡Q‚‡S€Ã‡Q‚O€jB€İ€€B€Ş€ë€^Û†€â€F€}À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€Ş\€
B€tB€uB€pB€qB€hB€|B€oB€{B€zB€sB€:\€B€:B€:pÀB€\l‚LEX-PARENT-ENV-REGpÀB€\ìLEX-ENV-B-REG€pÀB€\ì‚LEX-CURRENT-VECTOR-REGpÀB€\¬‚LEX-ALL-VECTORS-REG€CKEY-VALUE€B€ÌC‚RETRIEVE-FUNCTION€ƒKEY-FUNCTIONB€¢CINDEX-NAMEB€>B€?ƒ€NUM1ƒ€NUM2B€ Ã€TABLE1B€,B€lB€nB€:B€:B€¦B€B€lB€:Ã€TEMP-T\€€\€€B€…B€ B€ŠB€‹B€©‚INTERNAL-FEF-OFFSETS\€F€F€i„VARIABLES-USED-IN-LEXICAL-CLOSURES\€B€nB€lB€B€¢B€~B€}B€ÌB€|B€sB€oB€|B€hB€qB€pB€uB€tÀ†€E€B€’‘†€ÀƒEXTRACT-KEY€ÒB€ØÀlRETRIEVE-€Àl€-€ÀB€ÚÒB€ÆÒB€ÅÒ¬EXTRACT-KEY-ÀB€/ÀB€ÈÀB€ÊÒB€±ÀB€²ÒƒENTRY-POINT€Àƒ€GETPÒB€µÒB€·ÒB€ÖÒ\€F€F€F€F€F€
F€	F€F€F€†À÷ÿ†Àúÿ†Àÿûÿ†Àüÿ†Àıÿ†Àşÿ†Àÿÿ†À€À\€)INTERNALB€Ş€À*MAPHASH€ÒB€¹Ò\€B€§B€ŞF€ÀB€3Òƒ€MAPT’QƒQ…QÿÛ‡Q†QÿÛPPA…Á‡ÁFÁKÁ†Q±äPPˆQ	P‡Q
ªP’UÁQŠŠƒQ„Q…Q†QFQQŠŠJU»GÁPPˆQ	P‡Q
ªP’HÁPP‡Q
šP’IÁ†5zä†S&wä†WLÃ‚QP˜ä†[MÃƒQP˜æ†WMÃƒQP˜eä†[LÃ‚QP˜_ä‚QŠCLQ‚QPšŠCÿcNÁƒQŠCMQƒQPšŠCÿcOÁPP’PÁPP’QÁ€QP’VÁäVSWÁNQWQŒCXÃWQXQPQ’
CPQ˜VÅñçQP’UÁ"äUSWÁOQWQŒCXÁYÛXQPQ’RÁäRQZÁ	äZS[ÁWQ[Q’Y]YÁZÅ÷çXQYQXQQQ’’QQ˜UÅŞçPPÓCQQPQˆQQˆTPPÓC€QŠŠTO€­B€Ş€€B€¦€ë€
†€@ŒF€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€¦\€B€B€¦B€:\€B€:B€:B€u\€B€\€B€iƒLEXICAL-PARENT-DEBUG-INFO€B€p€B€Ö’QÀP’ÀÂÿO€ºB€¦€€B€ª€ë€%R†€@\F€-À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€ª\€B€"B€:\€B€:B€:B€uB€:B€¦B€:B€Ì\€B€\€B€B€…B€B€¹B€p€B€ÖÒ\€B€YÀB€ÒB€ÅÒB€Æ’ÉP±äÃÚÆPCÁ:äCSDÁ€QDQ’Ã\ÃÀCÅ÷ç0üÉPÁÂ*äÁÜü€QÍPÁPÏPÎPÌP²ÁÀÌPÊPÿÛÁPÈPÄªÇÀÃÚÅPEÁÎPŠŠÌPËPÊPÁPÇPÂPŠŠJÅºEÁ	äESFÁ€QFQ’Ã\ÃÀEÅ÷çÃäÃPÀP’ÀÂÿRO€ÇB€ª€€B€°€ë€5|†€@¸F€GÀ$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€°\€B€`COPERATION€B€:\€B€pB€qB€¥B€¡B€sƒREL-EXISTS?€B€UB€VB€tB€B€¢B€£B€¤B€`\€B€\€B€B€€B€”‘B€eÀ\€B€šB€›B€ŸB€ B€¡B€B€œB€B€eB€¢B€¤ÀB€¥Ò†€ÀB€ÒB€–Ò¬ŠERROR - Relations to participate in relational set operation have not been provided.ÀB€˜ÒB€LÒ\€B€=ÀB€ÁÒ,ƒRelation ~s of ~S and ~SÀB€ŸÒB€k’€QüMSMÁäMSÿ5úçMQ€ÁP€Q‹CCÁäCWCÁP€Q’€ÃPPADÁJÁBÁHÁLÁAÁGÁKÁ@ÁFÁJæHæCæP€æCİFäGæä	€
PˆRFQGQ@QAQ æRHäHQPÿÛšIÁISHÁIQ	BEÁæFQHQ€Q@QÿÛPQFQGQª¨æRFQHQ@QBQ æRFQ@QKQGQAQLQHQBQJQDQCQ‹O€ÚB€°€1€\€pÀB€\,„FASL-RECORD-FILE-MACROS-EXPANDED\€B€8\€\€ê€DEFUN€†€'\€B€*†€.Ù‹\€B€†€[æ„\€B€†€=Ì#\€B€
†€Æ9\€B€‹†€z(‡\€B€Š†€{šÍ\€B€ †€:}n\€B€ˆ†€{Ä²\€B€‡†€Zió\€B€††€xõ¿\€B€…†€*ıj\€B€‚†€(Ì¢€€ where-clause-A)] RelB
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
	ŠæPBQJÿÛÿİ¨
ä†QBQ	Šä€QBQ
ŠŠü€QBQŠ’€ÁAÅ½çQŠ@ÁÛ‚Û…QŠ†QŠä@Q…QP’†QP’’ŠräƒQ‚Á@QCÁväCSBÃ…QP’ŠräQDÛDÑƒQFÁEÁüEQFSGÁ…QGQ’ŠCEÃÁFÅFóçDQTüBQ†QP’ŠräQFÛFÑ„QDÁHÁüHQDSIÁ†QIQ’ŠCHÃÁDÅDóçFQ’Á„Q‚Á5üPBQŠP˜äBQ
ŠŠ„QP˜ä‚QBQ
ŠŠŠ’‚ÁQBQüBQŠƒQP˜
ä…QBQ’QP˜æQ…Qü‚QBQŠŠ’‚ÁQ†QBQ’ŠŠ’ÁCÅŠç€QQ‚QƒO€CB€Ô€€B€İ€ë€$8”†€B$F€\À$€ÀB€:B€V]€F€€LMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540810. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "XLD" :NAME "RENAME" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760360779. :AUTHOR "REL3" :LENGTH-IN-BYTES 4902. :LENGTH-IN-BLOCKS 10. :BYTE-SIZE 16.)                                      pp2€\€©COMPILE-DATA\€ì€SW-MFG,GODZILLA†€‡¤\ÑF€F€\€pÀ,COMPILER,VERSION€\€F€F€pÀB€),‚OPTIMIZE-SWITCH€†€©ƒQFASL-SOURCE-FILE-UNIQUE-ID€1€\€pÀl€FSì‚MAKE-FASLOAD-PATHNAME€\€ê€QUOTE€B€$\€B€8ª€NIL€\€B€8\€¬€RTMS\€B€8ì€RENAME\€B€8¬€LISP\€B€8F€©€BASEF€
é€FONTS€\€©*CODE-FONT*€é*COMMENT-FONT*é*STRING-FONT*€)PACKAGE€©€RTMS©€MODE©COMMON-LISP€‚RENAME-ATTRIBUTEO€PƒRENAME-ATTR€€B€P€ë€!TÉ†€`!xF€uÀ$€ÀB€:pÀ¬€TICLì€ART-Q€]€F€€:B€:B€:j€T€F€pÀ¬€SYS€l‚DEBUG-INFO-STRUCT€B€P\€ÃRELATION-NAME€ê€&REST€CATTRIBUTESB€:\€B€cÃRELATION-TUPLEƒ€POS€Ã€POS-K€ƒ€STO€ƒREL-OWNER-IDÃATTRIBUTE-LISTKEY-LISTƒ€IMP€Ã€ATTR-LÃATTRIBUTE-NAMEC‚NEW-ATTRIBUTE-NAMEB€cB€:\€)‚MACROS-EXPANDED€\€ê€SECOND*SEVENTH€ê€SIXTH€ê€FOURTHê€THIRD€ê€FIRST€ª€PROGª€SETFéDOCUMENTATION€ì¡Use this function to rename attributes in a relation.

   RELATION-NAME  - Name of the relation whose attributes are to be renamed.
   ATTRIBUTES     - Specify old-attribute and new-attribute names.

   Example: (RENAME-ATTRIBUTE 'parts 'number 'id 'name 'description).€€ƒ*PKG-STRING*ÑCƒ*PROVIDE-STATUS-MESSAGES*€Ñƒ*ACTIVE-DB*€Ñƒ*PROVIDE-ERROR-MESSAGES*ÑC‚*SYSTEM-RELATIONS*‘‚ACTIVE-DATABASE€ÒƒVALIDATE-SYMÒC‚CONVERT-ATTRIBUTESÒªSTRING-EQUALÀpÀB€]¬MEMBER-TEST€Òê€TERPRIÒl†ERROR - The attributes cannot be renamed because €ÀªWRITE-STRINGÒê€PRIN1€Òì‚ is a system relation.À\€,OWNER-IDìSAVE-DIRECTORYlATTRIBUTES¬€KEY€¬TUPLE-FORMAT¬‚IMPLEMENTATION-TYPE€l‚STORAGE-STRUCTURE€¬€DOC€ÀƒGET-RELATIONÒì‚ERROR - The relation €À,„ is not defined in the database Àê€EQUAL€ÀpÀB€]lPOSITION*€Òì‚ERROR - The attribute À¬‚ is not defined in €Àe€.ÀjWRITE-CHARÒ¬„ is already defined in the relation ÀpÀB€Uì€FIRSTNÒpÀB€],*APPEND€ÒìThe attribute À¬‚ will be renamed to €€æR€Qÿİ	’€ÁæR@QüISIÁISÿ5ûçIQ@Ã
Š@Á€QPP˜
ää€Pˆ€QˆPˆR€QPÿÛšBAÁæ	ä€Pˆ€QˆPˆPˆRASEÁA[FÁAQBGÁAYBHÁAQBBDÁ@S@W@YLÁKÁJÁYüJQÿİ	’JÁæRJQFQPšBÁæä€PˆJQˆPˆ€QˆPˆRKQFQP˜ää€PˆKQˆPˆ€QˆPˆRBQFQ’KQBkFQ
C
C’FÁJQGQPšCÁ
äCQGQ’KQCkGQ
C
C’GÁä€PˆJQˆ PˆKQˆPˆLSJÁLWKÁLÇK¥çSO€§B€P€€Ã‚RENAME-ATTRIBUTE-ARRAY€ë€
†€@F€À$€ÀB€:B€W]€F€€:B€:B€:B€ZF€€_B€¨\€B€aB€jB€kB€cê€IGNOREB€:B€:B€:€ƒ„RENAME-ATTRIBUTE-UTILITY-ARRAY-LIST€’€QQ‚QƒQ¤O€³B€¨€€ƒRENAME-ATTRIBUTE-FLAVOR€€ë€
†€@F€À$€ÀB€:B€W]€F€€:B€:B€:B€ZF€€_B€´\€B€aB€jB€kB€±B€eB€:B€:B€:€C„RENAME-ATTRIBUTE-UTILITY-REDEF-REL’€QQ‚Q„Q¤O€¾B€´€€Ã‚RENAME-ATTRIBUTE-LIST€€ë€
†€@F€À$€ÀB€:B€W]€F€€:B€:B€:B€ZF€€_B€¿\€B€aB€jB€kB€cB€±B€:B€:B€:€B€²’€QQ‚QƒQ¤O€ÈB€¿€€ƒRENAME-ATTRIBUTE-STRUCT€€ë€
†€@F€À$€ÀB€:B€W]€F€€:B€:B€:B€ZF€€_B€É\€B€aB€jB€kB€±B€eB€:B€:B€:€B€½’€QQ‚Q„Q¤O€ÒB€É€€B€²€ë€G†€AF€,À$€ÀB€:B€W]€F€€:B€:B€:B€ZF€€_B€²\€B€aB€jB€kB€cB€:\€B€nB€oB€c\€B€q\€B€sB€xB€ypÀB€]lXR-BQ-LIST€‚SYSTEM-RELATION€ÀB€…ÀB€aÀê€STRINGÒª€LISTÒ\€lMODIFIEDP€lATTRIBUTES¬€KEY€ÀB€8À‚DELETE-OR-MODIFYÒ‚SYSTEM-ATTRIBUTEÀª€AND€ÀB€nÀ\€ìATTRIBUTE-NAMEÀB€áÀÃ‚SAVE-SYSTEM-RELATIONS€’PÿİPP€QŠšPÿİ	PQ’	P‚Q’š
¨ƒSƒWƒYBÁAÁ@ÁüPÿİPPP€QŠšPP@QŠššPP	PAQ’’Š
¨BS@ÁBWAÁBÇAãç„O€íB€²€€B€½€ë€<†€A F€$À$€ÀB€:B€W]€F€€:B€:B€:B€ZF€€_B€½\€B€aB€jB€kB€eB€:\€Ã‚SYSTEM-ATTRIBUTE-LIST€ÃATTR-DES-PAIR€CTUPLE-LISTB€lC€SSC‚TUPLE-FORMAT-LIST€ƒ€DOC€ƒ€DIR€\€B€q\€B€sê€FIFTH€B€tB€uB€z€CƒGET-SYSTEM-ATTRIBUTE-LIST€ÒÃ‚CREATE-ATTR-DESCRIPTORÒêSTRING-UPCASE€Ò*‚READ-FROM-STRINGÒÃ€TUPLESÀRETRIEVEÒƒREDEFINE-RELÒB€ìÒÃSAVE-RELATION€’€QŠ@ÁƒYBCÁƒQBBDÁƒUBEÁJƒQŒCFÁƒWGÁQ@Q’AÁ€QŠŠPÿİšBÁ€QŠŠAQCQDQ‚QEQFQGQBQ	J	¸
€€QŠŠŒO€	B€½€‚RENAME-DATABASE€O€
CRENAME-DB€€B€
€ë€7y)†€`74F€°À$€ÀB€:B€W]€F€€:B€:B€:B€ZF€€_B€
\€B€bCDATABASES€B€:\€B€ÃDATABASE-NAME€C‚NEW-DATABASE-NAME€*PATHNAMEƒ€PATHREL-NAMESAVE-DIRƒ‚RELATION-TUPLE-LIST€ƒ€DB-LB€:B€:B€:CREL-TUPLE€\€B€q\€pÀ¬€ZLC€,DO-NAMEDpÀB€Uì‚INHIBIT-STYLE-WARNINGSB€sB€xB€yB€zB€{,•Used to rename a database.

   DATABASES - Specify old-database-name and new-database-name.

   Example: (RENAME-DATABASE parts suppliers micro-parts micro-suppliers).€€B€~ÑB€ÑÃ‚*SYSTEM-RELATION-KEY*€Ñƒƒ*SYSTEM-RELATION-ATTRIBUTES*ÑB€}Ñƒ„*SYSTEM-RELATION-STORAGE-STRUCTURE*€ÑÃ„*SYSTEM-RELATION-BASE-IMPLEMENTATION*€ÑB€€ÑB€‘B€‚ÒB€ƒÒpÀB€]ìSTRING-EQUAL*€ÒB€ˆÒì‡ERROR - The database to rename has to be the active database €ÀB€ŠÒB€‹ÒB€ÀB€ŸÒìƒERROR - The new database name À,„ is identical to the actual nameÀB€áÀlRETRIEVE-€Àl€-€ÀªCONCATENATE€ÒªFIND-SYMBOL€ÒB€àÀ\€ìRELATION-NAME€ìSAVE-DIRECTORYÀC‚GET-SAVE-DIRECTORYÒ¬€.XLDÀjPROBE-FILEÒì€.LISP€Àì€.XFASLÀì€.QFASLÀ\€ê€MEMBERB€aB€©€TEST\€B€8B€…À\€lMODIFIEDP€ÀB€âÒB€çÒB€ìÒªDELETE-FILE€Òl€.€À¬€XLD€ÀB€…ÀB€‡Òì€XLD#>€ÀªRENAME-FILE€Òì€QFASL€Àì€XFASL€À¬€LISPÀÃSAVE-DATABASE€ÒìThe database €Àì‚ has been renamed to €À¬ƒRenaming database completed.€€æR@QüHSHÁHSÿ5ûçHQ@Á@Sÿİ’AÁ@Wÿİ’BÁAäBæRAQP
æ
ä€PˆPˆPˆRBQAQ
ä
ä€PˆBQˆPˆRBQÀPP	PPPªP’IÁPPPPÿİÿÛPJI»GÁ‚FÁPFQAQP¢ ŠâPFQAQ!P¢ ŠâPFQAQ"P¢ ŠâPFQAQ#P¢ ŠDÁväPÿİ$P%Pÿİ&Š'¨(€DQ)ˆIÛIÑGQKÁJÁcüJQKSLÁLSEÁLWFÁPFQAQPEQ*P²CÁDÛPCQ+Pš ŠDÁäEQP,P-˜æDQPFQBQPEQ*P.PJº/’7üPCQ+Pš)Š1üPCQ0Pš ŠDÁ	äEQP,P-˜âåPCQ0PíıPCQ1Pš ŠDÁ	äEQP,P-˜ÒåPCQ1PİıPCQ2Pš ŠDÁ	äEQP,P-˜ÂåPCQ2PÍıÿÛCJÃÁKÅK›çüP3ˆä€4PˆAQˆ5PˆBQˆPˆ€6PˆBO€OB€
€‚RENAME-RELATION€O€PCRENAME-REL€B€P€ë€7‚;†€à7F€¹À$€ÀB€:B€W]€F€€:B€:B€:B€ZF€€_B€P\€B€bCRELATIONS€B€:\€B€ZÃ‚LAST-NEW-RELATION-NAMEB€fB€eB€iB€lB€B€B€Ã€RESULTC‚DB-RELATIONS-LIST€B€ƒCALL-SAVE-DBÃ€REL-L€B€:B€aC‚NEW-RELATION-NAME€B€Z\€B€q\€B€uB€sB€xB€yB€zB€{,“Rename relations in the active database.

   RELATIONS - Specify <old-rel-name new-rel-name>

   Example: (RENAME-RELATION rel1 new-rel1 rel2 new-rel2)€À†€€ƒ*AUTO-SAVE*€ÑB€~ÑB€ÑB€€ÑB€ÑB€'ÑB€(ÑB€}ÑB€)ÑB€*‘B€‚ÒB€áÀlRETRIEVE-€Àl€-€ÀB€2ÒB€3ÒB€àÀ\€ìRELATION-NAME€,OWNER-IDÀB€…ÀB€‡Ò*REVERSE€ÒB€ƒÒB€ÒB€™ÀB€›ÒB€ˆÒì‚ERROR - The relation €ÀB€ŠÒB€‹Ò¬† cannot be renamed because it is a system relation.€À,„ is not defined in the database ÀB€ÀB€ŸÒ¬„ is already defined in the database ÀB€¢ÒB€¤Ò\€,OWNER-IDìSAVE-DIRECTORYlATTRIBUTES¬€KEY€¬TUPLE-FORMAT¬‚IMPLEMENTATION-TYPE€l‚STORAGE-STRUCTURE€¬€DOC€ÀB€–Ò,‚RENAME-RELATION-ÀB€áÒl€.€À¬€XLD€ÀB€9Òì€XLD#>€ÀB€GÒì€XFASL€Àì€QFASL€À¬€LISPÀB€ìÒìThe relation €Àì‚ has been renamed to €€€æR@QüMSMÁMSÿ5äMQJ™÷åMQ@ÁPPPPPªP’NÁP
PP	PÿİÿÛPJN»KÃMÁäMQBPP˜æMQBI]IÁMÅôçIQŠJÁ@S@W@YQÁPÁOÁXüOQŠOÁPQŠPÁOäPæROQŠJQPšBÁæäOQŠPP˜ä€PˆOQ ˆ!PˆR€PˆOQ ˆ"PˆP ˆ#P$ˆRPQŠJQP˜æPQŠPP˜ää€PˆPQ ˆ%PˆP ˆ#P$ˆRBQJQ&’PQŠBkJQ
C
C'’JÁQSOÁQWPÁQÇP¦ç@SŠ@WŠ@YQÁPÁOÁcüOQ(PÿÛ)šBCÁCSDÁCYBEÁP*PEQšP’NÁOQPQCQN™CWHÁPHQPPOQ+Š,P²FÁGÛPFQ-Pš.ŠGÁäGQPHQPPPQŠ,P/PJº0üPFQ1Pš.ŠGÁìçPFQ2Pš.ŠGÁåçPFQ3Pš.ŠGÁŞçLİGä4€ä€5PˆOQ ˆ6PˆPQ ˆ#P$ˆPQAÁQSOÁQWPÁQÇP›çSO€ƒB€P€€Ã‚RENAME-RELATION-ARRAY€€ë€"†€@ÈF€À$€ÀB€:B€W]€F€€:B€:B€:B€ZF€€_B€„\€B€aB€aB€±B€:\€CARRAY-NAMEÃNEW-ARRAY-NAME\€B€q\€B€ß€B€áÀì€ARRAY€ÀB€2ÒB€Òê€INTERNÒpÀB€U¬‚COPY-ARRAY-CONTENTS€ÀB€âÒª€EVALÒC„RENAME-RELATION-UTILITY-ARRAY-LIST’P€QPšŠŠ@ÁPQPšŠŠAÁP@QAQ	š
ˆ€QQ”O€˜B€„€€Ã‚RENAME-RELATION-FLAVOR€ë€F€ÀF€À$€ÀB€:B€W]€F€€:B€:B€:B€ZF€€_B€™\€B€aB€aB€eB€:B€:B€:€C„RENAME-RELATION-UTILITY-REDEF-REL€’€QQ‚QœO€£B€™€€ƒ‚RENAME-RELATION-LIST€ë€F€ÀF€À$€ÀB€:B€W]€F€€:B€:B€:B€ZF€€_B€¤\€B€aB€aB€±B€:B€:B€:€B€—’€QQ”O€­B€¤€€Ã‚RENAME-RELATION-STRUCT€ë€F€ÀF€À$€ÀB€:B€W]€F€€:B€:B€:B€ZF€€_B€®\€B€aB€aB€eB€:B€:B€:€B€¢’€QQ‚QœO€·B€®€€B€—€ë€OF€€F€1À$€ÀB€:B€W]€F€€:B€:B€:B€ZF€€_B€—\€B€aB€aB€:B€:\€B€q\€B€ß€B€àÀB€…ÀB€aÀB€ÒB€âÒ\€lMODIFIEDP€ìRELATION-NAME€ÀB€ÀB€8ÀB€–ÒB€çÒB€èÀ\€ìRELATION-NAME€ÀƒSYSTEM-INDEXÀƒENTRY-POINT€Àƒ€GETPÒƒ€PUTP’PÿİPP€QŠšPÿİ	P
PQ’’Š’¨PÿİPP€QŠšP	P
PQ’’ŠŠ¨PÿİPP€QŠšP	P
PQ’’ŠŠ¨Q€QP’P˜€QÿÛPœO€ËB€—€€B€¢€ë€*>¦†€@*üF€hÀ$€ÀB€:B€W]€F€€:B€:B€:B€ZF€€_B€¢\€B€aB€aB€eB€:\€B€÷B€øB€ùCINDEX-LISTB€lB€úB€ûB€jB€kB€üB€ıDOMAINS€B€:CINDEX-INFOB€:\€B€q\€B€xB€yB€#B€ßB€sB€vB€wB€B€tB€uB€z€B€}ÑB€'ÑB€(ÑC‚*SYSTEM-INDEX-KEY*ÑCƒ*SYSTEM-INDEX-ATTRIBUTES*€‘B€ÒB€ÒB€ÀB€ÒB€lÀCMODIFIEDP€ÀB€hÀƒ€KEY€Àê€FORMATÀB€üÀB€ıÀ‚DEFINE-RELATION€ÒB€ÇÀB€…ÀB€aÀB€áÒB€âÒ\€ìRELATION-NAME€ÀB€ÀB€8ÀB€–ÒB€çÒ\€lINDEX-NAMElINDEX-TYPE¬€KEY€ÀB€ÒQTRIEVE€ÒB€àÀ\€,DOMAINS€ÀB€áÀ,DELETE-€Àl€-€ÀB€2ÒB€3ÒÃ€INSERTÒ‚DESTROY-RELATION’€QŠ@Á‚YBDÁ‚QBBEÁ‚UBFÁ‚[GÁ‚QBHÁJ‚QŒCIÁ‚WJÁGQ@Q	’AÁ€Q
PÿİšBÁQAQPDQPÿİPEQPHQPFQPIQPJQJ¸PÿİPP€QŠšPPPQ’’ŠŠ¨PPPPPPQŠš ªCÁä!PP"PPPPQŠš ªBKÁCQLÁäLSMÁ#P$PDQ%PMW&ªP'’NÁQGQM[ÿÛÿİMSN±LÅìçBäQ
PBQ’(€Q)ŒO€íB€¢€1€\€pÀB€],„FASL-RECORD-FILE-MACROS-EXPANDED\€B€8\€\€pÀB€U¬€DEFF†€b\€ê€DEFUN€†€'\€B€%†€(Ì¢\€B€#†€*ıj\€B€†€Zió\€B€ß†€.Ù‹\€B€z†€[æ„\€B€y†€=Ì#\€B€x†€z(‡\€B€w†€:}n\€B€v†€xõ¿\€B€u†€{Ä²\€B€t†€2»=\€B€s†€{šÍ€€dir new-database-name
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
  (rename-relation-utility-redef-rel relation-name new-relation-LMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540817. :SYSTEM-TYPE :LOGICAL :VERSION 2. :TYPE "XLD" :NAME "RESTORE" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760360130. :AUTHOR "REL3" :LENGTH-IN-BYTES 4432. :LENGTH-IN-BLOCKS 9. :BYTE-SIZE 16.)                                      pp2€\€©COMPILE-DATA\€ì€SW-MFG,GODZILLA†€‡¤ÒÎF€F€\€pÀ,COMPILER,VERSION€\€F€F€pÀB€),‚OPTIMIZE-SWITCH€†€©ƒQFASL-SOURCE-FILE-UNIQUE-ID€1€\€pÀl€FSì‚MAKE-FASLOAD-PATHNAME€\€ê€QUOTE€B€$\€B€8ª€NIL€\€B€8\€¬€RTMS\€B€8,RESTORE€\€B€8¬€LISP\€B€8F€é€FONTS€\€©*CODE-FONT*€é*COMMENT-FONT*é*STRING-FONT*€©€BASEF€
)PACKAGE€©€RTMS©€MODE©COMMON-LISP€€ƒGET-RELATION€ë€X†€„ÄF€9À$€ÀB€:pÀ¬€TICLì€ART-Q€]€F€€:B€:B€:j€T€F€pÀ¬€SYS€l‚DEBUG-INFO-STRUCT€B€P\€RELATIONƒPROJECT-LISTÃ€MANIP?j&OPTIONAL€\€SAVING?€B€:B€:\€ƒQTRIEVE-VAR€\€)‚MACROS-EXPANDED€\€pÀB€\lXR-BQ-LISTª€SETF€pÀB€T,USER-ID€ÑC‚*SYSTEM-VIEW-KEY*€Ñƒ*SYSTEM-VIEW-ATTRIBUTES*ÑÃ‚*SYSTEM-RELATION-KEY*€Ñƒƒ*SYSTEM-RELATION-ATTRIBUTES*‘C‚CONVERT-ATTRIBUTESÒ‚SYSTEM-RELATION€À\€¬€DISKìSAVE-DIRECTORYÀpÀB€\,*APPEND€ÒªSTRING-EQUALÀÃRELATION-NAME€Àê€STRINGÒª€LISTÒQTRIEVE€ÒƒSYSTEM-VIEW€À\€,‚VIEW-DEFINITION€Àª€AND€ÀCVIEW-NAME€ÀOWNER-IDÀª€EVALÒB€PÒƒ€DIR€ÀÃLOAD-RELATION€’QŠÁ	PP
PQ’PPP€QŠšªB@Á æ‚æPPPPPPP€QŠšPPPššªB@Áä@QŠ€ÃQ‚Qœ€QÿÛ”@ûå@äƒæ€QP@W˜€Q@Y”O€ B€P€ÃLOAD-DATABASE€O€ŠLOAD-DB€€B€Š€ë€mêA†€àm@F€WÀ$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€Š\€Ã€DBNAMEê€&REST€ƒKEYWORD-LISTª€&KEYB€cjDIRECTORY€j‚&ALLOW-OTHER-KEYS€B€:\€B€–B€˜*PATHNAMETEMP-DIRƒTEMP-STATUS€ƒDIR-CHANGED?B€:B€–B€:B€:B€:pÀB€\¬€TEM€B€`SYS-TUP€ƒ€REL€ƒ€PATHB€:B€:pÀB€\¬.CASE.ITEM.€\€B€i\€B€lpÀB€T¬€SENDpÀB€\¬SELECT-MEMQ€pÀB€Tl‚CONDITION-CASE-IF€pÀB€TìCONDITION-CASEê€UNLESSpÀ¬€ZLC€,DO-NAMEDpÀB€Tì‚INHIBIT-STYLE-WARNINGSpÀB€Tl‚CONDITION-BIND-IF€pÀB€TìCONDITION-BINDpÀB€Tì‚CATCH-CONTINUATION-IF€pÀB€Tl‚CATCH-CONTINUATIONpÀB€Tì€ERRSETª€PROGB€mª€WHENéDOCUMENTATION€¬–A database saved on the disk can be loaded using this function.

   DBNAME    - Name of the database to be restored.
   DIRECTORY - Name of the directory in which it can be found.€À†€“ €pÀl€EH¬‚*CONDITION-HANDLERS*ÑÃƒ*SYSTEM-STORAGE-STRUCTURE-KEY*ÑÃ„*SYSTEM-STORAGE-STRUCTURE-ATTRIBUTES*€ÑC‚*SYSTEM-RELATIONS*ÑCƒ*PROVIDE-STATUS-MESSAGES*€ÑÃ*DONOT-COMMIT*Ñƒ‚*RESTORE-OPERATION*€Ñ‚*SAVE-DIRECTORY*Ñƒ*ACTIVE-DB*€Ñƒ*PROVIDE-ERROR-MESSAGES*ÑB€rÑB€sÑƒ*PKG-STRING*Ñƒ„*SYSTEM-RELATION-STORAGE-STRUCTURE*€ÑÃ„*SYSTEM-RELATION-BASE-IMPLEMENTATION*€‘\€iDIRECTORY€ÀpÀB€\ìSTORE-KEYARGS€ÒF€§ÀRESTORE€À‚ACTIVE-DATABASE€ÒB€}ÀlRETRIEVE-€Àl€-€ÀªCONCATENATE€ÒªFIND-SYMBOL€ÒB€uÀ\€ìRELATION-NAME€ÀB€{ÀCMODIFIEDP€Àl€T€ÀB€~Òê€TERPRIÒ,ERROR - ÀªWRITE-STRINGÒê€PRIN1€Òì† is the current database and it has modified relationsÀ¬Š          Please resolve this conflict by either saving or destroying this database€À¬…          before restoring a saved database€ÀƒVALIDATE-SYMÒB€‡À\€B€‡ÀƒGET-KEYWORD-VALUE-PREREQÒÃGET-DIRECTORY€Ò,‚system-relation€Àl€.€ÀpÀ,€ì€G3813€ÀF€PÀê€ERROR€ÀpÀB€\ìERRSET-HANDLERÀpÀB€4ìDIRECTORY-LISTÒ¬€.XLDÀjPROBE-FILEÒì€.XFASLÀì€.LISP€À‚ACTIVE-DATABASEPÒ‚DESTROY-DATABASEÒpÀB€ğì€G3821€ÀF€À)VERBOSE€Àª€LOADÒl‚ERROR - Database €Àìƒ does not exist in directory €ÀÃCOMMIT-TUPLES€Àƒ€PUTPÒ*REVERSE€ÒB€ˆÒB€Ò\€ìRELATION-NAME€ìSAVE-DIRECTORYÀB€aÒpÀB€\¬MEMBER-TEST€ÒpÀB€ğì€G3856€ÀF€À\€pÀ¬€NET€ìNETWORK-ERROR€pÀB€l‚UNKNOWN-HOST-NAME€ÀpÀB€\¬‚CONDITION-CASE-THROWÀì€.QFASLÀ)‚CONDITION-NAMES€ÀB€ÀB€ÀB€|À\€ìSAVE-DIRECTORYÀ‚DELETE-OR-MODIFYÒƒDESTROY-REL€ÒpÀB€\ìSTRING-EQUAL*€Ò\€lMODIFIEDP€À\€B€:À\€ª€NOT€\€ê€EQUAL€€\€ê€SEARCHì€SYSTEMB€|À\€¬€DISKÀ\€B€YÀ\€B€'€\€B€*ì€SYSTEMB€|ÀÃINIT-WHERE-OPTÒƒSYSTEM-STORAGE-STRUCTUREÀ\€ì‚STORAGE-STRUCTURE-NAMEÀ\€B€{Ã‚STORAGE-STRUCTURE-NAME¬€ISAMÀƒ€ATTRÀÃ€WHERE€Àê€VALUESÀ\€¬€AVL€ÀÃMODIFY-TUPLES€ÒÃSYSTEM-OPTFUNCÀ\€B€{Ã‚STORAGE-STRUCTURE-TYPE¬€ISAMÀÃ€TUPLESÀ\€\€l€=€¬€AVL€ìOPT-AVL-EQUAL€¬€RTMS\€l€<€¬€AVL€lOPT-AVL-LT¬€RTMS\€l€>€¬€AVL€lOPT-AVL-GT¬€RTMS\€l€<=¬€AVL€lOPT-AVL-LT¬€RTMS\€l€>=¬€AVL€lOPT-AVL-GT¬€RTMS\€¬€AND€¬€AVL€¬OPT-AVL-AND€¬€RTMS\€ì€EQUAL€¬€AVL€ìOPT-AVL-EQUAL€¬€RTMS\€ì€LESSP€¬€AVL€lOPT-AVL-LT¬€RTMS\€¬STRING-LESSP¬€AVL€lOPT-AVL-LT¬€RTMS\€,GREATERP¬€AVL€lOPT-AVL-GT¬€RTMS\€,‚STRING-GREATERP€¬€AVL€lOPT-AVL-GT¬€RTMS\€l€OR¬€AVL€lOPT-AVL-OR¬€RTMS\€¬STRING-EQUAL¬€AVL€ìOPT-AVL-EQUAL€¬€RTMSÀÃ€INSERTÒì‚ERROR - The Directory À,‚ does not exist€€@ä@QPÿİAÑ ÿİPÿÛUPˆ,äPPPPPªP’FÁPPPPP P!P"šÿÛPJF»ää#€$P%ˆP&ˆ'P%ˆ#€(P%ˆ#€)P%ˆÿÛ]¬Zÿ€Qÿİ*’€Áõå@QüGSGÁäGSÿ5úçGQ@Áæ+PP"’@Á,P@Q-’@ÁP@Q.ŠCÃ€QP/P0P²BÁ1P2PT3P4P1PÿÛJCHÃPJCIÃÖCQ5Š"ŠJ!BJ!Bÿ\ü\ÿr•CQÀ
Ú	ÜPP€Q6P¢BÃ7ˆæPP€Q8P¢BÃ7ˆæPP€Q9P¢BÃ7ˆ#ä:€äP;ˆ<P=PT3P4P<PÿİJCFÃPJCJÃÖBQ>PäÿİüÿÛ?š"ˆE\ü\KÁü	ä#€@P%ˆ€Q&ˆAP%ˆP&ˆ	Únı
Ü	ÚJÛJÑPHÁFÁ	üFQHSÿÛBPCšCFÃÁHÅHõçJÛJÑPDŠHÁFÁüFQHSLÃ+PCQE˜
ÜÿİCFÃÁHÅHòç
ÚPPPPÿİFªIÁ•äISMÁNÛOÛMQ"ŠPGPHšBNÁNWOÁNSNÃPPI˜{æJPKPTLPMPJPJCPÃPJCQÃÖPOQ€QPNQ6P²7ŠâPOQ€QPNQ9P²7ŠâPOQ€QPNQ8P²7ŠâPOQ€QPNQNP²7ŠJ!BJ!Bÿ\
ü\QÁOPQ‹RÁPPRæQPü<æPCQ€QPNQ6P²7ˆæPCQ€QPNQ9P²7ˆæPCQ€QPNQ8P²7ˆ	æPCQ€QPNQNP²7ˆäPÿİPRPNQ"šSPCQ"ŠT¨üPDÁÚNQUˆDQÀüOQCQVæEİIÅkçPÿİÿİWPXPT¨PÿİYPZP[PT¨EäPÿİ\PSPCQ"ŠT¨]€^PP_PP`PF¨ä^PaP_PbP`PcPdPJe¸fPÿÛgPT˜fPhPiPj˜ür#€kP%ˆCQ&ˆlP~€Q]¬ZÿJ]¬ZP
ÚO€ŠB€Š€‚LOAD-ENVIRONMENTO€‹LOAD-ENV€B€‹€ë€&Bª†€`&\F€hÀ$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€‹\€ENVNAME€B€•B€–B€—B€cB€˜B€™B€:\€B€–B€˜B€›B€‡B€–B€:B€:\€B€i\€B€·B€¹B€»B€½B€¿B€ÀB€mB€Â,’Load a saved environment.

   ENVNAME   - Name of the environment to be restored.
   DIRECTORY - Name of the directory in which it can be found.€B€ÇÑC‚*ENVIRONMENT-NAME*ÑB€ËÑB€Ğ‘\€B€ÕÀB€×ÒB€êÒ\€B€‡ÀB€ìÒB€}ÀpÀB€ğì€G3959€ÀF€„ÀB€ôÀB€öÀB€íÒB€øÒB€~ÒB€ãÒì‚ERROR -The directory €ÀB€åÒB€æÒ,‚ does not exist.Àl‚rtms-environment-€ÀB€İÒ¬€.XLDÀB€úÒì€.XFASLÀì€.LISP€ÀB€ÀB€ÒB€ Ò¬Environment À, definedÀ¬‚ERROR - Environment Àìƒ does not exist in directory €€@ä@QPÿİAÑ €Qÿİ	’€ÁæR@QüDSDÁäDSÿ5úçDQ@Á
P@Q’@ÁPPPTPPPÿÛJCEÃPJCFÃÖ@QŠCÃŠŠJ!BJ!Bÿ\ü\ÿäCQ
üä€PˆCQˆPˆRP€Q¢BÁPBQPšŠBÁæPBQPšŠBÁæPBQPšŠBÁ	äBQPäÿİüÿÛ ˜€P€Q!
ää€"Pˆ€Qˆ#Pˆ€
ä€$Pˆ€Qˆ%Pˆ@QŠˆRO€ªB€‹€€B€ˆ€ë€J¥”†€àJ@F€ïÀ$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€ˆ\€B€|B€•B€–B€—B€cB€˜B€™B€:\€B€–B€˜INDICES€B€›ƒ€XLDFÃ€XFASLFÃ€QFASLFÃ€LISPF€ÃRELATION-INFO€Ã€TEMP1€Ã€TEMP2€B€‡B€–B€:B€:B€:B€:B€ CINDEX-INFO\€B€i\€ê€THIRD€ê€SECONDê€FIRST€B€³B€lB€©B€·B€¹B€»B€½B€¿B€ÀB€mB€Â,“Load a saved relation.

   RELATION-NAME    - Name of the relation to be restored.
   DIRECTORY        - Name of the directory in which it can be found.À†€“ €B€ÇÑC‚*SYSTEM-INDEX-KEY*ÑCƒ*SYSTEM-INDEX-ATTRIBUTES*€ÑB€ËÑB€ÍÑB€rÑB€sÑB€ÎÑB€ÏÑB€ĞÑB€Ê‘\€B€ÕÀB€×ÒF€×ÀB€ÚÒB€êÒB€{ÀB€ÒCRELATIONP€ÒB€ãÒ¬‚ERROR -The relation ÀB€åÒB€æÒ,„ does not exist in the database ÀB€‡ÀB€uÀ\€ìSAVE-DIRECTORYÀB€|ÀêSTRING-UPCASE€ÒB€~ÒB€Ò\€B€‡ÀB€ìÒpÀB€ğì€G4015€ÀF€—ÀB€ôÀB€öÀB€}ÀpÀB€ğì€G4023€ÀF€ÀB€íÒB€øÒì‚ERROR -The directory €À,‚ does not exist.Àl€-€ÀB€İÒ¬€.XLDÀì€.XFASLÀì€.QFASLÀì€.LISP€ÀB€úÒéCREATION-DATE€Àª€OPENÒB€ÀB€Òê€CLOSE€Ò¬SYSTEM-INDEXÀB€ Òƒ‚DEFINE-SYSTEM-INDEX€ÒÃ‚COMMIT-SYSTEM-RELATIONÒ\€B€{B€|¬SYSTEM-INDEXÀ\€lMODIFIEDP€À\€B€YÀB€Ò\€¬€DISKÀ\€B€:ÀƒSYSTEM-INDEXÀ\€lINDEX-NAME¬€KEY€lINDEX-TYPEÀ\€lATTRIBUTES¬‚IMPLEMENTATION-TYPE€l‚STORAGE-STRUCTURE€ÀÃ‚CREATE-INDEX-RELATION€’@ä@QPÿİAÑ ÿİPÿÛU€æÿÛ]ÜZÿ€Qÿİ’€Áõå€QPP˜æ€Qˆæëå€Pˆ€QˆPˆPˆáı@QüLSLÁäLSÿ5úçLQ@Á@æP€QPP˜äPüP
PP	PPP€Q Š!š"ªB!’@Á#P@Q$’@Á%P&PT'P(P%PÿİJCMÃPJCNÃÖÜ)P*P+PT'P(P*PÿÛJCOÃPJCPÃÖ@Q,ŠKÃ-Š!ŠJ!BJ!Bÿ\ü\ÿäKQüä€.PˆKQˆ/PˆÿÛ\ıP0P€Q1ªCÁ)PCQ2P1šDÁ)PCQ3P1šEÁ)PCQ4P1šFÁ)PCQ5P1šGÁDQ6ˆäEQ6ˆä7PDQ8ŠIÃÿ‹7PEQ8ŠJÃÿ‹yäDQüEQ9PäÿİüÿÛ:˜IQ;ˆJQ;Š5üDQ6ˆ*æEQ6ˆäEQ&üFQ6ˆäFQ!üGQ6ˆäGQü€Q<P=
ä>€Ä?€Pÿİ@PAPBPCªüÚ å€Pˆ€QˆPˆPˆ–ıDQ9PäÿİüÿÛ:š!ˆE\ü\QÁPÿİPP€Q Š!šDPEPC¨€QPP˜'æFPPGPPPP€Q!š"ªBÁäP
PHP	PPP€Q Š!š"ªBHÁBQOÁäOSRÁ€QRSHSRWR[HWH[JI¸OÅóçÚ€Q]ÜZÿJ]ÜZPÚO€ôB€ˆ€1€\€pÀB€\,„FASL-RECORD-FILE-MACROS-EXPANDED\€B€8\€\€ê€DEFUN€†€'\€pÀB€T¬€DEFF†€b\€B€Á†€z(‡\€B€À†€{šÍ\€B€¿†€:}n\€B€Á†€,a\€B€À†€=Ì#\€B€¿†€-i\€B€½†€~Éz\€B€»†€<p‘\€B€¹†€`sN\€B€·†€|Äô\€B€µ†€(Ì¢\€B€³†€*ıj\€B€°†€6\€B€¯†€+­–\€B€­†€jA´\€B€«†€<i—\€B€©†€aM*\€B€m†€[æ„\€B€l†€.Ù‹€€VL-AND" "RTMS")
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
  DIRECTORY            - New directory in which this relation is to be saved.  (MODIFY-RELATION RELATION &REST KEYWORD-LIST &KEY &OPTIONAL RELATION-NAME ADD-ATTRIBUTES DELETE-ATTRIBUTES RENAME-ATTRIBUTES IMPLEMENTATION-TYPE STORAGE-STRUCTURE FORMAT KEY DOCUMENTATION DIRECTORY &ALLOW-OTHER-KEYS)€€B€›‘B€ÜÀl€~SÀB€	ÀB€ ÀÃADD-ATTRIBUTESÀC‚DELETE-ATTRIBUTES€ÀC‚RENAME-ATTRIBUTES€ÀB€£ÀB€¬ÀB€ßÀB€¢ÀB€¡ÀB€ ÀB€­ÒB€ßÒB€	’PAÁPÿÛPPQP‚QPƒQ	P„Q
P…QP†QP‡QPˆQP QP‹QPŠQJº@ÃššA‘Q@Q”O€¤	B€
	€1€\€B€â\€B€å\€B€8\€B€LMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540824. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "XLD" :NAME "RETRIEVE" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760360396. :AUTHOR "REL3" :LENGTH-IN-BYTES 13504. :LENGTH-IN-BLOCKS 27. :BYTE-SIZE 16.)                                   pp2€\€©COMPILE-DATA\€ì€SW-MFG,GODZILLA†€‡¤ÜÏF€F€\€pÀ,COMPILER,VERSION€\€F€F€pÀB€),‚OPTIMIZE-SWITCH€†€©ƒQFASL-SOURCE-FILE-UNIQUE-ID€1€\€pÀl€FSì‚MAKE-FASLOAD-PATHNAME€\€ê€QUOTE€B€$\€B€8ª€NIL€\€B€8\€¬€RTMS\€B€8,RETRIEVE\€B€8¬€LISP\€B€8F€é€FONTS€\€©*CODE-FONT*€é*COMMENT-FONT*é*STRING-FONT*€©€BASEF€
)PACKAGE€©€RTMS©€MODE©COMMON-LISP€€ƒOBTAIN-PROJECT-&-FORMAT€€ë€I†€A¬F€TÀ$€ÀB€:pÀ¬€TICLì€ART-Q€]€F€€:B€:B€:j€T€F€pÀ¬€SYS€l‚DEBUG-INFO-STRUCT€B€P\€ÃRELATION-NAME€ƒPROJECT-LISTƒTUPLE-FORMATCSORT-LIST€ƒ‚*CURRENT-ATTRIBUTES*Ã‚*CURRENT-TUPLE-FORMAT*B€:\€Ã€A-P-L€ƒ‚ACTUAL-PROJECT-LIST€C‚FINAL-PROJECT-LISTÃ€ITEMS€C‚TEMP-TUPLE-FORMAT€B€:B€:B€:ƒ€ATTRB€:CATTRIBUTE€\€)‚MACROS-EXPANDED€\€ª€PROGpÀ¬€ZLC€,DO-NAMEDpÀB€Tì‚INHIBIT-STYLE-WARNINGSª€SETF€pÀB€\,*APPEND€ÒpÀB€Tì€FIRSTNÒª€LISTÒF€ĞÀÃPARSE-PROJECT€ÒªSTRING-EQUALÀpÀB€\¬MEMBER-TEST€ÒjCOPY-LIST€’æ„QÃAÁQBÁ‚æ…QüQŠC‚QŠCÿcCÃvä‚QCQ…Q
C’süC?räQŠC‚Q’lü5æQŠÁ‚ä‚5æ‚QŠ‚Á€Q„QQ…Q‚QPPADÁAÁÁEÑAQGÁFÁüFQGSHÁH5æHQüHSCFÃÁGÅGòçEQ@Á‚QŠC@QŠCxä‚Q
JÿÛ@QŠC‚QŠCÿcC’‚Áƒ,äQBÁGÛGÑƒQEÁIÁ"üIQESJÃQP	˜æJQ„QP	˜äQJQŠ’ÁAQJQŠ’AÃüJæQ
ŠÃüÿÛCIÃÁEÅEÜçDQ‚ÁQ‚QAQBQ„O€ƒB€P€€ƒOBTAIN-SORT€€ë€	1k†€@	F€:À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€„\€ª€SORTCQUICK-SORTB€:B€:\€B€o\€B€qB€tB€vB€w€ƒ*PROVIDE-ERROR-MESSAGES*‘B€|ÒƒVALIDATE-SYMÒê€TERPRIÒ,…ERROR - Illegally specified sort clause€ÀªWRITE-STRING’€ä€5æ€QŠ
ü€5ä€æ€Ûü€Sÿ5ä€S€Áä5æQŠ
ü5äæÛüSÿ5äSÁ€ä@Ñ€QBÁAÁüAQBSÿİ’CAÃÁBÅBöç@Q€ÁÿÛ€$ää€Pˆ€ßüäBÛBÑQ@ÁCÁüCQ@Sÿİ’CCÃÁ@Å@öçBQÁÿÛää€Pˆß€QQ‚O€–B€„€€ƒOBTAIN-WIDE€€ë€	1†€@	„F€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€—\€Ã€WIDEP€‚NUMBER-PER-LINE€B€:\€Ã€STATUS\€B€o\€B€w€B€‘‘B€“Ò,ERROR - ÀB€•Òê€PRIN1€Ò¬„ is not a legal number specification€@İ€ ä€1äæ€QüæLÁ5äSÁ1äQÿæîä€PˆQˆPˆ@ÛüÛ@QQ‚O€©B€—€€C‚PROCESS-QUICK-SORT€ë€>†€@ØF€#À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€ª\€Ã€TUPLE€B€B€hB€:\€€gB€:B€:B€:B€lB€:\€B€o\€B€qB€tB€vB€w€ê€STRINGÒ\€B€Y\€B€YÀC‚QUICK-SORT-PREREQ€ÒB€‚ÒB€’AÑ‚QCÁBÁüBQCSDÁD5æDQüDSŠCBÃÁCÅCñçAQ@ÁQäÿİüCÛCÑQAÁEÁüEQASŠCEÃÁAÅA÷çCQ@Q’Áä€QŠQ@QœRO€»B€ª€€ƒPROCESS-SORT€ë€2†€AF€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€¼\€€³B€
B€aB€hB€dC‚*CURRENT-DOMAINS*€B€:\€DOMAINS€CKEY-VALUE€ATTR-POSB€l\€B€o\€B€wB€q€B€ÀB€Ò¬€ANYPÀ*REVERSE€ÒB€c’ƒQCÁä„QŠCCSBÃÿ5æBQüBSBÃ„QPšŠCÿcAÁ	äBQ„QP˜äAQ…QŒCüP@]@ÁCÅáç@QŠ@Á€QQ‚Q@Q¤O€ÎB€¼€€C‚CONVERT-ATTRIBUTES€ë€(†€@PF€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€Ï\€ÃATTRIBUTE-LISTB€:\€B€:B€:B€:B€m\€B€o\€B€qB€tB€vB€w€B€|ÒB€·ÒêSTRING-UPCASE€’€æR€5æ€QŠ€Á@Ñ€QBÁAÁüAQBSCÁòCQŠüC7äCQŠüCQCAÃÁBÅBíç@O€İB€Ï€€‚EXTRACT-KEY-HEAP€ë€†€@F€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€Ş\€B€ØKEY-LISTB€ÇƒWHERE-CLAUSEªPACKAGE-NAMEB€:B€:B€:€RO€êB€Ş€€C‚RETRIEVE-INTERNAL€€ë€Td†€àT€F€\À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€ë\€B€`ƒKEYWORD-LISTB€:\€ B€³C‚CURRENT-ATTRIBUTES‚CURRENT-DOMAINS€ƒCURRENT-KEY€ƒƒCURRENT-IMPLEMENTATION-TYPE€CƒCURRENT-STORAGE-STRUCTURE€B€hƒ‚CURRENT-TUPLE-FORMATB€iB€Èƒ€INTOB€aB€èê€PRINT€Ã€QPRINTê€STREAM‚OUTPUT-FILE-NAMEB€bB€¡ÃRETURN-TUPLES€B€
ƒ€CARDƒ‚RETRIEVE-INDEX-NAME€B€ÃKEYWORD-VALUESB€£CINDEX-NAMEB€:B€aB€³B€:B€:\€B€o\€pÀB€T¬€SENDpÀB€Tl‚CONDITION-BIND-IF€pÀB€TìCONDITION-BINDpÀB€Tì‚CATCH-CONTINUATION-IF€pÀB€Tl‚CATCH-CONTINUATIONpÀB€Tì€ERRSETB€qpÀB€\lXR-BQ-LIST*SEVENTH€ê€SIXTH€ê€FIFTH€ê€FOURTHê€THIRD€ê€SECONDê€FIRST€B€wÀ†€ A€pÀl€EH¬‚*CONDITION-HANDLERS*Ñ‚*OUTPUT-WINDOW*€ÑCƒ*PROVIDE-STATUS-MESSAGES*€ÑÃ‚*SYSTEM-RELATION-KEY*€Ñƒƒ*SYSTEM-RELATION-ATTRIBUTES*Ñƒ‚*DEFAULT-ANYP-WIDTH*ÑC‚*SYSTEM-INDEX-KEY*ÑCƒ*SYSTEM-INDEX-ATTRIBUTES*€Ñƒ*PKG-STRING*Ñƒ„*SYSTEM-RELATION-STORAGE-STRUCTURE*€ÑÃ„*SYSTEM-RELATION-BASE-IMPLEMENTATION*€Ñƒ*ACTIVE-DB*€ÑB€‘Ñƒ‚*PARAMETER-CHECKING*‘‚ACTIVE-DATABASE€ÒB€’Ò\€lATTRIBUTES,DOMAINS€¬€KEY€¬‚IMPLEMENTATION-TYPE€l‚STORAGE-STRUCTURE€¬TUPLE-FORMAT¬CARDINALITY€ÀƒGET-RELATIONÒB€“Òl‚ERROR - Relation €ÀB€•ÒB€§Ò,„ does not exist in the database À\€B€üPROJECT€Ã€WHERE€B€ıÃ€OUTPUTê€FORMATƒ€NUM€ƒ€WIDEB€şB€³B€
B€ÿƒ€DIR€ƒ€DOC€ƒ€KEY€ƒ€IMP€ƒ€STO€Ã€UNIQUEB€B€ÀƒGET-KEYWORD-VALUE-PREREQÒƒ‚DE-NEST-KEYWORD-LISTÒC‚GET-KEYWORD-VALUE€ÒB€·ÀlRETRIEVE-€Àl€-€ÀªCONCATENATE€ÒªFIND-SYMBOL€ÒƒSYSTEM-INDEXÀ\€lINDEX-NAMEÀª€AND€ÀB€ÀB€`ÀB€ÜÒB€|ÒB€À,ERROR - À,… is not a defined index on the relation ÀB€ıÀF€ÀB€—ÒB€„Ò†€ÀB€PÒìˆERROR - There are no legal attributes contained in the PROJECT clause€À†€ÀƒEXTRACT-KEY€ÒF€ĞÀƒ‚CALCULATE-ATTRIBUTESÒÃUNIQUE-TUPLES€ÒB€¼ÒB€ªÒB€ÏÒB€ÒB€yÒB€aÒÃRETRIEVE-INTO€Òƒ‚UNCONVERT-ATTRIBUTESÒ‚SYSTEM-RELATION€À\€¬CARDINALITY€ÀQTRIEVE€ÒC‚PRINTREL-INTERNAL*ÒpÀ,€ì€G7239€ÀF€‡Àê€ERROR€ÀpÀB€\ìERRSET-HANDLERÀiEXPOSED-P€À©APPEND-ITEM€Àì‚~s tuple~:P retrieved€ÀB€=Òl€ €À,ƒ~%~s tuple~:P retrieved€Àì€ tupleÀe€sÀjWRITE-CHARÒl retrieved€ä€ä€QŠ€ÁæR€QPÿÛš@Á@æ	ä€Pˆ€QˆPˆPˆR@S€Á@W@Á@SAÁ@WBÁ@[CÁ@QBDÁ@UBEÁ@YBGÁ@QBBUÁäPQ’üQŠÁPQ’XÁJXQŒCVÁ0ä.äP PP!PP"ªP#’[Á$PP%P
P&P'P(P€Q)Š*š'P+PVQ)Š*š*šÿÛ$PJ[»BVÁæä€,PˆJXQŒCˆ-Pˆ€QˆRX[âÿİLÁXQBMÁJXQŒCPÁJXQŒCNÁ	JXQŒCSÁJXQŒCOÁæMİSä.PæMÛSæMäJXQŒCJXQŒC/P0PARÁYÁæR
JXQŒCJXQŒC/P1PAWÁTÃæJW'äR€QXWJXQŒCTQâWQAQGQ2P3PAHÁFÁQÁKÁäKæä€4PˆRÿÛKäR€QAQCQBQEQLQJXQŒC5P6PACÁEÁIÁZÁP PDQ!PEQ"ªP#’[Á€QAQKQCQLQIQZQJ[»@ÁKQF+	æ@QKQFQ7P8PAFÁKÁ@ÁJXQŒä@ä@Q9Š@ÁTä@
ä@QTQKQFQAQBQ:²@ÁæRWä@Eä@QWQFQ;šôı@>äHÛXW<ŠâAQFÁF5æFQ*ŠFÁFQ\Áä\Sÿ5ä\QB)ŠAQ'P=˜æHQ\QB)Š
ü\Sÿ5
æ\SAQ'P=˜äHQ\S*Š>’HÁ\ÅâçKæä€4PˆRKQH+æ@QKQHQ?š@ÁHQKÁXSŠJÁä€QJQ@QKQFQCQDQEQQQQAQJ@¸æRXäRN	ä@Q]ÁDä€]Sˆ]Åûç>üMæPæO8äKQAŠKÁJäJXQŒCâQQQÁJQâ€Q@QKQRQRQOQPQKQQQŠC™æQQ
üQQ	PÿÛKQŠCQQŠCÿcC>’ÿİÿİBPPCPP'P(PJQâ€Q)Š*šDªBMQSQJE¸SHæDäFPGPTHPIPFPÿÛJC^ÃPJC_ÃÖJPŠ*ŠJ!BJ!B\äP_ÁKPÿÛLP@QŠCMš_‘KPNPüOäOQˆOQOP@QŠCM˜ü€€@QŠCˆPPˆ@QŠCæQPRˆSPˆJQâ€ÿSä@€O€rB€ë€€ƒ‚RETRIEVE-FLAVOR-HASH€ë€†€ÀF€
À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€s\€B€`B€ØB€aB€BB€èÃKEY-VALUE-LISTB€B€:B€:B€:€ì€flavorÀÃRETRIEVE-HASH€’€QQ‚QƒQ„Q…QP†QJ¼O€B€s€€ƒ‚RETRIEVE-FLAVOR-HEAP€ë€†€ÀF€	À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€€\€B€`B€ØB€aB€BB€èÃHEAP-TRAVERSALB€B€:B€:B€:€ƒ‚QTRIEVE-FLAVOR-HEAP€’†ä†Qü€QQ‚QƒQ„Q¬O€‹B€€€€B€~€ë€2~†€B<F€LÀ$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€~\€B€`B€ØB€aB€BB€èB€ÈB€CB€B€:\€B€:B€:pÀB€\l‚LEX-PARENT-ENV-REGpÀB€\ìLEX-ENV-B-REG€pÀB€\ì‚LEX-CURRENT-VECTOR-REGpÀB€\¬‚LEX-ALL-VECTORS-REG€‚RETRIEVE-BUCKET€ÃHASH-RELATION€ƒ‚TEMP-ATTRIBUTE-LIST€ƒ‚CONV-ATTRIBUTE-LIST€CTUPLE-LISTCKEY-VALUE%B€:B€:B€:\€€o\€B€tB€vB€qB€w©‚INTERNAL-FEF-OFFSETS\€F€i„VARIABLES-USED-IN-LEXICAL-CLOSURES\€B€¡€ª€EVALÒƒENTRY-POINT€Àƒ€GETPÒ\€F€F€
À\€)INTERNALB€~€À*MAPHASH€Ò*GETHASH€ÒB€yÒB€ÜÒì€FLAVORÀpÀB€\ìSTRING-EQUAL*€ÒÃ‚PROJECT-FLAVOR-PREREQ€Òƒ‚FAST-PROJECT-FLAVOR€Òì€STRUCTÀB€·ÀB€·ÒB€KÒB€\Òƒ‚FAST-PROJECT-STRUCT€ÒF€ÀC‚EVAL-WHERE-PREREQ€Ò‚FAST-EVAL-WHERE€ÒB€a’„5æ„Qˆä„İüR‡QP’GÁ…æPPÓCGQü…QKÁäJQKSGQ	’
’JÁKÅøç†QŠPäQŠIÁJQIQ’ü†QŠPäLÑQNÁMÁ
üMQP€QŠNSšCMÃÁNÅNôçLQŠIÁJQIQ’JÁ„Q±æ„QQ€QPPAHÁ„ÁJQ„QHQšJÁJäJQQ‚QšJÃÿÛ
’FÁFO€¿B€~€€B€°€ë€
†€@ŒF€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€°\€KEY-VAL€Ã€TUPLESB€:\€B€:B€:B€–\€B€o\€B€wiƒLEXICAL-PARENT-DEBUG-INFO€B€‘€B€y’QÀP’ÀÂÿO€ÎB€°€€B€»€ë€#†€@œF€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€»\€B€ÉB€ØB€:\€B€:B€:B€:B€³B€:B€:B€:\€B€o\€B€qB€tB€v€@Ñ€QBÁAÁüAQBSCÁDÛDÑQFÁEÁüEQCQFSÿ‹CEÃÁFÅFöçDQCAÃÁBÅBæç@O€ÚB€»€€C‚RETRIEVE-LIST-AVL€€ë€†€ÀF€
À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€Û\€B€`B€ØB€aB€BB€èB€|B€B€:B€:B€:€¬€LISTÀƒRETRIEVE-AVL’€QQ‚QƒQ„Q…QP†QJ¼O€æB€Û€€ƒ‚RETRIEVE-FLAVOR-AVL€€ë€†€ÀF€
À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€ç\€B€`B€ØB€aB€BB€èB€|B€B€:B€:B€:€ì€FLAVORÀB€å’€QQ‚QƒQ„Q…QP†QJ¼O€ñB€ç€€ƒ‚RETRIEVE-STRUCT-AVL€€ë€†€ÀF€
À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€ò\€B€`B€ØB€aB€BB€èB€|B€B€:B€:B€:€ì€STRUCTÀB€å’€QQ‚QƒQ„Q…QP†QJ¼O€üB€ò€€C‚RETRIEVE-LIST-HASH€ë€†€ÀF€
À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€ı\€B€`B€ØB€aB€BB€èB€|B€B€:B€:B€:€¬€LISTÀB€~’€QQ‚QƒQ„Q…QP†QJ¼O€B€ı€€C‚RETRIEVE-LIST-HEAP€ë€†€ÀF€	À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€\€B€`B€ØB€aB€BB€èB€ B€B€:B€:B€:€C‚QTRIEVE-LIST-HEAP€’†ä†Qü€QQ‚QƒQ„Q¬O€B€€€ƒ‚RETRIEVE-STRUCT-HASH€ë€†€ÀF€
À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€\€B€`B€ØB€aB€BB€èB€|B€B€:B€:B€:€ì€STRUCTÀB€~’€QQ‚QƒQ„Q…QP†QJ¼O€B€€€ƒ‚RETRIEVE-STRUCT-HEAP€ë€	†€ÀF€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€\€B€`B€ØB€aB€BB€èB€ B€B€:B€:B€:€B€«ÀB€¬Òƒ‚QTRIEVE-STRUCT-HEAP€’†	ä€QQ‚QƒQ„Q†QP’´€QQ‚QƒQ„Q¬O€(B€€€ÃSELECT-TUPLES€€ë€	†€à	@F€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€)\€B€`ê€&REST€B€ôª€&KEYj&OPTIONAL€jDIRECTORY€êDOCUMENTATION€B€=ƒ‚IMPLEMENTATION-TYPE€B€üB€Bê€NUMBERB€<B€ıB€şB€B€
B€ÿC‚STORAGE-STRUCTURE€B€ÉB€EB€;B€?j‚&ALLOW-OTHER-KEYS€B€:\€B€ôB€5B€6B€=B€7B€üB€BB€8B€<B€ıB€şB€B€
B€ÿB€9B€ÉB€EB€;B€?\€éDOCUMENTATION€ì¿‡Same as Retrieve except that all attributes are retrieved.

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
   UNIQUE               - If T, only unique tuples are retrieved.À†€“ €\€iDIRECTORY€B€=é€FORMAT©‚IMPLEMENTATION-TYPE€©€INTO©€KEY€é€NUMBERé€OUTPUTé€PRINT€é€QPRINTiQUICK-SORT©€SORTé€STREAMi‚STORAGE-STRUCTURE€é€TUPLESé€UNIQUEé€WHERE€©€WIDEÀpÀB€\ìSTORE-KEYARGS€ÒB€:Àê€LIST*€ÒRETRIEVE’@ä@QPÿİAÑ €QPÿÛ@Qš”O€VB€)€€B€c€ë€$XÔ†€A$0F€|À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€c\€B€ÉƒSORT-CLAUSE€B€ØƒDOMAIN-LIST€B€:\€AVL-TREEÃ€DOMAINB€çB€|ƒNEW-ELEMENT€B€`CSORT-ORDERÃSORT-ELEMENT%€‚ALREADY-SORTED-PƒSORT-ELEMENTB€ÉB€|\€B€o\€B€qB€w€B€(ÑB€‘ÑCƒ*PROVIDE-WARNING-MESSAGES*‘B€|ÒB€’ÒB€ÀB€ÒB€yÒ\€,NUMBERP€,STRINGP€¬€ATOMÀB€Ò\€¬€ASC€l€GT¬€GTE€l€GElINCREASING¬€DES€¬€DESClDECREASINGl€LT¬€LTE€l€LEÀB€“ÒlWARNING - ÀB€•ÒB€§Òl† is not an attribute nor a recognized sort keywordÀì„          This element will be ignoredÀ,‡ERROR - No attributes specified in the sort clause --> €À¬ƒ        Sort can not proceedÀB€aÒB€·Àì€-TEMP-Àê€GENSYMÒB€·ÒB€KÒ*‚READ-FROM-STRINGÒê€APPENDÒ‚INSERT-AVL-LIST€ÒB€«Àƒ€PUTPÒB€ÛÒ\€¬€DES€¬€DESClDECREASINGl€LT¬€LTE€l€LEÀB€Í’Q±äÛ‚QBÁü5æäQŠüSÿ5äSÁQIÁ=äISÿİ’GÁæRGQ‚QP	˜äƒQŠCGQ‚QP	šŠCÿcƒQŒCAÁBQGQŠ
’BÁAQPP	˜æ€QQ‚QšÿGQPP	˜äFQâGQFÁü
ä€PˆGQˆPˆ€PˆIÅÃçHäHBæä€PˆQˆ€PˆR€Q‚QBQšCÁƒQŠ‚QBQšBƒÁPPP‚Š¢ŠEÁ€QJÁCQKÁüJSŠJŠÿÛŠÿÛŠš
CDÃ@QKSBQ‚QƒQÿÛEQJº@ÁJÅKÅJççEQ@QP ˜EQ‚Q‚QBQÿİÿÛEQJ!º€ÁEQÿÛP ˜FQ"PP	˜ä€Q#Š€Á€O€B€c€€MAPTUPLE€ë€N†€@F€0À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€\€CDBFUNCTIONRELATIONB€:B€:\€B€o\€	B€qB€tB€vB€wB€B€B€B€B€B€=ì”Map a function on all the tuples in a relation using MAPCAR.

   DBFUNCTION  - Function to be applied to each and every tuple.
   RELATION    - Name of the relation.€€B€ ÑB€‘‘B€-ÒpÀB€bì€G7595€ÀF€>ÀB€fÀB€hÀjFUNCTIONP€ÒB€|ÒB€“Ò¬„ERROR - Illegal function definition€ÀB€•ÒB€’ÒB€ÉÀB€U’€æRPPTP	PPÿİJC@ÃPJCAÃÖ€Q
ŠŠJ!BJ!B\æä€PˆRQŠÁæRAÛAÑQPÿİšCÁBÁüBQCS€‹CBÃÁCÅC÷çAO€¢B€€€ƒ€MAPT€ë€F†€@ˆF€,À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€£\€B€˜B€™B€:B€:\€B€o\€B€qB€tB€wB€B€B€B€B€B€=¬”Map a function on all the tuples in a relation using MAPC.

   DBFUNCTION  - Function to be applied to each and every tuple.
   RELATION    - Name of the relation.€€B€ ÑB€‘‘B€-ÒpÀB€bì€G7635€ÀF€>ÀB€fÀB€hÀB€ ÒB€|ÒB€“Ò¬„ERROR - Illegal function definition€ÀB€•ÒB€’ÒB€ÉÀB€U’€æRPPTP	PPÿİJC@ÃPJCAÃÖ€Q
ŠŠJ!BJ!B\æä€PˆRQŠÁæRQPÿİš@Áä@S€ @ÅüçO€³B€£€€ÃPRINT-RELATION€ë€†€à@F€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€´\€B€™B€2B€ôB€3B€4B€5B€6B€=B€7B€B€üB€BB€8B€<B€ıB€şB€B€
B€ÿB€9B€ÉB€EB€?B€:B€:\€B€ôB€5B€6B€=B€7B€B€üB€BB€8B€<B€ıB€şB€B€
B€ÿB€9B€ÉB€EB€?\€B€=ì¿
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
   UNIQUE               - If T, only unique tuples are retrieved.À†€“ €\€B€AB€=B€BB€CiINDEX-NAMEB€DB€EB€FB€GB€HB€IB€JB€KB€LB€MB€NB€OB€QÀB€SÒB€U’@ä@QPÿİAÑ €Q@Q”O€ÃB€´€B€´O€´PRINTREL€B€:€ë€
†€à
@F€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€:\€B€`B€2B€ôB€3B€4B€5B€6B€=B€7B€B€üB€BB€8B€<B€ıB€:B€şB€B€
B€ÿB€9B€ÉB€EB€?B€:B€:\€B€ôB€5B€6B€=B€7B€B€üB€BB€8B€<B€ıB€:B€şB€B€
B€ÿB€9B€ÉB€EB€?\€B€=ì¿ªSame as Retrieve except that all tuples are retrieved.

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
   UNIQUE               - If T, only unique tuples are retrieved.€À†€” €\€B€AB€=B€BB€CB€ÂB€DB€EB€FB€GB€H)PROJECT€B€IB€JB€KB€LB€MB€NB€OB€QÀB€SÒB€;ÀB€EÀB€TÒB€U’@ä@QPÿİAÑ €QPÿİPÿİ@Qª	”O€ÓB€:€€CATTR-CALC€€ë€F†€@˜F€)À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€Ô\€CCALC-LIST€B€ØB€:\€ÃNEW-CALC-LIST€‚NEW-CALC-ELEMENTÃ€RESULTÃRESULT-ELEMENTB€:C€X€\€B€o\€B€qB€tª€PUSHB€w€B€ ÒB€ÜÒB€ÀB€ÒB€ƒÒF€ÀB€ÔÒB€yÒB€|’€Qˆä€QŠQP˜+ä€ğ€7ä€QŠAÃQP˜ äAQCBÃBÁAQŠü€5ä€QDÁäDSEÃQP	PAAÁCÁBQCQ
’BÁ@QAQŠ
’@ÁDÅíçü€Q@ÁBQ@Q‚O€èB€Ô€€B€º€ë€)d†€@”F€;À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€º\€B€B€gB€:\€B€áB€:B€:B€:Ã€ATTR%€\€B€o\€ê€PROG2€B€wB€qB€tB€v€B€k‘B€’Ò\€l€LT¬€LTE€l€LElDECREASING¬€DESC¬€DES€l€GT¬€GTE€l€GElINCREASING¬€ASC€ÀB€ÀB€Ò\€l€LTl€LE¬€LTE€lDECREASING¬€DESC¬€DES€ÀÃ€DBGTP€ÀB€|ÒB€yÒ\€l€GTl€GE¬€GTE€lINCREASING¬€ASC€ÀB€“ÒlWARNING - ÀB€•ÒB€§Ò,‡ is neither a valid quick-sort keyword nor an attribute.€AÑ€QCÁBÁJüBQDÛCSÿİ’DÁæ@ÛRDQPPš8âCWÿİ’PP˜äDQQP˜ä@QDQ	P
’
Š’@Ã$üCWÿİ’PP˜äDQQP˜ä@QDQíıD5äDQŠCäDQQP˜òçä€PˆDQˆPˆÿÛCBÃÁCÅC´ç@O€B€º€€B€U€ë€†€à@F€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€U\€B€`B€2B€ôB€3B€4B€5B€6B€=B€7B€B€üB€BB€8B€<B€ıB€:B€şB€B€
B€ÿB€9B€ÉB€EB€;B€?B€:B€:\€B€ôB€5B€6B€=B€7B€B€üB€BB€8B€<B€ıB€:B€şB€B€
B€ÿB€9B€ÉB€EB€;B€?\€B€=ì¿ÒRetrieve some tuples from a relation satisying a where clause.

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
   UNIQUE               - If T, only unique tuples are retrieved. À†€• €\€B€AB€=B€BB€CB€ÂB€DB€EB€FB€GB€HB€ÒB€IB€JB€KB€LB€MB€NB€OB€PB€QÀB€SÒB€ë’@ä@QPÿİAÑ €Q@Q”O€ B€U€€B€~€ë€ií†€AhF€„À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€~\€B€`B€dB€aB€eB€bB€:\€
B€gNEW-ATTRB€hB€k‚PROJECT-ELEMENT€ÃNEW-ATTR-PART2B€gB€bB€:B€l\€B€o\€€tB€B€B€æB€qB€w€B€%ÑB€k‘B€ÏÒB€ÀB€ÒB€’ÒB€“ÒlWARNING - ÀB€•ÒB€§Ò¬ƒ is not an attribute of the Àl relation€À,†WARNING - Improperly specified project element €Àe€.ÀB€pÒB€ÜÒl          À,ƒ is an attribute of the Àl relation.ÀF€ÀB€ÔÒB€yÒB€|ÒB€Í’‚QŠ@Á‚Û@QFÁ„QGÁªüFSAÁA5"æAQQP˜	äAQB]BÁAQ‚]‚ÁG‡ä“üA5æAQÿİää	€
PˆAQˆPˆ€QˆPˆ‚üA
æ~ä	€PˆASˆPˆvüASŠQP˜änä	€PˆAQˆPˆ	€PˆASˆPˆ€QˆPˆ[üAWÿ5*äGæASŠQP˜äQŠCASŠQPšŠCÿcÿkƒQŒCüPüGSC]CÁAWQPPAEÁDÃ‚Q’‚ÁASEQ’B]BÁ.üAWŠQP˜æAQB]BÁG æPüASAW’B]BÁAWŠAÃ‚]‚ÁGæAQQP˜äQŠCAQQPšŠCÿcƒQŒCüPüGSC]CÁFÅGÅFTçBQŠBÁCQŠCÁ‚Q@Á‚Û@QHÁäHSIÃ‚QP˜æIQ‚]‚ÁHÅõç‚QŠBQCQƒO€8B€~€€B€\€ë€	)†€„	PF€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€\\€B€ØB€4\€B€éC*PKG-NAME*B€:\€B€:B€:B€:B€m\€B€o\€B€qB€tB€v€B€B‘B€·Àl€:€ÀB€·ÒB€KÒB€ƒ’væPÁ@Ñ€QBÁAÁüAQBSCÁğC7äPQPCQŠ¢ŠüCQCAÃÁBÅBëç@O€GB€\€€B€Y€ë€L¨†€@øF€\À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€Y\€B€³B€aB€hB€:\€B€áELEMENT€B€:B€:B€:B€ãB€:Ã€%TUPLEATT-LISTVAL-LISTB€:B€:B€:B€l\€B€o\€ê€UNLESSB€B€B€qB€tB€vB€w€ê€EQUAL€ÀB€ãÀB€|ÒƒPARSE-WHERE€ÒB€\ÒjMAKUNBOUNDÒB€ƒÒB€·ÒB€ÜÒB€ÀB€ÒB€ªÒ*NREVERSE’BÑ‚QDÁCÁüCQDSEÁE5äEWÿ5	äESPPEWšŠB’üEQCCÃÁDÅDèçBQ‚ÁQŠDÛCÁCCäCSšCDS€DæCSˆCÅDÅóı€QFÁFäFSGÁQGQIÁHÁäHS	ŠISÈHÅIÅHøçJÛJÑ‚QLÁKÁ*üKQLSMÁM5æMQ
Š	ŠCüM
òMWŠQP˜äMWŠ	ŠüMWAÁMSæMòMS
Š	ŠAQŠÈBüAQŠCKÃÁLÅLÔçJQ@]@ÁFÅºçLÛLÑ‚QJÁFÁüFQJSEÁE5æEQüESŠCFÃÁJÅJñçLQÁ@QŠ@ÃQ‚QƒO€\B€Y€€B€[€ë€<šp†€B<øF€ÖÀ$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€[\€B€`B€üB€³B€aB€hB€øB€ùB€úB€bB€ôB€öB€:\€ƒ€TEMPB€gCOLD-VALUESB€iB€:B€lƒ€ATTDB€áC%ATTRIBUTEB€:B€:ATTR-RELCATTR-INTO€Ã€TEMP-D\€B€o\€B€B€B€B€qB€tB€B€w€ƒ‚*VALIDITY-CHECKING*€ÑB€‘ÑÃ‚*SYSTEM-ATTRIBUTE-KEY*ÑÃƒ*SYSTEM-ATTRIBUTE-ATTRIBUTES*€ÑB€#ÑB€$‘B€]À\€lATTRIBUTESÀB€ÀB€`ÀB€·ÒB€|ÒB€`Ò‚SYSTEM-ATTRIBUTEÀ\€,‚DOMAIN-FUNCTION€ìDEFAULT-VALUE€¬€DOC€ÀB€PÀÃATTRIBUTE-NAMEÀB€ÜÒƒ€DOM€ÀpÀB€T¬€DEF€ÀB€AÀƒ€ANYPÀB€yÒ\€B€BÀB€HÒB€ÒB€ÍÒB€CÀ\€B€CÀB€DÀ\€B€DÀB€BÀB€=À\€B€=ÀB€@À\€B€@À\€B€AÀÃ€DEFRELÒ\€,‚DOMAIN-FUNCTION€À¬€ANYPÀB€·ÒB€“Òì‚ERROR - The attribute ÀB€•ÒB€§Òì in relation €À¬‚ and the attribute €À,ƒ in the output relation Àl„ have different domain predicates.À¬ƒERROR - The output relation À¬  does not have all the attributes required to insert the retrieved tuples. €Àì€ has €Àì… as attributes and the retrieve call requires À¬ƒ attributes in the relation Àl‚ to be projected.€ÀB€ÉÀÃ€INSERT’	PP
PPPPQŠšªB@Á–æ„Q@ÁAÛ@QDÁHäDSEÁFÛPPPPPPP€QŠšPPE5æEQüEWÿ5æEWüÿÛŠššªBFÁäAQE5æEQüESPFSPFWPE5æF[üÿÛ²	üAQE5æEQüESPP’’’AÁE5äESŠüEQC]CÁDÅ¸çP Q’Bâ…QDÃHÏ	äHQCQP˜äHQGÁöıRGäP Q’Bâ…QüCQŠBŠ…ÁQAQPP Q’Bâ†Q P!P Q’Bâ‡Q"P…Q#P$P Q’BâˆQ%P&P Q’BP'P Q’BJº(˜æR@S@ÃŠC„QŠC|_ä„Q@QJÁIÁVüISJSLÁKÁMÛK5äKSKÁKQŠQP˜EäPP)PPPPP€QŠšPPKQŠššªBPP)PPPPPQŠšPPL5æLQüLSŠššªBMÃræMQ*P+æä,€-P.ˆKQ/ˆ0P.ˆ€Q/ˆ1P.ˆLQ/ˆ2P.ˆQ/ˆ3P.ˆRIÅJÅIäJ§çüä,€4P.ˆQ/ˆ5P.ˆQ/ˆ6P.ˆ@Q/ˆ7P.ˆƒQ/ˆ8P.ˆ€Q/ˆ9P.ˆRPP’BÁÚÚQ:P‚Q’;BSÀBWÀ€O€”B€[€€B€Z€ë€0†€@hF€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€Z\€B€ÉB€:\€
B€:B€:B€–B€˜B€šB€œƒRESULT-TABLEB€:B€ãƒHASH-BUCKET€\€€o\€B€qB€tB€wB€¥\€F€
B€¨\€B€É€©€TESTÀB€XÀ*‚MAKE-HASH-TABLE€ÒB€´ÒpÀB€\lMEMBER-EQLÒpÀB€T,PUTHASH€Ò\€F€†ÀÀ\€B€±B€Z€ÀB€³’PP’FÁ€QGÁäGSHÁIÛHQFQ’IÁäHQIQæHQHQI]üHQHQFQ˜GÅêç€Û	P
PÓCFQ€O€°B€Z€€B€®€ë€†€@ŒF€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€®\€B€ãC€Y€B€:\€B€:B€:B€–\€B€o\€B€wB€ÍB€š€€QÀ\ÀÂÿO€½B€®€1€\€pÀB€\,„FASL-RECORD-FILE-MACROS-EXPANDED\€B€8\€\€ê€DEFUN€†€'\€pÀB€T¬€DEFF†€b\€B€W†€6\€B€õ†€J=ø\€B€æ†€¥˜\€B€†€z(‡\€B€†€{šÍ\€B€†€:}n\€B€†€xõ¿\€B€†€Zió\€B€†€{Ä²\€B€†€2»=\€B€†€.Ù‹\€B€†€-i\€B€†€~Éz\€B€†€<p‘\€B€†€`sN\€B€†€|Äô\€B€	†€aM*\€B€w†€[æ„\€B€v†€(Ì¢\€B€t†€*ıj\€B€q†€=Ì#€€y soon.
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
G2€B€wB€qB€tB€v€B€k‘B€’Ò\€l€LT¬€LTE€l€LElDECREASING¬€DESC¬€DES€l€GT¬€GTE€l€GElINCREASING¬€ASC€ÀB€ÀB€Ò\€l€LTl€LE¬€LTE€lDECREASING¬€DESC¬€DES€ÀÃ€DBGTP€ÀB€|ÒB€yÒ\€l€GTl€GE¬€GTE€lINCREASING¬€ASC€ÀB€“ÒlWARNING - ÀB€•ÒB€§Ò,‡ is neither a valid quick-sort keyword nor an attribute.€AÑ€QCÁBÁJüBQDÛCSÿİ’DÁæ@ÛRDQPPš8âCWÿİ’PP˜äDQQP˜ä@QDQ	P
’
Š’@Ã$üCWÿİ’PP˜äDQQP˜ä@QDQíıD5äDQŠCäDQQP˜òçä€PˆDQˆPˆÿÛCBÃÁCÅC´ç@O€B€º€€B€U€ë€†€à@F€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€U\€B€`B€2B€ôB€3B€4B€5B€6B€=B€7B€B€üB€BB€8B€<B€ıB€:B€şB€B€
B€ÿB€9B€ÉB€EB€;B€?B€:B€:\€B€ôB€5B€6B€=B€7B€B€üB€BB€8B€<B€ıB€:B€şB€B€
B€ÿB€9B€ÉB€EB€;B€?\€B€=ì¿ÒRetrieve some tuples from a relation satisying a where clause.

   RELATION-NAME        - Name of the reLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540830. :SYSTEM-TYPE :LOGICAL :VERSION 2. :TYPE "XLD" :NAME "SAVE" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760360526. :AUTHOR "REL3" :LENGTH-IN-BYTES 6527. :LENGTH-IN-BLOCKS 13. :BYTE-SIZE 16.)  pp2€\€©COMPILE-DATA\€ì€SW-MFG,GODZILLA†€‡¤^ĞF€F€\€pÀ,COMPILER,VERSION€\€F€F€pÀB€),‚OPTIMIZE-SWITCH€†€©ƒQFASL-SOURCE-FILE-UNIQUE-ID€1€\€pÀl€FSì‚MAKE-FASLOAD-PATHNAME€\€ê€QUOTE€B€$\€B€8ª€NIL€\€B€8\€¬€RTMS\€B€8¬€SAVE\€B€8¬€LISP\€B€8F€©€BASEF€
é€FONTS€\€©*CODE-FONT*€é*COMMENT-FONT*é*STRING-FONT*€)PACKAGE€©€RTMS©€MODE©COMMON-LISP€€‚SAVE-FLAVOR-HASH€ë€	F€ÀF€À$€ÀB€:pÀ¬€TICLì€ART-Q€]€F€€:B€:B€:j€T€F€pÀ¬€SYS€l‚DEBUG-INFO-STRUCT€B€P\€RELATION*PATHNAMEÃREL-DEFINITIONB€:B€:\€)‚MACROS-EXPANDED€\€pÀB€\¬XR-BQ-LIST*€pÀB€\lXR-BQ-LIST€ª€SETFÀƒ€VAR1ÀB€8ÀƒENTRY-POINT€Àƒ€GETPÒª€LISTÒƒ€PUTPÀ\€B€k\€B€8B€lÀê€LIST*€ÒpÀB€Tl‚DUMP-FORMS-TO-FILE’Q‚QPPP€QP’’š	PP€Q’
Pšš”O€uB€P€€‚SAVE-FLAVOR-HEAP€ë€	F€ÀF€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€v\€B€`B€aB€bB€:B€:\€B€d\€B€gB€i€B€jÀB€kÀB€8ÀB€lÀB€mÒB€nÒB€oÀ\€B€k\€B€8B€lÀB€rÒB€t’Q‚QPPP€QP’’š	PP€Q’
Pšš”O€ƒB€v€€‚SAVE-FLAVOR-AVL€€ë€	F€ÀF€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€„\€B€`B€aB€bB€:B€:\€B€d\€B€gB€i€B€jÀB€kÀB€8ÀB€lÀB€mÒB€nÒB€oÀ\€B€k\€B€8B€lÀB€rÒB€t’Q‚QPPP€QP’’š	PP€Q’
Pšš”O€‘B€„€€ÃSAVE-LIST-AVL€€ë€	F€ÀF€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€’\€B€`B€aB€bB€:B€:\€B€d\€B€gB€i€B€jÀB€kÀB€8ÀB€lÀB€mÒB€nÒB€oÀ\€B€k\€B€8B€lÀB€rÒB€t’QPPP€QP’’š‚Q	PP€Q’
Pšš”O€ŸB€’€€ÃSAVE-LIST-HASH€ë€	F€ÀF€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€ \€B€`B€aB€bB€:B€:\€B€d\€B€gB€i€B€jÀB€kÀB€8ÀB€lÀB€mÒB€nÒB€oÀ\€B€k\€B€8B€lÀB€rÒB€t’QPPP€QP’’š‚Q	PP€Q’
Pšš”O€­B€ €€ÃSAVE-LIST-HEAP€ë€	F€ÀF€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€®\€B€`B€aB€bB€:B€:\€B€d\€B€gB€i€B€jÀB€kÀB€8ÀB€lÀB€mÒB€nÒB€oÀ\€B€k\€B€8B€lÀB€rÒB€t’QPPP€QP’’š‚Q	PP€Q’
Pšš”O€»B€®€€‚SAVE-STRUCT-AVL€€ë€	F€ÀF€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€¼\€B€`B€aB€bB€:B€:\€B€d\€B€gB€i€B€jÀB€kÀB€8ÀB€lÀB€mÒB€nÒB€oÀ\€B€k\€B€8B€lÀB€rÒB€t’Q‚QPPP€QP’’š	PP€Q’
Pšš”O€ÉB€¼€€‚SAVE-STRUCT-HASH€ë€	F€ÀF€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€Ê\€B€`B€aB€bB€:B€:\€B€d\€B€gB€i€B€jÀB€kÀB€8ÀB€lÀB€mÒB€nÒB€oÀ\€B€k\€B€8B€lÀB€rÒB€t’Q‚QPPP€QP’’š	PP€Q’
Pšš”O€×B€Ê€€‚SAVE-STRUCT-HEAP€ë€	F€ÀF€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€Ø\€B€`B€aB€bB€:B€:\€B€d\€B€gB€i€B€jÀB€kÀB€8ÀB€lÀB€mÒB€nÒB€oÀ\€B€k\€B€8B€lÀB€rÒB€t’Q‚QPPP€QP’’š	PP€Q’
Pšš”O€åB€Ø€ÃSAVE-DATABASE€O€æSAVE-DB€€B€æ€ë€<Xì†€`<|F€”À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€æ\€ÃDATABASE-NAME€ê€&REST€ƒKEYWORD-LISTª€&KEYj&OPTIONAL€jDIRECTORY€j‚&ALLOW-OTHER-KEYS€B€:\€\€B€òB€:pÀB€\l‚LEX-PARENT-ENV-REGpÀB€\ìLEX-ENV-B-REG€pÀB€\ì‚LEX-CURRENT-VECTOR-REGpÀB€\¬‚LEX-ALL-VECTORS-REG€B€õƒ€KEYSTEMP-DIRB€aCERROR-FLAGB€:B€òB€:B€:\€B€d\€pÀB€Tl‚CONDITION-BIND-IF€pÀB€TìCONDITION-BINDpÀB€Tì‚CATCH-CONTINUATION-IF€pÀB€Tl‚CATCH-CONTINUATIONpÀB€Tì€ERRSETª€PROGB€iB€j©‚INTERNAL-FEF-OFFSETS\€F€8i„VARIABLES-USED-IN-LEXICAL-CLOSURES\€ƒExit block SAVE-DATABASEB€éDOCUMENTATION€ì˜Save all system relations and the user-defined, modified relations.

   DATABASE-NAME    - Name of the database to be saved.
   DIRECTORY         - Name of the directory in which it is to be saved.€€pÀl€EH¬‚*CONDITION-HANDLERS*ÑCƒ*PROVIDE-STATUS-MESSAGES*€ÑC‚*ENVIRONMENT-NAME*Ñƒ*DATABASE-DOCUMENTATION*Ñƒ„*SYSTEM-RELATION-STORAGE-STRUCTURE*€ÑÃ„*SYSTEM-RELATION-BASE-IMPLEMENTATION*€Ñ‚*SAVE-DIRECTORY*Ñƒ*PROVIDE-ERROR-MESSAGES*Ñƒ*ACTIVE-DB*€ÑÃƒ*TRANSACTION-FORMS-POSTPONED*€Ñ‚*TRANSACTION-ON*‘\€iDIRECTORY€ÀpÀB€\ìSTORE-KEYARGS€ÒF€&ÀjCOPY-LIST€Ò‚ACTIVE-DATABASE€ÒƒVALIDATE-SYMÒB€æÀB€8ÀB€nÒpÀB€\,*APPEND€Òê€TERPRIÒ,ˆERROR - Only the current database may be (or needs to be) saved.ÀªWRITE-STRINGÒ,„        The current database is Àê€PRIN1€Ò\€ƒ€DIR€ÀƒGET-KEYWORD-VALUE-PREREQÒê€STRINGÀÃGET-DIRECTORY€Ò¬€.XLDÀªCONCATENATE€Ò‚SYSTEM-RELATION€À\€lMODIFIEDP€À\€B€YÀ‚DELETE-OR-MODIFYÒ\€ìSAVE-DIRECTORYÀpÀ,€ì€G8924€ÀF€ïÀê€ERROR€ÀpÀB€\ìERRSET-HANDLERÀpÀB€4ìDIRECTORY-LISTÒB€jÀB€ ÀB€À‚DEFINE-DATABASE*ÀB€6Àƒ€DOC€Àƒ€ENV€ÀB€tÒl‚ERROR - Directory À,‚ does not exist€À\€F€†€€F€
À\€)INTERNALB€æ€Àƒ€MAPTÒ¬€The À¬ƒ database has been saved in €@ä@QPÿİFÑ KÑKÃPÿÛU@QŠGÁ€æÿÛšü€Qÿİ’€ÁùåäPPP€Q’PGQ’šŠ’À…üP€+æ
åå€Pˆ€PˆPˆÜı@QüLSLÁäLSÿ5úçLQ@ÁP@Q’@ÁP@Q ŠHÃ€Q!P"¢IÁ	PH+æ#Pÿİÿİ$P%P&¨#Pÿİÿİ'PHQŠ&¨HQ	À(P)PT*P+P(PÿÛJCMÃPJCNÃÖHQ,ŠŠJ!BJ!Bÿ\ü\ÿäIQ-P.PPš-P/PPš0P€Q1PHQ2PP3PP²šš4
ü
å€5PˆHQˆ6Pˆwı7P8PÓC#P9	ä€:Pˆ€Qˆ;PˆHQˆJeç€QJ\PO€[B€æ€€B€U€ë€W†€@\F€8À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€U\€Ã€TUPLE€B€:\€B€:B€:B€úTEMP-RELB€B€:B€:\€B€d\€B€B€	B€B€B€ê€UNLESSB€jiƒLEXICAL-PARENT-DEBUG-INFO€B€í€B€ÑB€"ÑC*PKG-NAME*Ñƒƒ*SYSTEM-RELATION-ATTRIBUTES*‘B€nÒ\€ìRELATION-NAME€ìSAVE-DIRECTORYÀƒPROJECT-LISTÒB€8ÀB€;Ò*‚READ-FROM-STRINGÒpÀB€Cì€G8933€ÀF€[ÀB€GÀB€IÀB€KÒB€0Òl‚ERROR - Directory ÀB€2ÒB€4Ò,‚ does not exist€ÀB€6ÀÃSAVE-RELATION€’€QŠPP	šBDÁ
PPDSšŠCÁDWDÁPPTPPPÿÛJCEÃPJCFÃÖDQŠŠJ!BJ!Bÿ\ü\ÿæä€PˆDQˆPˆÁÜÀPÿÛXCQPDQœO€xB€U€‚SAVE-ENVIRONMENTO€ySAVE-ENV€B€y€ë€:Uä†€`:hF€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€y\€ENVNAME€B€ñB€òB€óB€ôB€õB€öB€:\€
B€òB€õB€aB€6B€REL-IMP€REL-STO€B€òB€:B€:\€B€d\€	B€B€	B€B€B€B€iB€iB€B€jB€l‘Save an environment.

   ENVNAME   - Name of the environment to be saved.
   DIRECTORY - Name of the directory in which it is to be saved.€B€ÑCƒ*PROVIDE-WARNING-MESSAGES*Ñƒ‚*VALIDITY-CHECKING*€ÑB€ÑB€ ÑB€Ñƒ‚*PARAMETER-CHECKING*ÑB€!ÑÃ‚*AUTO-SAVE-RELATIONS*€ÑCƒ*RELATION-IMPLEMENTATION*€ÑB€"ÑB€$ÑB€%‘\€B€'ÀB€)ÒB€-ÒB€+ÒB€yÀB€8ÀB€nÒB€/Ò\€B€6ÀB€7ÒB€8ÀB€9Òl‚rtms-environment-€À¬€.XLDÀB€;ÒpÀB€Cì€G9006€ÀF€ÃÀB€GÀB€IÀB€KÒB€0Ò¬ERROR - The ÀB€2ÒB€4Òlƒ directory does not exist€Àl€-€ÀpÀB€\,SEARCH*€Òê€SUBSEQÒF€ÀC‚DEFINE-ENVIRONMENTÀ\€B€8CAUTO-SAVE€À\€B€8B€õÀ\€B€8Ã€ERRORSÀ\€B€8ƒ€PARAÀ\€B€8B€…À\€B€8B€†À\€B€8Ã€STATUSÀ\€B€8SYS-IMP€À\€B€8SYS-STO€À\€B€8VALIDITYÀ\€B€8WARNINGSÀB€t’@ä@QPÿİAÑ €Qÿİ’€ÁæR@QüGSGÁäGSÿ5úçGQ@ÃŠDÁäPPP€Q’PDQ’šŠ’À€P@Q’@ÁP@QŠCÃP€QPªBÁP PT!P"PPÿÛJCHÃPJCIÃÖCQ#ŠŠJ!BJ!Bÿ\ü\ÿ
æä$€%P&ˆCQ'ˆ(P&ˆRPJ)PP*’+šEÁP)PP*’ÿk+’FÁÿİ,PJUBQ-PP€Q’.PPP’/PP
P’0PPP’1PP	P’2PPEQ’3PPFQ’4PPP’5PPP’6PPP’7PPP’8PPP’JºŠ9]Z€O€±B€y€B€wO€wSAVE-REL€B€w€ë€^°¾†€à^@F€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€w\€	ÃRELATION-NAME€B€ñB€òB€óB€ôB€õƒ€SAVEª€TYPEB€öB€:\€\€B€òB€:B€úB€üB€şB€B€õB€¼B€½ÃINSERT-ROUTINEB€aTEMPLISTB€ON-DISK?ƒ€MODPB€6CATTRIBUTESƒ€IMP€C€SSƒTEMP-MESSAGEƒ€KEY€ƒTUPLE-FORMATB€Mƒ€TEMPƒQTRIEVE-VAR€CSAVE-TYPE€ƒALWAYS-SAVE€B€B€òB€:B€:\€B€d\€B€B€	B€B€B€B€iê€FIRST€*SEVENTH€ê€SIXTH€ê€FIFTH€ê€FOURTHê€THIRD€ê€SECONDB€B€iB€jB€\€F€SB€\€B€ÊB€,¯Save a relation if it is modified.

    RELATION-NAME - Name of the relation to be saved.
    DIRECTORY     - Name of the directory in which it is to be saved.
    SAVE          - If T, saves the relation even if the relation is not modified.
    TYPE          - Two types of save are allowed: COMMAND and XLD. This keyword can be used to
                    specify the type.À†€Ÿ €B€ÑC‚*SYSTEM-RELATIONS*ÑB€ÑB€kÑB€#ÑB€"ÑB€$ÑB€%‘\€B€'©€SAVE©€TYPEÀB€)ÒB€,ÒB€-ÒB€+ÒB€wÀB€8ÀB€nÒB€/Ò\€B€½B€6B€¼ÀB€7Ò\€
ìRELATION-NAME€lMODIFIEDP€ìSAVE-DIRECTORYlATTRIBUTES¬‚IMPLEMENTATION-TYPE€l‚STORAGE-STRUCTURE€¬€KEY€¬TUPLE-FORMAT¬€DOC€¬€DISKÀƒGET-RELATIONÒB€0Òì‚ERROR - The relation €ÀB€2ÒB€4Ò,„ is not defined in the database À\€B€½ÀC‚GET-KEYWORD-VALUE€Òƒ€XLD€À\€B€¼ÀÃSAVEREL-QFASL€ÒCOMMAND€À,ERROR - À,„ is an unrecognized save type .€ÀB€9ÒpÀB€Cì€G9052€ÀF€XÀB€GÀB€IÀB€KÒl‚ERROR - Directory À,‚ does not exist€ÀB€<ÀªSTRING-EQUALÀB€8ÀìRELATION-NAME€ÀB€;ÒêSTRING-UPCASE€Ò\€ìSAVE-DIRECTORYÀB€@Ò\€B€ü\€B€;\€B€8B€8B€kìRELATION-NAME€,‚SYSTEM-RELATION€À\€lMODIFIEDP€À\€B€YÀl€-€Àl€.€ÀB€8ÒìThe relation €Àì‚ has not been modifiedÀB€6ÀÃLOAD-RELATION€ÒF€ÀiDIRECTION€Àé€OUTPUTÀª€OPENÒìƒ~&(SETF *non-qfasl-restore* T)Àê€FORMATÒpÀB€\¬MEMBER-TEST€Ò¬ƒ~&(DEFREL-RESTORE ~S ~S ~S)€ÀB€ÅÀCMODIFIEDP€Àƒ€STO€ÀB€ÈÀB€ÉÀB€MÀ¬€hashÀpÀB€\ìSTRING-EQUAL*€Ò,†~&(PUTP '~S (make-hash-table :test 'equal) '~S)€ÀB€lÀ\€F€F€À\€B€VB€w€ÀB€XÒ,INSERT-€ÀB€qÒì‚~&(~S '~S '~S '~S '~S)À*REVERSE€Ò,„~&(SETF *non-qfasl-restore* NIL)Àê€CLOSE€Ò\€B€:ÀÃ‚SAVE-SYSTEM-RELATIONS€Òl„ has been saved in the directory €€@ä@QPÿİFÑ €æR€Qÿİ’€ÁæR@QŠLÁä
PPP€Q’PLQ’šŠ’
À€@Qü\S\Áä\Sÿ5úç\Q@ÁP@Q’@Á€QPÿİÿİ¢BXÁæ		ä€Pˆ€QˆPˆPˆRXWNÁX[OÁXQBPÁXUBQÁXYBRÁXQBBTÁJXQ
CUÁUWVÁU[MÁUSUÁP@Q’Bâ PYÁ!P@Q’BZÁYQ &ä€Q@Q"”YQ#&
æ	ä€$PˆYQˆ%PˆR@QOQ&’[Á'P(PT)P*P'PÿÛJC]ÃPJC^ÃÖ[Q+ŠŠJ!BJ!Bÿ\ü\ÿ
æ	ä€,Pˆ[Qˆ-PˆR[QO+æ.Pÿİ/P0PP1P2š€Q3Šš4P[QŠ5¨.Pÿİ6P7P8P5¨0P[QP9P€Q:PYQ;ŠJ2ºJÁKÛNæZæ[QO+
ää€<Pˆ€Qˆ=Pˆ€Mä€Q>POQ?˜ÿİ@PJUJQAPBPCšJÃDPE€QP/PF˜æJQGP€QPQHPQQIPÿÛJPRQKPTQLPUQMPVQ>POQJºE¨€QP/PF˜	æRQNPOäJQPP€QQPE WÛRPSPÓC€QTWä0PUPQQ9PRQ2ªVŠIÁJQWPIQ€QPQWQXŠTQ€QJE¸JQYPEJQZˆ.Pÿİ/P0PP1P2š€Q3Šš7P[P5¨€QP/PF˜æPSÁÚ\€SQÀ	ä€<Pˆ€Qˆ]Pˆ[Qˆ]Z€O€*B€w€€B€ €ë€†€@LF€À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€ \€Ã€%TUPLEB€:\€B€:B€:B€ú\€B€d\€B€jB€jB€¸€€QÀ\ÀÂÿO€7B€ €€B€ó€ë€Az5†€àA€F€»À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€ó\€B€`B€òB€:\€B€B€bB€aB€ƒ€CARDª€MOD€B€6B€ÄB€ÅB€ÆB€ÈB€ÉB€MB€ËB€ÇB€ÂB€:B€:\€B€d\€B€B€	B€B€B€B€iB€ĞB€ÑB€ÒB€ÓB€ÔB€ÕB€ÖB€iB€jÀ†€A€B€ÑB€ÜÑƒ*PKG-STRING*ÑB€ÑB€#ÑB€"ÑB€$ÑB€%‘B€+ÒB€óÀB€8ÀB€nÒB€/Ò\€B€6B€½B€¼ÀB€7Ò\€
ìRELATION-NAME€lMODIFIEDP€ìSAVE-DIRECTORYlATTRIBUTES¬‚IMPLEMENTATION-TYPE€l‚STORAGE-STRUCTURE€¬€KEY€¬TUPLE-FORMAT¬€DOC€¬CARDINALITY€ÀB€ìÒB€0Òì‚ERROR - The relation €ÀB€2ÒB€4Ò,„ is not defined in the database ÀB€9ÒpÀB€Cì€G9151€ÀF€èÀB€GÀB€IÀB€KÒ¬ERROR - The Àlƒ directory does not exist€ÀB€<ÀB€üÀB€»À\€ìSAVE-DIRECTORYÀB€@Ò\€B€üB€»,‚SYSTEM-RELATION€À\€lMODIFIEDP€À\€B€YÀìThe relation €Àl‡ has not been modified and thus does not need to be saved€ÀB€8Àl€-€À¬€.XLDÀB€;ÒÃDEFREL-RESTOREÀB€ÅÀƒCARDINALITY€ÀB€ÀB€ÀB€ÈÀB€ÉÀB€MÀB€6ÀB€ÒF€pÀì€SAVE-€ÀªFIND-SYMBOL€Ò\€B€:ÀB€ÒB€(Òl„ has been saved in the directory €€QŠ@Áä
PPP€Q’P@Q’šŠ’
À€PQ’Á€QPÿİÿİ¢BMÁæ		ä€Pˆ€QˆPˆPˆRMWEÁM[FÁMQBGÁMUBHÁMYBIÁMQBBJÁJMQ
CKÁKWLÁK[DÁKQBOÁKSKÁQFQ’CÁPPTPPPÿÛJCPÃPJCQÃÖCQŠŠJ!BJ!Bÿ\ü\ÿ
æ	ä€ PˆCQˆ!PˆRCQF+æ"Pÿİ#P$PP€Q’š%PCQŠ&¨"Pÿİ'P(P)P&¨EæCQF+
ää€*Pˆ€Qˆ+Pˆ€,PCQP-P€Q.P/²BÁ0P€QGQ1PHQ2PDQ3PÿÛ4PIQ5PJQ6PKQ7PLQ8PFQJº¢AÁOä€Q8PFQ9˜ÿİ:PJU,P;PHQ-PIQ/ªP<’QÁ€QBQAQQ™"Pÿİ#P$PP€Q’š(P=P&¨€QP#P>˜æPNÁÚ?€NQÀ	ä€*Pˆ€Qˆ@PˆCQˆ]uZ€O€lB€ó€€B€(€ë€*/ˆ†€`*F€YÀ$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€(\€B€ñê€IGNOREB€:\€€uB€aB€:B€:B€:B€:\€B€d\€
B€jB€pÀ¬€ZLC€,DO-NAMEDpÀB€Tì‚INHIBIT-STYLE-WARNINGSB€B€	B€B€B€B€i€B€ÑB€ÑB€ÑB€ÑB€ ÑB€#ÑB€ÜÑB€"ÑB€!ÑÃ‚*SYSTEM-RELATION-KEY*€ÑB€l‘\€B€YÀB€<À\€lMODIFIEDP€À\€ê€MEMBERB€»B€Ü©€TEST\€B€8B€üÀQTRIEVE€ÒpÀB€Cì€G9212€ÀF€xÀB€GÀB€IÀB€KÒB€nÒB€0Ò¬ERROR - The ÀB€2ÒB€4Òlƒ directory does not exist€ÀB€6ÀB€wÒB€8À¬€.XLDÀB€;ÒB€jÀB€ ÀB€ÀB€LÀB€MÀB€NÀB€t’PPPPPPª‹æSPPTPPPÿÛJCBÃPJCCÃÖPŠŠJ!BJ!Bÿ\ü\ÿ
æ
ä€PˆPˆPˆRCÛCÑ	PEÁDÁ	üDQESPPšCDÃÁEÅEõç PPP!P"¢AÃ#P$PPš#P%PPš&PPPP'PP(PP²šš)SO€
B€(€1€\€pÀB€\,„FASL-RECORD-FILE-MACROS-EXPANDED\€B€8\€\€ê€DEFUN€†€'\€pÀB€T¬€DEFF†€b\€B€}†€(Ì¢\€B€{†€*ıj\€B€Ö†€{šÍ\€B€Õ†€:}n\€B€Ô†€xõ¿\€B€Ó†€Zió\€B€Ò†€{Ä²\€B€Ñ†€2»=\€B€Ğ†€z(‡\€B€i†€6\€B€j†€[æ„\€B€†€=Ì#\€B€†€-i\€B€†€~Éz\€B€†€<p‘\€B€	†€`sN\€B€†€|Äô\€B€i†€.Ù‹\€B€g†€N¦ª€€ogn
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
B€jB€jB€¸€€QÀ\ÀÂÿO€7B€ €€B€ó€ë€Az5†€àA€F€»À$€ÀB€:B€V]€F€€:B€:B€:B€YF€€^B€ó\€B€`B€òB€:\€B€B€bB€aB€ƒ€CARDª€MOD€B€6B€ÄB€ÅB€ÆB€ÈB€ÉB€MB€ËB€ÇB€ÂB€:B€:\€B€d\€B€B€	B€B€B€B€iB€ĞB€ÑB€ÒB€ÓB€ÔB€ÕB€ÖB€iB€jÀ†€A€B€ÑB€ÜÑƒ*PKG-STRING*ÑB€ÑB€#ÑB€"ÑB€$ÑB€%‘B€+ÒB€óÀB€8ÀB€nÒB€/Ò\€B€6B€½B€¼ÀB€7Ò\€
ìRELATION-NAME€lMODIFIEDP€ìSAVE-DIRECTORYlATTRIBUTES¬‚IMPLEMENTATION-TYPE€l‚STORAGE-STRUCTURE€¬€KEY€¬TUPLE-FORMAT¬€DOC€¬CARDINALITY€ÀB€ìÒB€0Òì‚ERROR - The relation €ÀB€2ÒB€4Ò,„ is not defined in the database ÀB€9ÒpÀB€Cì€G9151€ÀF€èÀB€GÀB€IÀB€KÒ¬ERROR - The Àlƒ directory does not exist€ÀB€<ÀB€üÀB€»À\€ìSAVE-DIRECTORYÀB€@Ò\€B€üB€»,‚SYSTEM-RELATION€À\€lMODIFIEDP€À\€B€YÀìThe relation €Àl‡ has not been modified and thus does not need to be saved€ÀB€8Àl€-€À¬€.XLDÀB€;ÒÃDEFREL-RESTOREÀB€ÅÀƒCARDINALLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540837. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "LISP" :NAME "STARTER-KIT-DESTROY" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :CHARACTERS T :NOT-BACKED-UP T :CREATION-DATE 2749846960. :AUTHOR "REL3" :LENGTH-IN-BYTES 669. :LENGTH-IN-BLOCKS 1. :BYTE-SIZE 8.)

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
