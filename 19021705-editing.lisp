
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
�oB��B����Define a new database.

   DB-NALMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540781. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "XLD" :NAME "MODIFY-AVL" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760360011. :AUTHOR "REL3" :LENGTH-IN-BYTES 6865. :LENGTH-IN-BLOCKS 14. :BYTE-SIZE 16.)                                  pp2�\���COMPILE-DATA\��SW-MFG,�GODZILLA����[�F�F�\�p�,�COMPILER,�VERSION�\�F�F�p�B�),�OPTIMIZE-SWITCH�����QFASL-SOURCE-FILE-UNIQUE-ID�1�\�p�l�FS�MAKE-FASLOAD-PATHNAME�\��QUOTE�B�$\�B�8��NIL�\�B�8\���RTMS\�B�8l�MODIFY-AVL\�B�8��LISP\�B�8F���BASEF�
�FONTS�\���*CODE-FONT*��*COMMENT-FONT*�*STRING-FONT*�)�PACKAGE���RTMS��MODE��COMMON-LISP���MODIFY-LIST-AVL���(p���(F���$��B�:p���TICL�ART-Q�]�F��:B�:B�:j�T�F�p���SYS�l�DEBUG-INFO-STRUCT�B�P\�ÁRELATION-NAME�ÁATTRIBUTE-LISTÁKEY-ATTRIBUTESC�MODIFY-ATTRIBUTES�ÁMODIFY-VALUES���WHERE-CLAUSE�DOM-DEF��INDICES�B�:\��DOMAINS��KEY-DOMAIN-LIST�C�KEY-VALUE�ÁINSERT-TUPLES���MODE�MODIFIED-TUPLES��NUMBER-MODIFIED���PACKAGE-NAMEC�REBALANCEP��TEMP-ATTRIBUTE-LIST�ÂTERMINATION-CONDITION���TOTAL-INSERT-TUPLES�ÂTOTAL-NUMBER-MODIFIED���TREEÀTUPLESÁDOMAIN-ELEMENTÂSTRING-ATTRIBUTE-LIST��STRING-MODIFY-ATTRIBUTES��KEY%�BEGINNING-VALUE%��TERMINATION-CLAUSE%�B�:B�:�BEGINNING-VALUE�ÀTUPLE�\�)�MACROS-EXPANDED�\��UNLESS�THIRD��FIRST��SECOND��PROG��SETF������*PKG-STRING*�C�*SYSTEM-RELATIONS*��STRING-UPCASE�Ҫ�STRING-EQUAL�p�B�\��MEMBER-TEST���STRING�l�:*��-KEY-DOMAINS*����CONCATENATE��*�READ-FROM-STRINGҪ�EVAL�*�REVERSE�Ҫ�LIST҃�PROJECT-LIST�C�CONVERT-ATTRIBUTES�B�p��EXTRACT-KEY-AVL�҃�ENTRY-POINT����GETP�\�\�B�Y�B�Y���LEP��F���C�EVAL-WHERE-PREREQ��j�MAKUNBOUND�l�TERMINATE��\�ÀBOGUS�B���LOCATE�������LIST-AVL-KEY-MODIFY��p�B�\,�*APPEND��,�FINISHED�p�B�\�STRING-EQUAL*�҃�PUTP҃�MODIFY-TUPLE��INSERT-LIST-AVL��F�L߀Q�PP�
�	PP
P�Q�P�����QO��OQ
B@]@�O���@Q���Q�Q�B�A���Q�C��PG��Q�QAQ�QGQ�B��QP�M�B�Q�PP�B��Q��Q�Q�P�BSBWB[T�S�R�`�TQB&�J��PRQBTQB�J��Q�Q�QPPAI���IQV�U�CU�US�CVS�V�US�U�V���PD�JF�PC�SSW�"�F�MQPQ�QAQWQJQP�Q�QHQFQ�ۄQQQIQ�Q P!PAC�W�F�H�D�M�CQKQ"�K�LQFaL�H�JF'�DQ#P$��C��R�S�T�R��QMQP%�K*�IQV�U�CU�US�CVS�V�US�U�V���N�KQX��Q�QXS�Q�Q�QIQJ&�E]E�X����Q�QEQ�Q�Q'�K�LQKQ�O��B�P��C�MODIFY-FLAVOR-AVL���(m���(F���$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B�`B�aB�bB�cB�dB�eB�fB�gB�:\�B�iB�jB�kB�lB�mB�nB�oB�pB�qB�rB�sB�tB�uB�vB�wB�xB�{B�|B�}B�:B�:B�~B�\�B��\���B��B��B��B��B���DOCUMENTATION���This function will is the driver for the function which will modify the tuples of the specified list avl
   represented relation. A count of the total number of tuples modified is returned.

   RELATION-NAME     - The name of the relation whose tuples will be modified.
   ATTRIBUTE-LIST    - A list of all of the attributes in the relation in string form.
   KEY-ATTRIBUTES    - A list of the attributes which make form the key of the relation.
   MODIFY-ATTRIBUTES - A list of the attributes to be modified.
   MODIFY-VALUES     - A list of the expressions by which the attributes will be modified.
   WHERE-CLAUSE      - An s-expression which is used as a predicate to select the tuples to be modified.
   DOM-DEF           - A list of elements. Each element is a list containing the name of the attribute, the
                       domain of the element and the default value of the attribute.
   INDICES           - A boolean value which indicates of there are any indices defined on this relation.����B���B���B���B�
�B���B���l�:*��-KEY-DOMAINS*��B���B���B���B���B���B���B�p�B���B���B���\�\�B�Y�B�Y�B���F���B���B���l�TERMINATE��\�B��B��B����LOCATE�����ÂFLAVOR-AVL-KEY-MODIFY��B���,�FINISHED�B���B���B���C�INSERT-FLAVOR-AVL��F�L߀Q�PP�
�	PP
P�Q�P�����QO��OQ
B@]@�O���@Q���Q�Q�BA���Q�C��PG��Q�QAQ�QGQ�B��QP�M�B�Q�PP�B�BSBWB[R�Q�P�a�RQB&�J��PPQBRQB�J��Q�Q�QPPAI���IQT�S�CS�SS�CTS�T�SS�S�T���PD�JF�PC�QSU�#�F�MQ�Q��QAQUQJQP�Q�QHQFQ�ۄQ�QIQ�Q P!PAC�U�F�H�D�M�CQKQ"�K�LQFaL�H�JF'�DQ#P$��C��P�Q�R�P��QMQP%�K*�IQT�S�CS�SS�CTS�T�SS�S�T���N�KQV��Q�QVS�Q�Q�QIQJ&�E]E�V���Q�QEQ�Q�Q'�K�LQKQ�O��B����C�MODIFY-STRUCT-AVL���(m���(F���$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B�`B�aB�bB�cB�dB�eB�fB�gB�:\�B�iB�jB�kB�lB�mB�nB�oB�pB�qB�rB�sB�tB�uB�vB�wB�xB�{B�|B�}B�:B�:B�~B�\�B��\���B��B��B��B��B��B����This function will is the driver for the function which will modify the tuples of the specified list avl
   represented relation. A count of the total number of tuples modified is returned.

   RELATION-NAME     - The name of the relation whose tuples will be modified.
   ATTRIBUTE-LIST    - A list of all of the attributes in the relation in string form.
   KEY-ATTRIBUTES    - A list of the attributes which make form the key of the relation.
   MODIFY-ATTRIBUTES - A list of the attributes to be modified.
   MODIFY-VALUES     - A list of the expressions by which the attributes will be modified.
   WHERE-CLAUSE      - An s-expression which is used as a predicate to select the tuples to be modified.
   DOM-DEF           - A list of elements. Each element is a list containing the name of the attribute, the
                       domain of the element and the default value of the attribute.
   INDICES           - A boolean value which indicates if there are indices defined on the relation.�����B���B���B���B�
�B���B���l�:*��-KEY-DOMAINS*��B���B���B���B���B���B���B�p�B���B���B���\�\�B�Y�B�Y�B���F���B���B���l�TERMINATE��\�B��B��B����LOCATE�����ÂSTRUCT-AVL-KEY-MODIFY��B���,�FINISHED�B���B���B���C�INSERT-STRUCT-AVL��F�L߀Q�PP�
�	PP
P�Q�P�����QO��OQ
B@]@�O���@Q���Q�Q�BA���Q�C��PG��Q�QAQ�QGQ�B��QP�M�B�Q�PP�B�BSBWB[R�Q�P�a�RQB&�J��PPQBRQB�J��Q�Q�QPPAI���IQT�S�CS�SS�CTS�T�SS�S�T���PD�JF�PC�QSU�#�F�MQ�Q��QAQUQJQP�Q�QHQFQ�ۄQ�QIQ�Q P!PAC�U�F�H�D�M�CQKQ"�K�LQFaL�H�JF'�DQ#P$��C��P�Q�R�P��QMQP%�K*�IQT�S�CS�SS�CTS�T�SS�S�T���N�KQV��Q�QVS�Q�Q�QIQJ&�E]E�V���Q�QEQ�Q�Q'�K�LQKQ�O��B������MODIFY-FLAVOR-TUPLES��*h���F�>�$��B�:B�V]�F��:B�:B�:B�YF��^B��\��RELATIONB�aB�cB�dÀWHERE�B�fB�wB�rB�:\���ATOM-ATTRIBUTE-LIST�ÂATOM-MODIFY-ATTRIBUTESÁFLAVOR-PACKAGE��DATA��CONV-ATTRIBUTE-LIST�B�:B�:B�:B�:B��A-TUPLE�B�:B�:��ATTR��VAL��TEMPVAL�\�B��\�B��p���ZLC�,�DO-NAMEDp�B�T�INHIBIT-STYLE-WARNINGSB��������*VALIDITY-CHECKING*��B����TYPEP��B�p҃�UNCONVERT-ATTRIBUTES�ÂPROJECT-FLAVOR-PREREQ�҃�FAST-PROJECT-FLAVOR�҃�QUOTE-TUPLE��C�FORM-ALIST�p�B�\,�SUBLIS*��B���B���B�
�B���C�DOM-CHECK��p�B�T,�SET-IN-INSTANCE���S��C�B��QBQ�@��QBQ�A��Q	�D��QDQ
�C�EцQCQH�G�F�4�FQGSHSJ�I�AQ�QL�K�!�KSLSN�M�O�JQ�@Q�NQ��O��Q�PP���OQMQ�Q��IQMQOQ�K�L�K�L��IQCF��G�H�G�H��EO�B������MODIFY-STRUCT-TUPLES��L����F�b�$��B�:B�V]�F��:B�:B�:B�YF��^B�\�B��B�aB�cB�dB��B�fB�wB�rB�:\�B��B��C�DELORMOD?�ÂSTRUCT-ATTRIBUTE-LIST���NUM-MODIFIED��TEMP-STRUCT��STRUCT-MODIFY-ATTRIBUTES��STRING-RELATION-NAMEB�:B�:B�:B�:��STRUCT-TUPLEB�:B�:B�:B�:B��B����STRUCT-ATTR�B��\�B��\�p�B�\l�XR-BQ-LISTB��B�B�B������B��B���B���B���B���B��B���ÂSUPER-FAST-EVAL-WHERE��B��B��B��B���B���B�
�B���B��B���B�8�D߀Q�G�HсQJ�I�	�IQPGQJS�CI��J�J��HQ	�C�J�JтQH�K�	�KQPGQHS�CK��H�H��JQ	�F��Q	�@��Q	�A��QJ�b�JSL�B�I�I�CQN�M��MQLQNS��CM��N�N��IQE��Q��EQ
��Q�Q�B�N�N�AQ�QFQI�H�P�O�/�OQPSHSISS�R�Q�T�EQ�@Q�RQ��T��Q�PP���TQQQ�Q��B�PSQLQ
�PTQ
�
�����CO��P�H�I�P�H�I��B�D�JŞ�DO�+B���ÀMODAVL��-��@�F��$��B�:B�V]�F��:B�:B�:B�YF��^B�,\�B�vB�qC�TEMP-TREE�B�:\��MODTREE�\�B��\�B��B���FOURTH�F���B�,�F����BALANCE2��Q�QB�Q�QPPA����@��QB@Q���Q�QPPA������Q�S��[���݀Q�Q�Q�O�>B�,��B�������F���$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B�vB�aB�bB�iB�~C�TERMINATION-CLAUSEB�mB�`B�eB�qB�oB�lB�dB�cB�rB�fB�:\���COMPARISON-OPERATOR�ÂCURRENT-NODE-KEY-VALUE�MOD-TREE\�B��\�B��B������ÀLOCATE�B���ÁLOCATE-STAGE-2��DELETE-SEARCH��B���B���B�Y�C�LESS-THAN����NODE-COMPARE҆���B��҃�GREATER-THAN�F���B�=�l�TERMINATE��,�RESTART���EQUAL���LOCATE-STAGE-2��LOCATE��BALANCE1�,�FINISHED��PROCESS-LIST-AVL-MODIFY�����QP��QP��QP���QB��Q�Q	�BA��S
&�P��QAQ�Q�@�&�QP��[�Q�Q�Q�Q�Q�Q�Q�Q Q�Q�Q�Q
Q�Q�QPPA������ ���B�]�@Q&1�QP�-�QB�Q�Q�Q�Q�Q�Q�Q�Q Q�Q�Q�Q
Q�Q�QPPA������ ���B��QBBQ� s�Q QPPA ��� �Pg�Pe�@QP�b�P���[�Q�Q�Q�Q�Q�Q�Q�Q Q�Q�Q�Q
Q�Q�QPPA������ ���B��QP��QP��P���YBQ� �Q QPPA ��� �P�P���QP�'�QP�#�QP��Q�Q�Q�Q Q�Q�Q�Q�Q�Q�Q�Q�Q
Q�Q�QPPA������ ���BÀ����P���QP��Q�Q�Q�Q�Q�QP�Q�Q Q�Q�Q�Q
Q�Q�QPPA������ ������Q�Q Q�Q�Q�Q�O�_B����B�������F���$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B�vB�aB�bB�iB�~B�GB�mB�`B�eB�qB�oB�lB�dB�cB�rB�fB�:\�B�IB�JB�K\�B��\�B��B������B�O�B���B�P��DELETE-SEARCH��B���ÁPROJECT-FLAVOR�B�Y�B�R�B�S҆���B���B�U�F���B�=�l�TERMINATE��,�RESTART��B�Y��LOCATE-STAGE-2��LOCATE�B�\�,�FINISHED�C�PROCESS-FLAVOR-AVL-MODIFY�����QP��QP��QP���QB��Q�Q	�BA��S
&�P��QAQ�Q�@�&�QP��[�Q�Q�Q�Q�Q�Q�Q�Q Q�Q�Q�Q
Q�Q�QPPA������ ���B�]�@Q&1�QP�-�QB�Q�Q�Q�Q�Q�Q�Q�Q Q�Q�Q�Q
Q�Q�QPPA������ ���B��QBBQ� s�Q QPPA ��� �Pg�Pe�@QP�b�P���[�Q�Q�Q�Q�Q�Q�Q�Q Q�Q�Q�Q
Q�Q�QPPA������ ���B��QP��QP��P���YBQ� �Q QPPA ��� �P�P���QP�'�QP�#�QP��Q�Q�Q�Q Q�Q�Q�Q�Q�Q�Q�Q�Q
Q�Q�QPPA������ ���BÀ����P���QP��Q�Q�Q�Q�Q�QP�Q�Q Q�Q�Q�Q
Q�Q�QPPA������ ������Q�Q Q�Q�Q�Q�O�vB����B��������F���$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B�vB�aB�bB�iB�~B�GB�mB�`B�eB�qB�oB�lB�dB�cB�rB�fB�:\�B�IB�JB�K\�B��\�B��B������B�O�B���B�P��DELETE-SEARCH��B���ÁPROJECT-STRUCT�B�Y�B�R�B�S҆���B���B�U�F���B�=�l�TERMINATE��,�RESTART��B�Y��LOCATE-STAGE-2��LOCATE�B�\�,�FINISHED�C�PROCESS-STRUCT-AVL-MODIFY�����QP��QP��QP���QB��Q�Q�Q	�BA��S
&�P��QAQ�Q�@�&�QP��[�Q�Q�Q�Q�Q�Q�Q�Q Q�Q�Q�Q
Q�Q�QPPA������ ���B�]�@Q&1�QP�-�QB�Q�Q�Q�Q�Q�Q�Q�Q Q�Q�Q�Q
Q�Q�QPPA������ ���B��QBBQ� s�Q QPPA ��� �Pg�Pe�@QP�b�P���[�Q�Q�Q�Q�Q�Q�Q�Q Q�Q�Q�Q
Q�Q�QPPA������ ���B��QP��QP��P���YBQ� �Q QPPA ��� �P�P���QP�'�QP�#�QP��Q�Q�Q�Q Q�Q�Q�Q�Q�Q�Q�Q�Q
Q�Q�QPPA������ ���BÀ����P���QP��Q�Q�Q�Q�Q�QP�Q�Q Q�Q�Q�Q
Q�Q�QPPA������ ������Q�Q Q�Q�Q�Q�O�
B����B�^��`׆��(F�w�$��B�:B�V]�F��:B�:B�:B�YF��^B�^\�B�vB�aB�eB�oB�qB�mB�~B�`B�GB�bB�iB�lB�dB�cB�rB�fB�:\�
ÁDELETE-TUPLES���KEY-MODIFIEDB�K�NEW-NODEB�5B�wB�{B�:C�NODE-TUPLEÀTUPLE%\�B��\���B�:B�B��B��B�����
�B�
�B���B�*�B���p�B�\l�MEMBER-EQL�B���B���l�TERMINATE��F���B�,�F���B�\�B���B����LOCATE�����B���B�=�,�FINISHED� QF��FS
QP�A�F��A��AV�S�Q�Q�@�J�k�@Q�Q����SG�
�GSHËQ��HQE]E�G���@Q�C�a��E4�QB	��Q Q
�B���Q�QB	��P���QD�DU�D[����<�DQ�DQB���[�QDQPPAD���B�DYBQ��(�Q�QPPA���� ��QEQ�CۀSI��IS	��Q�Q��ɁQ
QIS�Q�Q�Q�QJ��ISC]C�I���QCQ��QP�4�S�Q�Q�*�QB�Q Q�Q�Q�QP�Q�Q�Q�Q�Q�Q
Q�Q�QPPA����������B��QBBQ���Q�QPPA����P�����P���ۀQ�Q�Q�Q�Q�Q�O��B�^��B�u��h熀�0F��$��B�:B�V]�F��:B�:B�:B�YF��^B�u\�B�vB�aB�eB�oB�qB�mB�~B�`B�GB�bB�iB�lB�dB�cB�rB�fB�:\�B����DELETE-FLAVOR-TUPLE�B��B�KB�5B�wB�{B�:B�:��FLAVOR-TUPLEC�LIST-TUPLEB��\�B��\���B�:B��B�B��B������B�
�B���B�m�B���B�*�B���B���l�TERMINATE��F���B�,�F���B�\�B���B����LOCATE�����B���B�=�,�FINISHED� QF��FS
QP�B�F��B��S�S�Q�Q�H�G��GSHSJ�I�JQ��Q�Q��JQ@]@�IQA]A�G�H�G�H��BS�@QJ�]�@Q�Q	����SG�
�GSK�AQ
��KQE]E�G���@Q�C�a��E4�QB��Q Q�B���Q�QB��P���QD�DU�D[����.�DQ�DQB���[�QDQPPAD���C�DYCQ���Q�QPPA������QEQ��AQ�C�a���Q�Q
Q�Q�Q�QAQ�QJ��QP�7�S�Q�Q��Q�Q�*�QB�Q Q�Q�Q�QP�Q�Q�Q�Q�Q�Q
Q�Q�QPPA����������C��QBCQ���Q�QPPA����P�����P���ۀQ�Q�Q�Q�Q�Q�O��B�u��B����j솀�4F���$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B�vB�aB�eB�oB�qB�mB�~B�`B�GB�bB�iB�lB�dB�cB�rB�fB�:\�B����DELETE-STRUCT-TUPLE�B��B�KB�5B�wÂSTRING-TEMP-ATTRIBUTESB�{B�:B�:B�#B��B��\�B��\���B�:B��B�B��B������B���B�
�B���B���B���B�*�B���B���l�TERMINATE��F���B�,�F���B�\�B��B����LOCATE�����B���B�=�,�FINISHED��Q�F� QG��GS
QP�B�G��B��S�SFQFQ�Q�I�H��HSISK�J�KQ��Q�Q	��KQ@]@�JQA]A�H�I�H�I��BT�@QJ�]�@Q�Q
����SH�
�HSL�AQ��LQE]E�H���@Q�C�a��E5�QB��Q Q�Q�B���Q�QB��P���QD�DU�D[����-�DQ�DQB���[�QDQPPAD���C�DYCQ���Q�QPPA������QEQ���Q�Q�Q
Q�Q�Q�QAQ�QJ��a���QP�8�SFQFQ�Q��Q�Q	�*�QB�Q Q�Q�Q�QP�Q�Q�Q�Q�Q�Q
Q�Q�QPPA����������C��QBCQ���Q�QPPA����P�����P���ۀQ�Q�Q�Q�Q�Q�O��B���1�\�p�B�\,�FASL-RECORD-FILE-MACROS-EXPANDED\�B�8\�\��DEFUN���'\�B�:��x��\�B�(��.ً\�B���(̢\�B���*�j\�B����[�\�B����=�#\�B����{��\�B����z(�\�B����:}n\�B����6��tribute-list dom-def))
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
        (concatenate 'string string-relatLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540787. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "XLD" :NAME "MODIFY-REL" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760360646. :AUTHOR "REL3" :LENGTH-IN-BYTES 4758. :LENGTH-IN-BLOCKS 10. :BYTE-SIZE 16.)                                  pp2�\���COMPILE-DATA\��SW-MFG,�GODZILLA������F�F�\�p�,�COMPILER,�VERSION�\�F�F�p�B�),�OPTIMIZE-SWITCH�����QFASL-SOURCE-FILE-UNIQUE-ID�1�\�p�l�FS�MAKE-FASLOAD-PATHNAME�\��QUOTE�B�$\�B�8��NIL�\�B�8\���RTMS\�B�8l�MODIFY-REL\�B�8��LISP\�B�8F���BASEF�
�FONTS�\���*CODE-FONT*��*COMMENT-FONT*�*STRING-FONT*�)�PACKAGE���RTMS��MODE��COMMON-LISP��ÂCREATE-ATTR-DESCRIPTOR��,f��@�F�:�$��B�:p���TICL�ART-Q�]�F��:B�:B�:j�T�F�p���SYS�l�DEBUG-INFO-STRUCT�B�P\�ÁATTRIBUTE-LISTÂSYSTEM-ATTRIBUTE-LIST�B�:\�ÁATTR-DES-PAIR��ATTR-DEF�ATTR-DOC��DOM�C�FOUND-ATTRC�ATTR-TUPLEB�:B�:B�:C�ATTR-NAME�ÀTUPLESB�:B�:\�)�MACROS-EXPANDED�\��THIRD��FIFTH��FOURTH�SECONDp�B�Tl�CONDITION-BIND-IF�p�B�T�CONDITION-BINDp�B�T�CATCH-CONTINUATION-IF�p�B�Tl�CATCH-CONTINUATIONp�B�T�ERRSET��PROGp���ZLC�,�DO-NAMEDp�B�T�INHIBIT-STYLE-WARNINGS��SETF�p�l�EH��*CONDITION-HANDLERS*�p�,��G0360��F�B��ERROR��p�B�\�ERRSET-HANDLER�p�B�\�STRING-EQUAL*�Ҫ�LIST�B�f�p�B�T��DEF����DOC��p�B�\,�*APPEND��FрQH�G�O�GQHSI�DہQJ�3�JSE�PPTPPP��JCK�PJCL��EWIQ�	�J!BJ!B\�D�EQBA�EUBB�E[C�@QEW
PCQPAQPBQ	�	��@�J�D�J��D
�@QI5�IQ�IQ	��@����CG��H�H��@O��B�P��C�GET-SYSTEM-ATTRIBUTE-LIST���(��@DF��$��B�:B�V]�F��:B�:B�:B�YF��^B��\�ÁRELATION-NAME�B�:B�:B�:�Â*SYSTEM-ATTRIBUTE-KEY*�Ã*SYSTEM-ATTRIBUTE-ATTRIBUTES*�у�*PKG-STRING*у�*SYSTEM-RELATION-STORAGE-STRUCTURE*��Ä*SYSTEM-RELATION-BASE-IMPLEMENTATION*���STRING�l�RETRIEVE-��l�-����CONCATENATE�Ҫ�FIND-SYMBOL���SYSTEM-ATTRIBUTE���STRING-EQUAL�B����STRING-UPCASE��B���P	PP
PP�P�@�PPPPPP�Q����PJ@�O��B������REDEFINE-REL��"a��BHF�?�$��B�:B�V]�F��:B�:B�:B�YF��^B��\�	B��B�c��IMP�C�SS��KEY�C�TUPLE-FORMAT-LIST�B����DIR�C�TUPLE-LISTB�:\��TEMP-REL�STATUS?�\�B�l\�p�B�\l�XR-BQ-LISTB���C�*PROVIDE-STATUS-MESSAGES*���GENSYM�B���*�READ-FROM-STRING�B�����STO��B����FORMAT�B���B����DEFINE-RELATION�҃�SYSTEM-INDEX�B���B���B���\��RELATION-NAME��B���B�8���EVAL��DELETE-OR-MODIFY�C�RELATIONP���DESTROY-RELATION҃�DELETE-INDEX-TUPLES���RENAME-RELATION��B�j�ÀINSERT�PA����@��@Q�QP�QP�Q	P�Q
P�QP�QP�QJ��AQ�RP��PP�Q��PPP@Q������Q��Q�@Q�Q�@Q�Q������Q��P�Q�AQ��O��B�����MODIFY-RELATION���U����U@F�+�$��B�:B�V]�F��:B�:B�:B�YF��^B��\��RELATION�&REST���KEYWORD-LIST��&KEYj�&OPTIONAL�B��ÁADD-ATTRIBUTESC�DELETE-ATTRIBUTES�C�RENAME-ATTRIBUTES���IMPLEMENTATION-TYPE�C�STORAGE-STRUCTURE�B��B���DOCUMENTATION�j�DIRECTORY�j�&ALLOW-OTHER-KEYS�B�:\�#B��B��B��B��B��B��B��B��B��B��B�郀REL���ATTRB��B���FORMAT1���KEY1B��B���MOD-ATTR�MOD-VALS��RENAME-ATTRSC�NEW-ATTRS�C�CURRENT-ATTRIBUTESB�j��DELETE-ATTRS�OLD-VALSB��B�:B�:��ATT�B�:B�:�DOM-DEF�B�:\�B�l\��FIRST�B��B�qB�nB�p*�SEVENTH��SIXTH�B�oB�B��p�B�Tl�COND-EVERYB�|B���DOCUMENTATION�쿇Modify various features of a relation.

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
  DIRECTORY            - New directory in which this relation is to be saved.����� �C�*PKG-NAME*�Â*SYSTEM-RELATION-KEY*�у�*SYSTEM-RELATION-ATTRIBUTES*�B���B���B���B��у�*VALIDITY-CHECKING*�у�*ACTIVE-DB*���*PROVIDE-ERROR-MESSAGES*у�*PARAMETER-CHECKING*�\�
�RELATION-NAME��ADD-ATTRIBUTESi�DELETE-ATTRIBUTES�i�RENAME-ATTRIBUTES���IMPLEMENTATION-TYPE�i�STORAGE-STRUCTURE��FORMAT��KEY�B�i�DIRECTORY��p�B�\�STORE-KEYARGS���ACTIVE-DATABASE�҃�VALIDATE-SYM�\�
B���ADD-ATTR��DELETE-ATTR���RENAME-ATTR�B��B��B��B��B��B����GET-KEYWORD-VALUE-PREREQ�\�l�ATTRIBUTES���GET-RELATION��TERPRI�l�ERROR - Relation ����WRITE-STRING��PRIN1��,� does not exist in the database �\�B���C�GET-KEYWORD-VALUE��\���TUPLE-FORMAT�B���B�8�B���\�B���\��SAVE-DIRECTORY�\�B���\��DOCUMENTATION��\�B���\���KEY��\�B���\�B���B�j��RETRIEVE�\�l�ATTRIBUTES�SAVE-DIRECTORY��DOC���TUPLE-FORMAT��IMPLEMENTATION-TYPE�l�STORAGE-STRUCTURE���KEY��j�COPY-LIST��\�B��C�CONVERT-ATTRIBUTES�B���p�B�\��MEMBER-TEST��p�B�\,�DELETE*��,�ERROR - �� is not an attribute in the relation ��,�. It can not be deleted.���PROJECT-LIST�B���B�P�\�B��p�B�\l�SUBST-EQL��,�. It can not be renamed.�\�B��\�B���B���B���,�MODIFY-��l�-��B���B����SYSTEM-RELATION��B���B���B����PROJECT��\�ÁATTRIBUTE-NAME�DOMAIN-FUNCTION��ÀWHERE��B���B���\�l�ATTRIBUTES��IMPLEMENTATION-TYPE�l�STORAGE-STRUCTURE���KEY���RENAME-ATTRIBUTE��DEFINE-ATTRIBUTE�C�ATTRIBUTE��C�DESTROY-ATTRIBUTE��ÁDELETE-TUPLES��ÁINSERT-TUPLES��B�Ғ@�@QP��A�����R�Q����R@Q�[S[��[S�5��[Q@�P@Q�@��QP���	BW��	��P��Q�P�P�RP@Q�BO����P�S��� POQ!�!��T�]�\�"P@Q�BR��SQ#P�S�TQ PRQ!�!��T�]�\�$P@Q�BQ��SQ%P�S�TQ PQQ!�!��T�]�\�&P@Q�BP��SQ'P�S�TQ PPQ!�!��T�]�\�(P@Q�BM�)P@Q�BN�M�N��Q*P��+�X��Q,P���BW�WS-�L�.P@Q�&�.P@Q�B/�\��\S^�LQ0P1��^QLQ2�L����3P�^Q�4P��Q�5P�\���XQWSLQ6�X�LQ�Q7�8�L�9P@Q�)�9P@Q�B/�\�"�\S^�^5�^SLQ0P1��^W^SLQ:�L����3P�^5�^S�^Q�4P��Q�;P�\���<P@Q��LQ<P@Q�B�L�P
P!�Z��
�=P@Q�B�QK�LQMQ�WUBNQ�WYBPQ�WQBBOQ�WQBQQ�W[RQ�WWXQ	J>��RZS�ZW
�KS9�?P@P	PAPPB�PC�b�DPPPSQTQ0PEP�QF�!�]�]�GPHPIP*P��JP0PEP�QK�!�J+�`�_��_Q`Sa�aS?PPaWB�L�!�C_��`�`��]Q��Jb��QMP���BW�9P@Q�BU�<P@Q�BV�.P@Q�BY�U�QUQN�V�QVQO�Y�QPPYQQ�P�Q*P��+�X��QJP��R��Q*PXQS�=P@Q�BK��QKQT�SO�aB�����MODIFY-DATABASE���-v��``F�I�$��B�:B�V]�F��:B�:B�:B�YF��^B�b\�	�DATABASEB��B��B��B��ÁDATABASE-NAME�B��B��B��B�:\�B��B�lB��B�胀TEMPB��B�:B�:\�B�l\�B��B�|B��B�l�Modify various features of the active database.

  DATABASE      - Name of the database to be modified.
  DATABASE-NAME - New name for this database.
  DIRECTORY     - New directory in which this database is to be saved.
  DOCUMENTATION - New description for this database.��B��B��B��B�	�\��DATABASE-NAME�B�B��B��B��B��B���B� ��ERROR - The database to modify has to be the active database ��B�"�B�#�\�B�kB��B���B��\�B���B�&�B���,�*SAVE-DIRECTORY*�B���B���\�B���,�*DATABASE-DOCUMENTATION*�\�B�k��RENAME-DATABASE��@�@QP��A���	��R�Q��
����R�QP����P�P�R@Q�ESE��ES�5��EQ@�P@Q�@�P@Q�BD�	�PPP��DQ�BG�F�P@Q�BD�	�PPP��DQ�BG�F�P@Q�BD��QDQ�G�F�DQ��O�|B�b���MODIFY-ATTRIBUTE��<t$���<�F���$��B�:B�V]�F��:B�:B�:B�YF��^B�}\�B��B�]B��B��B��B��B�SÁDEFAULT-VALUE�B��B��B��B�:\�B��B�SB��B��B��B�nC�ATTRIBUTES��TUPLE-FORMAT��NUM�B��B�:B�:B�:B�:B�:B��\�B�l\�B�qB��B��B�B��B��B�|B��B���Modify various features of an attribute in a given relation.

  RELATION       - Name of the relation in which the attribute to be modified exists.
  ATTRIBUTE      - Name of the attribute to be modified.
  ATTRIBUTE-NAME - New name for this attribute.
  DEFAULT-VALUE  - New default value for this attribute.
  DOCUMENTATION  - New description.
  FORMAT         - New print width to be used for this attribute.����A�C�*PROVIDE-WARNING-MESSAGES*�B��B��B��B�	�\��ATTRIBUTE-NAME�DEFAULT-VALUE�B�B��B��B��B��\�l�ATTRIBUTES��TUPLE-FORMAT�B��B� �l�ERROR - Relation ��B�"�B�#�,� does not exist in the database �B�?�B���B�A�,�ERROR - �� is not an attribute in the relation ��\�B��B��B��B���B��\�B���B�&�B���B�Q�\�B�SB�T�B�j�B�U�B���B���B���B�4�B���B���B���C�DOM-CHECK�Ҫ�AND��B�S�\��DEFAULT-VALUE��B���\�B���l�WARNING - �� is not a valid format value.���EQUAL��p�B�\l�POSITION*��p�B�T�FIRSTN��APPEND�B�P�\���TUPLE-FORMAT�B�8�\�B���\���DOC��\�B���B�[�@�@Q	P��A�
����R�Q����R�QP���BG�G�	��P��Q�P�P�R�Q�B��GSP��	��P��Q�P��Q�R@Q�ISI��IS�5��IQ@�P@Q�@�P@Q�BE�<�EQ�QL�PPP P��!PP"P�Q#�$�J%�N�M��MQNSO�OS&PPOW'�(�$�CM��N�N��LQ)��P��*PP"P�Q#�$�P+P�Q#�$�$�,PEQ$�-����K�J�.P@Q�BE�.�E1���/P�EQ�0P�K� �GSF�GWG��QFQ1P2�H�GQ3�EQ$�HkGQ
C4�G�5P��P"P�Q#�$�6P7PGQ$�$�-�K�J�8P@Q�BE��P��*PP"P�Q#�$�P+P�Q#�$�$�9PEQ$�-�K�J�:P@Q�BE��Q�QEQ;�K�J�EQ��O��B�}�1�\�p�B�\,�FASL-RECORD-FILE-MACROS-EXPANDED\�B�8\�\��DEFUN���'\�B�����9\�B����{Ĳ\�B����2�=\�B����z(�\�B�Æ�.ً\�B����[�\�B����(̢\�B���*�j\�B�|��=�#\�B�{��-i\�B�y��~�z\�B�w��<p�\�B�u��`sN\�B�s��|��\�B�q��{��\�B�p��x��\�B�o��Zi�\�B�n��:}n��     (setf new-attrs (car (get-keyword-value '(add-attr) keyword-list)))
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
PPPPPP�Q����PJ@�O��B������REDEFINE-REL��"a��BHF�?�$��B�:B�V]�F��:B�:B�:B�YF��^B��\�	B��B�c��IMP�C�SS��KEY�C�TUPLE-FORMAT-LIST�B����DIR�C�TUPLE-LISTB�:\��TEMP-REL�STATUS?�\�B�l\�p�B�\l�XR-BQ-LISTB���C�*PROVIDE-STATUS-MESSAGES*���GENSYM�B�LMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540793. :SYSTEM-TYPE :LOGICAL :VERSION 2. :TYPE "LISP" :NAME "PRINT" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :CHARACTERS T :NOT-BACKED-UP T :CREATION-DATE 2755785296. :AUTHOR "REL3" :LENGTH-IN-BYTES 13236. :LENGTH-IN-BLOCKS 13. :BYTE-SIZE 8.)

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
 (format *standLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540797. :SYSTEM-TYPE :LOGICAL :VERSION 2. :TYPE "XLD" :NAME "PRINT" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760360237. :AUTHOR "REL3" :LENGTH-IN-BYTES 3284. :LENGTH-IN-BLOCKS 7. :BYTE-SIZE 16.)  pp2�\���COMPILE-DATA\��SW-MFG,�GODZILLA����>�F�F�\�p�,�COMPILER,�VERSION�\�F�F�p�B�),�OPTIMIZE-SWITCH�����QFASL-SOURCE-FILE-UNIQUE-ID�1�\�p�l�FS�MAKE-FASLOAD-PATHNAME�\��QUOTE�B�$\�B�8��NIL�\�B�8\���RTMS\�B�8�PRINT�\�B�8��LISP\�B�8F���BASEF�
�FONTS�\���*CODE-FONT*��*COMMENT-FONT*�*STRING-FONT*�)�PACKAGE���RTMS��MODE��COMMON-LISP����PRINT-TUPLE���C���A�F�S�$��B�:p���TICL�ART-Q�]�F��:B�:B�:j�T�F�p���SYS�l�DEBUG-INFO-STRUCT�B�P\�ÀTUPLE���TUPLE-FORMAT*�PATHNAME�OUTPUT-TO-WINDOWÀBLANKS�STREAMB�:\�C�ATTR-LIST���LEN�B�:B�:B�:À%TUPLEB�:B�:��TUP�ÀTUPFMTC�X�B�:B�:\�)�MACROS-EXPANDED�\�p�B�Tl�COND-EVERY�WITH-OUTPUT-TO-STRING���SETF��PROGp���ZLC�,�DO-NAMEDp�B�T�INHIBIT-STYLE-WARNINGS���PRINT-TUPLE*�l�|����LIST�j�MAKE-STRING-OUTPUT-STREAM���PRIN1��*�GET-OUTPUT-STREAM-STRING��STRING��SUBSEQ�l�@����CONCATENATE���APPEND��PRINC���TERPRI���Q�Q�Q�Q�Q�BрQD�C�v�CQDSE�P�@�EQ�QG�F�+�FSGSI�H��J�HQJQ�JQ�HÊCA�@QAQI#�	PHQJIm
�P�	PHQ�QJIQAc
��HQ��P��@�F�G�F�G��@QF�'�FSJ�J5�K�Lۂ�JS�Q�L�K݅�JS�Q�L�K��L�Kۂ�JQ�Q�K�L݅�JQ�Q�K�L�F���L�Kۂ�Q�K�L݅�Q�K�L�@QCC��D�D��BO��B�P��B�z��@���AhF�L�$��B�:B�V]�F��:B�:B�:B�YF��^B�z\�B�`B�aB�bB�dB�eB�:\�
B�hB�:B�iB�:B�:B�:B�jB�kB�:B�:\�B�n\�B�sB�qB�tB�wB�y�l�|��B���l�~S��FORMAT�B���B���l�@��B���B����QA�|�ASB�C�Dۂ�P�Q�D�C݄�P�Q�D�C�BQ�QC�E�R�ESCSG�F���PFQ�FÊC@�G#�PFQJGm�	P
�F�H�Iۂ�FQ�Q�I�H݄�FQ�Q�I�H��PFQ�QJGQ@c�
�F�I�Hۂ�FQ�Q�H�I݄�FQ�Q�H�I�I�Hۂ�P�Q�H�I݄�P�Q�H�I�E�C�E�C��I�Hۂ�Q�H�I݄�Q�H�I�Ań�O��B�z���PRINT-TUPLE-WIDE��eᆀ��F�|�$��B�:B�V]�F��:B�:B�:B�YF��^B��\�ÀTUPLESC�PROJECT-ATTRIBUTES�NUMBER-PER-LINE�B�cB�bB�eB�:\�ÀITEMS��ATT-STS�ÀSCREENB�:B�:B�:��ATTRB�`C�LINE-ITEM���LINE-LENGTH�ÁNUMBER-IN-LINEB�:B�:B�:��VAL�ÀAT-ST�ÀVAL-ST�DUM-VAL�B�:B�:B�l\�B�n\�B�rB�qB�tB�wB�yp�B�T��SENDB�s���Àp�l�TV�DEFAULT-SCREEN��HEIGHT��WIDTH��l�~S�B���C�PRINT-TUPLE-WIDE*��B����ITEM1��C�ATTRIBUTE��B�|�l�: �B�}�B�~�B��l�  �p�B�\,�*APPEND��B���l� ��*�REVERSE��P�P�y�UJ�nJB�CсQE�D��DQESF���PFQ��CCD��E�E��CQA��	�Q�Q�Q�Q�QAQBQJ	��QD���DSG�H�I�J߁QGQAQM�L�K�o�KSLSMSO�N�F�P�Q���PNQ��CP��QJ+�JIaOaPaB#�R�Sۄ�Q
�S�R݅�Q
�S�R�J�I�HQ@]@�H�HQPFQP�PN7��T�NQTQ�TQ��NQQ�QQ�P��H�S�Rۄ�FQ�Q�P�Q�NQ�Q�P�Q�R�S݅�FQ�Q�P�Q�NQ�Q�P�Q�R�S�J�JIaOaPaI�K�L�M�K�L�M��H�HQ@]@�P@]@�S�Rۄ�Q
�R�S݅�Q
��Q
�R�S�D�f�@Q�O��B����B����
=���A
�F�G�$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B��B��B��B�bB�eB��B��B�:\�B�:B�`B��B��B��B�:B�:B�:B��B��B��B��B�:B�:\�B�n\�B�qB�sB�tB�wB�y�l�~S�B���B���B�~�l�: �B���l�  ��Q@�v�@SA�B�C�D߁QAQ�QG�F�E�S�ESFSGSJ�I�H�K���PIQ��CK��QD+�JCaJaKa�#�L�Mۃ�Q�M�L݄�Q�M�L�D�C�B�M�Lۃ�HQ�Q�P�Q�IQ�Q�	P�Q�L�M݄�HQ�Q�P�Q�IQ�Q�	P�Q�L�M�D�JCaJaKaC�E�F�G�E�F�G��M�Lۃ�Q�L�M݄�Q��Q�L�M�@Ŋ�O��B����C�PRINT-WIDE-FORMAT���*m��B�F�C�$��B�:B�V]�F��:B�:B�:B�YF��^B��\�
�RELATIONB��B��B�cB�bC�ITEM-LIST�ÁLIST-OF-TUPLESC�ATTRIBUTES��CARDB�eB�:\�C�NEW-ITEMS���TEMP\�B�n\�B��B�s��*OUTPUT-WINDOW*�у�*ACTIVE-DB*����Relation:  ��B���B���B�|��  Database:  ���DATABASE�,�  Cardinality:  ��ITEMS���INDEX����PUTP�l� ��B��Ҭ�Relation: ~S  Database:  ~S  Cardinality: ~S�B����~%Relation: ~S  Database:  ~S  Cardinality: ~S�B���B���B����CLOSE��i�SET-ITEMS����PP�QP�	PPP
P�P�Q�C@��QP��CA��AQ�JP�P�@]@�� Q� QP�QP�Q� Q� Q���Q��QP�QP�Q��Q��Q���Q@Q��Q�Q�Q�Q�Q Q������Q���P�Q��O��B����C�PRINTREL-INTERNAL*��CG���C�F�E�$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B��B��B��B����WIDE-FORMAT�B�eÁOUTPUT-TO-FILEB�aÀHEADER��TAILB��j�&OPTIONAL�\�ÀPRINT?B�Y\�ÁRETURN-TUPLES�B�:B�:\�B�bB�c�ROWLINE�B�gB�ރ�QTRIEVE-VAR�ÁNEW-ITEM-LIST�B�d�FIRST���LASTB�:B�:B�:B��B�:B�:B����FORMB�jB�kB�hB�lC�Y�B�:\�B�n\�B�qB��p�B�Tl�CONDITION-BIND-IF�p�B�T�CONDITION-BINDp�B�T�CATCH-CONTINUATION-IF�p�B�Tl�CATCH-CONTINUATIONp�B�T�ERRSETB�tB�wB�yB�s���1�f�p�l�EH��*CONDITION-HANDLERS*�B���j�*STANDARD-OUTPUT*���*PROVIDE-ERROR-MESSAGES*�B���STRING-UPCASE��*�READ-FROM-STRING�p�,��G5649��F����ERROR��p�B�\�ERRSET-HANDLER�i�EXPOSED-P��B�|�p�B�$�G5657��F���\���OPEN�\�i�DIRECTION��OUTPUT�B��Ҫ�EVAL�p�B�$�G5665��F���B�2�B�3�B�0�B���,�ERROR - ���WRITE-STRING�B�~�,� is a bad file.��B���p�B�Tl�LISTARRAY���SCROLL-TO-BOTTOMҬ�~%~s�B���B���B���l�~S�j�+��p�B�\l�SIMPLE-MAKE-ARRAY��l� ����Relation :  �B���B���,�    Database :  �B�����    Cardinality :  ��B���B����Relation :  ~S    Database :  ~S    Cardinality :  ~S���~%Relation :  ~S    Database :  ~S   Cardinality :  ~S�l�|��l�~s�B���B���l�@��B���B���B���B���B���B�P�B���B���v�݀7�JтQL�K�	�KQLSM�	�
�CK��L�L��JQ��PPTPPP��JCJ�PJCN��P��J!BJ!B\BA���Aۆ@�5�PPTPPP��JCK�PJCO��P�QP���J!BJ!B�PPTPPP��JCL�PJCJ�ֆQ@�PP���\B@��
��P��Q� P���Q@�A�!P�"�D�A�#���A��P�����Q$P�Q%�@�@Q$P�Q%�@Q&�S���L���Q	��Q�QAQ@QDQ�Q�ۊQ�Q
J'�O�OтQ�QL�K�J��JQKSLSQ�P�Q�QQ���(PPQ%��CCJ��K�L�K�L��OQ���Q�C�k�Q)PE�a	J����-J*�B��Q�C�k�Q)PE�a	J���� J*�G����A$�+PCF�,P-P�Q	�.P�/P-PP0P�1P�Q�F]F�BQF]F��Q!P��CE��EQ�J2P3�JEaH���Q��Q4P�Q	�P�Q%��Q��QBQ%��Q�@�@Q5P�Q	�P�Q%�@Q�@QBQ%�@Q�6P�C�O�OтQ�QL�K�J�/�JQKSLSS�R�T���7PRQ%�RÊCT�CQ-PTQS#�8PRQJSm9�:P�8PRQGQJSQTc9�;�RQ�<P��6P��C�CJ��K�L�K�L��A�CQF]F�O�O�CQL�K�9�KQLSU�V�U5�J�W�@�UWV��5�VS�VQ@Q=�W�J݅�UWV��5�VS�VQ�Q=�W�J�WQ�W�J�@�UQ@Q=�J�W݅�UQ�Q=�J�W�JQCK��L�L��@�@Q�@QBQ%�@Q�A�BQF]F���Q��QBQ%��Q�DQFQ>�?�D��
�DQ�Q�Q@QAQGQ�Q@�?�D�DQ�C�mI�@�@QBQ%�@Q&���QBQ%� �A�DQBQ�?�D�A	�APDQ��QHQIQ�BP3��O�LB���1�\�p�B�\,�FASL-RECORD-FILE-MACROS-EXPANDED\�B�8\�\��DEFUN���'\�B���-i\�B���~�z\�B���<p�\�B���`sN\�B���|��\�B����aM*\�B�y��(̢\�B�w��*�j\�B�t��=�#\�B�s��[�\�B�r��5%�\�B�q���9��))
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
  DIRECTORY            - New directory in which this relation is to be saved.  (MODIFY-RELATION RELATION &REST KEYWORD-LIST &KEY &OPTIONAL RELATION-NAME ADD-ATTRIBUTES DELETE-ATTRIBUTES RENAME-ATTRIBUTES IMPLEMENTATION-TYPE STORAGE-STRUCTURE FORMAT KEY DOCUMENTATION DIRECTORY &ALLOW-OTHER-KEYS)��B���B���l�~S�B��	�B���ÁADD-ATTRIBUTES�C�DELETE-ATTRIBUTES��C�RENAME-ATTRIBUTES��B���B���B���B���B���B���B���B���B��	�PA�P��PP�QP�QP�Q	P�Q
P�QP�QP�QP�QP QP�QP�QJ�@���A��Q@Q�O��	B�
	�1�\�B��\�B��\�B�8\�B�LMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540804. :SYSTEM-TYPE :LOGICAL :VERSION 2. :TYPE "XLD" :NAME "RELATION-OPS" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760360567. :AUTHOR "REL3" :LENGTH-IN-BYTES 12012. :LENGTH-IN-BLOCKS 24. :BYTE-SIZE 16.)                               pp2�\���COMPILE-DATA\��SW-MFG,�GODZILLA������F�F�\�p�,�COMPILER,�VERSION�\�F�F�p�B�),�OPTIMIZE-SWITCH�����QFASL-SOURCE-FILE-UNIQUE-ID�1�\�p�l�FS�MAKE-FASLOAD-PATHNAME�\��QUOTE�B�$\�B�8��NIL�\�B�8\���RTMS\�B�8��RELATION-OPS\�B�8��LISP\�B�8F���BASEF�
�FONTS�\���*CODE-FONT*��*COMMENT-FONT*�*STRING-FONT*�)�PACKAGE���RTMS��MODE��COMMON-LISP��ÁJOIN-INTERNAL���Q�S���Q@F���$��B�:p���TICL�ART-Q�]�F��:B�:B�:j�T�F�p���SYS�l�DEBUG-INFO-STRUCT�B�P\���KEYWORD-LISTB�:\�!��RELA-ATTRIBUTES-USER��RELB-ATTRIBUTES-USER�UNKNOWN-ATTRIBUTES-USER��PRINT�C�ALL-ATTRS���FROM-CLAUSE�ÁJRELB-PROJECT���JRELA-FORMAT��JRELB-FORMAT��JRELC-FORMAT��TEMPC�JOIN-ATTRC�JOIN-INSERT-LISTÀWHERE�ÀATTRSAÀATTRSB��A-JOIN-ATTRCÁREADER-PACKAGEÀJRELA�ÀJRELB�ÀJRELC���IMPA��SSA��ATTR-IMPC�JRELB-IMPLEMENTATION-TYPE��JRELB-STORAGE-STRUCTURE�C�JRELB-KEY�B�`B�:B�:C�ATTRIBUTE�B�:B�:\�)�MACROS-EXPANDED�\�p�B�T�INHIBIT-STYLE-WARNINGSp���ZLC�,�DO-NAMED�FOURTH�FIFTH��SIXTH��THIRD��SECOND�FIRST�p�B�Tl�COND-EVERY��PROG��SETF���� ���*DEFAULT-ANYP-WIDTH*у�*PKG-STRING*�C�*PROVIDE-WARNING-MESSAGES*��*PROVIDE-ERROR-MESSAGES*��ACTIVE-DATABASE���TERPRIҬ�ERROR - No parameters passed to JOIN���WRITE-STRING�B�e�\���INTOB�o��FROM��IMP���STO���KEY��FORMAT��DIR���DOC�B�eÀTUPLES�PROJECT�ÀUNIQUE��GET-KEYWORD-VALUE-PREREQ�\�B���C�GET-KEYWORD-VALUE��\�B�o�\�B���\�B���\�B�����LIST҃�VALIDATE-SYM��WARNING - More than two relations are provided for joining. The first two will be considered.����ERROR - The relations to be joined are not provided.�l�*���ADD-DOT�Ҫ�PACKAGE-NAME҆��C�PARSE-FROM-CLAUSE���GLOBAL�p�B�\�STRING-EQUAL*��ÀGLOBAL��ERROR - The FROM clause has not specified the relations to be joined.��\�l�ATTRIBUTES��IMPLEMENTATION-TYPE�l�STORAGE-STRUCTURE���KEY���TUPLE-FORMAT��CARDINALITY����GET-RELATION�l�ERROR - Relation ���PRIN1��,� does not exist���STRING-UPCASE�Ҫ�FIND-SYMBOL��C�CONVERT-ATTRIBUTESҪ�STRING-EQUAL�p�B�\��MEMBER-TEST��,�ERROR - �l� is not an attribute of relation ��,� is not an attribute of either relation���� is an attribute of both relations: �� and ��e�.�j�WRITE-CHARҬ�        It is unclear which attribute should be used�F���ÂPARSE-JOIN-ATTRIBUTES��p�B�\,�*APPEND��*�REVERSE���STRING�l�.����CONCATENATE�҃�PROJECT-LIST�F���C�JOIN-INTO��C�JOIN-EVAL���REMOVE-DOT-ATTR��\�B���ÁUNIQUE-TUPLES��,�INSERT-��l�-��\�B���\�B���p�B�T�STRING-APPEND���SYSTEM-RELATION��ÁRELATION-NAME��B���\�l�MODIFIEDP���CARDINALITY���DELETE-OR-MODIFY�,�SYSTEM-RELATION��\�l�MODIFIEDP��\�B���C�PRINTREL-INTERNAL*҃�JOIN���R�Q�[S[��[S�5��[Q�����	�
P�RR�K�P�S�P�Q�CC��CWC�P�Q���P�Q�BT�P�Q�B���M�P�Q�BE�P�Q��T�C�P��C�P�Q�BJ��J5�JQ�J�J�JS�R��R]�\�J�JW�S��R]�\�J�R�]�\�J�S�]�\�JQ
��	�P�]����]�\�R
�S�E��	�P�RS�RQS�E�RQP�SQP��E�R�RQ�S�SQ�C�Q�EQRQSQKQPQBQ@QAQPPAA�@�B�P�K�S�R�E�Q�R�RQ�C��S�SQ��PQ�QQP��QQ &�PQ�R�S
��	�!P�RS�RQS�RQ"P��#�N�N
��	�$P�RQ%�&P�RSQ"P��#�O�O
��	�$P�SQ%�&P�RNWBBOWBBy�NQJ�OQN�JQO�@QJ�AQ@�JQA�NSR�NWN�NWU�N[V�NUBG�NSN�OSS�OWO�OWX�O[Y�OQBZ�OUBH�OSO�R	�S�R�SQ'�(�S�
�S�SQ'�(�S�RQ'�(�R�@Q)�@�AQ)�A�BQ)�B�@Q\��\S^�NQ*P+��	�	�,P�^Q%�-P�RQ%�R\���AQ\��\S^�OQ*P+��	�	�,P�^Q%�-P�SQ%�R\���BQ\�6�\S^�NQ*P+��^QOQ*P+�
��	�,P�^Q%�.P�R^QNQ*P+��^QOQ*P+���	�,P�^Q%�/P�RQ%�0P�SQ%�1P2�	�3P�R\���KQPQFQNQOQRQSQ4P5PAF�P�K�]�]�NQ`�_��_QRQ`S�C_��`�`��]Q`�`�FQ]�\��\QSQ]S�C\��]�]��`Q6�D�J�KQ7�K�^��^S^U*P+�	�JQ8PSQ'�9P^S'�:��JQ^S'��6�J�^���JQ7�K�GQHQ6��DQPQ;�BI�T�W�TQKQPQ�QIQUQVQWQ<P=PAW�T��RRQSQNQOQFQZQMQYQXQQQ
J>�L�RQ'�SQ'��*�D�PQ^��^S^U*P+��DQ^S?��DQ^S�6�D�^���DQP�NQ`�`�FQ]�\��\QSQ]S�C\��]�]��`Q6�D�LQDQPQ;�L�@P�Q��L�LQA�L�TM�LK�BPW�8PWQB'�CPWUB�8PDP�Q�B�UQ'�CPEP�Q�B�VQ'�:�F�P(�`�TQKQLQW�WWB�KQTQ`�GP��*PHPTQI�'��JP��W�WWBB�JLQ�C�a�K�GP��*PHPLP�MP���K�P�Q��LCD�T�TQLQKQ��������W�WWBB�NP�Q�B�IQ����LQ�CJO�TC&�PPLQKQ��������NP�Q�B�CKQ�Cx�NP�Q�BP��KQ�CC6��NP�Q�B����LQ�CJO�STQ�R�O��B�P��B������`8F��$��B�:B�V]�F��:B�:B�:B�YF��^B��\��&REST�B�`��&KEYB��B��j�&OPTIONAL�B��B�oB��j�DIRECTORY��DOCUMENTATION�B����IMPLEMENTATION-TYPE�B��C�STORAGE-STRUCTURE�B�eB��B��j�&ALLOW-OTHER-KEYS�B�:\�B�`B��B��B�oB��B�B�B��B�B��B�B�eB��B��\��DOCUMENTATION���This function provides the capability to combine two relations into a new relation
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
   UNIQUE               - If T, only unique tuples will be part of the resultant relation.��\���FROM)�PROJECT��WHERE���INTOi�DIRECTORY�B��FORMAT��IMPLEMENTATION-TYPE���KEY�i�STORAGE-STRUCTURE��PRINT��TUPLES�UNIQUE�p�B�\�STORE-KEYARGS��B�P�@�@QP��A��@Q�O�B����ÁPROCESS-WHERE���U���A�F�g�$��B�:B�V]�F��:B�:B�:B�YF��^B�\�ÀTUPLEAB�pC�WHERE-EXP�B�tB�uB�qB�:\�C�NEW-WHERE�B�lB�:B�:B�:B�:C�X�\�B�\�p�B�\l�XR-BQ-LISTB��B��B��B���j�OR�B��B���B�8�B���B���p�B�\l�POSITION*��B���B���B���ÁREMOVE-DOT-REL�B����EQUAL��p�B�\l�MEMBER-EQL�B�֒�5-�BтUD�C��CQDS�5>BCC��D�D��BQPD�SD�DтUB�E��EQ�Q�QBS�Q�Q�Q�CE��B�B��DQ
C��5�Q�Q�Q��Q�Q�Q�B@�@�S&�Q@�@�Q����Q����QD�_�DSF��F7&�PFQ�	�!�FQ
���QP��FQ���Q��PFQ
���QP	��Q�C�A�@Q��FQ���+*��FQ��QP��PFQ��QP	��Q�C�A�@QAQ�F�F7�PFQ�	��FQ
���QP�
�FQ���Q��@QFQ
��@QFQ��@�Dš�@O�2B�����PROCESS-SET-RELATION��/q��@�F�B�$��B�:B�V]�F��:B�:B�:B�YF��^B�3\�B��B��B�:\�C�ATTRIBUTES�DOMAINS�B�:��ATT�B�:B�:��ATTR\�B�\�B��B��B��B��B�����*ACTIVE-DB*��B���\�l�ATTRIBUTES,�DOMAINS��B���B��Ҭ�ERROR - The �B���B���l� relation is not defined in the database ��B���B���B���,�ERROR - �� is not an attribute in the relation ��B���B�ђ�QP���@�@�	��P	��Q
�P	�P
���E�@S��@QBA�@Q	B@��;�5�Q����QB��BSC�@QP����P	�CQ
�P	��Q
�P���B���B�BсQE�D��DQESF�@Q�CFQ@QP��C�cAQ�CCD��E�E��BQA��Q@��Q@QAQ�O�KB�3��C�SET-COMPATIBILITY���0p��A0F�@�$��B�:B�V]�F��:B�:B�:B�YF��^B�L\���RELA��RELBB�pB�qB�:\���ATTRIBUTESA���ATTRIBUTESB��DOMAINSA�DOMAINSBB�:B�:B�:B�:ÀATTRA�ÀATTRB���DOMA��DOMB\�B�\�B��B���B���F���B�3�B���l�ERROR - Relations �B���B���� and ��l  do not have the same number of attributes, thus they are not compatible.��l�ERROR - Attribute �� of relation ��,� and attribute ���� are not compatible domains���Q�QPPAB�@����R�Q�QPPAC�A����R@Q�CAQ�C|���P��Q	�
P��Q	�P�R@QAQBQCQG�F�E�D�&�DSESFSGSK�J�I�H�JQK+���P�HQ	�P��Q	�P�IQ	�P��Q	�P�RD�E�F�G�D�E�F�G��SO�jB�L���SET-CREATE-RELC���B����@F�`�$��B�:B�V]�F��:B�:B�:B�YF��^B�k\�B�U��RELCB�`B�pB��B�:\���ATTRIBUTE-DESCRIPTORB�=B����DOM�B��B����MODP��QTRIEVE-VAR�C�SS��TUPLE-FORMATB�:B�:B�:B�@B�wB�=\�B�\�	B��B��B��B��B��B� B��B��B��������Â*SYSTEM-RELATION-KEY*�у�*SYSTEM-RELATION-ATTRIBUTES*�B���\�	l�MODIFIEDP��SAVE-DIRECTORYl�ATTRIBUTES��IMPLEMENTATION-TYPE�l�STORAGE-STRUCTURE���KEY���TUPLE-FORMAT��DOC�,�DOMAINS��B���B���B���B����QTRIEVE��\�B���B���\�B���\�B���\�B���\�B���\�B���B���B�w�B���B���B���B���B���B���B���ÀDEFREL�PPPPP	P�Q
���BG�GSF�P�Q�B�GWB�G[A�P�Q�B�GQBD�P�Q�B�GUBH�P�Q�B�Q��P�Q�BE�P�Q�B�JGQ�CI�JGQ�CC��!�5�Q���JуQL�K��KQLSM�AQ�CMQAQP��C�cCQ�CCK��L�L��JQC��QA�CQN�AQO��@QOSPNS���@�N�O���Q@QPDQPHQPEQPIQP�QPBQJ��O��B�k����RELATION-DIFFERENCE���#\ۆ��#F��$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B��B�`B��B��B��B��B�B�B��B�B�B��B�eB��B��B�B�:\�B�`B��B��B�B�B��B�B�B��B�eB��B��ÀTEMPA���TEMP-TUPLES�B�lÀTABLE�B�UB�VB�tÀPRINT1�TUPLES1�B�pÀWHEREAB�qÀWHEREBÀATTRSCB�sB�:ÀTUPLE�B�:B�"\�B�\�B��B��B��B��#Difference of the tuples in two relations.

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
   UNIQUE               - If T, only unique tuples will be part of the resultant relation.������*VALIDITY-CHECKING*��\�B�
B�B�B�B�B�B�B�B�B�B��B���ACTIVE-DATABASEP�l�DIFFERENCE�����ÂVALIDATE-RELATION-OPS�ҩ�TEST�B�/�*�MAKE-HASH-TABLE��B���B���B���B���B���\�B���B���B�o��RETRIEVE�*�GETHASH��p�B�T,�PUTHASH��B�=�ÀINSERT�*�CLRHASH��\�l�ATTRIBUTES��TUPLE-FORMAT�B��҃�UNCONVERT-ATTRIBUTES�B���C�DIFFERENCE�\�B���B�ے@�@QP��A����R@QP	P
PAS�Z�T�Y�R�X�W�Q�V�U�P��RPP�O�QQ��PWQP��PP@Q�BPXQ	J�N�[��[S\�\Q\QOQ�
COQ�[���PQ��P��PUQPP@Q�BPVQ	J�]�
�]S^�OQ��^QM]M�]���M�R�PL��RQPMQPYQ�LQ�OQ�T�MSC�R�RQP���BL�RQMQYQ�LSZQ���������LW����MQ�CJ�RS&�PQP���BL� PMQUQ�LSZQ���������!P@Q�B	�U�LULSUQ"�B�LW����MQ�CJ�SRQ�P�O��B����ÂRELATION-INTERSECTION���#YՆ��#F�|�$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B��B�`B��B��B��B��B�B�B��B�B�B��B�eB��B��B�B�:\�B�`B��B��B�B�B��B�B�B��B�eB��B��B��ÀTEMPB�B�lB��B�UB�VB�tB��B��B�pB��B�qB��B��B�sB�:B��B�:ÀTUPLEB\�B�\�B��B��B��B��#Intersection of tuples in two relations.

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
   UNIQUE               - If T, only unique tuples will be part of the resultant relation.����B���\�B�
B�B�B�B�B�B�B�B�B�B��B��B���l�DIFFERENCE�����B���B���B���B�o�B���B���B�/�B���B���B���B���B���B���\�B���B���B�=�B���B���\�l�ATTRIBUTES��TUPLE-FORMAT�B���B���B��Ҫ�INTERSECTION�\�B���B�ے@�@QP��A����R@QP	P
PAT�Z�S�Y�R�X�W�Q�V�U�P��RPQP��PUQPVQJ�N�PP�O�NQ[��[S\�\Q\QOQ�
COQ�[���QQ��P��PP@Q�BPXQPWQ	J�]�
�]S^�OQ��^QM]M�]���M�R�PL��RQPMQPYQ�LQ�OQ�S�MTC�R�RQP���BL�RQMQYQ�LSZQ���������LW����MQ�CJ�RT&�PQP���BL� PMQUQ�LSZQ���������!P@Q�B	�U�LULSUQ"�B�LW����MQ�CJ�SRQ�P�O��B����ÁRELATION-UNION��$Z؆��$F�~�$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B��B�`B��B��B��B��B�B�B��B�B�B��B�eB��B��B�B�:\�B�`B��B��B�B�B��B�B�B��B�eB��B��B��B�UB�VB�tB��B��B��B�lC�TEMP-UNIONB�pB��B�qB��B��B�sB�:B��B�:\�B�\�B��B��B��B��Union of tuples in two relations.

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
   UNIQUE               - If T, only unique tuples will be part of the resultant relation.����B���\�B�
B�B�B�B�B�B�B�B�B�B��B��B���l�DIFFERENCE�����B���B���B�/�B���B���B���B���\�B���B���B���B�o�B���B���B���B���B�=�B���B���B���\�l�ATTRIBUTES��TUPLE-FORMAT�B����UNION��B���\�B���B���B��@�@QP��A����R@QP	P
PAR�Z�Q�Y�O�X�W�N�V�U�M��RPP�P�MQ��PP@Q�BPUQPVQP��	J�S�[��[S\�\Q\QPQ�
CPQ�[���O�OQPSQPYQ�T�NQ��P��PP@Q�BPWQPXQ	J�]�
�]S\�PQ��\QT]T�]���T�O�PL��OQPTQPYQ�LQ�PQ�Q�SQTQ�R5�O�OQ��PYQ�OR*�MQP���BL�PSQTQ�UQ�LSZQ ���������!P@Q�B	�U�LULSUQ"�B�LW����SQTQ��CJ#�SOQ�M�O��B����ÁVALIDATE-WHERE��_І�AHF�q�$��B�:B�V]�F��:B�:B�:B�YF��^B��\���WHERE-CLAUSEC�RELA-NAME�C�RELB-NAME�ÀATTSA�ÀATTSB�B�:\�ÁTEST-ATTRIBUTEÁTEST-RELATION�\�B�\�B��B� B��B���B�C�B���B���,�ERROR - �B���B���l� improperly formed where subclause�B�.�B���B���B���� is an unrecognized attribute��,�ERROR -��l� is not a relation in the �l� database����S�Q��S�QJ�	�S�5�W�5�[�5
���P��Q�	P�R�W
�A��W�@�A�@Q�QP�A�@Q�QP�<���P�@Q�P�R�QA+�@Q�QP�*���P��W�P�R�QA+�@Q�QP����P��W�P�R��P�AQ�P�P�P�R�[
�A��[�@�A�@Q�QP��@Q�QP�	���P�@Q�P�R�QA+�@Q�QP�
���P��[�P�RS�QA+�@Q�QP�
���P��[�P�RS��P��[
��P�P�P�RO�B�����PRE-RELATION-OPS��9��@pF�F�$��B�:B�V]�F��:B�:B�:B�YF��^B�\�B�`B�:\�B�lB�UB�VB�tB��B�pB��B�qB��B����VAR�B�s\�B�\�B� B��B��B���\�B���B���B���B���B���\�B�o�\�B���B���\�B���\�B���P�Q�B@��@5�@Q�@�@S�A��AQ�C�K�@WJ��5�JQJ��PJQ�BF�	PJQ�B
�E�AQ@Y
C@�@W�B��BQ�C�K�@[J��5�JQJ��PJQ�BH�	PJQ�B
�G�P�Q�BC�P�Q�BD�C5�CWJ��5
�JQJ��	PJQ�B
�I�C5�CS�CQ�C�AQEQFQBQGQHQCQIQDQKQ�O� B���B����X���BF�h�$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B�gB�tB�uB�mB�rB�dB�bB�cB�:\���PERIOD-INDEXÂMYSTERY-RELATION-NAME�B�:B�@ÀTEMP1�\�B�\�B��B��B���B���B���B���p�B��l�STRING-SEARCH-CHAR�B�.�B���B���B���B����SUBSEQ�l�*��*�READ-FROM-STRING�B�ߒ�QB���BSC�D�C5.�QC�CS��CQ����QCW����PCWJ�����@�Q�CW�D���J�Q�DQ�	�D�DQ
���@�CW��
�����9�PCQJ�����@�"�CQ�D����Q�DQ�	�
�DQ
����CQ��
������QCQ�����QCQ������QCQ�����QCQ�����QCQ����C5�CWC�@+�CQ�@kJ@a�P	� �CQ�J@Q��AÁQ	��QCQ������AQ�Q	��QCQ�������QCQ��@�QCQ����B�\�Q�Q�Q�Q�Q�Q�Q�Q�O�4B����B����o���A�F���$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B�mB�rB�hB�pB�qB�tB�uB�:\�
B�lB�:B�@B�:B�:B�:B�:ÀATTR1�B�:ÀATTR2�\�B�\�B��B��B��B���B���l�*��B���B���B���B�0�B�.�B���B���B���B���B���B���B�-�B�ʒ�Q@���@QA�C�ASB��B7�BQ��QP����Q�Q/�B�B7�BQ��QP����Q�Q �PBQJ������QBQ	���PBQJ�����
�QBQ	���QBQ
����QBQ����AŽ�Q�@��ۂۅQ��Q���@Q�QP��QP���r�Q��@QC�v�CSBÅQP��r�QD�DуQF�E��EQFSG��QGQ��CE��F�F��DQT�BQ�QP��r�QF�FфQD�H��HQDSI��QIQ��CH��D�D��FQ����Q��5�PBQ�P��BQ
���QP��QBQ
�������QBQ�BQ��QP�
�QBQ��QP��Q�Q��QBQ������Q�QBQ������CŊ�Q�Q�Q�O�CB����B����$8���B$F�\�$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B�vB�mB�rB�`B�kB�wB�xB�yB�:\��lB�:B�:ÀA-ATTRB�@��ATTD\�B�\�� B��B��B��B��B���Â*SYSTEM-ATTRIBUTE-KEY*�Ã*SYSTEM-ATTRIBUTE-ATTRIBUTES*��\�l�ATTRIBUTES��IMPLEMENTATION-TYPE�l�STORAGE-STRUCTURE���KEY���CARDINALITY���TUPLE-FORMAT�B����SYSTEM-ATTRIBUTE�\�,�DOMAIN-FUNCTION��DEFAULT-VALUE���DOC����AND��B���B���B�.�B���B���ÁATTRIBUTE-NAME�B���B���B�w�p�B�T��DEF��B���B���B���\�B���B���B���\�B���B���\�B���B���\�B���B���\�B���\�B�����...��B����QP������e�Q@��ۂQ@QB�A�)�ASBSD�C�E�PPPP	P
PPCQ���
PPCQ�����BE��QDQPESPEWPE[�����A�B�A�B��Q�QPP�Q�B�QPP�Q�B�QPP�Q�BPP�Q�B�QP P�Q�BP!P�Q�B�"PJ�#������Q�Q��S�ÇQ�O�jB����B����^ۆ���F�}�$��B�:B�V]�F��:B�:B�:B�YF��^B��\�
B�tB�uB�pB�qB�hB�|B�oB�{B�zB�sB�:\�B�:B�:p�B�\l�LEX-PARENT-ENV-REGp�B�\�LEX-ENV-B-REG�p�B�\�LEX-CURRENT-VECTOR-REGp�B�\��LEX-ALL-VECTORS-REG�C�KEY-VALUE�B��C�RETRIEVE-FUNCTION���KEY-FUNCTIONB��C�INDEX-NAMEB�>B�?��NUM1��NUM2B��ÀTABLE1B�,B�lB�nB�:B�:B��B��B�lB�:ÀTEMP-T\��\���B��B� B��B��B����INTERNAL-FEF-OFFSETS\�F�F�i�VARIABLES-USED-IN-LEXICAL-CLOSURES\�B�nB�lB�B��B�~B�}B��B�|B�sB�oB�|B�hB�qB�pB�uB�t���E�B��������EXTRACT-KEY��B���l�RETRIEVE-��l�-��B���B���B��Ҭ�EXTRACT-KEY-�B�/�B���B���B���B��҃�ENTRY-POINT����GETP�B���B���B���\�F�F�F�F�F�
F�	F�F�F����������������������������������\�)�INTERNALB����*�MAPHASH��B���\�B��B��F��B�3҃�MAPT��Q�Q�Q�ۇQ�Q��PPA����F�K��Q��PP�Q	P�Q
�P�U��Q���Q�Q�Q�QFQ�Q��JU�G�PP�Q	P�Q
�P�H�PP�Q
�P�I��5z�S&w�WLÂQP��[MÃQP��WMÃQP�e�[LÂQP�_�Q�CLQ�QP��C�cN��Q�CMQ�QP��C�cO�PP�P�PP�Q��QP�V��VSW�NQWQ�CX�WQXQPQ�
CPQ�V���QP�U�"�USW�OQWQ�CX�Y�XQPQ�R��RQZ�	�ZS[�WQ[Q�Y]Y�Z���XQYQXQQQ��QQ�U���PP�CQQ�PQ�QQ�TPP�C�Q���TO��B����B����
��@�F��$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B��B��B�:\�B�:B�:B�u\�B�\�B��i�LEXICAL-PARENT-DEBUG-INFO�B�p�B�֒�Q�P����O��B����B����%R��@\F�-�$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B�"B�:\�B�:B�:B�uB�:B��B�:B��\�B�\�B��B��B��B��B�p�B���\�B�Y�B��B���B�ƒ�P�����PC�:�CSD��QDQ��\��C���0��P��*�����Q�P�P�P�P�P����P�P���P�PĪ�����PE��P���P�P�P�P�P�P��JźE�	�ESF��QFQ��\��E������P�P����RO��B����B����5|��@�F�G�$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B�`C�OPERATION�B�:\�B�pB�qB��B��B�s��REL-EXISTS?�B�UB�VB�tB��B��B��B��B�`\�B�\�B��B���B���B�e�\�B��B��B��B��B��B��B��B��B�eB��B���B��҆���B��B��Ҭ�ERROR - Relations to participate in relational set operation have not been provided.�B���B�L�\�B�=�B���,�Relation ~s of ~S and ~S�B���B�k��Q�MSM��MS�5��MQ��P�Q�CC��CWC�P�Q���PPAD�J�B�H�L�A�G�K�@�F�J�H�C�P��C�F�G��	�
P�RFQGQ@QAQ��RH�HQP���I�ISH�IQ	BE��FQHQ�Q@Q��P�QFQGQ���RFQHQ@QBQ��RFQ@QKQGQAQLQHQBQJQDQCQ�O��B���1�\�p�B�\,�FASL-RECORD-FILE-MACROS-EXPANDED\�B�8\�\��DEFUN���'\�B�*��.ً\�B����[�\�B����=�#\�B�
���9\�B����z(�\�B����{��\�B� ��:}n\�B����{Ĳ\�B����Zi�\�B����x��\�B����*�j\�B����(̢�� where-clause-A)] RelB
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
	���PBQJ�����
�QBQ	���QBQ
����QBQ����AŽ�Q�@��ۂۅQ��Q���@Q�QP��QP���r�Q��@QC�v�CSBÅQP��r�QD�DуQF�E��EQFSG��QGQ��CE��F�F��DQT�BQ�QP��r�QF�FфQD�H��HQDSI��QIQ��CH��D�D��FQ����Q��5�PBQ�P��BQ
���QP��QBQ
�������QBQ�BQ��QP�
�QBQ��QP��Q�Q��QBQ������Q�QBQ������CŊ�Q�Q�Q�O�CB����B����$8���B$F�\�$��B�:B�V]�F��LMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540810. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "XLD" :NAME "RENAME" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760360779. :AUTHOR "REL3" :LENGTH-IN-BYTES 4902. :LENGTH-IN-BLOCKS 10. :BYTE-SIZE 16.)                                      pp2�\���COMPILE-DATA\��SW-MFG,�GODZILLA����\�F�F�\�p�,�COMPILER,�VERSION�\�F�F�p�B�),�OPTIMIZE-SWITCH�����QFASL-SOURCE-FILE-UNIQUE-ID�1�\�p�l�FS�MAKE-FASLOAD-PATHNAME�\��QUOTE�B�$\�B�8��NIL�\�B�8\���RTMS\�B�8�RENAME\�B�8��LISP\�B�8F���BASEF�
�FONTS�\���*CODE-FONT*��*COMMENT-FONT*�*STRING-FONT*�)�PACKAGE���RTMS��MODE��COMMON-LISP��RENAME-ATTRIBUTEO�P��RENAME-ATTR��B�P��!TɆ�`!xF�u�$��B�:p���TICL�ART-Q�]�F��:B�:B�:j�T�F�p���SYS�l�DEBUG-INFO-STRUCT�B�P\�ÁRELATION-NAME��&REST�C�ATTRIBUTESB�:\�B�cÁRELATION-TUPLE��POS�ÀPOS-K���STO���REL-OWNER-IDÁATTRIBUTE-LIST�KEY-LIST��IMP�ÀATTR-LÁATTRIBUTE-NAMEC�NEW-ATTRIBUTE-NAMEB�cB�:\�)�MACROS-EXPANDED�\��SECOND*�SEVENTH��SIXTH��FOURTH�THIRD��FIRST���PROG��SETF�DOCUMENTATION��Use this function to rename attributes in a relation.

   RELATION-NAME  - Name of the relation whose attributes are to be renamed.
   ATTRIBUTES     - Specify old-attribute and new-attribute names.

   Example: (RENAME-ATTRIBUTE 'parts 'number 'id 'name 'description).����*PKG-STRING*�C�*PROVIDE-STATUS-MESSAGES*�у�*ACTIVE-DB*���*PROVIDE-ERROR-MESSAGES*�C�*SYSTEM-RELATIONS*��ACTIVE-DATABASE�҃�VALIDATE-SYM�C�CONVERT-ATTRIBUTESҪ�STRING-EQUAL�p�B�]��MEMBER-TEST���TERPRI�l�ERROR - The attributes cannot be renamed because ����WRITE-STRING��PRIN1��� is a system relation.�\�,�OWNER-ID�SAVE-DIRECTORYl�ATTRIBUTES��KEY���TUPLE-FORMAT��IMPLEMENTATION-TYPE�l�STORAGE-STRUCTURE���DOC����GET-RELATION��ERROR - The relation ��,� is not defined in the database ��EQUAL��p�B�]l�POSITION*���ERROR - The attribute ��� is not defined in ��e�.�j�WRITE-CHARҬ� is already defined in the relation �p�B�U�FIRSTN�p�B�],�*APPEND���The attribute ��� will be renamed to ���R�Q��	����R@Q�ISI�IS�5��IQ@�
�@��QPP�
���P��Q�P�R�QP���BA��	��P��Q�P�P�RASE�A[F�AQBG�AYBH�AQBBD�@S@W@YL�K�J�Y�JQ��	�J��RJQFQP�B����P�JQ�P��Q�P�RKQFQP����P�KQ�P��Q�P�RBQFQ�KQBkFQ
C
C�F�JQGQP�C�
�CQGQ�KQCkGQ
C
C�G���P�JQ� P�KQ�P�LSJ�LWK�L�K��SO��B�P��ÂRENAME-ATTRIBUTE-ARRAY��
��@F��$��B�:B�W]�F��:B�:B�:B�ZF��_B��\�B�aB�jB�kB�c�IGNOREB�:B�:B�:���RENAME-ATTRIBUTE-UTILITY-ARRAY-LIST���Q�Q�Q�Q�O��B�����RENAME-ATTRIBUTE-FLAVOR���
��@F��$��B�:B�W]�F��:B�:B�:B�ZF��_B��\�B�aB�jB�kB��B�eB�:B�:B�:�C�RENAME-ATTRIBUTE-UTILITY-REDEF-REL��Q�Q�Q�Q�O��B����ÂRENAME-ATTRIBUTE-LIST���
��@F��$��B�:B�W]�F��:B�:B�:B�ZF��_B��\�B�aB�jB�kB�cB��B�:B�:B�:�B����Q�Q�Q�Q�O��B�����RENAME-ATTRIBUTE-STRUCT���
��@F��$��B�:B�W]�F��:B�:B�:B�ZF��_B��\�B�aB�jB�kB��B�eB�:B�:B�:�B����Q�Q�Q�Q�O��B����B����G��AF�,�$��B�:B�W]�F��:B�:B�:B�ZF��_B��\�B�aB�jB�kB�cB�:\�B�nB�oB�c\�B�q\�B�sB�xB�yp�B�]l�XR-BQ-LIST��SYSTEM-RELATION��B���B�a��STRINGҪ�LIST�\�l�MODIFIEDP�l�ATTRIBUTES��KEY��B�8��DELETE-OR-MODIFY��SYSTEM-ATTRIBUTE���AND��B�n�\��ATTRIBUTE-NAME�B���ÂSAVE-SYSTEM-RELATIONS��P��PP�Q��P��	P�Q�	P�Q��
��S�W�YB�A�@��P��PPP�Q��PP@Q���PP	PAQ���
�BS@�BWA�B�A���O��B����B����<��A F�$�$��B�:B�W]�F��:B�:B�:B�ZF��_B��\�B�aB�jB�kB�eB�:\�ÂSYSTEM-ATTRIBUTE-LIST�ÁATTR-DES-PAIR�C�TUPLE-LISTB�lC�SSC�TUPLE-FORMAT-LIST���DOC���DIR�\�B�q\�B�s�FIFTH�B�tB�uB�z�C�GET-SYSTEM-ATTRIBUTE-LIST��ÂCREATE-ATTR-DESCRIPTOR��STRING-UPCASE��*�READ-FROM-STRING�ÀTUPLES��RETRIEVE҃�REDEFINE-REL�B���ÁSAVE-RELATION���Q�@��YBC��QBBD��UBE�J�Q�CF��WG��Q@Q�A��Q��P���B��Q��AQCQDQ�QEQFQGQBQ	J	�
��Q���O�	B����RENAME-DATABASE�O�
C�RENAME-DB��B�
��7y)��`74F���$��B�:B�W]�F��:B�:B�:B�ZF��_B�
\�B�bC�DATABASES�B�:\�B�ÁDATABASE-NAME�C�NEW-DATABASE-NAME�*�PATHNAME��PATH�REL-NAME�SAVE-DIR��RELATION-TUPLE-LIST���DB-LB�:B�:B�:C�REL-TUPLE�\�B�q\�p���ZLC�,�DO-NAMEDp�B�U�INHIBIT-STYLE-WARNINGSB�sB�xB�yB�zB�{,�Used to rename a database.

   DATABASES - Specify old-database-name and new-database-name.

   Example: (RENAME-DATABASE parts suppliers micro-parts micro-suppliers).��B�~�B���Â*SYSTEM-RELATION-KEY*�у�*SYSTEM-RELATION-ATTRIBUTES*�B�}у�*SYSTEM-RELATION-STORAGE-STRUCTURE*��Ä*SYSTEM-RELATION-BASE-IMPLEMENTATION*��B���B��B���B���p�B�]�STRING-EQUAL*��B����ERROR - The database to rename has to be the active database ��B���B���B���B����ERROR - The new database name �,� is identical to the actual name�B���l�RETRIEVE-��l�-����CONCATENATE�Ҫ�FIND-SYMBOL��B���\��RELATION-NAME��SAVE-DIRECTORY�C�GET-SAVE-DIRECTORYҬ�.XLD�j�PROBE-FILE��.LISP���.XFASL��.QFASL�\��MEMBERB�aB����TEST\�B�8B���\�l�MODIFIEDP��B���B���B��Ҫ�DELETE-FILE��l�.����XLD��B���B����XLD#>����RENAME-FILE���QFASL���XFASL����LISP�ÁSAVE-DATABASE���The database ��� has been renamed to ����Renaming database completed.���R@Q�HSH�HS�5��HQ@�@S���A�@W���B�A�B�RAQP�
�
��P�P�P�RBQAQ�
�
��P�BQ�P�RBQ�PP	PPP�P�I�PPPP����PJI�G��F�PFQAQP� ��PFQAQ!P� ��PFQAQ"P� ��PFQAQ#P� �D�v�P��$P%P��&�'�(�DQ)�I�I�GQK�J�c�JQKSL�LSE�LWF�PFQAQPEQ*P�C�D�PCQ+P� �D��EQP,P-��DQPFQBQPEQ*P.PJ�/�7�PCQ+P�)�1�PCQ0P� �D�	�EQP,P-���PCQ0P��PCQ1P� �D�	�EQP,P-���PCQ1P��PCQ2P� �D�	�EQP,P-���PCQ2P����CJ��K�K���P3���4P�AQ�5P�BQ�P��6P�BO�OB�
��RENAME-RELATION�O�PC�RENAME-REL�B�P��7�;���7F���$��B�:B�W]�F��:B�:B�:B�ZF��_B�P\�B�bC�RELATIONS�B�:\�B�ZÂLAST-NEW-RELATION-NAMEB�fB�eB�iB�lB�B�B�ÀRESULTC�DB-RELATIONS-LIST�B���CALL-SAVE-DBÀREL-L�B�:B�aC�NEW-RELATION-NAME�B�Z\�B�q\�B�uB�sB�xB�yB�zB�{,�Rename relations in the active database.

   RELATIONS - Specify <old-rel-name new-rel-name>

   Example: (RENAME-RELATION rel1 new-rel1 rel2 new-rel2)�������*AUTO-SAVE*��B�~�B��B���B���B�'�B�(�B�}�B�)�B�*�B���B���l�RETRIEVE-��l�-��B�2�B�3�B���\��RELATION-NAME�,�OWNER-ID�B���B���*�REVERSE��B���B��B���B���B����ERROR - The relation ��B���B��Ҭ� cannot be renamed because it is a system relation.��,� is not defined in the database �B���B��Ҭ� is already defined in the database �B���B���\�,�OWNER-ID�SAVE-DIRECTORYl�ATTRIBUTES��KEY���TUPLE-FORMAT��IMPLEMENTATION-TYPE�l�STORAGE-STRUCTURE���DOC��B���,�RENAME-RELATION-�B���l�.����XLD��B�9��XLD#>��B�G��XFASL���QFASL����LISP�B����The relation ��� has been renamed to ����R@Q�MSM�MS�5�MQJ���MQ@�PPPPP�P�N�P
PP	P����PJN�K�M��MQBPP��MQBI]I�M���IQ�J�@S@W@YQ�P�O�X�OQ�O�PQ�P�O�P�ROQ�JQP�B���OQ�PP���P�OQ �!P�R�P�OQ �"P�P �#P$�RPQ�JQP��PQ�PP����P�PQ �%P�P �#P$�RBQJQ&�PQ�BkJQ
C
C'�J�QSO�QWP�Q�P��@S�@W�@YQ�P�O�c�OQ(P��)�BC�CSD�CYBE�P*PEQ�P�N�OQPQCQN�CWH�PHQPPOQ+�,P�F�G�PFQ-P�.�G��GQPHQPPPQ�,P/PJ�0��PFQ1P�.�G���PFQ2P�.�G���PFQ3P�.�G���L�G�4���5P�OQ �6P�PQ �#P$�PQA�QSO�QWP�Q�P��SO��B�P��ÂRENAME-RELATION-ARRAY���"��@�F��$��B�:B�W]�F��:B�:B�:B�ZF��_B��\�B�aB�aB��B�:\�C�ARRAY-NAMEÁNEW-ARRAY-NAME\�B�q\�B�߀B����ARRAY��B�2�B���INTERN�p�B�U��COPY-ARRAY-CONTENTS��B��Ҫ�EVAL�C�RENAME-RELATION-UTILITY-ARRAY-LIST�P�QP���@�P�QP���A�P@QAQ	�
��Q�Q�O��B����ÂRENAME-RELATION-FLAVOR��F��F��$��B�:B�W]�F��:B�:B�:B�ZF��_B��\�B�aB�aB�eB�:B�:B�:�C�RENAME-RELATION-UTILITY-REDEF-REL���Q�Q�Q�O��B������RENAME-RELATION-LIST��F��F��$��B�:B�W]�F��:B�:B�:B�ZF��_B��\�B�aB�aB��B�:B�:B�:�B����Q�Q�O��B����ÂRENAME-RELATION-STRUCT��F��F��$��B�:B�W]�F��:B�:B�:B�ZF��_B��\�B�aB�aB�eB�:B�:B�:�B����Q�Q�Q�O��B����B����OF��F�1�$��B�:B�W]�F��:B�:B�:B�ZF��_B��\�B�aB�aB�:B�:\�B�q\�B�߀B���B���B�a�B��B���\�l�MODIFIEDP��RELATION-NAME��B��B�8�B���B���B���\��RELATION-NAME����SYSTEM-INDEX���ENTRY-POINT����GETP҃�PUTP�P��PP�Q��P��	P
P�Q�����P��PP�Q��P	P
P�Q�����P��PP�Q��P	P
P�Q������Q�QP�P��Q��P�O��B����B����*>���@*�F�h�$��B�:B�W]�F��:B�:B�:B�ZF��_B��\�B�aB�aB�eB�:\�B��B��B��C�INDEX-LISTB�lB��B��B�jB�kB��B���DOMAINS�B�:C�INDEX-INFOB�:\�B�q\�B�xB�yB�#B��B�sB�vB�wB�B�tB�uB�z�B�}�B�'�B�(�C�*SYSTEM-INDEX-KEY*�C�*SYSTEM-INDEX-ATTRIBUTES*��B��B��B��B��B�l�C�MODIFIEDP��B�h���KEY���FORMAT�B���B����DEFINE-RELATION��B���B���B�a�B���B���\��RELATION-NAME��B��B�8�B���B���\�l�INDEX-NAMEl�INDEX-TYPE��KEY��B���QTRIEVE��B���\�,�DOMAINS��B���,�DELETE-��l�-��B�2�B�3�ÀINSERT��DESTROY-RELATION��Q�@��YBD��QBBE��UBF��[G��QBH�J�Q�CI��WJ�GQ@Q	�A��Q
P���B��QAQPDQP��PEQPHQPFQPIQPJQJ�P��PP�Q��PPP�Q�����PPPPPP�Q�� �C��!PP"PPPP�Q�� �BK�CQL��LSM�#P$PDQ%PMW&�P'�N��QGQM[����MSN�L���B�Q
PBQ�(��Q)�O��B���1�\�p�B�],�FASL-RECORD-FILE-MACROS-EXPANDED\�B�8\�\�p�B�U��DEFF��b\��DEFUN���'\�B�%��(̢\�B�#��*�j\�B���Zi�\�B�߆�.ً\�B�z��[�\�B�y��=�#\�B�x��z(�\�B�w��:}n\�B�v��x��\�B�u��{Ĳ\�B�t��2�=\�B�s��{����dir new-database-name
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
  (rename-relation-utility-redef-rel relation-name new-relation-LMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540817. :SYSTEM-TYPE :LOGICAL :VERSION 2. :TYPE "XLD" :NAME "RESTORE" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760360130. :AUTHOR "REL3" :LENGTH-IN-BYTES 4432. :LENGTH-IN-BLOCKS 9. :BYTE-SIZE 16.)                                      pp2�\���COMPILE-DATA\��SW-MFG,�GODZILLA������F�F�\�p�,�COMPILER,�VERSION�\�F�F�p�B�),�OPTIMIZE-SWITCH�����QFASL-SOURCE-FILE-UNIQUE-ID�1�\�p�l�FS�MAKE-FASLOAD-PATHNAME�\��QUOTE�B�$\�B�8��NIL�\�B�8\���RTMS\�B�8,�RESTORE�\�B�8��LISP\�B�8F��FONTS�\���*CODE-FONT*��*COMMENT-FONT*�*STRING-FONT*���BASEF�
)�PACKAGE���RTMS��MODE��COMMON-LISP����GET-RELATION��X����F�9�$��B�:p���TICL�ART-Q�]�F��:B�:B�:j�T�F�p���SYS�l�DEBUG-INFO-STRUCT�B�P\��RELATION��PROJECT-LISTÀMANIP?j�&OPTIONAL�\��SAVING?�B�:B�:\���QTRIEVE-VAR�\�)�MACROS-EXPANDED�\�p�B�\l�XR-BQ-LIST��SETF�p�B�T,�USER-ID��C�*SYSTEM-VIEW-KEY*���*SYSTEM-VIEW-ATTRIBUTES*�Â*SYSTEM-RELATION-KEY*�у�*SYSTEM-RELATION-ATTRIBUTES*�C�CONVERT-ATTRIBUTES��SYSTEM-RELATION��\���DISK�SAVE-DIRECTORY�p�B�\,�*APPEND�Ҫ�STRING-EQUAL�ÁRELATION-NAME���STRINGҪ�LIST��QTRIEVE�҃�SYSTEM-VIEW��\�,�VIEW-DEFINITION����AND��C�VIEW-NAME���OWNER-ID���EVAL�B�P҃�DIR��ÁLOAD-RELATION���Q���	PP
P�Q�PPP�Q���B@� ��PPPPPPP�Q��PPP���B@��@Q��ÁQ�Q��Q���@��@��QP@W��Q@Y�O� B�P�ÁLOAD-DATABASE�O���LOAD-DB��B����m�A���m@F�W�$��B�:B�V]�F��:B�:B�:B�YF��^B��\�ÀDBNAME�&REST���KEYWORD-LIST��&KEYB�cj�DIRECTORY�j�&ALLOW-OTHER-KEYS�B�:\�B��B��*�PATHNAME�TEMP-DIR��TEMP-STATUS���DIR-CHANGED?B�:B��B�:B�:B�:p�B�\��TEM�B�`�SYS-TUP���REL���PATHB�:B�:p�B�\��.CASE.ITEM.�\�B�i\�B�lp�B�T��SENDp�B�\��SELECT-MEMQ�p�B�Tl�CONDITION-CASE-IF�p�B�T�CONDITION-CASE�UNLESSp���ZLC�,�DO-NAMEDp�B�T�INHIBIT-STYLE-WARNINGSp�B�Tl�CONDITION-BIND-IF�p�B�T�CONDITION-BINDp�B�T�CATCH-CONTINUATION-IF�p�B�Tl�CATCH-CONTINUATIONp�B�T�ERRSET��PROGB�m��WHEN�DOCUMENTATION���A database saved on the disk can be loaded using this function.

   DBNAME    - Name of the database to be restored.
   DIRECTORY - Name of the directory in which it can be found.����� �p�l�EH��*CONDITION-HANDLERS*�Ã*SYSTEM-STORAGE-STRUCTURE-KEY*�Ä*SYSTEM-STORAGE-STRUCTURE-ATTRIBUTES*��C�*SYSTEM-RELATIONS*�C�*PROVIDE-STATUS-MESSAGES*��Á*DONOT-COMMIT*у�*RESTORE-OPERATION*���*SAVE-DIRECTORY*у�*ACTIVE-DB*���*PROVIDE-ERROR-MESSAGES*�B�r�B�sу�*PKG-STRING*у�*SYSTEM-RELATION-STORAGE-STRUCTURE*��Ä*SYSTEM-RELATION-BASE-IMPLEMENTATION*��\�i�DIRECTORY��p�B�\�STORE-KEYARGS��F����RESTORE���ACTIVE-DATABASE��B�}�l�RETRIEVE-��l�-����CONCATENATE�Ҫ�FIND-SYMBOL��B�u�\��RELATION-NAME��B�{�C�MODIFIEDP��l�T��B�~��TERPRI�,�ERROR - ���WRITE-STRING��PRIN1��� is the current database and it has modified relations���          Please resolve this conflict by either saving or destroying this database����          before restoring a saved database����VALIDATE-SYM�B���\�B����GET-KEYWORD-VALUE-PREREQ�ÁGET-DIRECTORY��,�system-relation��l�.��p�,��G3813��F�P��ERROR��p�B�\�ERRSET-HANDLER�p�B�4�DIRECTORY-LISTҬ�.XLD�j�PROBE-FILE��.XFASL��.LISP���ACTIVE-DATABASEP��DESTROY-DATABASE�p�B���G3821��F���)�VERBOSE����LOAD�l�ERROR - Database ��� does not exist in directory ��ÁCOMMIT-TUPLES����PUTP�*�REVERSE��B���B��\��RELATION-NAME��SAVE-DIRECTORY�B�a�p�B�\��MEMBER-TEST��p�B���G3856��F��\�p���NET��NETWORK-ERROR�p�B�l�UNKNOWN-HOST-NAME��p�B�\��CONDITION-CASE-THROW��.QFASL�)�CONDITION-NAMES��B��B��B�|�\��SAVE-DIRECTORY��DELETE-OR-MODIFY҃�DESTROY-REL��p�B�\�STRING-EQUAL*��\�l�MODIFIEDP��\�B�:�\���NOT�\��EQUAL��\��SEARCH�SYSTEMB�|�\���DISK�\�B�Y�\�B�'�\�B�*�SYSTEMB�|�ÁINIT-WHERE-OPT��SYSTEM-STORAGE-STRUCTURE�\��STORAGE-STRUCTURE-NAME�\�B�{ÂSTORAGE-STRUCTURE-NAME��ISAM���ATTR�ÀWHERE���VALUES�\���AVL��ÁMODIFY-TUPLES��ÁSYSTEM-OPTFUNC�\�B�{ÂSTORAGE-STRUCTURE-TYPE��ISAM�ÀTUPLES�\�\�l�=���AVL��OPT-AVL-EQUAL���RTMS\�l�<���AVL�l�OPT-AVL-LT��RTMS\�l�>���AVL�l�OPT-AVL-GT��RTMS\�l�<=��AVL�l�OPT-AVL-LT��RTMS\�l�>=��AVL�l�OPT-AVL-GT��RTMS\���AND���AVL���OPT-AVL-AND���RTMS\��EQUAL���AVL��OPT-AVL-EQUAL���RTMS\��LESSP���AVL�l�OPT-AVL-LT��RTMS\���STRING-LESSP��AVL�l�OPT-AVL-LT��RTMS\�,�GREATERP��AVL�l�OPT-AVL-GT��RTMS\�,�STRING-GREATERP���AVL�l�OPT-AVL-GT��RTMS\�l�OR��AVL�l�OPT-AVL-OR��RTMS\���STRING-EQUAL��AVL��OPT-AVL-EQUAL���RTMS�ÀINSERT��ERROR - The Directory �,� does not exist��@�@QP��A����P��UP�,�PPPPP�P�F�PPPPP P!P"���PJF���#�$P%�P&�'P%�#�(P%�#�)P%���]�Z��Q��*�����@Q�GSG��GS�5��GQ@��+PP"�@�,P@Q-�@�P@Q.�CÀQP/P0P�B�1P2PT3P4P1P��JCH�PJCI��CQ5�"�J!BJ!B�\�\�r�CQ�
�	�PP�Q6P�B�7��PP�Q8P�B�7��PP�Q9P�B�7�#�:��P;�<P=PT3P4P<P��JCF�PJCJ��BQ>P������?�"�E\�\K��	�#�@P%��Q&�AP%�P&�	�n�
�	�J�J�PH�F�	�FQHS��BPC�CF��H�H��J�J�PD�H�F��FQHSL�+PCQE�
���CF��H�H��
�PPPP��F�I���ISM�N�O�MQ"�PGPH�BN�NWO�NSN�PPI�{�JPKPTLPMPJPJCP�PJCQ��POQ�QPNQ6P�7��POQ�QPNQ9P�7��POQ�QPNQ8P�7��POQ�QPNQNP�7�J!BJ!B�\
�\Q�OPQ�R�PPR�QP�<�PCQ�QPNQ6P�7��PCQ�QPNQ9P�7��PCQ�QPNQ8P�7�	�PCQ�QPNQNP�7��P��PRPNQ"�SPCQ"�T��PD��NQU�DQ��OQCQV��E�I�k�P����WPXPT�P��YPZP[PT�E�P��\PSPCQ"�T�]�^PP_PP`PF��^PaP_PbP`PcPdPJe�fP��gPT�fPhPiPj��r#�kP%�CQ&�lP~�Q]�Z�J]�ZP
�O��B����LOAD-ENVIRONMENTO���LOAD-ENV�B����&B���`&\F�h�$��B�:B�V]�F��:B�:B�:B�YF��^B��\��ENVNAME�B��B��B��B�cB��B��B�:\�B��B��B��B��B��B�:B�:\�B�i\�B��B��B��B��B��B��B�mB��,�Load a saved environment.

   ENVNAME   - Name of the environment to be restored.
   DIRECTORY - Name of the directory in which it can be found.�B���C�*ENVIRONMENT-NAME*�B���B�Б\�B���B���B���\�B���B���B�}�p�B���G3959��F���B���B���B���B���B�~�B����ERROR -The directory ��B���B���,� does not exist.�l�rtms-environment-��B��Ҭ�.XLD�B����.XFASL��.LISP��B��B��B� Ҭ�Environment �,� defined���ERROR - Environment �� does not exist in directory ��@�@QP��A���Q��	����R@Q�DSD��DS�5��DQ@�
P@Q�@�PPPTPPP��JCE�PJCF��@Q�C���J!BJ!B�\�\��CQ
���P�CQ�P�RP�Q�B�PBQP��B��PBQP��B��PBQP��B�	�BQP������ ��P�Q!�
���"P��Q�#P��
��$P��Q�%P�@Q��RO��B����B����J�����J@F���$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B�|B��B��B��B�cB��B��B�:\�B��B���INDICES�B����XLDFÀXFASLFÀQFASLFÀLISPF�ÁRELATION-INFO�ÀTEMP1�ÀTEMP2�B��B��B�:B�:B�:B�:B��C�INDEX-INFO\�B�i\��THIRD��SECOND�FIRST�B��B�lB��B��B��B��B��B��B��B�mB��,�Load a saved relation.

   RELATION-NAME    - Name of the relation to be restored.
   DIRECTORY        - Name of the directory in which it can be found.���� �B���C�*SYSTEM-INDEX-KEY*�C�*SYSTEM-INDEX-ATTRIBUTES*��B���B���B�r�B�s�B���B���B���B�ʑ\�B���B���F���B���B���B�{�B��C�RELATIONP��B��Ҭ�ERROR -The relation �B���B���,� does not exist in the database �B���B�u�\��SAVE-DIRECTORY�B�|��STRING-UPCASE��B�~�B��\�B���B���p�B���G4015��F���B���B���B�}�p�B���G4023��F��B���B����ERROR -The directory ��,� does not exist.�l�-��B��Ҭ�.XLD��.XFASL��.QFASL��.LISP��B����CREATION-DATE����OPEN�B��B���CLOSE�Ҭ�SYSTEM-INDEX�B� ҃�DEFINE-SYSTEM-INDEX��ÂCOMMIT-SYSTEM-RELATION�\�B�{B�|��SYSTEM-INDEX�\�l�MODIFIEDP��\�B�Y�B��\���DISK�\�B�:���SYSTEM-INDEX�\�l�INDEX-NAME��KEY�l�INDEX-TYPE�\�l�ATTRIBUTES��IMPLEMENTATION-TYPE�l�STORAGE-STRUCTURE��ÂCREATE-INDEX-RELATION��@�@QP��A����P��U����]�Z��Q�������QPP��Q�����P��Q�P�P���@Q�LSL��LS�5��LQ@�@�P�QPP��P�P
PP	PPP�Q �!�"�B!�@�#P@Q$�@�%P&PT'P(P%P��JCM�PJCN���)P*P+PT'P(P*P��JCO�PJCP��@Q,�K�-�!�J!BJ!B�\�\��KQ���.P�KQ�/P���\�P0P�Q1�C�)PCQ2P1�D�)PCQ3P1�E�)PCQ4P1�F�)PCQ5P1�G�DQ6��EQ6��7PDQ8�I���7PEQ8�J���y�DQ�EQ9P������:�IQ;�JQ;�5�DQ6�*�EQ6��EQ&�FQ6��FQ!�GQ6��GQ��Q<P=�
�>��?�P��@PAPBPC������P��Q�P�P���DQ9P������:�!�E\�\Q�P��PP�Q �!�DPEPC��QPP�'�FPPGPPPP�Q!�"�B��P
PHP	PPP�Q �!�"�BH�BQO��OSR��QRSHSRWR[HWH[JI�O���ڀQ]�Z�J]�ZP�O��B���1�\�p�B�\,�FASL-RECORD-FILE-MACROS-EXPANDED\�B�8\�\��DEFUN���'\�p�B�T��DEFF��b\�B����z(�\�B����{��\�B����:}n\�B����,a\�B����=�#\�B����-i\�B����~�z\�B����<p�\�B����`sN\�B����|��\�B����(̢\�B����*�j\�B����6\�B����+��\�B����jA�\�B����<i�\�B����aM*\�B�m��[�\�B�l��.ً��VL-AND" "RTMS")
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
  DIRECTORY            - New directory in which this relation is to be saved.  (MODIFY-RELATION RELATION &REST KEYWORD-LIST &KEY &OPTIONAL RELATION-NAME ADD-ATTRIBUTES DELETE-ATTRIBUTES RENAME-ATTRIBUTES IMPLEMENTATION-TYPE STORAGE-STRUCTURE FORMAT KEY DOCUMENTATION DIRECTORY &ALLOW-OTHER-KEYS)��B���B���l�~S�B��	�B���ÁADD-ATTRIBUTES�C�DELETE-ATTRIBUTES��C�RENAME-ATTRIBUTES��B���B���B���B���B���B���B���B���B��	�PA�P��PP�QP�QP�Q	P�Q
P�QP�QP�QP�QP QP�QP�QJ�@���A��Q@Q�O��	B�
	�1�\�B��\�B��\�B�8\�B�LMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540824. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "XLD" :NAME "RETRIEVE" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760360396. :AUTHOR "REL3" :LENGTH-IN-BYTES 13504. :LENGTH-IN-BLOCKS 27. :BYTE-SIZE 16.)                                   pp2�\���COMPILE-DATA\��SW-MFG,�GODZILLA������F�F�\�p�,�COMPILER,�VERSION�\�F�F�p�B�),�OPTIMIZE-SWITCH�����QFASL-SOURCE-FILE-UNIQUE-ID�1�\�p�l�FS�MAKE-FASLOAD-PATHNAME�\��QUOTE�B�$\�B�8��NIL�\�B�8\���RTMS\�B�8,�RETRIEVE\�B�8��LISP\�B�8F��FONTS�\���*CODE-FONT*��*COMMENT-FONT*�*STRING-FONT*���BASEF�
)�PACKAGE���RTMS��MODE��COMMON-LISP���OBTAIN-PROJECT-&-FORMAT���I���A�F�T�$��B�:p���TICL�ART-Q�]�F��:B�:B�:j�T�F�p���SYS�l�DEBUG-INFO-STRUCT�B�P\�ÁRELATION-NAME���PROJECT-LIST��TUPLE-FORMATC�SORT-LIST���*CURRENT-ATTRIBUTES*Â*CURRENT-TUPLE-FORMAT*B�:\�ÀA-P-L���ACTUAL-PROJECT-LIST�C�FINAL-PROJECT-LISTÀITEMS�C�TEMP-TUPLE-FORMAT�B�:B�:B�:��ATTRB�:C�ATTRIBUTE�\�)�MACROS-EXPANDED�\���PROGp���ZLC�,�DO-NAMEDp�B�T�INHIBIT-STYLE-WARNINGS��SETF�p�B�\,�*APPEND��p�B�T�FIRSTNҪ�LIST�F���ÁPARSE-PROJECT�Ҫ�STRING-EQUAL�p�B�\��MEMBER-TEST��j�COPY-LIST����Q��A��QB���Q���Q�C�Q�C�cC�v�QCQ�Q
C�s�C?r�Q�C�Q�l��5�Q�����5�Q����Q�Q�Q�Q�QPPAD�A���E�AQG�F��FQGSH�H5�HQ�HSCF��G�G��EQ@��Q�C@Q�Cx�Q
J��@Q�C�Q�C�cC����,�QB�G�GуQE�I�"�IQESJÁQP	��JQ�QP	��QJQ����AQJQ��A��J�Q
������CI��E�E��DQ���Q�QAQBQ�O��B�P����OBTAIN-SORT���	1k��@	�F�:�$��B�:B�V]�F��:B�:B�:B�YF��^B��\���SORTC�QUICK-SORTB�:B�:\�B�o\�B�qB�tB�vB�w��*PROVIDE-ERROR-MESSAGES*�B�|҃�VALIDATE-SYM��TERPRI�,�ERROR - Illegally specified sort clause����WRITE-STRING���5�Q�
��5�����S�5�S����5�Q�
��5�����S�5�S����@рQB�A��AQBS���CA��B�B��@Q���ۀ$���P������B�BсQ@�C��CQ@S���CC��@�@��BQ���ہ���P��߀Q�Q�O��B������OBTAIN-WIDE���	1��@	�F��$��B�:B�V]�F��:B�:B�:B�YF��^B��\�ÀWIDEP��NUMBER-PER-LINE�B�:\�ÀSTATUS\�B�o\�B�w�B���B���,�ERROR - �B����PRIN1�Ҭ� is not a legal number specification�@݀ �1��Q���L���5�S���1�Q�����P��Q�P�@����@Q�Q�O��B����C�PROCESS-QUICK-SORT��>��@�F�#�$��B�:B�V]�F��:B�:B�:B�YF��^B��\�ÀTUPLE�B��B�hB�:\��gB�:B�:B�:B�lB�:\�B�o\�B�qB�tB�vB�w��STRING�\�B�Y\�B�Y�C�QUICK-SORT-PREREQ��B���B���AтQC�B��BQCSD�D5�DQ�DS�CB��C�C��AQ@��Q����C�CсQA�E��EQAS�CE��A�A��CQ@Q����Q��Q@Q�RO��B������PROCESS-SORT��2��A�F��$��B�:B�V]�F��:B�:B�:B�YF��^B��\���B�
B�aB�hB�dC�*CURRENT-DOMAINS*�B�:\��DOMAINS�C�KEY-VALUE��ATTR-POSB�l\�B�o\�B�wB�q�B��B��Ҭ�ANYP�*�REVERSE��B�c��QC��Q�CCSB��5�BQ�BSBÄQP��C�cA�	�BQ�QP��AQ�Q�C�P@]@�C���@Q�@��Q�Q�Q@Q�O��B����C�CONVERT-ATTRIBUTES��(��@PF��$��B�:B�V]�F��:B�:B�:B�YF��^B��\�ÁATTRIBUTE-LISTB�:\�B�:B�:B�:B�m\�B�o\�B�qB�tB�vB�w�B�|�B����STRING-UPCASE����R�5�Q���@рQB�A��AQBSC��CQ��C7�CQ��CQCA��B�B��@O��B�����EXTRACT-KEY-HEAP����@F��$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B���KEY-LISTB�ǃ�WHERE-CLAUSE��PACKAGE-NAMEB�:B�:B�:�RO��B����C�RETRIEVE-INTERNAL���Td���T�F�\�$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B�`��KEYWORD-LISTB�:\� B��C�CURRENT-ATTRIBUTES�CURRENT-DOMAINS���CURRENT-KEY���CURRENT-IMPLEMENTATION-TYPE�C�CURRENT-STORAGE-STRUCTURE�B�h��CURRENT-TUPLE-FORMATB�iB�ȃ�INTOB�aB���PRINT�ÀQPRINT�STREAM�OUTPUT-FILE-NAMEB�bB��ÁRETURN-TUPLES�B�
��CARD��RETRIEVE-INDEX-NAME�B��ÁKEYWORD-VALUESB��C�INDEX-NAMEB�:B�aB��B�:B�:\�B�o\�p�B�T��SENDp�B�Tl�CONDITION-BIND-IF�p�B�T�CONDITION-BINDp�B�T�CATCH-CONTINUATION-IF�p�B�Tl�CATCH-CONTINUATIONp�B�T�ERRSETB�qp�B�\l�XR-BQ-LIST*�SEVENTH��SIXTH��FIFTH��FOURTH�THIRD��SECOND�FIRST�B�w��� A�p�l�EH��*CONDITION-HANDLERS*��*OUTPUT-WINDOW*��C�*PROVIDE-STATUS-MESSAGES*��Â*SYSTEM-RELATION-KEY*�у�*SYSTEM-RELATION-ATTRIBUTES*у�*DEFAULT-ANYP-WIDTH*�C�*SYSTEM-INDEX-KEY*�C�*SYSTEM-INDEX-ATTRIBUTES*�у�*PKG-STRING*у�*SYSTEM-RELATION-STORAGE-STRUCTURE*��Ä*SYSTEM-RELATION-BASE-IMPLEMENTATION*�у�*ACTIVE-DB*��B��у�*PARAMETER-CHECKING*��ACTIVE-DATABASE��B���\�l�ATTRIBUTES,�DOMAINS���KEY���IMPLEMENTATION-TYPE�l�STORAGE-STRUCTURE���TUPLE-FORMAT��CARDINALITY����GET-RELATION�B���l�ERROR - Relation ��B���B���,� does not exist in the database �\�B���PROJECT�ÀWHERE�B��ÀOUTPUT�FORMAT��NUM���WIDEB��B��B�
B����DIR���DOC���KEY���IMP���STO�ÀUNIQUEB��B���GET-KEYWORD-VALUE-PREREQ҃�DE-NEST-KEYWORD-LIST�C�GET-KEYWORD-VALUE��B���l�RETRIEVE-��l�-����CONCATENATE�Ҫ�FIND-SYMBOL�҃�SYSTEM-INDEX�\�l�INDEX-NAME���AND��B��B�`�B���B�|�B��,�ERROR - �,� is not a defined index on the relation �B���F���B���B��҆��B�P��ERROR - There are no legal attributes contained in the PROJECT clause�������EXTRACT-KEY��F�����CALCULATE-ATTRIBUTES�ÁUNIQUE-TUPLES��B���B���B���B���B�y�B�a�ÁRETRIEVE-INTO�҃�UNCONVERT-ATTRIBUTES��SYSTEM-RELATION��\���CARDINALITY���QTRIEVE��C�PRINTREL-INTERNAL*�p�,��G7239��F����ERROR��p�B�\�ERRSET-HANDLER�i�EXPOSED-P����APPEND-ITEM���~s tuple~:P retrieved��B�=�l� ��,�~%~s tuple~:P retrieved��� tuple�e�s�j�WRITE-CHAR�l� retrieved����Q����R�QP���@�@�	��P��Q�P�P�R@S��@W@�@SA�@WB�@[C�@QBD�@UBE�@YBG�@QBBU��P�Q���Q���P�Q�X�JXQ�CV�0�.�P PP!PP"�P#�[�$PP%P
P&P'P(P�Q)�*�'P+PVQ)�*�*���$PJ[�BV����,P�JXQ�C�-P��Q�RX[���L�XQBM�JXQ�CP�JXQ�CN�	JXQ�CS�JXQ�CO���M�S�.P��M�S�M�JXQ�CJXQ�C/P0PAR�Y��R
JXQ�CJXQ�C/P1PAW�T��JW'�R�QXWJXQ�CTQ�WQAQGQ2P3PAH�F�Q�K��K���4P�R��K�R�QAQCQBQEQLQJXQ�C5P6PAC�E�I�Z�P PDQ!PEQ"�P#�[��QAQKQCQLQIQZQJ[�@�KQF+	�@QKQFQ7P8PAF�K�@�JXQ��@�@Q9�@�T�@
�@QTQKQFQAQBQ:�@��RW�@E�@QWQFQ;���@>�H�XW<��AQF�F5�FQ*�F�FQ\��\S�5�\QB)�AQ'P=��HQ\QB)�
�\S�5
�\SAQ'P=��HQ\S*�>�H�\���K���4P�RKQH+�@QKQHQ?�@�HQK�XS�J��QJQ@QKQFQCQDQEQQQ�QAQJ@��RX�RN	�@Q]�D��]S�]���>�M�P�O8�KQA�K�J�JXQ�C�QQQ�JQ�Q@QKQRQRQOQPQKQQQ�C��QQ
�QQ	P��KQ�CQQ�C�cC>�����BPPCPP'P(PJQ�Q)�*�D�BMQSQJE�SH�D�FPGPTHPIPFP��JC^�PJC_��JP�*�J!BJ!B\�P_�KP��LP@Q�CM�_�KPNP��O�OQ�OQOP@Q�CM����@Q�C�PP�@Q�C�QPR�SP�JQ��S�@�O�rB������RETRIEVE-FLAVOR-HASH�����F�
�$��B�:B�V]�F��:B�:B�:B�YF��^B�s\�B�`B��B�aB�BB��ÁKEY-VALUE-LISTB�B�:B�:B�:��flavor�ÁRETRIEVE-HASH���Q�Q�Q�Q�Q�QP�QJ�O�B�s����RETRIEVE-FLAVOR-HEAP�����F�	�$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B�`B��B�aB�BB��ÁHEAP-TRAVERSALB�B�:B�:B�:���QTRIEVE-FLAVOR-HEAP����Q��Q�Q�Q�Q�Q�O��B����B�~��2~��B<F�L�$��B�:B�V]�F��:B�:B�:B�YF��^B�~\�B�`B��B�aB�BB��B��B�CB�B�:\�B�:B�:p�B�\l�LEX-PARENT-ENV-REGp�B�\�LEX-ENV-B-REG�p�B�\�LEX-CURRENT-VECTOR-REGp�B�\��LEX-ALL-VECTORS-REG��RETRIEVE-BUCKET�ÁHASH-RELATION���TEMP-ATTRIBUTE-LIST���CONV-ATTRIBUTE-LIST�C�TUPLE-LISTC�KEY-VALUE%B�:B�:B�:\��o\�B�tB�vB�qB�w��INTERNAL-FEF-OFFSETS\�F�i�VARIABLES-USED-IN-LEXICAL-CLOSURES\�B�����EVAL҃�ENTRY-POINT����GETP�\�F�F�
�\�)�INTERNALB�~��*�MAPHASH��*�GETHASH��B�y�B����FLAVOR�p�B�\�STRING-EQUAL*��ÂPROJECT-FLAVOR-PREREQ�҃�FAST-PROJECT-FLAVOR���STRUCT�B���B���B�K�B�\҃�FAST-PROJECT-STRUCT��F���C�EVAL-WHERE-PREREQ���FAST-EVAL-WHERE��B�a��5�Q����R�QP�G���PP�CGQ���QK��JQKSGQ	�
�J�K���Q�P��Q�I�JQIQ���Q�P��LсQN�M�
�MQP�Q�NS�CM��N�N��LQ�I�JQIQ�J��Q��Q�Q�QPPAH���JQ�QHQ�J�J�JQ�Q�Q�J���
�F�FO��B�~��B����
��@�F��$��B�:B�V]�F��:B�:B�:B�YF��^B��\��KEY-VAL�ÀTUPLESB�:\�B�:B�:B��\�B�o\�B�wi�LEXICAL-PARENT-DEBUG-INFO�B���B�y��Q�P����O��B����B����#��@�F��$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B��B��B�:\�B�:B�:B�:B��B�:B�:B�:\�B�o\�B�qB�tB�v�@рQB�A��AQBSC�D�DсQF�E��EQCQFS��CE��F�F��DQCA��B�B��@O��B����C�RETRIEVE-LIST-AVL������F�
�$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B�`B��B�aB�BB��B�|B�B�:B�:B�:���LIST���RETRIEVE-AVL��Q�Q�Q�Q�Q�QP�QJ�O��B������RETRIEVE-FLAVOR-AVL������F�
�$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B�`B��B�aB�BB��B�|B�B�:B�:B�:��FLAVOR�B����Q�Q�Q�Q�Q�QP�QJ�O��B������RETRIEVE-STRUCT-AVL������F�
�$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B�`B��B�aB�BB��B�|B�B�:B�:B�:��STRUCT�B����Q�Q�Q�Q�Q�QP�QJ�O��B����C�RETRIEVE-LIST-HASH�����F�
�$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B�`B��B�aB�BB��B�|B�B�:B�:B�:���LIST�B�~��Q�Q�Q�Q�Q�QP�QJ�O�B����C�RETRIEVE-LIST-HEAP�����F�	�$��B�:B�V]�F��:B�:B�:B�YF��^B�\�B�`B��B�aB�BB��B� B�B�:B�:B�:�C�QTRIEVE-LIST-HEAP����Q��Q�Q�Q�Q�Q�O�B�����RETRIEVE-STRUCT-HASH�����F�
�$��B�:B�V]�F��:B�:B�:B�YF��^B�\�B�`B��B�aB�BB��B�|B�B�:B�:B�:��STRUCT�B�~��Q�Q�Q�Q�Q�QP�QJ�O�B�����RETRIEVE-STRUCT-HEAP��	���F��$��B�:B�V]�F��:B�:B�:B�YF��^B�\�B�`B��B�aB�BB��B� B�B�:B�:B�:�B���B��҃�QTRIEVE-STRUCT-HEAP���	�Q�Q�Q�Q�Q�QP���Q�Q�Q�Q�Q�O�(B���ÁSELECT-TUPLES���	���	@F��$��B�:B�V]�F��:B�:B�:B�YF��^B�)\�B�`�&REST�B����&KEYj�&OPTIONAL�j�DIRECTORY��DOCUMENTATION�B�=��IMPLEMENTATION-TYPE�B��B�B�NUMBERB�<B��B��B��B�
B��C�STORAGE-STRUCTURE�B��B�EB�;B�?j�&ALLOW-OTHER-KEYS�B�:\�B��B�5B�6B�=B�7B��B�BB�8B�<B��B��B��B�
B��B�9B��B�EB�;B�?\��DOCUMENTATION�쿇Same as Retrieve except that all attributes are retrieved.

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
   UNIQUE               - If T, only unique tuples are retrieved.���� �\�i�DIRECTORY�B�=�FORMAT��IMPLEMENTATION-TYPE���INTO��KEY��NUMBER�OUTPUT�PRINT��QPRINTi�QUICK-SORT��SORT�STREAMi�STORAGE-STRUCTURE��TUPLES�UNIQUE�WHERE���WIDE�p�B�\�STORE-KEYARGS��B�:��LIST*���RETRIEVE�@�@QP��A���QP��@Q��O�VB�)��B�c��$XԆ�A$0F�|�$��B�:B�V]�F��:B�:B�:B�YF��^B�c\�B����SORT-CLAUSE�B�؃�DOMAIN-LIST�B�:\��AVL-TREEÀDOMAINB��B�|��NEW-ELEMENT�B�`C�SORT-ORDERÁSORT-ELEMENT%��ALREADY-SORTED-P��SORT-ELEMENTB��B�|\�B�o\�B�qB�w�B�(�B���C�*PROVIDE-WARNING-MESSAGES*�B�|�B���B��B���B�y�\�,�NUMBERP�,�STRINGP���ATOM�B���\���ASC�l�GT��GTE�l�GEl�INCREASING��DES���DESCl�DECREASINGl�LT��LTE�l�LE�B���l�WARNING - �B���B���l� is not an attribute nor a recognized sort keyword��          This element will be ignored�,�ERROR - No attributes specified in the sort clause --> ����        Sort can not proceed�B�a�B����-TEMP-��GENSYM�B���B�K�*�READ-FROM-STRING��APPEND��INSERT-AVL-LIST��B�����PUTP�B���\���DES���DESCl�DECREASINGl�LT��LTE�l�LE�B�͒�Q��ۂQB���5��Q���S�5�S���QI�=�IS���G��RGQ�QP	��Q�CGQ�QP	��C�c�Q�CA�BQGQ�
�B�AQPP	��Q�Q�Q��GQPP	��FQ�GQF��
��P�GQ�P��P�I���H�HB���P��Q��P�R�Q�QBQ�C��Q��QBQ�B��PPP����E��QJ�CQK��JS�J��������
CD�@QKSBQ�Q�Q��EQJ�@�J�K�J��EQ@QP �EQ�Q�QBQ����EQJ!���EQ��P �FQ"PP	��Q#����O��B�c���MAPTUPLE��N��@�F�0�$��B�:B�V]�F��:B�:B�:B�YF��^B��\�C�DBFUNCTION�RELATIONB�:B�:\�B�o\�	B�qB�tB�vB�wB�B�B�B�B�B�=�Map a function on all the tuples in a relation using MAPCAR.

   DBFUNCTION  - Function to be applied to each and every tuple.
   RELATION    - Name of the relation.��B� �B���B�-�p�B�b�G7595��F�>�B�f�B�h�j�FUNCTIONP��B�|�B��Ҭ�ERROR - Illegal function definition��B���B���B���B�U���RPPTP	PP��JC@�PJCA�րQ
��J!BJ!B\���P�R�Q����RA�AсQP���C�B��BQCS��CB��C�C��AO��B������MAPT��F��@�F�,�$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B��B��B�:B�:\�B�o\�B�qB�tB�wB�B�B�B�B�B�=��Map a function on all the tuples in a relation using MAPC.

   DBFUNCTION  - Function to be applied to each and every tuple.
   RELATION    - Name of the relation.��B� �B���B�-�p�B�b�G7635��F�>�B�f�B�h�B���B�|�B��Ҭ�ERROR - Illegal function definition��B���B���B���B�U���RPPTP	PP��JC@�PJCA�րQ
��J!BJ!B\���P�R�Q����R�QP���@��@S� @���O��B����ÁPRINT-RELATION�����@F��$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B��B�2B��B�3B�4B�5B�6B�=B�7B�B��B�BB�8B�<B��B��B��B�
B��B�9B��B�EB�?B�:B�:\�B��B�5B�6B�=B�7B�B��B�BB�8B�<B��B��B��B�
B��B�9B��B�EB�?\�B�=�
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
   UNIQUE               - If T, only unique tuples are retrieved.���� �\�B�AB�=B�BB�Ci�INDEX-NAMEB�DB�EB�FB�GB�HB�IB�JB�KB�LB�MB�NB�OB�Q�B�S�B�U�@�@QP��A���Q@Q�O��B���B��O���PRINTREL�B�:��
���
@F��$��B�:B�V]�F��:B�:B�:B�YF��^B�:\�B�`B�2B��B�3B�4B�5B�6B�=B�7B�B��B�BB�8B�<B��B�:B��B��B�
B��B�9B��B�EB�?B�:B�:\�B��B�5B�6B�=B�7B�B��B�BB�8B�<B��B�:B��B��B�
B��B�9B��B�EB�?\�B�=쿪Same as Retrieve except that all tuples are retrieved.

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
   UNIQUE               - If T, only unique tuples are retrieved.����� �\�B�AB�=B�BB�CB��B�DB�EB�FB�GB�H)�PROJECT�B�IB�JB�KB�LB�MB�NB�OB�Q�B�S�B�;�B�E�B�T�B�U�@�@QP��A���QP��P��@Q�	�O��B�:��C�ATTR-CALC���F��@�F�)�$��B�:B�V]�F��:B�:B�:B�YF��^B��\�C�CALC-LIST�B��B�:\�ÁNEW-CALC-LIST��NEW-CALC-ELEMENTÀRESULTÁRESULT-ELEMENTB�:C�X�\�B�o\�B�qB�t��PUSHB�w�B���B���B��B���B���F���B���B�y�B�|��Q��Q��QP�+���7�Q�AÁQP� �AQCB�B�AQ���5�QD��DSEÁQP	PAA�C�BQCQ
�B�@QAQ�
�@�D�����Q@�BQ@Q�O��B����B����)d��@�F�;�$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B��B�gB�:\�B��B�:B�:B�:ÀATTR%�\�B�o\��PROG2�B�wB�qB�tB�v�B�k�B���\�l�LT��LTE�l�LEl�DECREASING��DESC��DES�l�GT��GTE�l�GEl�INCREASING��ASC��B��B���\�l�LTl�LE��LTE�l�DECREASING��DESC��DES��ÀDBGTP��B�|�B�y�\�l�GTl�GE��GTE�l�INCREASING��ASC��B���l�WARNING - �B���B���,� is neither a valid quick-sort keyword nor an attribute.�AрQC�B�J�BQD�CS���D��@�RDQPP�8�CW���PP��DQ�QP��@QDQ	P
�
��@�$�CW���PP��DQ�QP��@QDQ��D5�DQ�C�DQ�QP�����P�DQ�P���CB��C�C��@O�B����B�U�����@F��$��B�:B�V]�F��:B�:B�:B�YF��^B�U\�B�`B�2B��B�3B�4B�5B�6B�=B�7B�B��B�BB�8B�<B��B�:B��B��B�
B��B�9B��B�EB�;B�?B�:B�:\�B��B�5B�6B�=B�7B�B��B�BB�8B�<B��B�:B��B��B�
B��B�9B��B�EB�;B�?\�B�=��Retrieve some tuples from a relation satisying a where clause.

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
   UNIQUE               - If T, only unique tuples are retrieved. ���� �\�B�AB�=B�BB�CB��B�DB�EB�FB�GB�HB��B�IB�JB�KB�LB�MB�NB�OB�PB�Q�B�S�B��@�@QP��A���Q@Q�O� B�U��B�~��i톀AhF���$��B�:B�V]�F��:B�:B�:B�YF��^B�~\�B�`B�dB�aB�eB�bB�:\�
B�g�NEW-ATTRB�hB�k�PROJECT-ELEMENT�ÁNEW-ATTR-PART2B�gB�bB�:B�l\�B�o\��tB�B�B��B�qB�w�B�%�B�k�B���B��B���B���B���l�WARNING - �B���B��Ҭ� is not an attribute of the �l� relation��,�WARNING - Improperly specified project element ��e�.�B�p�B���l�          �,� is an attribute of the �l� relation.�F���B���B�y�B�|�B�͒�Q�@���@QF��QG���FSA�A5"�AQ�QP�	�AQB]B�AQ�]��G���A5�AQ�������	�
P�AQ�P��Q�P���A
�~�	�P�AS�P�v�AS��QP��n�	�P�AQ�P�	�P�AS�P��Q�P�[�AW�5*�G�AS��QP��Q�CAS��QP��C�c�k�Q�C�P�GSC]C�AW�QPPAE�DÂQ���ASEQ�B]B�.�AW��QP��AQB]B�G �P�ASAW�B]B�AW�AÂ]��G�AQ�QP��Q�CAQ�QP��C�c�Q�C�P�GSC]C�F�G�FT�BQ�B�CQ�C��Q@���@QH��HSIÂQP��IQ�]��H���Q�BQCQ�O�8B�~��B�\��	)���	PF��$��B�:B�V]�F��:B�:B�:B�YF��^B�\\�B��B�4\�B��C�*PKG-NAME*B�:\�B�:B�:B�:B�m\�B�o\�B�qB�tB�v�B�B�B���l�:��B���B�K�B���v�P��@рQB�A��AQBSC��C7�P�QPCQ����CQCA��B�B��@O�GB�\��B�Y��L���@�F�\�$��B�:B�V]�F��:B�:B�:B�YF��^B�Y\�B��B�aB�hB�:\�B���ELEMENT�B�:B�:B�:B��B�:À%TUPLE�ATT-LIST�VAL-LISTB�:B�:B�:B�l\�B�o\��UNLESSB�B�B�qB�tB�vB�w��EQUAL��B���B�|҃�PARSE-WHERE��B�\�j�MAKUNBOUND�B���B���B���B��B���B���*�NREVERSE�BтQD�C��CQDSE�E5�EW�5	�ESPPEW��B��EQCC��D�D��BQ���Q�D�C�CC�CS�CDS�D�CS�C�D����QF�F�FSG��QGQI�H��HS	�IS�H�I�H��J�JтQL�K�*�KQLSM�M5�MQ
�	��C�M
�MW��QP��MW�	��MWA�MS��M�MS
�	�AQ��B�AQ�CK��L�L��JQ@]@�Fź�L�LтQJ�F��FQJSE�E5�EQ�ES�CF��J�J��LQ��@Q�@ÁQ�Q�O�\B�Y��B�[��<�p��B<�F���$��B�:B�V]�F��:B�:B�:B�YF��^B�[\�B�`B��B��B�aB�hB��B��B��B�bB��B��B�:\���TEMPB�gC�OLD-VALUESB�iB�:B�l��ATTDB��C�%ATTRIBUTEB�:B�:�ATTR-RELC�ATTR-INTO�ÀTEMP-D\�B�o\�B�B�B�B�qB�tB�B�w���*VALIDITY-CHECKING*��B���Â*SYSTEM-ATTRIBUTE-KEY*�Ã*SYSTEM-ATTRIBUTE-ATTRIBUTES*��B�#�B�$�B�]�\�l�ATTRIBUTES�B��B�`�B���B�|�B�`��SYSTEM-ATTRIBUTE�\�,�DOMAIN-FUNCTION��DEFAULT-VALUE���DOC��B�P�ÁATTRIBUTE-NAME�B��҃�DOM��p�B�T��DEF��B�A���ANYP�B�y�\�B�B�B�H�B���B���B�C�\�B�C�B�D�\�B�D�B�B�B�=�\�B�=�B�@�\�B�@�\�B�A�ÀDEFREL�\�,�DOMAIN-FUNCTION����ANYP�B���B����ERROR - The attribute �B���B���� in relation ���� and the attribute ��,� in the output relation �l� have different domain predicates.���ERROR - The output relation ��  does not have all the attributes required to insert the retrieved tuples. ��� has ��� as attributes and the retrieve call requires ��� attributes in the relation �l� to be projected.��B���ÀINSERT�	PP
PPPP�Q���B@���Q@�A�@QD�H�DSE�F�PPPPPPP�Q��PPE5�EQ�EW�5�EW�������BF��AQE5�EQ�ESPFSPFWPE5�F[����	�AQE5�EQ�ESPP���A�E5�ES��EQC]C�DŸ�P Q�B�QD�H�	�HQCQP��HQG���RG�P Q�B�Q�CQ�B����QAQPP Q�B�Q P!P Q�B�Q"P�Q#P$P Q�B�Q%P&P Q�BP'P Q�BJ�(���R@S@ÊC�Q�C|_�Q@QJ�I�V�ISJSL�K�M�K5�KSK�KQ�QP�E�PP)PPPPP�Q��PPKQ����BPP)PPPPP�Q��PPL5�LQ�LS����BM�r�MQ*P+���,�-P.�KQ/�0P.��Q/�1P.�LQ/�2P.��Q/�3P.�RI�J�I�J����,�4P.��Q/�5P.��Q/�6P.�@Q/�7P.��Q/�8P.��Q/�9P.�RPP�B��ځQ:P�Q�;�BS�BW��O��B�[��B�Z��0��@hF��$��B�:B�V]�F��:B�:B�:B�YF��^B�Z\�B��B�:\�
B�:B�:B��B��B��B����RESULT-TABLEB�:B�チHASH-BUCKET�\��o\�B�qB�tB�wB��\�F�
B��\�B�����TEST�B�X�*�MAKE-HASH-TABLE��B���p�B�\l�MEMBER-EQL�p�B�T,�PUTHASH��\�F����\�B��B�Z��B���PP�F��QG��GSH�I�HQFQ�I��HQIQ��HQHQI]�HQHQFQ�G����	P
P�CFQ��O��B�Z��B������@�F��$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B��C�Y�B�:\�B�:B�:B��\�B�o\�B�wB��B����Q�\���O��B���1�\�p�B�\,�FASL-RECORD-FILE-MACROS-EXPANDED\�B�8\�\��DEFUN���'\�p�B�T��DEFF��b\�B�W��6\�B����J=�\�B�憀��\�B���z(�\�B���{��\�B���:}n\�B���x��\�B���Zi�\�B���{Ĳ\�B���2�=\�B���.ً\�B���-i\�B���~�z\�B���<p�\�B���`sN\�B���|��\�B�	��aM*\�B�w��[�\�B�v��(̢\�B�t��*�j\�B�q��=�#��y soon.
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
G2�B�wB�qB�tB�v�B�k�B���\�l�LT��LTE�l�LEl�DECREASING��DESC��DES�l�GT��GTE�l�GEl�INCREASING��ASC��B��B���\�l�LTl�LE��LTE�l�DECREASING��DESC��DES��ÀDBGTP��B�|�B�y�\�l�GTl�GE��GTE�l�INCREASING��ASC��B���l�WARNING - �B���B���,� is neither a valid quick-sort keyword nor an attribute.�AрQC�B�J�BQD�CS���D��@�RDQPP�8�CW���PP��DQ�QP��@QDQ	P
�
��@�$�CW���PP��DQ�QP��@QDQ��D5�DQ�C�DQ�QP�����P�DQ�P���CB��C�C��@O�B����B�U�����@F��$��B�:B�V]�F��:B�:B�:B�YF��^B�U\�B�`B�2B��B�3B�4B�5B�6B�=B�7B�B��B�BB�8B�<B��B�:B��B��B�
B��B�9B��B�EB�;B�?B�:B�:\�B��B�5B�6B�=B�7B�B��B�BB�8B�<B��B�:B��B��B�
B��B�9B��B�EB�;B�?\�B�=��Retrieve some tuples from a relation satisying a where clause.

   RELATION-NAME        - Name of the reLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540830. :SYSTEM-TYPE :LOGICAL :VERSION 2. :TYPE "XLD" :NAME "SAVE" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :NOT-BACKED-UP T :CREATION-DATE 2760360526. :AUTHOR "REL3" :LENGTH-IN-BYTES 6527. :LENGTH-IN-BLOCKS 13. :BYTE-SIZE 16.)  pp2�\���COMPILE-DATA\��SW-MFG,�GODZILLA����^�F�F�\�p�,�COMPILER,�VERSION�\�F�F�p�B�),�OPTIMIZE-SWITCH�����QFASL-SOURCE-FILE-UNIQUE-ID�1�\�p�l�FS�MAKE-FASLOAD-PATHNAME�\��QUOTE�B�$\�B�8��NIL�\�B�8\���RTMS\�B�8��SAVE\�B�8��LISP\�B�8F���BASEF�
�FONTS�\���*CODE-FONT*��*COMMENT-FONT*�*STRING-FONT*�)�PACKAGE���RTMS��MODE��COMMON-LISP���SAVE-FLAVOR-HASH��	F��F��$��B�:p���TICL�ART-Q�]�F��:B�:B�:j�T�F�p���SYS�l�DEBUG-INFO-STRUCT�B�P\��RELATION*�PATHNAMEÁREL-DEFINITIONB�:B�:\�)�MACROS-EXPANDED�\�p�B�\��XR-BQ-LIST*�p�B�\l�XR-BQ-LIST���SETF���VAR1�B�8���ENTRY-POINT����GETPҪ�LIST҃�PUTP�\�B�k\�B�8B�l��LIST*��p�B�Tl�DUMP-FORMS-TO-FILE��Q�QPPP�QP���	PP�Q�
P���O�uB�P���SAVE-FLAVOR-HEAP��	F��F��$��B�:B�V]�F��:B�:B�:B�YF��^B�v\�B�`B�aB�bB�:B�:\�B�d\�B�gB�i�B�j�B�k�B�8�B�l�B�m�B�n�B�o�\�B�k\�B�8B�l�B�r�B�t��Q�QPPP�QP���	PP�Q�
P���O��B�v���SAVE-FLAVOR-AVL���	F��F��$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B�`B�aB�bB�:B�:\�B�d\�B�gB�i�B�j�B�k�B�8�B�l�B�m�B�n�B�o�\�B�k\�B�8B�l�B�r�B�t��Q�QPPP�QP���	PP�Q�
P���O��B����ÁSAVE-LIST-AVL���	F��F��$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B�`B�aB�bB�:B�:\�B�d\�B�gB�i�B�j�B�k�B�8�B�l�B�m�B�n�B�o�\�B�k\�B�8B�l�B�r�B�t��QPPP�QP����Q	PP�Q�
P���O��B����ÁSAVE-LIST-HASH��	F��F��$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B�`B�aB�bB�:B�:\�B�d\�B�gB�i�B�j�B�k�B�8�B�l�B�m�B�n�B�o�\�B�k\�B�8B�l�B�r�B�t��QPPP�QP����Q	PP�Q�
P���O��B����ÁSAVE-LIST-HEAP��	F��F��$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B�`B�aB�bB�:B�:\�B�d\�B�gB�i�B�j�B�k�B�8�B�l�B�m�B�n�B�o�\�B�k\�B�8B�l�B�r�B�t��QPPP�QP����Q	PP�Q�
P���O��B�����SAVE-STRUCT-AVL���	F��F��$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B�`B�aB�bB�:B�:\�B�d\�B�gB�i�B�j�B�k�B�8�B�l�B�m�B�n�B�o�\�B�k\�B�8B�l�B�r�B�t��Q�QPPP�QP���	PP�Q�
P���O��B�����SAVE-STRUCT-HASH��	F��F��$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B�`B�aB�bB�:B�:\�B�d\�B�gB�i�B�j�B�k�B�8�B�l�B�m�B�n�B�o�\�B�k\�B�8B�l�B�r�B�t��Q�QPPP�QP���	PP�Q�
P���O��B�����SAVE-STRUCT-HEAP��	F��F��$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B�`B�aB�bB�:B�:\�B�d\�B�gB�i�B�j�B�k�B�8�B�l�B�m�B�n�B�o�\�B�k\�B�8B�l�B�r�B�t��Q�QPPP�QP���	PP�Q�
P���O��B���ÁSAVE-DATABASE�O���SAVE-DB��B����<X솀`<|F���$��B�:B�V]�F��:B�:B�:B�YF��^B��\�ÁDATABASE-NAME��&REST���KEYWORD-LIST��&KEYj�&OPTIONAL�j�DIRECTORY�j�&ALLOW-OTHER-KEYS�B�:\�\�B��B�:p�B�\l�LEX-PARENT-ENV-REGp�B�\�LEX-ENV-B-REG�p�B�\�LEX-CURRENT-VECTOR-REGp�B�\��LEX-ALL-VECTORS-REG�B����KEYS�TEMP-DIRB�aC�ERROR-FLAGB�:B��B�:B�:\�B�d\�p�B�Tl�CONDITION-BIND-IF�p�B�T�CONDITION-BINDp�B�T�CATCH-CONTINUATION-IF�p�B�Tl�CATCH-CONTINUATIONp�B�T�ERRSET��PROGB�iB�j��INTERNAL-FEF-OFFSETS\�F�8i�VARIABLES-USED-IN-LEXICAL-CLOSURES\��Exit block SAVE-DATABASEB��DOCUMENTATION��Save all system relations and the user-defined, modified relations.

   DATABASE-NAME    - Name of the database to be saved.
   DIRECTORY         - Name of the directory in which it is to be saved.��p�l�EH��*CONDITION-HANDLERS*�C�*PROVIDE-STATUS-MESSAGES*��C�*ENVIRONMENT-NAME*��*DATABASE-DOCUMENTATION*у�*SYSTEM-RELATION-STORAGE-STRUCTURE*��Ä*SYSTEM-RELATION-BASE-IMPLEMENTATION*���*SAVE-DIRECTORY*��*PROVIDE-ERROR-MESSAGES*у�*ACTIVE-DB*��Ã*TRANSACTION-FORMS-POSTPONED*���*TRANSACTION-ON*�\�i�DIRECTORY��p�B�\�STORE-KEYARGS��F�&�j�COPY-LIST���ACTIVE-DATABASE�҃�VALIDATE-SYM�B���B�8�B�n�p�B�\,�*APPEND���TERPRI�,�ERROR - Only the current database may be (or needs to be) saved.���WRITE-STRING�,�        The current database is ��PRIN1��\���DIR���GET-KEYWORD-VALUE-PREREQ��STRING�ÁGET-DIRECTORY�Ҭ�.XLD���CONCATENATE���SYSTEM-RELATION��\�l�MODIFIEDP��\�B�Y��DELETE-OR-MODIFY�\��SAVE-DIRECTORY�p�,��G8924��F����ERROR��p�B�\�ERRSET-HANDLER�p�B�4�DIRECTORY-LIST�B�j�B� �B���DEFINE-DATABASE*�B�6���DOC����ENV��B�t�l�ERROR - Directory �,� does not exist��\�F����F�
�\�)�INTERNALB������MAPTҬ�The ��� database has been saved in �@�@QP��F��K�K�P��U@Q�G����ۚ��Q��������PPP�Q�PGQ�������P�+�
���P��P�P���@Q�LSL��LS�5��LQ@�P@Q�@�P@Q �HÀQ!P"�I�	PH+�#P����$P%P&�#P����'PHQ�&�HQ	�(P)PT*P+P(P��JCM�PJCN��HQ,��J!BJ!B�\�\��IQ-P.PP�-P/PP�0P�Q1PHQ2PP3PP���4�
�
��5P�HQ�6P�w�7P8P�C#P9�	��:P��Q�;P�HQ�Je�QJ\PO�[B����B�U��W��@\F�8�$��B�:B�V]�F��:B�:B�:B�YF��^B�U\�ÀTUPLE�B�:\�B�:B�:B���TEMP-RELB�B�:B�:\�B�d\�B�B�	B�B�B��UNLESSB�ji�LEXICAL-PARENT-DEBUG-INFO�B��B��B�"�C�*PKG-NAME*у�*SYSTEM-RELATION-ATTRIBUTES*�B�n�\��RELATION-NAME��SAVE-DIRECTORY���PROJECT-LIST�B�8�B�;�*�READ-FROM-STRING�p�B�C�G8933��F�[�B�G�B�I�B�K�B�0�l�ERROR - Directory �B�2�B�4�,� does not exist��B�6�ÁSAVE-RELATION���Q�PP	�BD�
PPDS��C�DWD�PPTPPP��JCE�PJCF��DQ��J!BJ!B�\�\����P�DQ�P����P��XCQPDQ�O�xB�U��SAVE-ENVIRONMENTO�y�SAVE-ENV�B�y��:U䆀`:hF���$��B�:B�V]�F��:B�:B�:B�YF��^B�y\��ENVNAME�B��B��B��B��B��B��B�:\�
B��B��B�aB�6B��REL-IMP��REL-STO�B��B�:B�:\�B�d\�	B�B�	B�B�B�B�iB�iB�B�jB�l�Save an environment.

   ENVNAME   - Name of the environment to be saved.
   DIRECTORY - Name of the directory in which it is to be saved.�B��C�*PROVIDE-WARNING-MESSAGES*у�*VALIDITY-CHECKING*��B��B� �B�у�*PARAMETER-CHECKING*�B�!�Â*AUTO-SAVE-RELATIONS*��C�*RELATION-IMPLEMENTATION*��B�"�B�$�B�%�\�B�'�B�)�B�-�B�+�B�y�B�8�B�n�B�/�\�B�6�B�7�B�8�B�9�l�rtms-environment-����.XLD�B�;�p�B�C�G9006��F���B�G�B�I�B�K�B�0Ҭ�ERROR - The �B�2�B�4�l� directory does not exist��l�-��p�B�\,�SEARCH*���SUBSEQ�F��C�DEFINE-ENVIRONMENT�\�B�8C�AUTO-SAVE��\�B�8B���\�B�8ÀERRORS�\�B�8��PARA�\�B�8B���\�B�8B���\�B�8ÀSTATUS�\�B�8�SYS-IMP��\�B�8�SYS-STO��\�B�8�VALIDITY�\�B�8�WARNINGS�B�t�@�@QP��A���Q������R@Q�GSG��GS�5��GQ@��D��PPP�Q�PDQ������P@Q�@�P@Q�C�P�QP�B�P PT!P"PP��JCH�PJCI��CQ#��J!BJ!B�\�\�
��$�%P&�CQ'�(P&�RPJ)PP*�+�E�P)PP*��k+�F���,PJUBQ-PP�Q�.PPP�/PP
P�0PPP�1PP	P�2PPEQ�3PPFQ�4PPP�5PPP�6PPP�7PPP�8PPP�J��9�]Z�O��B�y�B�wO�w�SAVE-REL�B�w��^�����^@F��$��B�:B�V]�F��:B�:B�:B�YF��^B�w\�	ÁRELATION-NAME�B��B��B��B��B����SAVE��TYPEB��B�:\�\�B��B�:B��B��B��B�B��B��B��ÁINSERT-ROUTINEB�a�TEMPLISTB��ON-DISK?��MODPB�6C�ATTRIBUTES��IMP�C�SS��TEMP-MESSAGE��KEY���TUPLE-FORMATB�M��TEMP��QTRIEVE-VAR�C�SAVE-TYPE���ALWAYS-SAVE�B�B��B�:B�:\�B�d\�B�B�	B�B�B�B�i�FIRST�*�SEVENTH��SIXTH��FIFTH��FOURTH�THIRD��SECONDB�B�iB�jB�\�F�SB�\�B��B�,�Save a relation if it is modified.

    RELATION-NAME - Name of the relation to be saved.
    DIRECTORY     - Name of the directory in which it is to be saved.
    SAVE          - If T, saves the relation even if the relation is not modified.
    TYPE          - Two types of save are allowed: COMMAND and XLD. This keyword can be used to
                    specify the type.���� �B��C�*SYSTEM-RELATIONS*�B��B�k�B�#�B�"�B�$�B�%�\�B�'��SAVE��TYPE�B�)�B�,�B�-�B�+�B�w�B�8�B�n�B�/�\�B��B�6B���B�7�\�
�RELATION-NAME�l�MODIFIEDP��SAVE-DIRECTORYl�ATTRIBUTES��IMPLEMENTATION-TYPE�l�STORAGE-STRUCTURE���KEY���TUPLE-FORMAT��DOC���DISK���GET-RELATION�B�0��ERROR - The relation ��B�2�B�4�,� is not defined in the database �\�B���C�GET-KEYWORD-VALUE�҃�XLD��\�B���ÁSAVEREL-QFASL���COMMAND��,�ERROR - �,� is an unrecognized save type .��B�9�p�B�C�G9052��F�X�B�G�B�I�B�K�l�ERROR - Directory �,� does not exist��B�<���STRING-EQUAL�B�8��RELATION-NAME��B�;��STRING-UPCASE��\��SAVE-DIRECTORY�B�@�\�B��\�B�;\�B�8B�8B�k�RELATION-NAME�,�SYSTEM-RELATION��\�l�MODIFIEDP��\�B�Y�l�-��l�.��B�8��The relation ��� has not been modified�B�6�ÁLOAD-RELATION��F��i�DIRECTION���OUTPUT���OPEN��~&(SETF *non-qfasl-restore* T)��FORMAT�p�B�\��MEMBER-TEST�Ҭ�~&(DEFREL-RESTORE ~S ~S ~S)��B���C�MODIFIEDP����STO��B���B���B�M���hash�p�B�\�STRING-EQUAL*��,�~&(PUTP '~S (make-hash-table :test 'equal) '~S)��B�l�\�F�F��\�B�VB�w��B�X�,�INSERT-��B�q��~&(~S '~S '~S '~S '~S)�*�REVERSE��,�~&(SETF *non-qfasl-restore* NIL)��CLOSE��\�B�:�ÂSAVE-SYSTEM-RELATIONS��l� has been saved in the directory ��@�@QP��F����R�Q������R@Q�L��
PPP�Q�PLQ����
��@Q�\S\��\S�5��\Q@�P@Q�@��QP�����BX��		��P��Q�P�P�RXWN�X[O�XQBP�XUBQ�XYBR�XQBBT�JXQ
CU�UWV�U[M�USU�P@Q�B� PY�!P@Q�BZ�YQ &�Q@Q"�YQ#&
�	��$P�YQ�%P�R@QOQ&�[�'P(PT)P*P'P��JC]�PJC^��[Q+��J!BJ!B�\�\�
�	��,P�[Q�-P�R[QO+�.P��/P0PP1P2��Q3��4P[Q�5�.P��6P7P8P5�0P[QP9P�Q:PYQ;�J2�J�K�N�Z�[QO+
���<P��Q�=P��M�Q>POQ?���@PJUJQAPBPC�J�DPE��QP/PF��JQGP�QPQHPQQIP��JPRQKPTQLPUQMPVQ>POQJ�E��QP/PF�	�RQNPO��JQPP�QQPE�W�RPSP�C�QT�W�0PUPQQ9PRQ2�V�I�JQWPIQ�QPQWQX�TQ�QJE�JQYPE�JQZ�.P��/P0PP1P2��Q3��7P[P5��QP/PF��PS��\�SQ�	��<P��Q�]P�[Q�]Z�O�*B�w��B� ����@LF��$��B�:B�V]�F��:B�:B�:B�YF��^B� \�À%TUPLEB�:\�B�:B�:B��\�B�d\�B�jB�jB����Q�\���O�7B� ��B����Az5���A�F���$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B�`B��B�:\�B�B�bB�aB���CARD��MOD�B�6B��B��B��B��B��B�MB��B��B��B�:B�:\�B�d\�B�B�	B�B�B�B�iB��B��B��B��B��B��B��B�iB�j���A�B��B��у�*PKG-STRING*�B��B�#�B�"�B�$�B�%�B�+�B���B�8�B�n�B�/�\�B�6B��B���B�7�\�
�RELATION-NAME�l�MODIFIEDP��SAVE-DIRECTORYl�ATTRIBUTES��IMPLEMENTATION-TYPE�l�STORAGE-STRUCTURE���KEY���TUPLE-FORMAT��DOC���CARDINALITY��B���B�0��ERROR - The relation ��B�2�B�4�,� is not defined in the database �B�9�p�B�C�G9151��F���B�G�B�I�B�KҬ�ERROR - The �l� directory does not exist��B�<�B���B���\��SAVE-DIRECTORY�B�@�\�B��B��,�SYSTEM-RELATION��\�l�MODIFIEDP��\�B�Y��The relation ��l� has not been modified and thus does not need to be saved��B�8�l�-����.XLD�B�;�ÁDEFREL-RESTORE�B�����CARDINALITY��B��B��B���B���B�M�B�6�B��F�p��SAVE-����FIND-SYMBOL��\�B�:�B��B�(�l� has been saved in the directory ���Q�@��
PPP�Q�P@Q����
��P�Q����QP�����BM��		��P��Q�P�P�RMWE�M[F�MQBG�MUBH�MYBI�MQBBJ�JMQ
CK�KWL�K[D�KQBO�KSK��QFQ�C�PPTPPP��JCP�PJCQ��CQ��J!BJ!B�\�\�
�	�� P�CQ�!P�RCQF+�"P��#P$PP�Q��%PCQ�&�"P��'P(P)P&�E�CQF+
���*P��Q�+P��,PCQP-P�Q.P/�B�0P�QGQ1PHQ2PDQ3P��4PIQ5PJQ6PKQ7PLQ8PFQJ��A�O�Q8PFQ9���:PJU,P;PHQ-PIQ/�P<�Q��QBQAQQ�"P��#P$PP�Q��(P=P&��QP#P>��PN��?�NQ�	��*P��Q�@P�CQ�]uZ�O�lB����B�(��*/���`*F�Y�$��B�:B�V]�F��:B�:B�:B�YF��^B�(\�B���IGNOREB�:\��uB�aB�:B�:B�:B�:\�B�d\�
B�jB�p���ZLC�,�DO-NAMEDp�B�T�INHIBIT-STYLE-WARNINGSB�B�	B�B�B�B�i�B��B��B��B��B� �B�#�B���B�"�B�!�Â*SYSTEM-RELATION-KEY*��B�l�\�B�Y�B�<�\�l�MODIFIEDP��\��MEMBERB��B����TEST\�B�8B����QTRIEVE��p�B�C�G9212��F�x�B�G�B�I�B�K�B�n�B�0Ҭ�ERROR - The �B�2�B�4�l� directory does not exist��B�6�B�w�B�8���.XLD�B�;�B�j�B� �B��B�L�B�M�B�N�B�t�PPPPPP���SPPTPPP��JCB�PJCC��P��J!BJ!B�\�\�
�
��P�P�P�RC�C�	PE�D�	�DQESPP�CD��E�E�� PPP!P"�A�#P$PP�#P%PP�&PPPP'PP(PP���)�SO�
B�(�1�\�p�B�\,�FASL-RECORD-FILE-MACROS-EXPANDED\�B�8\�\��DEFUN���'\�p�B�T��DEFF��b\�B�}��(̢\�B�{��*�j\�B����{��\�B����:}n\�B����x��\�B����Zi�\�B����{Ĳ\�B����2�=\�B����z(�\�B�i��6\�B�j��[�\�B���=�#\�B���-i\�B���~�z\�B���<p�\�B�	��`sN\�B���|��\�B�i��.ً\�B�g��N����ogn
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
B�jB�jB����Q�\���O�7B� ��B����Az5���A�F���$��B�:B�V]�F��:B�:B�:B�YF��^B��\�B�`B��B�:\�B�B�bB�aB���CARD��MOD�B�6B��B��B��B��B��B�MB��B��B��B�:B�:\�B�d\�B�B�	B�B�B�B�iB��B��B��B��B��B��B��B�iB�j���A�B��B��у�*PKG-STRING*�B��B�#�B�"�B�$�B�%�B�+�B���B�8�B�n�B�/�\�B�6B��B���B�7�\�
�RELATION-NAME�l�MODIFIEDP��SAVE-DIRECTORYl�ATTRIBUTES��IMPLEMENTATION-TYPE�l�STORAGE-STRUCTURE���KEY���TUPLE-FORMAT��DOC���CARDINALITY��B���B�0��ERROR - The relation ��B�2�B�4�,� is not defined in the database �B�9�p�B�C�G9151��F���B�G�B�I�B�KҬ�ERROR - The �l� directory does not exist��B�<�B���B���\��SAVE-DIRECTORY�B�@�\�B��B��,�SYSTEM-RELATION��\�l�MODIFIEDP��\�B�Y��The relation ��l� has not been modified and thus does not need to be saved��B�8�l�-����.XLD�B�;�ÁDEFREL-RESTORE�B�����CARDINALLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540837. :SYSTEM-TYPE :LOGICAL :VERSION 1. :TYPE "LISP" :NAME "STARTER-KIT-DESTROY" :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)" :CHARACTERS T :NOT-BACKED-UP T :CREATION-DATE 2749846960. :AUTHOR "REL3" :LENGTH-IN-BYTES 669. :LENGTH-IN-BLOCKS 1. :BYTE-SIZE 8.)

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
