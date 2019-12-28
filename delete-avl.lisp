
;;; -*- Mode:Common-Lisp; Package:RTMS; Base:10; Fonts:(*CODE-FONT* *COMMENT-FONT* *STRING-FONT*) -*-
;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved.
;;; DELETE-AVL
;;;
;;; The following function contains flavor references and thus are incompatable with CommonLisp. Their removal will not
;;; effect the functionality of RTMS.
;;;     delete-flavor-avl
;;;     flavor-avl-key-delete
;;;     process-flavor-avl-delete
;;;

(defun delete-list-avl (relation-name attribute-list key-attributes domains where-clause index-name
&aux key-value key-domain-list mode (number-deleted 0) rebalancep temp-attribute-list
termination-condition (total-number-deleted 0) tree package-name)
  (cond ((or (equal where-clause t)(equal where-clause '(t)))
 (putp index-name nil 'entry-point)
 (setf total-number-deleted (caar (retrieve 'system-relation 'project '("CARDINALITY")
       'where `(string-equal relation-name
        ,(string-upcase relation-name))
         'tuples t))))
(t
 ;;
 ;;  Obtain the key from the where clause from the particular relation.
 ;;
 (cond ((member (string-upcase relation-name) *system-relations* :test 'string-equal)
(setf key-domain-list (eval (read-from-string
        (concatenate 'string *pkg-string* ":*"
       (string-upcase relation-name)
       "-KEY-DOMAINS*")))))
       (t
(setf key-domain-list (car (project-list (list domains) attribute-list key-attributes)))))
 (cond ((symbolp relation-name)
(setf package-name (package-name (symbol-package relation-name))))
       (t
(setf package-name *pkg-string*)))
 (setf key-value (extract-key-avl attribute-list key-attributes key-domain-list where-clause
    package-name)
       tree (getp index-name 'entry-point))
 (cond ((null key-value)
(setf key-value (list (list key-attributes) '((t)) '((t))))))
 ;;
 ;;  Perform the actual deletion of the requested tuples
 ;;
 ;;
 ;;  Take each key extracted from the WHERE clause and retrieve the tuples selected by the individual keys.
 ;;
 ;;
 (do ((key% (first key-value) (cdr key%))
      (beginning-value% (second key-value) (cdr beginning-value%))
      (termination-clause% (third key-value) (cdr termination-clause%)))
     ((null key%) number-deleted)
   (cond ((equal (caar termination-clause%) t)
  (setf termination-condition t))
 (t
  (setf termination-condition (list 'lep (caar key%) (caar termination-clause%)))))
   ;;
   ;;  Since EVAL-WHERE has to be called once per node while searching, the process can be sped up by separating
   ;; the eval overhead from the actual operation. The call to prereq initializes all subsequent calls to FAST-EVAL-WHERE.
   ;;
   (multiple-value-setq (where-clause temp-attribute-list)
     (eval-where-prereq where-clause attribute-list relation-name))
   (progv temp-attribute-list nil
     (setf mode "TERMINATE"
   number-deleted 1)
     (do ((beginning-value (car beginning-value%) beginning-value))
 ((or (equal number-deleted 0)(string-equal mode "FINISHED")) t)
       (setf number-deleted 0)
       (multiple-value-setq (tree mode rebalancep number-deleted beginning-value)
 (list-avl-key-delete tree temp-attribute-list key-attributes key-domain-list beginning-value
       termination-condition "LOCATE" relation-name where-clause rebalancep
       number-deleted))
       (setf total-number-deleted (+ total-number-deleted number-deleted)
     rebalancep nil))))
 (putp index-name tree 'entry-point)))
  total-number-deleted)

(defun delavl (tree rebalancep temp-tree &aux modtree)
  (cond ((fourth tree)
 (multiple-value-setq (modtree rebalancep temp-tree)
   (delavl (fourth tree) rebalancep temp-tree))
 (rplaca (cdddr tree) modtree)
 (cond (rebalancep
(multiple-value-setq (tree rebalancep)
  (balance2 tree rebalancep)))))
(t
 (rplaca temp-tree (car tree))
 (setf tree (third tree)
       rebalancep t)))
  (values tree rebalancep temp-tree))

(defun balance1 (tree rebalancep
 &aux root-node-balance-factor left-subtree left-subtree-balance-factor right-subtree
 right-subtree-balance-factor)
  (setf root-node-balance-factor (second tree))
  (cond ((and (null (third tree))(null (fourth tree)))
 (rplaca (cdr tree) 0))
((equal root-node-balance-factor 1)
 (rplaca (cdr tree) 0))
((equal root-node-balance-factor 0)
 (rplaca (cdr tree) -1)
 (setf rebalancep nil))
((equal root-node-balance-factor -1)
 (setf right-subtree (fourth tree)
       right-subtree-balance-factor (second right-subtree))
 (cond ((<= right-subtree-balance-factor 0)
;;
;;  RR rotation
;;
(rplaca (cdddr tree) (third right-subtree))
(rplaca (cddr right-subtree) tree)
(cond ((equal right-subtree-balance-factor 0)
       (rplaca (cdr tree) -1)
       (rplaca (cdr right-subtree) 1)
       (setf rebalancep nil))
      (t
       (rplaca (cdr tree) 0)
       (rplaca (cdr right-subtree) 0)))
(setf tree right-subtree))
       (t
;;
;;  RL rotation
;;
(setf left-subtree (third right-subtree))
(setf left-subtree-balance-factor (second left-subtree))
(rplaca (cddr right-subtree) (fourth left-subtree))
(rplaca (cdddr left-subtree) right-subtree)
(rplaca (cdddr tree) (third left-subtree))
(rplaca (cddr left-subtree) tree)
(cond ((equal left-subtree-balance-factor -1)
       (rplaca (cdr tree) 1))
      (t
       (rplaca (cdr tree) 0)))
(cond ((equal left-subtree-balance-factor 1)
       (rplaca (cdr right-subtree) -1))
      (t
       (rplaca (cdr right-subtree) 0)))
(setf tree left-subtree)
(rplaca (cdr left-subtree) 0)))))
  (values tree rebalancep))

(defun balance2 (tree rebalancep
 &aux root-node-balance-factor left-subtree left-subtree-balance-factor right-subtree
 right-subtree-balance-factor)
  (setf root-node-balance-factor (second tree))
  (cond ((and (null (third tree))(null (fourth tree)))
 (rplaca (cdr tree) 0))
((equal root-node-balance-factor -1)
 (rplaca (cdr tree) 0))
((equal root-node-balance-factor 0)
 (rplaca (cdr tree) 1)
 (setf rebalancep nil))
((equal root-node-balance-factor 1)
 (setf left-subtree (third tree)
       left-subtree-balance-factor (second left-subtree))
 (cond ((>= left-subtree-balance-factor 0)
(rplaca (cddr tree) (fourth left-subtree))
(rplaca (cdddr left-subtree) tree)
(cond ((equal left-subtree-balance-factor 0)
       (rplaca (cdr tree) 1)
       (rplaca (cdr left-subtree) -1)
       (setf rebalancep nil))
      (t
       (rplaca (cdr tree) 0)
       (rplaca (cdr left-subtree) 0)))
(setf tree left-subtree))
       (t
(setf right-subtree (fourth left-subtree))
(setf right-subtree-balance-factor (second right-subtree))
(rplaca (cdddr left-subtree) (caddr right-subtree))
(rplaca (cddr right-subtree) left-subtree)
(rplaca (cddr tree) (cadddr right-subtree))
(rplaca (cdddr right-subtree) tree)
(cond ((equal right-subtree-balance-factor 1)
       (rplaca (cdr tree) -1))
      (t
       (rplaca (cdr tree) 0)))
(cond ((equal right-subtree-balance-factor -1)
       (rplaca (cdr left-subtree) -1))
      (t
       (rplaca (cdr left-subtree) 0)))
(setf tree right-subtree)
(rplaca (cdr right-subtree) 0)))))
  (values tree rebalancep))

(defun list-avl-key-delete (tree attribute-list key-attributes domains beginning-value termination-clause mode
    relation-name where-clause rebalancep number-deleted
    &aux comparison-operator current-node-key-value mod-tree)
  ;;
  ;;  Locate the node where the search will begin
  ;;
  (cond ((and (not (equal tree nil))
      (or (string-equal mode 'locate) (string-equal mode 'locate-stage-2)
  (string-equal mode "DELETE-SEARCH")))
 (setf current-node-key-value (car (project-list (list (caar tree)) attribute-list key-attributes)))
 (cond ((equal (car beginning-value) t)
(setf comparison-operator 'less-than))
       (t
(setf comparison-operator (node-compare beginning-value current-node-key-value domains))))
 (cond
   ;;
   ;;  The beginning reference key value is less than the current node value, take the left branch
   ;;
   ((and (equal comparison-operator 'less-than) (string-equal mode 'locate))
    (multiple-value-setq (mod-tree mode rebalancep number-deleted beginning-value)
      (list-avl-key-delete (third tree) attribute-list key-attributes domains beginning-value
    termination-clause mode relation-name where-clause rebalancep
    number-deleted))
    (rplaca (cddr tree) mod-tree)
    (cond (rebalancep
   (multiple-value-setq (tree rebalancep)
  (balance1 tree rebalancep))
   (cond (rebalancep
  (setf mode "TERMINATE"))
 (t
  (setf mode "RESTART")))))
    (cond ((and (not (string-equal mode "RESTART"))(not (string-equal mode "TERMINATE"))
(not (string-equal mode "FINISHED")))
   (multiple-value-setq (mod-tree mode rebalancep number-deleted beginning-value)
      (process-list-avl-delete tree attribute-list where-clause number-deleted rebalancep mode
         beginning-value relation-name termination-clause key-attributes
         domains))
   (setf tree mod-tree))))
   ((and (equal comparison-operator 'greater-than) (string-equal mode 'locate))
    (multiple-value-setq (mod-tree mode rebalancep number-deleted beginning-value)
       (list-avl-key-delete (cadddr tree) attribute-list key-attributes domains beginning-value
     termination-clause mode relation-name where-clause rebalancep
     number-deleted))
    (rplaca (cdddr tree) mod-tree)
    (cond (rebalancep
   (multiple-value-setq (tree rebalancep)
  (balance2 tree rebalancep))
   (cond (rebalancep
  (setf mode "TERMINATE"))
 (t
  (setf mode "RESTART"))))))
   ((string-equal comparison-operator 'equal)
;
;  Found a node that is equal to the current tuple as far as the key goes. This might not however be the only
; node in the tree which is equavilent with the current key value. This is because the key used in the retrieval
; may not be the complete key of the relation. Because of this, must continue to travel along the left path until
; the node is no longer equal.
;
    (setf mode  "LOCATE-STAGE-2")
    (multiple-value-setq (mod-tree mode rebalancep number-deleted beginning-value)
  (list-avl-key-delete (caddr tree)
         attribute-list key-attributes domains
         beginning-value termination-clause mode
         relation-name where-clause rebalancep number-deleted))
    ;;
    ;;  The first time control is passed to this location, the beginning node has been found
    ;;
    ;;
    ;;  Need to determine if the current node is to be deleted. This is done by EVALuating the where clause for the current node.
    ;; Also must be wary for more than a single tuple per node. If tuples get deleted from within the node but not the node itself,
    ;; searching can continue. This function must only be rewound when a rebalancing needs to be done.
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
   (multiple-value-setq (mod-tree mode rebalancep number-deleted beginning-value)
      (process-list-avl-delete tree attribute-list where-clause number-deleted rebalancep mode
         beginning-value relation-name termination-clause key-attributes
         domains))
   (setf tree mod-tree))))))
 ((null tree)
  (setf mode "DELETE-SEARCH")))
  (cond ((string-equal mode "RESTART")
 (multiple-value-setq (tree mode rebalancep number-deleted beginning-value)
   (list-avl-key-delete tree attribute-list key-attributes domains beginning-value termination-clause
 "LOCATE" relation-name where-clause rebalancep number-deleted))))
  (values tree mode rebalancep number-deleted beginning-value))

(defun super-fast-eval-where (val-list attribute-list where-clause
      &aux (result nil))
  (mapc
    (function (lambda (%tuple)
(mapc #'(lambda (x z)
  (set x z))
      attribute-list
      %tuple)
(if (eval where-clause)
    (setf result (cons %tuple result)))))
    val-list)
  result)

(defun process-list-avl-delete (tree attribute-list where-clause number-deleted rebalancep mode beginning-value
 relation-name termination-clause key-attributes domains
 &aux deleted-tuples mod-tree temp-tree tuples)
  ;;
  ;;  Any tuples to delete from the current node ??
  ;;
  (setf deleted-tuples (super-fast-eval-where (first tree) attribute-list where-clause))
  (cond ((> (length deleted-tuples) 0)
 ;;
 ;;  Need to determine which tuples from the current node need to be deleted
 ;;
 (mapc (function (lambda (node-tuple)
   (if (not (member node-tuple deleted-tuples :test 'equalp))
       (setf tuples (append (list node-tuple) tuples)))))
       (first tree))
 (setf number-deleted (+ number-deleted (length deleted-tuples)))
 ;;
 ;;  No tuples are left in the node, delete the node
 ;;
 (cond ((null tuples)
(setf beginning-value (car (project-list (list (caar tree)) attribute-list key-attributes)))
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
 (delavl (third tree) rebalancep temp-tree))
       (rplaca (cddr temp-tree) mod-tree)
       (if rebalancep
   (multiple-value-setq (tree rebalancep)
     (balance1 tree rebalancep))))))
       (t
(rplaca tree tuples)))))
  ;;
  ;;  If the current node is not to be deleted and it does not invalidate the termination clause, process the right subtree for deletion.
  ;;
  (cond ((not (string-equal mode "TERMINATE"))
 (cond ((super-fast-eval-where (first tree) attribute-list termination-clause)
(multiple-value-setq (mod-tree mode rebalancep number-deleted beginning-value)
  (list-avl-key-delete (cadddr tree) attribute-list key-attributes domains beginning-value
        termination-clause "LOCATE" relation-name where-clause rebalancep
        number-deleted))
(rplaca (cdddr tree) mod-tree)
(cond (rebalancep
       (multiple-value-setq (tree rebalancep)
 (balance2 tree rebalancep))
       (setf mode "TERMINATE"))))
       ((first tree)
(setf mode "FINISHED"
      beginning-value nil)))))
  (values tree mode rebalancep number-deleted beginning-value))

(defun delete-flavor-avl (relation-name attribute-list key-attributes domains where-clause index-name
  &aux key-value key-domain-list mode (number-deleted 0) rebalancep temp-attribute-list
  termination-condition (total-number-deleted 0) tree package-name)
  ;;
  ;;  Obtain the key from the where clause from the particular relation.
  ;;
  (cond ((or (equal where-clause t)(equal where-clause '(t)))
 (putp index-name nil 'entry-point)
 (setf total-number-deleted (caar (retrieve 'system-relation 'project '("CARDINALITY")
       'where `(string-equal relation-name
        ,(string-upcase relation-name))
       'tuples t))))
(t
 (cond ((member (string-upcase relation-name) *system-relations* :test 'string-equal)
(setf key-domain-list (eval (read-from-string
        (concatenate 'string *pkg-string* ":*"
       (string-upcase relation-name)
       "-KEY-DOMAINS*")))))
       (t
(setf key-domain-list (car (project-list (list domains) attribute-list key-attributes)))))
 (cond ((symbolp relation-name)
(setf package-name (package-name (symbol-package relation-name))))
       (t
(setf package-name *pkg-string*)))
 (setf key-value (extract-key-avl attribute-list key-attributes key-domain-list where-clause
    package-name)
       tree (getp index-name 'entry-point))
 (cond ((null key-value)
(setf key-value (list (list key-attributes) '((t)) '((t))))))
 ;;
 ;;  Perform the actual deletion of the requested tuples
 ;;
 ;;
 ;;  Take each key extracted from the WHERE clause and retrieve the tuples selected by the individual keys.
 ;;
 ;;
 (do ((key% (first key-value) (cdr key%))
      (beginning-value% (second key-value) (cdr beginning-value%))
      (termination-clause% (third key-value) (cdr termination-clause%)))
     ((null key%) number-deleted)
   (cond ((equal (caar termination-clause%) t)
  (setf termination-condition t))
 (t
  (setf termination-condition (list 'lep (caar key%) (caar termination-clause%)))))
   ;;
   ;;  Since EVAL-WHERE has to be called once per node while searching, the process can be sped up by separating
   ;; the eval overhead from the actual operation. The call to prereq initializes all subsequent calls to FAST-EVAL-WHERE.
   ;;
   (multiple-value-setq (where-clause temp-attribute-list)
     (eval-where-prereq where-clause attribute-list relation-name))
   (progv temp-attribute-list nil
     (setf mode "TERMINATE"
   number-deleted 1)
     (do ((beginning-value (car beginning-value%) beginning-value))
 ((or (equal number-deleted 0)(string-equal mode "FINISHED")) t)
       (setf number-deleted 0)
       (multiple-value-setq (tree mode rebalancep number-deleted beginning-value)
 (flavor-avl-key-delete tree temp-attribute-list key-attributes key-domain-list beginning-value
  termination-condition "LOCATE" relation-name where-clause rebalancep
  number-deleted))
       (setf total-number-deleted (+ total-number-deleted number-deleted)
     rebalancep nil))))
 (putp index-name tree 'entry-point)))
  total-number-deleted)

(defun flavor-avl-key-delete (tree attribute-list key-attributes domains beginning-value termination-clause mode
      relation-name where-clause rebalancep number-deleted
      &aux comparison-operator current-node-key-value mod-tree)
  ;;
  ;;  Locate the node where the search will begin
  ;;
  (cond ((and (not (equal tree nil))
      (or (string-equal mode 'locate) (string-equal mode 'locate-stage-2)
  (string-equal mode "DELETE-SEARCH")))
 (setf current-node-key-value (car (project-flavor (list (caar tree)) attribute-list key-attributes)))
 (cond ((equal (car beginning-value) t)
(setf comparison-operator 'less-than))
       (t
(setf comparison-operator
      (node-compare beginning-value current-node-key-value domains))))
 (cond
   ;;
   ;;  The beginning reference key value is less than the current node value, take the left branch
   ;;
       ((and (equal comparison-operator 'less-than) (string-equal mode 'locate))
(multiple-value-setq (mod-tree mode rebalancep number-deleted beginning-value)
  (flavor-avl-key-delete (third tree) attribute-list key-attributes domains beginning-value
   termination-clause mode relation-name where-clause rebalancep
   number-deleted))
(rplaca (cddr tree) mod-tree)
(cond (rebalancep
       (multiple-value-setq (tree rebalancep)
 (balance1 tree rebalancep))
       (cond (rebalancep
       (setf mode "TERMINATE"))
     (t
      (setf mode "RESTART")))))
(cond ((and (not (string-equal mode "RESTART"))(not (string-equal mode "TERMINATE"))
    (not (string-equal mode "FINISHED")))
       (multiple-value-setq (mod-tree mode rebalancep number-deleted beginning-value)
 (process-flavor-avl-delete tree attribute-list where-clause number-deleted rebalancep
       mode beginning-value relation-name termination-clause
       key-attributes domains))
       (setf tree mod-tree))))
       ((and (equal comparison-operator 'greater-than) (string-equal mode 'locate))
(multiple-value-setq (mod-tree mode rebalancep number-deleted beginning-value)
  (flavor-avl-key-delete (cadddr tree) attribute-list key-attributes domains beginning-value
   termination-clause mode relation-name where-clause rebalancep
   number-deleted))
(rplaca (cdddr tree) mod-tree)
(cond (rebalancep
       (multiple-value-setq (tree rebalancep)
 (balance2 tree rebalancep))
       (cond (rebalancep
       (setf mode "TERMINATE"))
     (t
      (setf mode "RESTART"))))))
       ((string-equal comparison-operator 'equal)
;;
;;  Found a node that is equal to the current tuple as far as the key goes. This might not however be the only
;; node in the tree which is equavilent with the current key value. This is because the key used in the retrieval
 ;; may not be the complete key of the relation. Because of this, must continue to travel along the left path until
;; the node is no longer equal.
;;
(setf mode  "LOCATE-STAGE-2")
(multiple-value-setq (mod-tree mode rebalancep number-deleted beginning-value)
  (flavor-avl-key-delete (caddr tree) attribute-list key-attributes domains beginning-value
   termination-clause mode relation-name where-clause rebalancep
   number-deleted))
;;
;;  The first time control is passed to this location, the beginning node has been found
;;
;;
;;  Need to determine if the current node is to be deleted. This is done by EVALuating the where clause
;; for the current node. Also must be wary for more than a single tuple per node. If tuples get deleted
;; from within the node but not the node itself, searching can continue. This function must only be rewound
;; when a rebalancing needs to be done.
;;
(cond ((or (string-equal mode "LOCATE") (string-equal mode "LOCATE-STAGE-2"))
       (setf mode "DELETE-SEARCH")))
(rplaca (cddr tree) mod-tree)
(cond (rebalancep
       (multiple-value-setq (tree rebalancep)
 (balance1 tree rebalancep))
       (cond (rebalancep
      (setf mode "TERMINATE"))
     (t
      (setf mode "RESTART")))))
(cond ((and (not (string-equal mode "RESTART"))(not (string-equal mode "TERMINATE"))
    (not (string-equal mode "FINISHED")))
       (multiple-value-setq (mod-tree mode rebalancep number-deleted beginning-value)
 (process-flavor-avl-delete tree attribute-list where-clause number-deleted rebalancep
       mode beginning-value relation-name termination-clause
       key-attributes domains))
       (setf tree mod-tree))))))
 ((null tree)
  (setf mode "DELETE-SEARCH")))
  (cond ((string-equal mode "RESTART")
 (multiple-value-setq (tree mode rebalancep number-deleted beginning-value)
   (flavor-avl-key-delete tree attribute-list key-attributes domains beginning-value termination-clause
   "LOCATE" relation-name where-clause rebalancep number-deleted))))
  (values tree mode rebalancep number-deleted beginning-value))

(defun process-flavor-avl-delete (tree attribute-list where-clause number-deleted rebalancep mode
   beginning-value relation-name termination-clause key-attributes domains
   &aux deleted-tuples mod-tree temp-tree tuples)
  ;;
  ;;  Any tuples to delete from the current node ??
  ;;
  (setf deleted-tuples (super-fast-eval-where (project-flavor (first tree) attribute-list attribute-list)
        attribute-list where-clause))
  (cond ((> (length deleted-tuples) 0)
 ;;
 ;;  Need to determine which tuples from the current node need to be deleted
 ;;
 (mapc (function (lambda (node-tuple list-tuple)
   (cond ((not (member list-tuple deleted-tuples :test 'equalp))
   (setf tuples (append (list node-tuple) tuples))))))
       (first tree)
       (project-flavor (first tree) attribute-list attribute-list))
 (setf number-deleted (+ number-deleted (length deleted-tuples)))
 ;;
 ;;  No tuples are left in the node, delete the node
 ;;
 (cond ((null tuples)
(setf beginning-value (car (project-flavor (list (caar tree)) attribute-list key-attributes)))
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
 (delavl (third tree) rebalancep temp-tree))
       (rplaca (cddr temp-tree) mod-tree)
       (cond (rebalancep
      (multiple-value-setq (tree rebalancep)
 (balance1 tree rebalancep)))))))
       (t
(rplaca tree tuples)))))
  ;;
  ;;  If the current node is not to be deleted and it does not invalidate the termination clause, process the right subtree for deletion.
  ;;
  (cond ((not (string-equal mode "TERMINATE"))
 (cond ((super-fast-eval-where (project-flavor (first tree) attribute-list attribute-list)
        attribute-list termination-clause)
(multiple-value-setq (mod-tree mode rebalancep number-deleted beginning-value)
  (flavor-avl-key-delete (cadddr tree) attribute-list key-attributes domains beginning-value
   termination-clause "LOCATE" relation-name where-clause rebalancep
   number-deleted))
(rplaca (cdddr tree) mod-tree)
(cond (rebalancep
       (multiple-value-setq (tree rebalancep)
 (balance2 tree rebalancep))
       (setf mode "TERMINATE"))))
       ((first tree)
(setf mode "FINISHED"
      beginning-value nil)))))
  (values tree mode rebalancep number-deleted beginning-value))

(defun delete-struct-avl (relation-name attribute-list key-attributes domains where-clause index-name
  &aux key-value key-domain-list mode (number-deleted 0) rebalancep temp-attribute-list
  termination-condition (total-number-deleted 0) tree package-name)
  (cond ((or (equal where-clause t)(equal where-clause '(t)))
 (putp index-name nil 'entry-point)
 (setf total-number-deleted (caar (retrieve 'system-relation 'project '("CARDINALITY")
       'where `(string-equal relation-name
        ,(string-upcase relation-name))
       'tuples t))))
(t
 ;;
 ;;  Obtain the key from the where clause from the particular relation.
 ;;
 (cond ((member (string-upcase relation-name) *system-relations* :test 'string-equal)
(setf key-domain-list (eval (read-from-string
        (concatenate 'string *pkg-string* ":*"
       (string-upcase relation-name)
       "-KEY-DOMAINS*")))))
       (t
(setf key-domain-list (car (project-list (list domains) attribute-list key-attributes)))))
 (cond ((symbolp relation-name)
(setf package-name (package-name (symbol-package relation-name))))
       (t
(setf package-name *pkg-string*)))
 (setf key-value (extract-key-avl attribute-list key-attributes key-domain-list where-clause
    package-name)
       tree (getp index-name 'entry-point))
 (cond ((null key-value)
(setf key-value (list (list key-attributes) '((t)) '((t))))))
 ;;
 ;;  Perform the actual deletion of the requested tuples
 ;;
 ;;
 ;;  Take each key extracted from the WHERE clause and retrieve the tuples selected by the individual
 ;;   keys.
 ;;
 (do ((key% (first key-value) (cdr key%))
      (beginning-value% (second key-value) (cdr beginning-value%))
      (termination-clause% (third key-value) (cdr termination-clause%)))
     ((null key%) number-deleted)
   (cond ((equal (caar termination-clause%) t)
  (setf termination-condition t))
 (t
  (setf termination-condition (list 'lep (caar key%) (caar termination-clause%)))))
   ;;
   ;;  Since EVAL-WHERE has to be called once per node while searching, the process can be sped up by separating
   ;; the eval overhead from the actual operation. The call to prereq initializes all subsequent calls to FAST-EVAL-WHERE.
   ;;
   (multiple-value-setq (where-clause temp-attribute-list)
     (eval-where-prereq where-clause attribute-list relation-name))
   (progv temp-attribute-list nil
     (setf mode "TERMINATE"
   number-deleted 1)
     (do ((beginning-value (car beginning-value%) beginning-value))
 ((or (equal number-deleted 0)(string-equal mode "FINISHED")) t)
       (setf number-deleted 0)
       (multiple-value-setq (tree mode rebalancep number-deleted beginning-value)
 (struct-avl-key-delete tree temp-attribute-list key-attributes key-domain-list beginning-value
  termination-condition "LOCATE" relation-name where-clause rebalancep
  number-deleted))
       (setf total-number-deleted (+ total-number-deleted number-deleted)
     rebalancep nil))))
 (putp index-name tree 'entry-point)))
  total-number-deleted)

(defun struct-avl-key-delete (tree attribute-list key-attributes domains beginning-value termination-clause mode
      relation-name where-clause rebalancep number-deleted
      &aux comparison-operator current-node-key-value mod-tree)
  ;;
  ;;  Locate the node where the search will begin
  ;;
  (cond ((and (not (equal tree nil))
      (or (string-equal mode 'locate) (string-equal mode 'locate-stage-2)
  (string-equal mode "DELETE-SEARCH")))
 (setf current-node-key-value (car (project-struct (list (caar tree)) attribute-list key-attributes
       relation-name)))
 (cond ((equal (car beginning-value) t)
(setf comparison-operator 'less-than))
       (t
(setf comparison-operator
      (node-compare beginning-value current-node-key-value domains))))
 (cond
   ;;
   ;;  The beginning reference key value is less than the current node value, take the left branch
   ;;
       ((and (equal comparison-operator 'less-than) (string-equal mode 'locate))
(multiple-value-setq (mod-tree mode rebalancep number-deleted beginning-value)
  (struct-avl-key-delete (third tree) attribute-list key-attributes domains beginning-value
   termination-clause mode relation-name where-clause rebalancep
   number-deleted))
(rplaca (cddr tree) mod-tree)
(cond (rebalancep
       (multiple-value-setq (tree rebalancep)
 (balance1 tree rebalancep))
       (cond (rebalancep
       (setf mode "TERMINATE"))
     (t
      (setf mode "RESTART")))))
(cond ((and (not (string-equal mode "RESTART"))(not (string-equal mode "TERMINATE"))
    (not (string-equal mode "FINISHED")))
       (multiple-value-setq (mod-tree mode rebalancep number-deleted beginning-value)
 (process-struct-avl-delete tree attribute-list where-clause number-deleted rebalancep
       mode beginning-value relation-name termination-clause
       key-attributes domains))
       (setf tree mod-tree))))
       ((and (equal comparison-operator 'greater-than) (string-equal mode 'locate))
(multiple-value-setq (mod-tree mode rebalancep number-deleted beginning-value)
  (struct-avl-key-delete (cadddr tree) attribute-list key-attributes domains beginning-value
   termination-clause mode relation-name where-clause rebalancep
   number-deleted))
(rplaca (cdddr tree) mod-tree)
(cond (rebalancep
       (multiple-value-setq (tree rebalancep)
 (balance2 tree rebalancep))
       (cond (rebalancep
       (setf mode "TERMINATE"))
     (t
      (setf mode "RESTART"))))))
       ((string-equal comparison-operator 'equal)
;;
;;  Found a node that is equal to the current tuple as far as the key goes. This might not however be the only
;; node in the tree which is equavilent with the current key value. This is because the key used in the retrieval
;; may not be the complete key of the relation. Because of this, must continue to travel along the left path until
;; the node is no longer equal.
;;
(setf mode  "LOCATE-STAGE-2")
(multiple-value-setq (mod-tree mode rebalancep number-deleted beginning-value)
  (struct-avl-key-delete (caddr tree) attribute-list key-attributes domains beginning-value
   termination-clause mode relation-name where-clause rebalancep
   number-deleted))
;;
;;  The first time control is passed to this location, the beginning node has been found
;;
;;
;;  Need to determine if the current node is to be deleted. This is done by EVALuating the where clause
;; for the current node. Also must be wary for more than a single tuple per node. If tuples get deleted
;; from within the node but not the node itself, searching can continue. This function must only be rewound
;; when a rebalancing needs to be done.
;;
 (cond ((or (string-equal mode "LOCATE")
   (string-equal mode "LOCATE-STAGE-2"))
       (setf mode "DELETE-SEARCH")))
(rplaca (cddr tree) mod-tree)
(cond (rebalancep
       (multiple-value-setq (tree rebalancep)
 (balance1 tree rebalancep))
       (cond (rebalancep
       (setf mode "TERMINATE"))
     (t
      (setf mode "RESTART")))))
(cond ((and (not (string-equal mode "RESTART"))(not (string-equal mode "TERMINATE"))
    (not (string-equal mode "FINISHED")))
       (multiple-value-setq (mod-tree mode rebalancep number-deleted beginning-value)
 (process-struct-avl-delete tree attribute-list where-clause number-deleted rebalancep
       mode beginning-value relation-name termination-clause
       key-attributes domains))
       (setf tree mod-tree))))))
 ((null tree)
  (setf mode "DELETE-SEARCH")))
  (cond ((string-equal mode "RESTART")
 (multiple-value-setq (tree mode rebalancep number-deleted beginning-value)
   (struct-avl-key-delete tree attribute-list key-attributes domains beginning-value termination-clause
   "LOCATE" relation-name where-clause rebalancep number-deleted))))
  (values tree mode rebalancep number-deleted beginning-value))

(defun process-struct-avl-delete (tree attribute-list where-clause number-deleted rebalancep mode
   beginning-value relation-name termination-clause key-attributes domains
   &aux deleted-tuples mod-tree temp-tree tuples)
  ;;
  ;;  Any tuples to delete from the current node ??
  ;;
  (setf deleted-tuples (super-fast-eval-where (project-struct (first tree) attribute-list attribute-list
          relation-name)
        attribute-list where-clause))
  (cond ((> (length deleted-tuples) 0)
 ;;
 ;;  Need to determine which tuples from the current node need to be deleted
 ;;
 (mapc (function (lambda (node-tuple list-tuple)
   (cond ((not (member list-tuple deleted-tuples :test 'equalp))
   (setf tuples (append (list node-tuple) tuples))))))
       (first tree)
       (project-struct (first tree) attribute-list attribute-list relation-name))
 (setf number-deleted (+ number-deleted (length deleted-tuples)))
 ;;
 ;;  No tuples are left in the node, delete the node
 ;;
 (cond ((null tuples)
(setf beginning-value
      (car (project-struct (list (caar tree)) attribute-list key-attributes relation-name)))
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
 (delavl (third tree) rebalancep temp-tree))
       (rplaca (cddr temp-tree) mod-tree)
       (cond (rebalancep
      (multiple-value-setq (tree rebalancep)
 (balance1 tree rebalancep)))))))
       (t
(rplaca tree tuples)))))
  ;;
  ;;  If the current node is not to be deleted and it does not invalidate the termination clause, process the right subtree for deletion.
  ;;
  (cond ((not (string-equal mode "TERMINATE"))
 (cond ((super-fast-eval-where (project-struct (first tree) attribute-list attribute-list relation-name)
        attribute-list termination-clause)
(multiple-value-setq (mod-tree mode rebalancep number-deleted beginning-value)
  (struct-avl-key-delete (cadddr tree) attribute-list key-attributes domains beginning-value
   termination-clause "LOCATE" relation-name where-clause rebalancep
   number-deleted))
(rplaca (cdddr tree) mod-tree)
(cond (rebalancep
       (multiple-value-setq (tree rebalancep)
 (balance2 tree rebalancep))
       (setf mode "TERMINATE"))))
       ((first tree)
(setf mode "FINISHED"
      beginning-value nil)))))
  (values tree mode rebalancep number-deleted beginning-value))

;; fragments
st)
    value-list (cons (list 'quote default-value) value-list)))))
      ((not (string-equal "RTMS-NO-VA  (t
  (setf mode "RESTART"))))))
   ((string-equal comparison-operator 'equal)
;
;  Found a node that is equal to the current tuple as far as the key goes. This might not however be the only
; node in the tree which is equavilent with the current key value. This is because the key used in the retrieval
; may not be the complete key of the relation. Because of this, must continue to travel along the left path until
; the node is no longer equal.
;
