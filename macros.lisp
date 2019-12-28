
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
