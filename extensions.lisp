;;; -*- Mode:Common-Lisp; Package:RTMS; Base:10 -*-

;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved


;;INITIAl-VALIDATION
;;
;; This purpose of this function is to provide initial validation
;; (error checking) for certain parameters. These types of checks are
;; common across many of the user callable functions in RTMS.
;;
;;   (1) Checks for active-database
;;   (2) Converts the name passed nto a string
;;   (3) Takes care of nesting of parens in the keyword-list
;;   (4) Sends the keyword list through the get-keyword-prereq cycle
;;   (5) Obtains information about the relation sent.
(defun initial-validation (relation-name keyword-list keyword-list-prereq info-attribute-list
			   &optional (relation-name-stringp nil))
  (let (relation-info)
    (cond ((not *parameter-checking*)
	   (setf  keyword-list (de-nest-keyword-list keyword-list)
		  relation-info (car (qtrieve 'system-relation *system-relation-attributes* info-attribute-list
					      *system-relation-key*
					      `(string-equal relation-name  ,(string relation-name))))))
	  ((or (not (active-database relation-name))
	       (not (setf relation-name (validate-name relation-name))))
	   (setf relation-name nil))
	  (t
	   ;; Remove levels of parens which might be present. This is here to support the pre-released version where the user was
	   ;; required to include all of the keywords in a list. NLMENU is one example which requires this to remain.
	   (setf keyword-list (get-keyword-value-prereq keyword-list-prereq (de-nest-keyword-list keyword-list))
		 relation-info (car (qtrieve 'system-relation *system-relation-attributes* info-attribute-list
					     *system-relation-key*
					     `(string-equal relation-name  ,(string relation-name)))))))
    (if (and relation-name relation-name-stringp)
	(setf relation-name (string-upcase relation-name)))
    (values relation-name keyword-list relation-info)))

(defun de-nest-keyword-list (keyword-list)
  (do ((keyword-list keyword-list (car keyword-list)))
      ((or (null keyword-list)(not (listp (car keyword-list)))) keyword-list)))

(defun move-relation (relation-name &optional  &key (direction 'out))
  (cond ((equalp direction 'out)
	 (mapc (function (lambda (system-relation-name)
		 (putp relation-name (retrieve system-relation-name 'tuples t
					       'where `(string-equal relation-name
								     ,(string relation-name)))
		       system-relation-name)))
	       '(system-relation system-attribute system-index))
	 (putp (concatenate 'string "MOVE-" (string relation-name)) (getp relation-name 'entry-point)
	       'entry-point))
	((equalp direction 'in)
	 (mapc (function (lambda (system-relation-name)
		 (if (getp relation-name system-relation-name)
		     (insert system-relation-name
			     'tuples (getp relation-name system-relation-name)))))
	       '(system-relation system-attribute system-index))
	 (putp relation-name (getp (concatenate 'string "MOVE-" (string relation-name)) 'entry-point)
	       'entry-point))))

(defun database-modifiedp ()
  (let ((modified-relation-list nil))
    (mapc (function (lambda (%tuple)
	    (if (car %tuple)
		(setf modified-relation-list (cons (cadr %tuple) modified-relation-list)))))
	  (retrieve 'system-relation 'project '(modifiedp relation-name) 'tuples t))
    modified-relation-list))


(defun get-attributes (relation-name)
  (caar (qtrieve 'system-relation *system-relation-attributes* '(attributes) *system-relation-key*
		 `(string-equal relation-name ,(string relation-name)))))

;;; fragnments

;; ,(string relation-name)))
;; system-relation-name)))
;; '(system-relation system-attribute system-index))
;; (putp (concatenate 'string "MOVE-" (string relation-name)) (getp relation-name 'entry-point)
;;       'entry-point))
;; ((equalp direction 'in)
;;  (mapc (function (lambda (system-relation-name)
;; 	 (if (getp relation-name system-relation-name)
;; 	     (insert system-relation-name
;; 		     'tuples (getp relation-name system-relation-name)))))
;;        '(system-relation system-attribute system-index))
;;
;;  (putp relation-name (getp (concatenate 'string "MOVE-" (string
;;  relatLMFL#!C(:HOST "SW-MFG" :BACKUP-DATE 2760540708. :SYSTEM-TYPE
;;  :LOGICAL :VERSION 2. :TYPE "LISP" :NAME "GLOBAL-FUNCTIONS"
;;  :DIRECTORY ("RTMS-DIR") :SOURCE-PATTERN "( :DIRECTORY
;;  (\"RTMS-DIR\") :NAME :WILD :TYPE :WILD :VERSION :NEWEST)"
;;  :CHARACTERS T :NOT-BACKED-UP T :CREATION-DATE 2753963282. :AUTHOR
;;  "REL3" :LENGTH-IN-BYTES 65836. :LENGTH-IN-BLOCKS 65. :BYTE-SIZE
;;  8.)
