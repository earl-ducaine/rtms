
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


;;; fragments

 'commit-tuples)))))))
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
 (func
