
;;; -*- Mode:Common-Lisp; Package:RTMS; Fonts:(*CODE-FONT* *COMMENT-FONT* *STRING-FONT*); Base:10 -*-
;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved
;;; HASH-OPT
;;;
;;; This file contains the following Explorer extensions to CommonLisp Standard as Indicated in the June 1985 Explorer Lisp
;;; Reference
;;;     errset
;;;
;;; The following function contains flavor references and thus are incompatable with CommonLisp. Their removal will not
;;; effect the functionality of RTMS.
;;;

(defun extract-key-hash (attribute-list key-list domains where-clause package-name
 &aux opt-func opt-func-list result key-domains)
  domains
  ;;
  ;;  This should be done before any of the extract key functions are called in retrieve
  ;;
  (setf attribute-list (unconvert-attributes attribute-list package-name)
key-list (unconvert-attributes key-list package-name)
domains (unconvert-attributes domains package-name))
  (cond ((not (or (not (listp where-clause)) (equal where-clause '(t))))
 (setf key-domains (project-list (list domains) attribute-list key-list))
 (cond ((listp where-clause)
;;
;; Obtain a list of the optimization functions which are known to RTMS at this time. The values come back from
;; retrieve as a list of lists. Remove the extra set of lists from the values. This list will be used in all of the
;; optimization functions.
;;
(setf opt-func-list
      (funcall (find-symbol (concatenate 'string "RETRIEVE-"
     *system-relation-base-implementation*
     "-" *system-relation-storage-structure*) *pkg-string*)
       'system-optfunc *system-optfunc-attributes* '(symbol-name optimize-function)
       *system-optfunc-key* (list 'string-equal 'storage-structure-type "HASH") nil
       'system-optfunc)))
       (t
(setf opt-func-list nil)))
 (cond ((setf opt-func (cadr (assoc (car where-clause) opt-func-list :test 'string-equal)))
(cond ((string-equal opt-func "OPT-HASH-AND")
       (setf result (opt-hash-and attribute-list key-list where-clause opt-func-list)))
      ((string-equal opt-func "OPT-HASH-EQUAL")
       (setf result (opt-hash-equal attribute-list key-list where-clause opt-func-list)))
      ((string-equal opt-func "OPT-HASH-OR")
       (setf result (opt-hash-or attribute-list key-list where-clause opt-func-list)))
      (t
       (setf result (funcall (find-symbol opt-func *pkg-string*)
       attribute-list key-list where-clause opt-func-list))))
(cond ((string-equal (car result) "k")
       (setf result (caddr result)))
      (t
       (setf result nil))))
       (t
(setf result nil))))
(t
 (setf result nil)))
  result)


;;; This function will process the rest of the where clause and determine if a hash key may be extracted. If so, the hash key is returned.
;;;
;;; For a hash key to be formed from an AND clause all of the attributes which make up the key must be present and present only once.
;;; Also, a key attribute may not be contained in the same leaf clause as a key attribute.
;;;
;;; The results returned from this function, as well as all optimization functions, are in the following form :
;;; (stat-character
;;;  list--of-lists-of-key-attributes-present
;;;  list-of-constant-values
;;;  list-of-non-key-attributes-present)
;;;  where
;;; stat-character - is one of the following :
;;;   k - a valid key has been formed
;;;   n - no key can be formed from where
;;;       clause (ever).
;;;   p - it is still possible to form a
;;;       but this sub-clause will not
;;;       contribute to it.
;;;   c - constant sub-clause - nothing to
;;;       add to the key.
;;;
(defun opt-hash-and (attribute-list key-list where-clause opt-func-list
       &aux do-continue element opt-func (result '("p" (nil) (nil) (nil))) (where-aux-list nil)
            (where-key-list nil) (where-val-list nil))
  ;;
   ;;   Process for all sub-clauses of the where-clause
   ;;
  (do ((where-clause (cdr where-clause) (cdr where-clause)))
      ((or (null where-clause) (string-equal (car result) "n")) result)
    (cond ((listp (car where-clause))
   ;;
    ;;  The element is a list, must be a function call. Call the appropriate optimization
    ;; function if one exists. Add the results from the function to the current results. The optimization functions which
    ;; are defined by RTMS initially are used as selections in the following COND clause to improve the speed of calling
   ;; these functions. If the function in the sub-clause is not from this group, the function name must be formed before
   ;; it can be invoked.
   ;;
   (cond ((setf opt-func (cadr (assoc (caar where-clause) opt-func-list :test 'string-equal)))
  (cond ((string-equal opt-func "OPT-HASH-AND")
 (setf result (opt-hash-and attribute-list key-list (car where-clause) opt-func-list)))
((string-equal opt-func "OPT-HASH-EQUAL")
 (setf result (opt-hash-equal attribute-list key-list
         (car where-clause) opt-func-list)))
((string-equal opt-func "OPT-HASH-OR")
 (setf result (opt-hash-or attribute-list key-list (car where-clause) opt-func-list)))
(t
 (setf result (funcall (find-symbol opt-func *pkg-string*)
         attribute-list key-list (car where-clause) opt-func-list))))
  (setf where-key-list (append where-key-list (second result))
where-val-list (append where-val-list (third result))
where-aux-list (append where-aux-list (fourth result))))))
  ((symbolp (car where-clause))
   ;;
   ;; The element is a symbol - either it is an attribute or a variable. In either case,
   ;; nothing concerning the key can be gained here.
   ;;
           (if (member (car where-clause) attribute-list :test 'equal)
       ;;
       ;;  Symbol is an attribute - imporperly formed where clause - no key possible
       ;;
       (setf result '("n" (nil) (nil) (nil)))))))
  ;;
  ;;  The where-clause has been parsed, now let us see what we have
  ;;
  (cond ((string-equal (car result) "n"))
((and where-key-list where-val-list)
 ;;
 ;; We have a possible key. To make final determination, all key attributes must be present in the where-key-list and they must
 ;; not repeated. If they are all there, they must be put in order.
 ;;
 (setf result '(()()()))
 (setf do-continue 't)
 ;;
 ;;  Determine if a key attribute is repeated in the where-key-list. If it is,
 ;; an illegal where clause was specified and no key can be found
 ;;
 (do ((where-key-list where-key-list where-key-list))
     ((or (null where-key-list) (null do-continue)))
   (setf element (car where-key-list)
 where-key-list (cdr where-key-list))
   (cond ((member element where-key-list :test 'equal)
  (setf do-continue nil)
  (setf result '("n" (nil) (nil) (nil))))))
 ;;
 ;;  Determine if all of the key attributes were contained in the where clause
 ;;
 (do ((key-list key-list (cdr key-list)))
     ((or (null key-list) (null do-continue)) do-continue)
   (if (setf do-continue (member (car key-list) where-key-list :test 'equal))
       (setf result
     (do ((where-key-list where-key-list (cdr where-key-list))
  (where-val-list where-val-list (cdr where-val-list))
  (where-aux-list where-aux-list (cdr where-aux-list)))
 ((equal (car where-key-list) (car key-list))
  (if (car key-list)
      (list (append (car result) (list (car where-key-list)))
       (append (cadr result) (list (car where-val-list)))
     (append (caddr result) (list (car where-aux-list))))))))))
 ;;
 ;;  Result contains the list of key attribute names found in the where-clause in the order in which they are present in the
 ;; key-list. If the two lists are equal, we have a key, otherwise no key is possible.
 ;;
 (if (and (equal (car result) key-list) result)
     (setf result (append (list "k") (list (list key-list)) (list (list (cadr result)))
   (list (cddr result))))
   (setf result '("n" (nil) (nil) (nil)))))
;;
;;  Constant expression - nothing to add
;;
((and (null where-key-list) (null where-aux-list))
 (setf result '("c" (nil) (nil) (nil))))
;;
;; A key is not possible from this where clause
;;
(t
 (setf result '("n" (nil) (nil) (nil)))))
  result)

;;; This function will process the rest of the where clause and determine if a hash key may be extracted. If so, the hash key is returned.
;;;
;;; For a hash key to be completely formed from an EQUAL clause all of the attributes which make up the key must be present and
;;; present only once. However, only two elements may be present in and EQUAL clause and only one may be an attribute. If the key is
;;; made up of more than one attribute, a complete key is not possible from this function, a partial key might be formed in this case.
;;;
;;;The results returned from this function, as well as all optimization functions, are in the following form :
;;; (stat-character
;;;  list--of-lists-of-key-attributes-present
;;;  list-of-constant-values
;;;  list-of-non-key-attributes-present)
;;;  where
;;; stat-character - is one of the following :
;;;   k - a valid key has been formed
;;;   n - no key can be formed from where clause (ever).
;;;   p - it is still possible to form a but this sub-clause will not contribute to it.
;;;   c - constant sub-clause - nothing to add to the key.
;;;
(defun opt-hash-equal (attribute-list key-list where-clause opt-func-list
       &aux opt-func (result '("p" (nil) (nil) (nil))) (where-aux-list nil)
       (where-key-list nil) (where-val-list nil))
  ;;
  ;;  Process for all sub-clauses of the where-clause
  ;;
  (do ((where-clause (cdr where-clause) (cdr where-clause)))
      ((or (null where-clause) (string-equal (car result) "n")) result)
    (cond ((and (listp (car where-clause)) (equal 'quote (caar where-clause)))
   (if (null where-val-list)
       (setf where-val-list (cadar where-clause))
       (setf result (append (list "c") (cdr result)))))
  ;;
  ;;  The element is a list, must be a function call. Call the appropriate optimization
  ;; function if one exists. Add the result from the function to the current results.
  ;; For this functin, only a constant value may be returned from the function call,
  ;; otherwise an improperly formed where-clause has been passed.
  ;;
  ((listp (car where-clause))
   (cond ((setf opt-func (cadr (assoc (caar where-clause) opt-func-list :test 'string-equal)))
  (cond ((string-equal opt-func "OPT-HASH-AND")
 (setf result (opt-hash-and attribute-list key-list (car where-clause) opt-func-list)))
((string-equal opt-func "OPT-HASH-EQUAL")
 (setf result (opt-hash-equal attribute-list key-list (car where-clause)
         opt-func-list)))
((string-equal opt-func "OPT-HASH-OR")
 (setf result (opt-hash-or attribute-list key-list (car where-clause) opt-func-list)))
(t
 (setf result (funcall (find-symbol opt-func *pkg-string*)
         attribute-list key-list (car where-clause) opt-func-list))))
  (if (null where-val-list)
      (setf where-val-list (car (third result))))
  ;;
  ;;  No key may be formed if the key list or the aux list is non-nil. Also, all values returned must be single.
  ;;
  (if (or (car (second result)) (car (fourth result)) (cdr (third result)))
      (setf result '("n" (nil) (nil) (nil)))))))
  ((symbolp (car where-clause))
   ;;
   ;; The element is a symbol, add the fact to the proper list
   ;;
           (if (member (car where-clause) attribute-list :test 'equal)
       ;;
       ;;  Symbol is an attribute
       ;;
       (if (member (car where-clause) key-list :test 'equal)
   ;;
   ;;  Attribute is a key attribute
   ;;
      (if (null where-key-list)
       (setf where-key-list (car where-clause)))
   ;;
   ;;  Attribute is not a key attribute
   ;;
   (if (null where-aux-list)
       (setf where-aux-list (car where-clause))))
       ;;  The symbol is not an attribute, must return a value, so EVALuate the symbol
       ;;
       (if (null where-val-list)
   (setf where-val-list (or (car (errset (eval (car where-clause)) nil)) (car where-clause)))
   (setf result (append (list "c") (cdr result))))))
  ;;
  ;;  Not a symbol, must be a value.
  ;;
          (t
   (if (null where-val-list)
       (setf where-val-list (car where-clause))
       (setf result (append (list "c") (cdr result)))))))
  ;;
  ;;  The where-clause has been parsed -- let us see what we have
  ;;
  (cond ((string-equal (car result) "n"))
((string-equal (car result) "c"))
((and where-key-list where-val-list)
 ;;
 ;; We have a possible key. To make final determination, all key attributes must be present in the where-key-list. If they are
 ;; all there they must be in order. However this optimization function only allows two parameters to be specified in the equal
 ;; clause, therefore the where-key-list and the key-list must be equal. If they are not equal, a key is still possible but not
 ;; completely from this routine
 ;;
         (if (equal (list where-key-list) key-list)
     (setf result (append (list "k") (list (list (list where-key-list)))
   (list (list (list where-val-list))) (list (list (list where-aux-list)))))
     (setf result (append (list "p") (list (list (list where-key-list)))
   (list (list (list where-val-list))) (list (list (list where-aux-list)))))))
((and (null where-key-list) (null where-aux-list))
 (setf where-clause (append (list 'equal) where-clause))
 (setf where-val-list (eval where-clause))
 (setf result (append (list "c") (list (list (list where-key-list)) )
 (list (list (list where-val-list))) (list (list (list where-aux-list))))))
;;
;;  Determine if two attributes were specified, if so this invalidates the possibility of there being a key
;;
((or (and (null where-aux-list) (null where-val-list)) (and where-key-list where-aux-list))
 (setf result '("n" (nil) (nil) (nil) (nil))))
(t
 (setf result '("p" (nil) (nil) (nil)))))
  result)

;;; This function will process the rest of the where clause and determine if a hash key may be extracted. If so, the hash key is returned.
;;;
;;; For a hash key to be formed from an OR clause all of the results from the sub-clauses must be keys or constant expressions.
;;; (stat = k or c). Because of this, it is possible for several keys to be returned from this function.
;;;
;;;The results returned from this function, as well as all optimization functions, are in the following form :
;;; (stat-character
;;;  list--of-lists-of-key-attributes-present
;;;  list-of-constant-values
;;;  list-of-non-key-attributes-present)
;;;  where
;;; stat-character - is one of the following :
;;;   k - a valid key has been formed
;;;   n - no key can be formed from where clause (ever).
;;;   p - it is still possible to form a but this sub-clause will not contribute to it.
;;;   c - constant sub-clause - nothing to add to the key.
(defun opt-hash-or (attribute-list key-list where-clause opt-func-list
       &aux a-result opt-func (result '("f" (nil) (nil) (nil))))
  ;;
  ;;  Process all of the sub-clauses of the where-clause
  ;;
  (do ((where-clause (cdr where-clause) (cdr where-clause)))
      ((or (null where-clause) (string-equal (car result) "n")) result)
    (cond ((listp (car where-clause))
   ;;
   ;;  The element is a list, must be a function call. Call the appropriate optimization function if one exists.
   ;;
   (cond ((setf opt-func (cadr (assoc (caar where-clause) opt-func-list :test 'string-equal)))
  (cond ((string-equal opt-func "OPT-HASH-AND")
 (setf a-result (opt-hash-and attribute-list key-list (car where-clause)
         opt-func-list)))
((string-equal opt-func "OPT-HASH-EQUAL")
 (setf a-result (opt-hash-equal attribute-list key-list (car where-clause)
    opt-func-list)))
((string-equal opt-func "OPT-HASH-OR")
 (setf a-result (opt-hash-or attribute-list key-list (car where-clause)
        opt-func-list)))
(t
 (setf a-result (funcall (find-symbol opt-func *pkg-string*)
         attribute-list key-list (car where-clause) opt-func-list))))
  (cond ((string-equal (car a-result) "k")
 ;;
 ;;  The sub-clause returned a valid key, add it to the list of keys formed.
 ;;
 (cond ((string-equal (car result) "f")
 (setf result a-result))
       (t
 (setf result (list "k"
      (append (second result) (second a-result))
      (append (third result) (third a-result))
      (append (fourth result) (fourth result)))))))
((string-equal (car a-result) "c"))
;;
;;  Result returned from the optimization function was not a constant expression or a valid key, therefore no
;; key may be formed from this OR sub-clause
;;
((or (string-equal (car a-result) "n") (string-equal (car a-result) "p"))
 (setf result '("n" (nil) (nil) (nil))))))))
  ((symbolp (car where-clause))
   ;;
   ;; The element i as a symbol
   ;;
           (if (member (car where-clause) attribute-list :test 'equal)
       ;;
       ;;  Symbol is an attribute
       ;;
               (setf result '("n" (nil) (nil) (nil)))))))
  result)
;;; Fragments


(cond ((listp (car where-clause))
   ;;
    ;;  The element is a list, must be a function call. Call the appropriate optimization
    ;; function if one exists. Add the results from the function to the current results. The optimization functions which
    ;; are defined by RTMS initially are used as selections in the following COND clause to improve the speed of calling
   ;; these functions. If the function in the sub-clause is not from this group, the function name must be formed before
   ;; it can be invoked.
   ;;
   (cond ((setf opt-func (cadr (assoc (caar where-clause) opt-func-list :test 'string-equal)))
  (cond ((string-equal opt-func "OPT-HASH-AND")
 (setf result (opt-hash-and attribute-list key-list (car where-clause) opt-func-list)))
((string-equ
