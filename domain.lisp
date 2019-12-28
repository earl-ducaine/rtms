
;;; -*- Mode:Common-Lisp; Package:RTMS; Base:10; Fonts:(*CODE-FONT* *COMMENT-FONT* *STRING-FONT*) -*-
;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved.
;;; DOMAIN
;;;
;;; This file contains the following Explorer extensions to CommonLisp Standard as Indicated in the June 1985 Explorer Lisp
;;; Reference
;;;      deff
;;;

(defun value-domainp (val)
  (cond ((typep val :list)
 '"LISTP")
((typep val :string)
 '"STRINGP")
((typep val :number)
  '"NUMBERP")
((typep val :atom)
 '"ATOM")
(t
 '"ANYP")))

(deff equalp-rtms 'rtms-equalp)
(deff *equalp 'rtms-equalp)
(defun rtms-equalp (&rest vals &aux val1 val2 dom1 dom2)
  (setf vals (do ((val-l vals (car val-l)))
 ((or (not (equal (length val-l) 1)) (not(listp (car val-l)))) val-l) ()))
  ;;
  ;;  only one value, cannot compare
  ;;
  (cond ((equal (length vals) 1)
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - Only one value provided, need at least two to compare."))
 nil)
(t
 (do ((vals-left vals (cdr vals-left)))
     ((equal (length vals-left) 1) (return t))
   (setf val1 (first vals-left)
 val2 (second vals-left)
 dom1 (value-domainp val1)
 dom2 (value-domainp val2))
   ;; if value-domainp returns anyp, it actually means that the domain is not supported
   (cond ((or (string-equal dom1 "ANYP")(string-equal dom2 "ANYP"))
  (return nil)))
   (cond ((string-equal dom1 dom2)
  (if (not (funcall (find-symbol (concatenate 'string "EQUAL-" dom1) *pkg-string*)
     val1 val2))
      (return nil)))
 ((or (and (string-equal dom1 "ATOM")(string-equal dom2 "STRINGP"))
      (and (string-equal dom2 "ATOM")(string-equal dom1 "STRINGP")))
  (if (not (equal-stringp val1 val2))
      (return nil)))
 (t
  (return nil)))))))

(defun equal-anyp (val1 val2)
  (equalp-rtms (list val1 val2)))

(defun equal-atom (val1 val2)
    (cond ((and (numberp val1) (numberp val2))
   (equal val1 val2))
  ((or (numberp val1)(numberp val2))
   nil)
  ((and (stringp val1) (stringp val2))
   (string-equal val1 val2))
  ((or (stringp val1)(stringp val2))
   nil)
  (t
   (string-equal (string val1) (string val2)))))

(defun equal-listp (list1 list2 &aux val1 val2)
  (do ((v1 list1 (cdr v1))(v2 list2 (cdr v2)))
      ((and (null v1)(null v2)) (return t))
    (setf val1 (car v1)
  val2 (car v2))
    (cond ((or (null val1) (null val2)) (return nil)))
    (if (not (*equalp val1 val2))
(return nil))))

(defun equal-numberp (val1 val2)
  (equal val1 val2))

(defun equal-stringp (val1 val2)
  (string-equal val1 val2))

;;;Comparison is made between the previous item (val1) and the present item (val2) to see if the previous item is >= present item. Then
;;; previous item assumes the value of the present and present gets the value of the top item in the rest of the item-list and the process is
;;; repeated till the  condition >= is not satisifed. NIL is returned. When no more elements are found in the item-list T is returned.
(defun gep (&rest vals &aux val1 val2 dom1 dom2)
  (setf vals (do ((val-l vals (car val-l)))
 ((or (not (equal (length val-l) 1)) (not(listp (car val-l)))) val-l) ()))
  ;; only one value, cannot compare
  (cond ((equal (length vals) 1)
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - Only one value provided, need at least two to compare."))
 nil)
(t
 (do ((vals-left vals (cdr vals-left)))
     ((equal (length vals-left) 1) (return t))
   (setf val1 (first vals-left)
 val2 (second vals-left)
 dom1 (value-domainp val1)
 dom2 (value-domainp val2))
   ;; if value-domainp returns anyp, it actually means that the domain is not supported
   (cond ((or (string-equal dom1 "ANYP")(string-equal dom2 "ANYP"))
  (return nil)))
   (cond ((string-equal dom1 dom2)
  (if (not (funcall (find-symbol (concatenate 'string "GE-" dom1) *pkg-string*) val1 val2))
      (return nil)))
 ;; any list is > to anything
 ((string-equal dom2 "LISTP")
  (return nil))
 ((string-equal dom1 "LISTP")
  ())
 ;; any number is < to anything
 ((string-equal dom2 "NUMBERP")
  ())
 ((string-equal dom1 "NUMBERP")
  (return nil))
 ((or (and (string-equal dom1 "ATOM")(string-equal dom2 "STRINGP"))
      (and (string-equal dom2 "ATOM")(string-equal dom1 "STRINGP")))
  (if (not (ge-stringp val1 val2))
      (return nil)))
 (t
  (return nil)))))))

(defun ge-anyp (val1 val2)
  (gtp (list val1 val2)))

(defun ge-atom (val1 val2)
  (cond ((and (numberp val1) (numberp val2))
 (or (> val1 val2)(equal val1 val2)))
((numberp val2)
 t)
((numberp val1)
 nil)
(t
 (setf val1 (string val1)
       val2 (string val2))
 (or (string-lessp val2 val1) (string-equal val1 val2)))))

(defun ge-listp (list1 list2 &aux val1 val2 (is-ge nil))
  (do ((v1 list1 (cdr v1))(v2 list2 (cdr v2)))
      ((or is-ge (and (null v1)(null v2))) (return t))
    (setf val1 (car v1)
  val2 (car v2))
    (cond ((null val2)
   (return t))
  ((null val1)
   (return nil)))
    ;; this is better than calling GEP because as soon as we find one val which is GT (and not equal) we stop the tests
    (cond ((gtp val1 val2)
   (setf is-ge t))
  ((not (*equalp val1 val2))
   (return nil)))))

(defun ge-numberp (val1 val2)
  (or (> val1 val2)(equal val1 val2)))

(defun ge-stringp (val1 val2)
  (or (string-lessp val2 val1)(string-equal val1 val2)))

;;; Comparison is made between the previous item and the present item to see if the previous item is > present item. Then previous item
;;; assumes the value of the present and present gets the value of the top item in the rest of the item-list and the process is repeated till
;;; the  condition > is not satisifed. NIL is returned. When no more elements are found in the item-list T is returned.
(defun gtp (&rest vals &aux val1 val2 dom1 dom2)
  (setf vals (do ((val-l vals (car val-l)))
 ((or (not (equal (length val-l) 1)) (not(listp (car val-l)))) val-l) ()))
  ;; only one value, cannot compare
  (cond ((equal (length vals) 1)
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - Only one value provided, need at least two to compare."))
 nil)
(t
 (do ((vals-left vals (cdr vals-left)))
     ((equal (length vals-left) 1) (return t))
   (setf val1 (first vals-left)
 val2 (second vals-left)
 dom1 (value-domainp val1)
 dom2 (value-domainp val2))
   ;; if value-domainp returns anyp, it actually means that the domain is not supported
   (cond ((or (string-equal dom1 "ANYP")(string-equal dom2 "ANYP"))
  (return nil)))
   (cond ((string-equal dom1 dom2)
  (if (not (funcall (find-symbol (concatenate 'string "GT-" dom1) *pkg-string*) val1 val2))
      (return nil)))
 ;; any list is > to anything
 ((string-equal dom2 "LISTP")
  (return nil))
 ((string-equal dom1 "LISTP")
  ())
 ;; any number is < to anything
 ((string-equal dom2 "NUMBERP")
  ())
 ((string-equal dom1 "NUMBERP")
  (return nil))
 ((or (and (string-equal dom1 "ATOM")(string-equal dom2 "STRINGP"))
      (and (string-equal dom2 "ATOM")(string-equal dom1 "STRINGP")))
  (if (not (gt-stringp val1 val2))
      (return nil)))
 (t
  (return nil)))))))

(defun gt-anyp (val1 val2)
  (gtp (list val1 val2)))

(defun gt-atom (val1 val2)
    (cond ((and (numberp val1) (numberp val2))
   (> val1 val2))
  ((numberp val2)
   t)
  ((numberp val1)
   nil)
  (t
   (string-lessp (string val2) (string val1)))))

(defun gt-listp (list1 list2 &aux val1 val2 (is-gt nil))
  (do ((v1 list1 (cdr v1))(v2 list2 (cdr v2)))
      ((or is-gt (and (null v1)(null v2))) (if is-gt (return t)(return nil)))
    (setf val1 (car v1)
  val2 (car v2))
    (cond ((null val2)
   (return t))
  ((null val1)
   (return nil)))
    (cond ((gtp val1 val2)
   (setf is-gt t))
  ((not (*equalp val1 val2))
   (return nil)))))

(defun gt-numberp (val1 val2)
  (> val1 val2))

(defun gt-stringp (val1 val2)
  (string-lessp val2 val1))


(defun lep (&rest vals &aux val1 val2 dom1 dom2)
  (setf vals (do ((val-l vals (car val-l)))
 ((or (not (equal (length val-l) 1)) (not(listp (car val-l)))) val-l) ()))
  ;; only one value, cannot compare
  (cond ((equal (length vals) 1)
 (if *provide-error-messages*
     (format *standard-output*  "~%ERROR - Only one value provided, need at least two to compare."))
 nil)
(t
 (do ((vals-left vals (cdr vals-left)))
     ((equal (length vals-left) 1) (return t))
   (setf val1 (first vals-left)
 val2 (second vals-left)
 dom1 (value-domainp val1)
 dom2 (value-domainp val2))
   ;; if value-domainp returns anyp, it actually means that the domain is not supported
   (cond ((or (string-equal dom1 "ANYP")(string-equal dom2 "ANYP"))
  (return nil)))
   (cond ((string-equal dom1 dom2)
  (if (not(funcall (find-symbol (concatenate 'string "LE-" dom1) *pkg-string*) val1 val2))
      (return nil)))
 ;; any list is > to anything
 ((string-equal dom1 "LISTP")
  (return nil))
 ((string-equal dom2 "LISTP")
  ())
 ((string-equal dom1 "NUMBERP")
  ())
 ((string-equal dom2 "NUMBERP")
  (return nil))
 ((or (and (string-equal dom1 "ATOM")(string-equal dom2 "STRINGP"))
      (and (string-equal dom2 "ATOM")(string-equal dom1 "STRINGP")))
  (if (not (le-stringp val1 val2))
      (return nil)))
 (t
  (return nil)))))))


(defun le-anyp (val1 val2)
  (lep (list val1 val2)))

(defun le-atom (val1 val2)
   (cond ((and (numberp val1) (numberp val2))
  (or (< val1 val2) (equal val1 val2)))
 ((numberp val1)
  t)
 ((numberp val2)
  nil)
 (t
  (setf val1 (string val1)
val2 (string val2))
  (or (string-lessp val1 val2)(string-equal val1 val2)))))

(defun le-listp (list1 list2 &aux val1 val2 (is-le nil))
  (do ((v1 list1 (cdr v1))(v2 list2 (cdr v2)))
      ((or is-le (and (null v1)(null v2))) (return t))
    (setf val1 (car v1)
  val2 (car v2))
    (cond ((null val1)
   (return t))
  ((null val2)
   (return nil)))
    ;; this is better than calling LEP because as soon as we find one val which is LT (and not equal) we stop the tests
    (cond ((ltp val1 val2)
   (setf is-le t))
  ((not (*equalp val1 val2))
   (return nil)))))

(defun le-numberp (val1 val2)
  (or (< val1 val2) (equal val1 val2)))

(defun le-stringp (val1 val2)
  (or (string-lessp val1 val2)(string-equal val1 val2)))

(defun ltp (&rest vals &aux val1 val2 dom1 dom2)
  (setf vals (do ((val-l vals (car val-l)))
 ((or (not (equal (length val-l) 1)) (not(listp (car val-l)))) val-l) ()))
  ;; only one value, cannot compare
  (cond ((equal (length vals) 1)
 (if *provide-error-messages*
     (format *standard-output* "~%ERROR - Only one value provided, need at least two to compare."))
 nil)
(t
 (do ((vals-left vals (cdr vals-left)))
     ((equal (length vals-left) 1) (return t))
   (setf val1 (first vals-left)
 val2 (second vals-left)
 dom1 (value-domainp val1)
 dom2 (value-domainp val2))
   ;; if value-domainp returns anyp, it actually means that the domain is not supported
   (cond ((or (string-equal dom1 "ANYP")(string-equal dom2 "ANYP"))
  (return nil)))
   (cond ((string-equal dom1 dom2)
  (if (not(funcall (find-symbol (concatenate 'string "LT-" dom1) *pkg-string*) val1 val2))
      (return nil)))
 ;; any list is > to anything
 ((string-equal dom1 "LISTP")
  (return nil))
 ((string-equal dom2 "LISTP")
  ())
 ;; any number is < to anything
 ((string-equal dom1 "NUMBERP")
  ())
 ((string-equal dom2 "NUMBERP")
  (return nil))
 ((or (and (string-equal dom1 "ATOM")(string-equal dom2 "STRINGP"))
      (and (string-equal dom2 "ATOM")(string-equal dom1 "STRINGP")))
  (if (not (lt-stringp val1 val2))
      (return nil)))
 (t
  (return nil)))))))

(defun lt-anyp (val1 val2)
  (ltp (list val1 val2)))

(defun lt-atom (val1 val2)
   (cond ((and (numberp val1) (numberp val2))
  (< val1 val2))
 ((numberp val1)
  t)
 ((numberp val2)
  nil)
 (t
  (string-lessp (string val1) (string val2)))))

(defun lt-listp (list1 list2 &aux val1 val2 (is-lt nil))
  (do ((v1 list1 (cdr v1))(v2 list2 (cdr v2)))
      ((or is-lt (and (null v1)(null v2))) (if is-lt (return t)(return nil)))
    (setf val1 (car v1)
  val2 (car v2))
    (cond ((null val1)
   (return t))
  ((null val2)
   (return nil)))
    (cond ((ltp val1 val2)
   (setf is-lt t))
  ((not (*equalp val1 val2))
   (return nil)))))

(defun lt-numberp (val1 val2)
  (< val1 val2))

(defun lt-stringp (val1 val2)
  (string-lessp val1 val2))


(defun notp (&rest vals)
  (setf vals  (do ((val-l vals (car val-l)))
  ((not (equal (length val-l) 1)) val-l) ()))
  (do ((vals-left vals (cdr vals-left)))
      ((null vals-left) (return t))
    (if (not (first vals-left))
(return nil))))


(defun aggregate-utility (relation-name attribute-names keyword-list
  &aux relation-tuple rel-tuple-list where-list unique index-name attribute-list
  domains-list key-list rel-imp rel-sto by card key-value-list)
  (block aggregate-utility
(if (not (active-database))
    (return-from aggregate-utility nil))
(cond ((null (setf relation-name (validate-sym relation-name)))
       (return-from aggregate-utility nil)))
(if (not (listp attribute-names))
    (setf attribute-names (list attribute-names)))
(setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
       ((or (null keyword-list)(not (listp (car keyword-list)))) keyword-list)))
(setf keyword-list (get-keyword-value-prereq  '(where unique by tuples) keyword-list)
      where-list (or (car (get-keyword-value '(where) keyword-list)) t)
      unique (car (get-keyword-value '(unique) keyword-list))
      by (car (get-keyword-value '(by) keyword-list)))
(if (listp by)
    (setf by (car by)))
(if by
    (setf attribute-names (cons by attribute-names)))
(setf relation-tuple (cadr (get-relation relation-name '("ATTRIBUTES" "DOMAINS" "KEY"
      "IMPLEMENTATION-TYPE" "STORAGE-STRUCTURE"
      "CARDINALITY") nil)))
(cond ((not relation-tuple)
       (if *provide-error-messages*
   (format *standard-output* "~%ERROR - The relation ~S is not defined in the database ~S"
   (read-from-string (string-upcase relation-name)) *active-db*))
       (return-from aggregate-utility nil)))
(setf attribute-list (first relation-tuple)
      domains-list (second relation-tuple)
      key-list (third relation-tuple)
      rel-imp (fourth relation-tuple)
      rel-sto (fifth relation-tuple)
      card (sixth relation-tuple))
(cond  ((or (equalp attribute-names '(nil)) (not attribute-names))
       (setf attribute-names attribute-list))
      (t
       (setf attribute-names (convert-attributes attribute-names))
       ;;
       ;;    verify that the attribute to rename exists, and check if its domain is numberp
       (mapc #'(lambda (attr)
 (cond ((null (member attr attribute-list :test 'string-equal))
         (if *provide-error-messages*
     (format *standard-output*
      "~%ERROR - The attribute ~S is not defined in ~S."
      attr (read-from-string (string-upcase relation-name))))
 (return-from aggregate-utility nil))))
     attribute-names)))
(multiple-value-setq (index-name key-value-list rel-sto key-list)
  (extract-key relation-name attribute-list key-list domains-list rel-sto where-list nil))
(setf rel-tuple-list (funcall (find-symbol (concatenate 'string "RETRIEVE-" rel-imp "-" rel-sto)
      *pkg-string*)
       relation-name attribute-list attribute-names key-list where-list key-value-list
       index-name))
(cond ((and rel-tuple-list unique)
       (setf rel-tuple-list (unique-tuples rel-tuple-list))))
(if rel-tuple-list
    (return-from aggregate-utility (list card rel-tuple-list))
    (return-from aggregate-utility nil))))

(defun average (relation-name attribute-name &rest keyword-list
&key &optional unique where by tuples &allow-other-keys
&aux  rel-tuple-list (result 0)(size 0) tuples? agg-tuples by? card)
  "Average of the values of a given attribute in a relation satisfying a where clause.

   RELATION-NAME  - Name of the relation.
   ATTRIBUTE-NAME - Name of the attribute whose average is to be found.
   UNIQUE         - If T, only unique values will be used.
   WHERE          - If a selection criterion is provided, only the satisfying values will be used.
   BY             - Name of the attribute to group average of the above attribute by.
   TUPLES         - If T, the resultant values will be returned rather than printed out as a table."
  unique where by tuples
  (block average
(if (not (setf rel-tuple-list (aggregate-utility relation-name attribute-name keyword-list)))
    (return-from average nil))
(setf card (car rel-tuple-list)
      rel-tuple-list (second rel-tuple-list))
(setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
       ((or (null keyword-list)(not (listp (car keyword-list)))) keyword-list)))
(setf tuples? (car (get-keyword-value '(tuples) (setf keyword-list (get-keyword-value-prereq
           '(tuples where by unique)
           keyword-list)))))
(cond ((setf by? (car (get-keyword-value '(by) keyword-list)))
       (if (listp by?)
   (setf by? (car by?)))
       (setf by? (string-upcase by?))
       (mapc #'(lambda (tuple &aux agg-tuple)
 (cond ((not (numberp (cadr tuple)))
 (if *provide-error-messages*
     (format *standard-output*
      "~%ERROR -- Can not find the average for ~S in the relation ~S because ~S is not a number."
      attribute-name (read-from-string (string-upcase relation-name))
      (car tuple)))
 (return-from average nil)))
 (if (setf agg-tuple (assoc (car tuple) agg-tuples :test 'equal))
     (setf (first (cadr agg-tuple)) (+ (first (cadr agg-tuple)) (cadr tuple))
    (second (cadr agg-tuple)) (incf (second (cadr agg-tuple))))
     (setf agg-tuples (cons (list (car tuple) (list (cadr tuple) 1)) agg-tuples))))
     rel-tuple-list)
       (if (not agg-tuples) (return-from average 0))
       (setf rel-tuple-list
     (mapcar #'(lambda (tuple)
  (list (car tuple) (/ (first (cadr tuple)) (float (second (cadr tuple))))))
     agg-tuples))
       (if tuples?
   (return-from average rel-tuple-list)
   (return-from average (printrel-internal* relation-name rel-tuple-list
         (list by? (read-from-string
       (concatenate 'string
         "AVERAGE-OF-" (string-upcase attribute-name))))
         nil nil nil nil '(20 30) t t card)))))
(mapc (function (lambda (tuple)
  (if (not (numberp (car tuple)))
      (progn
 (if *provide-error-messages*
     (format *standard-output*
      "~%ERROR -- Can not find the average for ~S in the relation ~S because ~S is not a number."
      attribute-name relation-name (car tuple)))
 (return-from average nil)))
  (setf result (+ result (car tuple)))
  (setf size (1+ size))))
      rel-tuple-list)
(if (not (equal size 0))
    (if tuples?
(return-from average (/ result (float size)))
(return-from average (printrel-internal* relation-name
      (list (list (/ result (float size))))
      (list (read-from-string
       (concatenate 'string "AVERAGE-OF-"
        (string-upcase attribute-name))))
      nil nil nil nil '(30) t t card)))
    (return-from average 0))))

(defun maximum (relation-name attribute-name &rest keyword-list
&key &optional where by tuples &allow-other-keys
&aux  rel-tuple-list tuples? agg-tuples by? card)
  "Maximum of the values of a given attribute in a relation satisfying a where clause.

   RELATION-NAME  - Name of the relation.
   ATTRIBUTE-NAME - Name of the attribute whose maximum is to be found.
   WHERE          - If a selection criterion is provided, only the satisfying values will be used.
   BY             - Name of the attribute to group maximum of the above attribute by.
   TUPLES         - If T, the resultant values will be returned rather than printed out as a table."
  where by tuples
  (block maximum
(if (not (setf rel-tuple-list (aggregate-utility relation-name attribute-name keyword-list)))
    (return-from maximum nil))
(setf card (car rel-tuple-list)
      rel-tuple-list (second rel-tuple-list))
(setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
       ((or (null keyword-list)(not (listp (car keyword-list)))) keyword-list)))
(setf tuples? (car (get-keyword-value '(tuples) (setf keyword-list (get-keyword-value-prereq
           '(tuples where by)
           keyword-list)))))
(if (setf by? (car (get-keyword-value '(by) keyword-list)))
    (progn
      (if (listp by?)
  (setf by? (car by?)))
      (setf by? (string-upcase by?))
      (mapc #'(lambda (tuple &aux agg-tuple)
(if (setf agg-tuple (assoc (car tuple) agg-tuples :test 'equal))
    (if (gtp (cadr tuple) (cadr agg-tuple))
 (setf (cadr agg-tuple) (cadr tuple)))
    (setf agg-tuples (cons (list (car tuple) (cadr tuple)) agg-tuples))))
    rel-tuple-list)
      (if (not agg-tuples)
  (return-from maximum nil))
      (if tuples?
  (return-from maximum agg-tuples)
  (return-from maximum (printrel-internal* relation-name agg-tuples
        (list by? (read-from-string
      (concatenate 'string "MAXIMUM-OF-"
            (string-upcase attribute-name))))
        nil nil nil nil '(20 30) t t card))))
    (setf agg-tuples rel-tuple-list))
(if tuples?
     (return-from maximum (maximum-internal (mapcar #'(lambda (tuple)
     (car tuple))
        agg-tuples)))
    (return-from maximum (printrel-internal*
      relation-name (list (list (maximum-internal (mapcar #'(lambda (tuple)
            (car tuple))
        agg-tuples))))
      (list (read-from-string (concatenate 'string "MAXIMUM-OF-" (string-upcase attribute-name))))
      nil nil nil nil '(30) t t card)))
(return-from maximum nil)))

(defun maximum-internal (values &aux temp-max)
  (setf temp-max (car values))
  (mapc (function (lambda (value)
    (if (gtp value temp-max)
(setf temp-max value))))
values)
  temp-max)

(defun minimum (relation-name attribute-name &rest keyword-list
&key &optional where by tuples &allow-other-keys
&aux  rel-tuple-list tuples? agg-tuples by? card)
  "Minimum of the values of a given attribute in a relation satisfying a where clause.

   RELATION-NAME  - Name of the relation.
   ATTRIBUTE-NAME - Name of the attribute whose minimum is to be found.
   WHERE          - If a selection criterion is provided, only the satisfying values will be used.
   BY             - Name of the attribute to group minimum of the above attribute by.
   TUPLES         - If T, the resultant values will be returned rather than printed out as a table."
  where by tuples
  (block minimum
    (if (not (setf rel-tuple-list (aggregate-utility relation-name attribute-name keyword-list)))
      (return-from minimum nil))
    (setf card (car rel-tuple-list)
  rel-tuple-list (second rel-tuple-list))
    (setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
   ((or (null keyword-list)(not (listp (car keyword-list)))) keyword-list)))
  (setf tuples? (car (get-keyword-value '(tuples) (setf keyword-list (get-keyword-value-prereq
            '(tuples where by) keyword-list)))))
  (if (setf by? (car (get-keyword-value '(by) keyword-list)))
      (progn
(if (listp by?)
    (setf by? (car by?)))
(setf by? (string-upcase by?))
(mapc #'(lambda (tuple &aux agg-tuple)
(if (setf agg-tuple (assoc (car tuple) agg-tuples :test 'equal))
    (if (ltp  (cadr tuple) (cadr agg-tuple))
(setf (cadr agg-tuple) (cadr tuple)))
    (setf agg-tuples (cons (list (car tuple) (cadr tuple)) agg-tuples))))
    rel-tuple-list)
       (if (not agg-tuples) (return-from minimum nil))
       (if tuples?
  (return-from minimum agg-tuples)
(return-from minimum (printrel-internal* relation-name agg-tuples
     (list by? (read-from-string
   (concatenate 'string "MINIMUM-OF-"
            (string-upcase attribute-name))))
     nil nil nil nil '(20 30) t t card))))
      (setf agg-tuples rel-tuple-list))
  (if tuples?
  (return-from minimum (minimum-internal (mapcar #'(lambda (tuple)
   (car tuple))
      agg-tuples)))
(return-from minimum (printrel-internal*
  relation-name (list (list (minimum-internal (mapcar #'(lambda (tuple)
        (car tuple))
           agg-tuples))))
  (list (read-from-string (concatenate 'string "MINIMUM-OF-" (string-upcase attribute-name))))
  nil nil nil nil '(30) t t card)))
    (return-from minimum nil)))

(defun minimum-internal (values &aux temp-min)
  (setf temp-min (car values))
  (mapc (function (lambda (value)
    (if (ltp value temp-min)
(setf temp-min value))))
values)
  temp-min)

(defun relationp (relation)
  (caar (qtrieve 'system-relation *system-relation-attributes* '("RELATION-NAME") *system-relation-key*
 (list 'string-equal 'relation-name (string-upcase relation)))))

(defun size (relation-name &rest keyword-list
     &key &optional unique where &allow-other-keys
     &aux rel-tuple-list)
  "Number of tuples in a relation satisfying a where clause.

   RELATION-NAME  - Name of the relation whose size is to be found.
   UNIQUE         - If T, only unique values will be used.
   WHERE          - If a selection criterion is provided, only the satisfying tuples will be used."
  unique where
  (if (not (setf rel-tuple-list (aggregate-utility (read-from-string (string-upcase relation-name))
      nil keyword-list)))
      nil
      (length (second rel-tuple-list))))


(defun between (value lower-bound upper-bound)
  "Checks if a given value is between two bounds.

   VALUE - Any value.
   LOWER-BOUND - Lower bound value.
   UPPER-BOUND - Upper bound value."
  (or (and (gep value lower-bound) (lep value upper-bound))
      (and (gep value upper-bound) (lep value lower-bound))))


(defun sum (relation-name attribute-name &rest keyword-list
&key &optional unique where by tuples &allow-other-keys
&aux  rel-tuple-list (result 0) tuples? agg-tuples by? card)
  "Sum of the values of a given attribute in a relation satisfying a where clause.

   RELATION-NAME  - Name of the relation.
   ATTRIBUTE-NAME - Name of the attribute whose sum is to be found.
   UNIQUE         - If T, only unique values will be used.
   WHERE          - If a selection criterion is provided, only the satisfying values will be used.
   BY             - Name of the attribute to group sum of the above attribute by.
   TUPLES         - If T, the resultant values will be returned rather than printed out as a table."
  unique where by tuples
  (block sum
    (if (not (setf rel-tuple-list (aggregate-utility relation-name attribute-name keyword-list)))
      (return-from sum nil))
    (setf card (car rel-tuple-list)
  rel-tuple-list (second rel-tuple-list))
    (setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
   ((or (null keyword-list)(not (listp (car keyword-list)))) keyword-list)))
  (setf tuples? (car (get-keyword-value '(tuples) (setf keyword-list (get-keyword-value-prereq
            '(tuples where by unique)
            keyword-list)))))
  (if (setf by? (car (get-keyword-value '(by) keyword-list)))
      (progn
(if (listp by?)
    (setf by? (car by?)))
(setf by? (string-upcase by?))
       (mapc #'(lambda (tuple &aux agg-tuple)
(if (not (numberp (cadr tuple)))
  (progn
    (if *provide-error-messages*
 (format *standard-output*
  "~%ERROR -- Can not find the sum for ~S in the relation ~S because ~S is not a number."
  attribute-name relation-name (car tuple)))
    (return-from sum nil)))
(if (setf agg-tuple (assoc (car tuple) agg-tuples :test 'equal))
    (setf (cadr agg-tuple) (+ (cadr agg-tuple) (cadr tuple)))
    (setf agg-tuples (cons tuple agg-tuples))))
    rel-tuple-list)
       (if (not agg-tuples) (return-from sum 0))
       (if tuples?
  (return-from sum agg-tuples)
(return-from sum (printrel-internal* relation-name agg-tuples
     (list by? (read-from-string (concatenate 'string
       "SUM-OF-" (string-upcase  attribute-name))))
     nil nil nil nil '(20 30) t t card))))
      (setf agg-tuples rel-tuple-list))
  (mapc (function (lambda (tuple)
      (if (not (numberp (car tuple)))
  (progn
    (if *provide-error-messages*
 (format *standard-output*
  "~%ERROR -- Can not find the sum for ~S in the relation ~S because ~S is not a number."
  attribute-name relation-name (car tuple)))
    (return-from sum nil)))
      (setf result (+ result (car tuple)))))
  agg-tuples)
  (if tuples?
      (return-from sum result)
    (return-from sum (printrel-internal* relation-name (list (list result))
     (list (read-from-string (concatenate 'string
          "SUM-OF-" (string-upcase attribute-name))))
     nil nil nil nil '(30) t t card)))))

(deff rtms-count 'count-rtms)
(defun count-rtms (relation-name attribute-name &rest keyword-list
&key &optional unique where by tuples &allow-other-keys
&aux  rel-tuple-list (result 0) tuples? agg-tuples by? card)
  "Number of the values of a given attribute in a relation satisfying a where clause.
   RELATION-NAME  - Name of the relation.
   ATTRIBUTE-NAME - Name of the attribute whose count is to be found.
   UNIQUE         - If T, only unique values will be used.
   WHERE          - If a selection criterion is provided, only the satisfying values will be used.
   BY             - Name of the attribute to group count of the above attribute by.
   TUPLES         - If T, the resultant values will be returned rather than printed out as a table."
  unique where by tuples
  (block count-rtms
    (if (not (setf rel-tuple-list (aggregate-utility relation-name attribute-name keyword-list)))
      (return-from count-rtms nil))
    (setf card (car rel-tuple-list)
  rel-tuple-list (second rel-tuple-list))
    (setf keyword-list (do ((keyword-list keyword-list (car keyword-list)))
   ((or (null keyword-list)(not (listp (car keyword-list)))) keyword-list)))
  (setf tuples? (car (get-keyword-value '(tuples) (setf keyword-list (get-keyword-value-prereq
            '(tuples where by unique)
            keyword-list)))))
  (if (setf by? (car (get-keyword-value '(by) keyword-list)))
      (progn
(if (listp by?)
    (setf by? (car by?)))
(setf by? (string-upcase by?))
       (mapc #'(lambda (tuple &aux agg-tuple)
 (if (setf agg-tuple (assoc (car tuple) agg-tuples :test 'equal))
     (setf (cadr agg-tuple) (incf (cadr agg-tuple)))
     (setf agg-tuples (cons (list (car tuple) 1) agg-tuples))))
     rel-tuple-list)
       (if (not agg-tuples)
   (return-from count-rtms 0))
       (if tuples?
  (return-from count-rtms agg-tuples)
(return-from count-rtms (printrel-internal* relation-name agg-tuples
     (list by? (read-from-string (concatenate 'string
       "COUNT-OF-" (string-upcase attribute-name))))
     nil nil nil nil '(20 30) t t card)))))
;
  (setf result (length rel-tuple-list))
  (if tuples?
      (return-from count-rtms result)
    (return-from count-rtms (printrel-internal* relation-name (list (list result))
     (list (read-from-string (concatenate 'string
        "COUNT-OF-" (string-upcase attribute-name))))
     nil nil nil nil '(30) t t card)))))


;;; fragments

ces)
  (modify-hash relation where rel-key rel-attributes attributes values "LIST" dom-def indices))

(defun modify-hash (relation where-clause key attribute-list attributes values imp dom-def indices
    &aux (bucket nil) (sec-keys nil) (num-modified 0) (result nil) hash-tuples
      num-bucket-modified (table (getp relation 'entry-point)

       (if (not agg-tuples) (return-from average 0))
       (setf rel-tuple-list
     (mapcar #'(lambda (tuple)
  (list (car tuple) (/ (first (cadr tuple)) (float (second (cadr tuple))))))
     agg-tuples))
       (if tuples?
   (return-from average rel-tuple-list)
   (return-from average (printrel-internal* relation-name rel-tuple-list
         (list by? (read-f
