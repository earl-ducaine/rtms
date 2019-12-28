
;;; -*- Mode:Common-Lisp; Package:RTMS; Fonts:(*CODE-FONT* *COMMENT-FONT* *STRING-FONT*); Base:10 -*-
;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved
'(declare (setq obsolete-function-warning-switch nil))
(defvar *active-db* nil)
(defvar *attach-detach-data* nil)
(defvar *auto-save-relations* nil)
(defvar *auto-save* nil)
(setq *auto-save* nil)
(defvar *ban-unknowns* nil)
(defvar *bf* 1)
(defvar *command-submenu* nil)
(defvar *completion-checks* nil)
(defvar *completion-help* nil)
(defvar *database-documentation* "")
(defvar *db-prinlength* 2)
(defvar *db-prinlevel* 2)
(defvar *db-saveable* t)
(defvar *dbms-commands* '(defdb defrel restoredb restorerel defview
  delete insert modify destroydb destroyrel
  select project retrieve join set-intersection
  set-union set-difference
  mapon-alltuples printrel savedb saverel))
(defvar *default-anyp-width* nil)
(defvar *default-anyp-value* "?")
(defvar *default-atom-width* nil)
(defvar *default-atom-value* "?")
(defvar *default-keyword-value* nil)
(defvar *default-listp-width* 20)
(defvar *default-listp-value* '(quote (?)))
(defvar *default-numberp-width* 8)
(defvar *default-numberp-value* 0)
;
;  Temporary solution until the values get changed on the band
;
(setq *default-anyp-width* 10)
(setq *default-atom-width* 10)
(setq *default-listp-width* 20)
(setq *default-numberp-width* 8)
(setq *default-stringp-width* 20)
(defvar *default-pkg* "user")
(defvar *default-stringp-width* 20)
(defvar *default-stringp-value* "")
(defvar *default-t-width* nil)
(defvar *display-submenu* nil)
(defvar *doc* nil)
(defvar *domain-list* '("anyp" "atom" "listp" "numberp" "stringp"))
(setq *domain-list* '(anyp atom listp numberp stringp))
(defvar *donot-commit* nil)
(defvar *environment-name* nil)
(defvar *equal-inserted* nil)
(defvar *help-submenu* nil)
(defvar *interaction* nil)
(defvar *long-string* '------------------------------------------------------------------------------------------------------------------------------------------)
(defvar *memory-management* t)
(defvar *menupane* nil)
(defvar *node-inserted* nil)
(defvar *non-qfasl-restore* nil)
(defvar *other-width* 12)
(defvar *output-window* nil)
(defvar *parameter-checking* t)
(defvar *password* nil)
(defvar *pkg-name* "RTMS:")
(defvar *pkg-string* "RTMS")
(defvar *print* t)
(defvar *print-style* nil)
(defvar *project-forms* t)
(defvar *provide-error-messages* t)
(defvar *provide-status-messages* t)
(defvar *provide-warning-messages* t)
(defvar *query-optimization* t)
(defvar *recover-attempted* nil)
(defvar *relation-implementation* "LIST-HEAP")
(setq *relation-implementation* "LIST-HEAP")
(defvar *rel-imp* "LIST")
(defvar *rel-sto* "HEAP")
(defvar *restore-operation* nil)
(defvar *restore/define-database-auto-save* t)
(defvar *save-directory* nil)
(defvar *save-user-id* nil)
(defvar *suppress-warning-messages* nil)
(defvar *suppress-error-messages* nil)
(defvar *system-attribute-attributes* nil)
(defvar *system-attribute-key* nil)
(defvar *system-attribute-key-domains* nil)
(defvar *system-domain-attributes* nil)
(defvar *system-domain-key-domains* nil)
(defvar *system-domain-key* nil)
(defvar *system-implementation-attributes* nil)
(defvar *system-implementation-key* nil)
(defvar *system-implementation-key-domains* nil)
(defvar *system-index-attributes* nil)
(defvar *system-index-key-domains* nil)
(defvar *system-index-key* nil)
(defvar *system-optfunc-attributes* nil)
(defvar *system-optfunc-key-domains* nil)
(defvar *system-optfunc-key* nil)
(defvar *system-relation-attributes* nil)
(defvar *system-relation-key-domains* nil)
(defvar *system-relation-base-implementation* "LIST")
(defvar *system-relation-key* nil)
(defvar *system-relation-storage-structure* "HEAP")
(setq *system-relation-storage-structure* "HEAP")
(defvar *system-relations* nil)
(defvar *system-storage-structure-attributes* nil)
(defvar *system-storage-structure-key-domains* nil)
(defvar *system-storage-structure-key* nil)
(defvar *system-view-attributes* nil)
(defvar *system-view-key-domains* nil)
(defvar *system-view-key* nil)
(defvar *system-whereopt-attributes* nil)
(defvar *system-whereopt-key-domains* nil)
(defvar *system-whereopt-key* nil)
(defvar *temp-reader-package* "RTMS")
(defvar *transaction-forms-postponed* nil)
(defvar *transaction-on* nil)
(defvar *temporary* nil)
(defvar *typeout-window* nil)
(defvar *validity-checking* t)
(defvar *where-opt* '())
(defvar *where-opt-macros* '())
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;       Interface global variables
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defvar dbms-comtab nil)
(defvar dbms-frame1 nil)
(defvar *line-area-values-modifiedp* nil)
(defvar *ucl-count-attr* '(:label "Attribute name:"
   :default nil
   :type (:documentation
    "Name of the attribute in the above relation." :sexp)))
(defvar *ucl-count-unique* '(:label "Unique?"
     :default nil
     :type (:documentation
      "If true, only the unique values of the attribute will be used in the calculations." :boolean)))
(defvar *ucl-by* '(:label "By"
  :default nil
  :type (:documentation
   "Specify the attribute to be used in grouping the data into categories." :sexp)))
(defvar *ucl-attributes* '(:label "Attributes: "
   :default nil
   :type (:documentation
    "Provide a list of attributes. If not all attributes all used." :sexp)))
(defvar *ucl-where* '(:label "Where clause: "
     :default *ui-where*
     :type (:documentation
      "Provide a selection criteria."
      :sexp)))
(defvar *ucl-into* '(:label "INTO :"
    :default *ui-into*
    :type (:documentation
     "Specify the relation to insert the resultant tuples into. If none specified, they are just printed out." :sexp)))
(defvar *ucl-dir* '(:label "Directory:"
   :default *ui-directory*
   :type (:documentation
    "Specify the save directory for the relation."
    :sexp)))
(defvar *ucl-doco* '(:label "Documentation:"
   :default *ui-doc*
   :type (:documentation
    "Specify the documentation for the output relation."
    :string)))
(defvar *ucl-doci* '(:label "Documentation:"
   :default ".."
   :type (:documentation
    "Specify the documentation for this relation."
    :string)))
(defvar *ucl-key* '(:label "Key:"
   :default *ui-key*
   :type (:documentation
    "Specify the key as a list of attributes."
    :sexp)))
(defvar *ucl-imp* '(:label "Implementation Type:"
   :default *ui-imp*
   :type (:documentation
    "Specify the implementation type."
    :sexp)))
(defvar *ucl-sto* '(:label "Storage Structure:"
   :default *ui-ss*
   :type (:documentation
    "Specify the storage structure type."
    :sexp)))
(defvar *ucl-qprint* '(:label "Formatted Output: "
     :default t
     :type (:documentation
      "Should the tuples returned be formatted?. If no, tuples are printed in the interaction pane." :boolean)))
(defvar *ucl-out* '(:label "Output File:"
   :default *ui-file*
   :type (:documentation
    "If the output is to be sent to a file, specify a pathname." :sexp)))
(defvar *ucl-sort* '(:label "Sort: "
    :default *ui-sort*
    :type (:documentation
     "Should the output be sorted? Legal values are: (<attribute-name order>) - order could be ASC or DES." :sexp)))
(defvar *ucl-format* '(:label "Tuple Format :"
      :default *ui-format*
      :type (:documentation
       "Specify the tuple format as a list of numbers representing the column width for each attribute. If not specified, the default format for this relation is used." :sexp)))
(defvar *ucl-wide* '(:label "Wide-Format :"
    :default *ui-wide*
    :type (:documentation
     "Should the tuples be printed in wide format instead of tabular format? - Wide format will be of the type <attribute: value>." :boolean)))
(defvar *ucl-num* '(:label "Number of attributes per line:"
   :default *ui-num*
   :type (:documentation
    "How many attributes per line if the tuples are printed using wide format?. Default is -1 indicating as many tuples per line as possible." :number)))
(defvar *ucl-print* '(:label "Print?:"
     :default t
     :type (:documentation
      "Should the results be printed or not?" :boolean)))
(defvar *ucl-tuples* '(:label "Tuples:"
      :default nil
      :type (:documentation
       "Should the results be returned as a list of tuples?" :boolean)))
(defvar *ucl-quick-sort* '(:label "Quick Sort:"
   :default nil
   :type (:documentation
    "Similar to sort except that it does not take user defined domains into consideration." :sexp)))
(defvar *ucl-stream* '(:label "Stream:"
      :default nil
      :type (:documentation
       "If the output is to be sent to a stream other than the output window, specify the stream name." :sexp)))
(defvar *ucl-unique* '(:label "Unique?:"
      :default nil
      :type (:documentation
     "If only unique tuples are desired, then this must be true." :boolean)))
(defvar *ucl-index-name* '(:label "Index-name:"
      :default nil
      :type (:documentation
     "If the data is to come from an index instead of the base relation." :sexp)))
(defvar *ucl-attr-desc* '(:label "Attribute descriptor pair:"
  :default *ui-attr-desc*
  :type (:documentation
   "List of attributes and their domains default, and documentation. EX. (a1 (dom <something> def <something>) a2) . If any values are not given there is a default for everything. So, the minimum necessary input is a list of attributes."
   :sexp)))
(defvar *ucl-pathname* '(:label "Pathname:"
 :default *ui-file*
 :type (:documentation
  "Specify the name of the input file."
  :sexp)))

(defvar *ucl-retrieve-rel* '(:label "Relation: "
     :default *ui-relation*
     :type (:documentation
      "Specify a relation whose tuples are to be retrieved."
	    :sexp)))
;;; fragments





"Attribute descriptor pair:"
  :default *ui-attr-desc*
  :type (:documentation
   "List of attributes and their domains default, and documentation. EX. (a1 (dom <something> def <something>) a2) . If any values are not given there is a default for everything. So, the minimum necessary input is a list of attributes."
   :sexp)))
(defvar *ucl-pathname* '(:label "Pathname:"
 :default *ui-file*
 :type (:documentation
  "Specify the name of the input file."
  :sexp)))
(defvar *ucl-retrieve-rel* '(:label "Relation: "
     :default *ui-relation*
     :type (:documentation
      "Specify a relation whose tuples are to be retrieved."
      :sexp)))
