
;;; -*- Mode:Common-Lisp; Package:RTMS; Fonts:(WIDER-MEDFNT MEDFNB MEDFNB HL7); Base:10 -*-
;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved
;**************************************************************************
;                             USER INTERFACE *
;      *
;      *
; 1. Issues to be considered later.          *
;   a) Output-window  ----> a specified file.                                      *
;   b) Parts of output (ex. a relation) to a ZMACS window.                         *
;   c) Use line area scrolling for interactive maintenance of database.            *
;      *
;      *
;      *
;      *
;  AUTHOR                         *
; CSL                  *
; Texas Instruments                 *
; .....                  *
; Version 0.0                 *
;**************************************************************************
;;;Change History
;;;  03.31.87  MRR  Changed DBMS-RC defflavor to make scroll-bar always appear.
;;;                 Changed references to XFASL files to XLD for Save-relation command.
;;;  04.01.87  MRR  Changed DBMS-RC defflavor to prevent pixel overlap of scroll-bar.
;;;  04.06.87  MRR  Fixed HELP-LINE-AREA-DEL to delete tuples using the display.
;;;                 Fixed mouse documentation strings for various windows.
;;;                 Fixed method (DBMS-RC :handle-unknown-input) to call Relation help functions
;;;                 correctly.
;;;  04.07.87  MRR  Fixed HELP-LINE-AREA. Made references to w:*remove-typeout-standard-message*
;;;                 for typeout windows. Fixed HELP-LINE-AREA-MOD for the case when the current
;;;                 package is not RTMS. (SPR #4197)
;;;  04.09.87  MRR  Added :sensitive-item-types initialization option to DBMS-RC defflavor so
;;;                 that only valid types are made mouse-sensitive. (see SPR #1858)
;;;                 Fixed command for sending display output to file.

;**************************************************************************
;                          INTERFACE GLOBAL VARIABLES                           *
;     These global variables are used to hold the latest user-values for the       *
;     variables in the choose-variables windows associated with various commands.  *
;**************************************************************************
(PUTPROP 'display nil 'ucl:items)
(PUTPROP 'display nil 'ucl:commands-wanting-on)
(PUTPROP 'command-menu nil 'ucl:items)
(PUTPROP 'command-menu nil 'ucl:commands-wanting-on)
(PUTPROP 'system-menu nil 'ucl:items)
(PUTPROP 'system-menu nil 'ucl:commands-wanting-on)
;;
(SETQ rtms:*default-pkg* *PACKAGE*)
(PKG-GOTO *pkg-string*)
(UCL:MAKE-SYNONYM '*ui-relation* nil)
(UCL:MAKE-SYNONYM '*ui-tuples* nil)
(UCL:MAKE-SYNONYM '*ui-transaction* nil)
(UCL:MAKE-SYNONYM '*ui-function* nil)
(UCL:MAKE-SYNONYM '*ui-attributes* nil)
(UCL:MAKE-SYNONYM '*ui-format* nil)
(UCL:MAKE-SYNONYM '*ui-file* nil)
(UCL:MAKE-SYNONYM '*ui-database* *active-db*)
(UCL:MAKE-SYNONYM '*ui-directory* (STRING-APPEND "SYS:" user-id ";"))  ;mrr 03.31.87
(UCL:MAKE-SYNONYM '*ui-type* 'xld)     ;mrr 03.31.87
(UCL:MAKE-SYNONYM '*ui-attr-desc* nil)
(UCL:MAKE-SYNONYM '*ui-doc* ".....")
(UCL:MAKE-SYNONYM '*ui-key* nil)
(UCL:MAKE-SYNONYM '*ui-imp* *system-relation-base-implementation*)
(UCL:MAKE-SYNONYM '*ui-ss* *system-relation-storage-structure*)
(UCL:MAKE-SYNONYM '*ui-viewdef* nil)
(UCL:MAKE-SYNONYM '*ui-where* T)
(UCL:MAKE-SYNONYM '*ui-values* nil)
(UCL:MAKE-SYNONYM '*ui-join-into* nil)
(UCL:MAKE-SYNONYM '*ui-over* T)
(UCL:MAKE-SYNONYM '*ui-into* nil)
(UCL:MAKE-SYNONYM '*ui-from* nil)
(UCL:MAKE-SYNONYM '*ui-wide* nil)
(UCL:MAKE-SYNONYM '*ui-num* -1)
(UCL:MAKE-SYNONYM '*ui-sort* nil)
(UCL:MAKE-SYNONYM '*ui-object* nil)
(UCL:MAKE-SYNONYM '*ui-rel2* nil)

(defparameter *line-area-documentation*
      '(:documentation ""
:mouse-L-1 "To see the entire line."
:mouse-M-2 "To delete the tuple."
:mouse-R-1 "To modify the tuple.")
  "The wholine documentation string when a line is selected.")

(defparameter *dbms-window-wholine-documentation*
      '(:documentation "Window for database output. Some items are made mouse-sensitive for inspection."
:mouse-R-1 "RTMS Command Menu"
:mouse-R-2 "System Menu")
      "The wholine documentation string when in the RTMS interface output window.")

(defparameter *interaction-wholine-documentation*
      '(:documentation "This window accepts user input. Input can also be provided through the command menu."
:mouse-R-1 "RTMS Command Menu"
:mouse-R-2 "System Menu"))
(defparameter *attribute-wholine-documentation*
      '(:mouse-any "To see this ATTRIBUTE's definition." ))
(defparameter  *dbms-object-wholine-documentation*    ;mrr 04.06.87
      '(:mouse-any "To see this object's definition." ))
(defparameter *relation-wholine-documentation* ;mrr 04.06.87
      '(:documentation ""
:mouse-L-1 "To see the RELATION definition."
:mouse-M-1 "To modify the RELATION features."
:mouse-R-1 "To retrieve this RELATION."))
(defparameter *database-wholine-documentation*
      '(:mouse-any "List the relations in this DATABASE, if it is active."))

;**************************************************************************
;                      FLAVORS AND METHODS   *
;      *
;     MENU-PANE  ... Used for the main menu that appears in the interface.         *
;     DBMS-WINDOW .. The output-window in the interface .. text-scrolling, mouse-  *
;                    sensitive and line-area-scrolling window.                     *
;     DBMS-WINDOW-WITH-TYPEOUT .. The actual flavor used for output-window. It is  *
;                                 the above flavor with typeout-mixin added to it  *
;                                 such that temporary, unimportant and informatory *
;                                 messages can be printed on the typeout-window and*
;                                 it disappears when the user hits any character.  *
;     INTERACTION-PANE .. The flavor used for interaction. It is basically the     *
;                         universal command loop typein flavor.                    *
;     DBMS-RC  ..  Flavor for the entire interface screen. Inclusion of the command*
;                  loop mixin makes the database interface to run under the        *
;                  UCL package.              *
;**************************************************************************
(DEFFLAVOR MENU-PANE ()
   (w:menu)
  (:default-init-plist :command-menu t
                       :dynamic t))
(DEFFLAVOR DBMS-WINDOW ()
   (W:LINE-AREA-TEXT-SCROLL-MIXIN
    W:FUNCTION-TEXT-SCROLL-WINDOW
    W:MOUSE-SENSITIVE-TEXT-SCROLL-WINDOW
    W:MARGIN-REGION-MIXIN
    W:SCROLL-BAR-MIXIN
    W:ANY-TYI-MIXIN
    W:WINDOW))
(DEFMETHOD (DBMS-WINDOW :line-area-mouse-documentation) ()
   *line-area-documentation*)

(DEFFLAVOR DBMS-WINDOW-WITH-TYPEOUT ()
   (W:TEXT-SCROLL-WINDOW-TYPEOUT-MIXIN DBMS-WINDOW)
  (:DEFAULT-INIT-PLIST :typeout-window '(W:typeout-window
    :Deexposed-typeout-action
    (:expose-for-typeout))))

(defmethod (DBMS-WINDOW-WITH-TYPEOUT :who-line-documentation-string) ()        ;mrr 04.06.87
  (multiple-value-bind
    (ignore m-s-i-type)
      (send *output-window* :mouse-sensitive-item w:mouse-x w:mouse-y)
    (case m-s-i-type
      (attribute  *attribute-wholine-documentation*)
      (relation   *relation-wholine-documentation*)
      (database   *database-wholine-documentation*)
      (dbms-object *dbms-object-wholine-documentation*)
      (t *dbms-window-wholine-documentation*))))

(DEFFLAVOR INTERACTION-PANE () (UCL:COMMAND-AND-LISP-TYPEIN-WINDOW
 W:PREEMPTABLE-READ-ANY-TYI-MIXIN))
(defmethod (INTERACTION-PANE  :who-line-documentation-string) ()
   *interaction-wholine-documentation*)        ;mrr 04.06.87

(DEFMETHOD (INTERACTION-PANE :before :SELECT) (&rest ignore)
    (SEND dbms-frame1 :expose))
(DEFMETHOD (INTERACTION-PANE :after :SELECT) (&rest ignore)
;  (PKG-GOTO "RTMS")
 )


(DEFFLAVOR DBMS-RC () (UCL:COMMAND-LOOP-MIXIN W:STREAM-MIXIN
       W:INFERIORS-NOT-IN-SELECT-MENU-MIXIN
       W:BORDERED-CONSTRAINT-FRAME-WITH-SHARED-IO-BUFFER)
  (:DEFAULT-INIT-PLIST :menu-panes '((s-m-pane system-menu))
                       :active-command-tables '(dbms-comtab)
       :all-command-tables '(dbms-comtab)
       :typein-handler :handle-typein-input
;The following change is being made to prevent the first character going
;into the interface buffer.
;         :io-buffer W:kbd-io-buffer
       :minimum-width (SEND W:default-screen :width)
       :minimum-height (SEND W:default-screen :height)
       :basic-help '(help)
       :print-function 'new-print
       :print-results? #'(LAMBDA () T)
       :panes
  `((o-pane dbms-window-with-typeout
     :blinker-p NIL              ;:blink
     :print-function DBMS-PRINTER
     :print-function-arg NIL
     :scroll-bar-side :right
     :scroll-bar-mode :maximum  ;mrr 03.31.87
     :borders nil       ;mrr 04.01.87
     :label ,(LIST :bottom :string "OUTPUT"
     :font fonts:cptfont)
     :font-map ,(LIST fonts:cptfontb)
     :sensitive-item-types ,(list 'relation 'attribute ;mrr 04.09.87
      'database 'dbms-object))
    (i-pane interaction-pane
     :save-bits T
     :blinker-p :OFF            ;:blink
     :label ,(LIST :bottom :string "Rtms Interface"
     :font fonts:medfnt)
     :borders 1
     :font-map ,(LIST fonts:medfnb))
    (s-m-pane menu-pane
     :font-map ,(LIST fonts:hl12b)
     :rows 1.
              :label NIL))
               :constraints  '((main . ((o-pane i-pane s-m-pane)
       ((s-m-pane 1 :lines))
       ((o-pane .8))
       ((i-pane :even))))))
  (:INIT-KEYWORDS :TYPEIN-HANDLER :handle-typein-input))

(DEFMETHOD (DBMS-RC :handle-unknown-input) (&AUX item)
  (case UCL:input-mechanism
    (UCL:menu (beep))
    (UCL:key-or-button (BEEP))
    (UCL:typein (SEND *terminal-io* :send-if-handles :fresh-line)
    (BEEP)
    (FORMAT *STANDARD-OUTPUT* " ** ~a"
    (OR UCL:error-message "Unrecognized input")))
    (OTHERWISE (IF (LISTP ucl:kbd-input)
      (CASE (FIRST ucl:kbd-input)
(:line-area (CASE (FOURTH ucl:kbd-input)
      (#\mouse-l-1 (HELP-LINE-AREA (CADR ucl:kbd-input)))
      (#\mouse-r-1 (HELP-LINE-AREA-MOD (CADR ucl:kbd-input)))
      (#\mouse-m-2 (HELP-LINE-AREA-DEL (CADR ucl:kbd-input)))))
;I think this help can be made lot faster now that we can recognize the type of
;the object right away.
(attribute (HELP-OBJECT (STRING
   (IF (LISTP (SETQ item (CADR ucl:kbd-input)))
       (CADR item)
     item))))
(database (HELP-OBJECT (STRING
   (IF (LISTP (SETQ item (CADR ucl:kbd-input)))
       (CADR item)
     item))))
(dbms-object (HELP-OBJECT (STRING
     (IF (LISTP (SETQ item (CADR ucl:kbd-input)))
  (CADR item)
       item))))
(relation (CASE (FOURTH ucl:kbd-input)
    (#\mouse-r-1 (retrieve
    (if (stringp (setq item (CADR ucl:kbd-input)))
        (read-from-string item) ;mrr 04.06.87
        item)))
    (#\mouse-m-1 (HELP-MODIFY
    (if (stringp (setq item (CADR ucl:kbd-input)))
        (read-from-string item) ;mrr 04.06.87
        item)))
    (otherwise (HELP-OBJECT (STRING
   (IF (LISTP (SETQ item (CADR ucl:kbd-input)))
       (CADR item)
     item))))))
(OTHERWISE (BEEP)))))))


;**************************************************************************
;                          DEFCOMMANDS FOR ALL DATABASE COMMANDS                   *
;      *
;     Each defcommand definition enables individual database commands and a few    *
;     help commands to become part of the database command table. If the reader    *
;     is familiar with UCL, the following DEFCOMMAND definitions will be           *
;     self-explanatory.                      *
;**************************************************************************
;**************************************************************************
;            DEFCOMMAND FOR ACTIVE DATABASE  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC active-database)()
            `(:description "Returns the name of the active database. (ACTIVE-DATABASE)"
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Returns the name of the active database."
      :keys ((#\SUPER-F #\SUPER-A)))
  (SEND *output-window* :append-item (FORMAT nil "~S" '(ACTIVE-DATABASE)))
  (SEND *output-window* :append-item (FORMAT nil "~S" (ACTIVE-DATABASE))))
;**************************************************************************
;            DEFCOMMAND FOR ABORT TRANSACTION                                      *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC abort-transaction)()
            `(:description "Terminates the special transaction processing. (ABORT-TRANSACTION)"
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Terminates the special transaction processing."
      :keys ((#\SUPER-T #\SUPER-A)))
  (SEND *output-window* :append-item (FORMAT nil "~S" '(ABORT-TRANSACTION)))
  (SEND *output-window* :append-item (FORMAT nil "~S" (ABORT-TRANSACTION))))
;**************************************************************************
;            DEFCOMMAND FOR BEGIN TRANSACTION                                      *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC begin-transaction)()
            `(:description "Begins the special transaction processing. (BEGIN-TRANSACTION)"
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Begins the special transaction processing."
      :keys ((#\SUPER-T #\SUPER-B)))
  (SEND *output-window* :append-item (FORMAT nil "~S" '(BEGIN-TRANSACTION)))
  (SEND *output-window* :append-item (FORMAT nil "~S" (BEGIN-TRANSACTION))))
;**************************************************************************
;            DEFCOMMAND FOR END TRANSACTION  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC end-transaction)()
            `(:description "Executes the database calls postponed due to special transaction processing and terminates the transaction.  (END-TRANSACTION)"
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Executes the database calls postponed due to special transaction processing and terminates the transaction."
      :keys ((#\SUPER-T #\SUPER-E)))
  (SEND *output-window* :append-item (FORMAT nil "~S" '(END-TRANSACTION)))
  (SEND *output-window* :append-item (FORMAT nil "~S" (END-TRANSACTION))))
;**************************************************************************
;            DEFCOMMAND FOR ENVIRONMENT STATUS                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC environment-status)()
            `(:description "Returns the values of the environment variables. (ENVIRONMENT-STATUS)"
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Returns the values of the environment variables."
      :keys ((#\SUPER-F #\SUPER-E)))
  (SEND *output-window* :append-item (FORMAT nil "~S" '(ENVIRONMENT-STATUS)))
  (ENVIRONMENT-STATUS))
;**************************************************************************
;            DEFCOMMAND FOR ATTACH RELATION  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC attach-relation) (relation att path tup dir doc key
          imp ss mem &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'attach-relation)
     (FORMAT NIL "  ~S"
      (CONS
        'attach-relation
        (ARGLIST 'attach-relation))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default nil
    :type (:documentation
     "Name of the relation to be attached."
     :sexp))
   ,*ucl-attr-desc*
   ,*ucl-pathname*
   ,*ucl-format*
   ,*ucl-dir*
   ,*ucl-doci*
   ,*ucl-key*
    ,*ucl-imp*
   ,*ucl-sto*
   (:label "Memory:"
    :default nil
    :type (:documentation
     "If the data is stored in the memory, then give the name of the variable that contains the data."
     :sexp))
     :label "Give parameters for ATTACH RELATION:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "used to attach a relation."
      :keys (#\SUPER-A))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(LIST 'ATTACH-RELATION
      relation
      (SETQ keywords
    (LIST 'format tup 'dir dir 'doc doc 'path path
  'key key 'imp imp 'sto ss 'att att 'mem mem)))))
  (ATTACH-RELATION relation keywords))
;**************************************************************************
;            DEFCOMMAND FOR RENAME ATTRIBUTE *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC rename-attribute) (relation old-new)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'rename-attribute)
     (FORMAT NIL "  ~S"
      (CONS
        'rename-attribute
        (ARGLIST 'rename-attribute))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default *ui-relation*
    :type (:documentation
     "Name of the relation whose attributes are to be renamed."
     :sexp))
   (:label "Attributes and their new names:"
    :default nil
    :type (:documentation
     "Specify a list of the attributes and their new names. For ex. (a1 new-a1 a2 new-a2...)"
     :sexp))
     :label "Give parameters for RENAME ATTRIBUTE:")
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "used to rename attributes in a relation."
      :keys ((#\SUPER-R #\SUPER-A)))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(APPEND (LIST 'RENAME-ATTRIBUTE
      relation) old-new)))
  (EVAL `(RENAME-ATTRIBUTE* ,relation ,@old-new)))
;**************************************************************************
;            DEFCOMMAND FOR RENAME RELATION  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC rename-relation) (old-new)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'rename-relation)
     (FORMAT NIL "  ~S"
      (CONS
        'rename-relation
        (ARGLIST 'rename-relation))))
      :arguments (:user-supplied (:label "Relations and their new names:"
    :default nil
    :type (:documentation
     "Specify a list of the relations and their new names. For ex. (rel-1 new-rel-1 rel-2 new-rel-2...)"
     :sexp))
     :label "Give parameters for RENAME RELATION:")
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "used to rename relations in the current database."
      :keys ((#\SUPER-R #\SUPER-R)))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(CONS 'RENAME-RELATION
      old-new)))
  (EVAL `(RENAME-RELATION* ,@old-new)))
;**************************************************************************
;            DEFCOMMAND FOR RENAME DATABASE  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC rename-database) (old-new)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'rename-database)
     (FORMAT NIL "  ~S"
      (CONS
        'rename-database
        (ARGLIST 'rename-database))))
      :arguments (:user-supplied (:label "Databases and their new names:"
    :default nil
    :type (:documentation
     "Specify a list of the databases and their new names. For ex. (db-1 new-db-1 db-2 new-db-2...)"
     :sexp))
     :label "Give parameters for RENAME DATABASE:")
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "used to rename databases."
      :keys ((#\SUPER-R #\HYPER-D)))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(CONS 'RENAME-DATABASE
      old-new)))
  (EVAL `(RENAME-DATABASE* ,@old-new)))
;**************************************************************************
;            DEFCOMMAND FOR DETACH RELATION  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC detach-relation) (relation path mem disk &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'detach-relation)
     (FORMAT NIL "  ~S"
      (CONS
        'detach-relation
        (ARGLIST 'detach-relation))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default nil
    :type (:documentation
     "Name of the relation to be Detached."
     :sexp))
   (:label "Pathname:"
    :default *ui-file*
    :type (:documentation
     "Specify the name of the file where the data is to be stored."
     :SEXP))
   (:label "Memory:"
    :default nil
    :type (:documentation
     "If the data is to be in the memory and not save it on the disk, give the name of a variable."
     :sexp))
   (:label "Disk:"
    :default nil
    :type (:documentation
     "Indicate if files corresponding to the relation are to be deleted from the disk."
     :boolean))
     :label "Give parameters for DETACH RELATION:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "used to detach a relation."
      :keys (#\SUPER-D))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(LIST 'DETACH-RELATION
      relation
      (SETQ keywords
    (LIST 'path path 'mem mem 'disk disk)))))
  (DETACH-RELATION relation keywords))
;**************************************************************************
;            DEFCOMMAND FOR INSERT TUPLES    *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC insert-tuples) (relation-name list-of-tuples attributes
   pathname &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'insert)
     (FORMAT NIL "  ~S"
      (CONS
        'insert
        (ARGLIST 'insert))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default *ui-relation*
    :type (
      :documentation "Specify the relation into which the tuples are to be inserted."
      :sexp))
   (:label "List of tuples:"
    :default *ui-tuples*
       :type (:documentation "Give a list of tuples to be inserted." :SEXP))
   (:label "Attributes:"
    :default nil
    :type (:documentation "If a list of attributes is provided, then values in the tuples are assumed to be in the same order."
:SEXP))
   (:label "Pathname:"
    :default *ui-file*
    :type (:documentation "If a list of tuples is not provided, then specify the file which contains the data."
     :SEXP))
 :label "Give parameters for INSERTING TUPLES:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to insert a list of tuples in a given relation."
      :keys (#\SUPER-I))
  (SEND *output-window* :append-item (FORMAT nil "~S"
        (LIST 'INSERT relation-name (SETQ keywords
    (LIST 'tuples list-of-tuples
   'attr attributes
   'path pathname)))))
  (INSERT relation-name keywords))

;**************************************************************************
;                DEFCOMMAND FOR MAPON ALLTUPLES                                    *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC maptuple) (relation dbfunction)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'maptuple)
     (FORMAT NIL "  ~S"
      (CONS
        'maptuple
        (ARGLIST
          'maptuple))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default *ui-relation*
    :type (:documentation
         "Give the relation to be mapped." :sexp))
   (:label "Function Definition"
    :default *ui-function*
    :type (:documentation
      "Specify a function definition."
      :sexp))
  :label "Map a function on all tuples using MAPCAR:")
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Maps a given function on all the tuples in a relation using MAPCAR."
      :keys ((#\SUPER-F #\SUPER-M)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'MAPTUPLE dbfunction relation)))
  (MAPTUPLE (EVAL dbfunction) relation))
;**************************************************************************
;                DEFCOMMAND FOR MAPON ALLTUPLES                                    *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC mapt) (relation dbfunction)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'mapt)
     (FORMAT NIL "  ~S"
      (CONS
        'mapt
        (ARGLIST
          'mapt))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default *ui-relation*
    :type (:documentation
         "Give the relation to be mapped." :sexp))
   (:label "Function Definition"
    :default *ui-function*
    :type (:documentation
      "Specify a function definition."
      :sexp))
  :label "Map a function on all tuples using MAPC:")
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Maps a given function on all the tuples in a relation using MAPC."
      :keys (#\SUPER-HYPER-F))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'MAPT dbfunction relation)))
  (MAPT (EVAL dbfunction) relation))
;**************************************************************************
;                    DEFCOMMAND FOR PRINT RELATION                                 *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC print-relation) (relation
    into dir doc key imp sto
    qprint to-file sort
    format wide number print
    tuples qsort stream unique
    &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'print-relation)
     (FORMAT NIL "  ~S"
      (CONS
        'print-relation
        (ARGLIST
          'print-relation))))
      :arguments (:user-supplied ,*ucl-retrieve-rel*
   ,*ucl-into*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-qprint*
   ,*ucl-out*
   ,*ucl-sort*
   ,*ucl-format*
   ,*ucl-wide*
     ,*ucl-num*
   ,*ucl-print*
   ,*ucl-tuples*
   ,*ucl-quick-sort*
   ,*ucl-stream*
   ,*ucl-unique*
 :label "Give parameters for PRINT RELATION ==>")
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Used to print tuples in a relation."
      :keys ((#\SUPER-F #\SUPER-P)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'RETRIEVE
      relation
      (SETQ keywords
    (LIST 'dir dir
   'doc doc
   'into into
   'qprint (NOT qprint) 'output-to-file to-file
   'sort sort 'format format
   'wide wide 'num number 'key key
   'print print 'tuples tuples
   'quick-sort qsort 'stream stream
   'unique unique 'imp imp 'sto sto)))))
  (RETRIEVE relation keywords))
;**************************************************************************
;                DEFCOMMAND  FOR RESTORE DATABASE                                  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC load-database) (database directory)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'load-database)
     (FORMAT NIL "  ~S"
      (CONS
        'load-database
        (ARGLIST
          'load-database))))
      :arguments (:user-supplied (:label "Database Name:"
    :default *ui-database*
    :type (
      :documentation "Name of the database to be loaded."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (
      :documentation "Name of the directory in which it is stored."
      :sexp))
 :label "Give parameters for LOAD DATABASE:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to load database from a given directory."
      :keys ((#\SUPER-L #\SUPER-D)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'LOAD-DATABASE database (LIST 'dir directory))))
  (LOAD-DATABASE database (LIST 'dir directory)))
;**************************************************************************
;                DEFCOMMAND  FOR RESTORE ENVIRONMENT                               *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC load-environment) (environment directory)
`(:description ,(STRING-APPEND (DOCUMENTATION 'load-environment)
     (FORMAT NIL "  ~S"
      (CONS
        'load-environment
        (ARGLIST
          'load-environment))))
      :arguments (:user-supplied (:label "Environment Name:"
    :default *ui-database*
    :type (
      :documentation "Name of the environment to be loaded."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (
      :documentation "Name of the directory in which it is stored."
      :sexp))
 :label "Give parameters for LOAD ENVIRONMENT:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to load environment from a given directory."
      :keys ((#\SUPER-L #\SUPER-E)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'LOAD-ENVIRONMENT environment (LIST 'dir directory))))
  (LOAD-ENVIRONMENT environment (LIST 'dir directory)))
;**************************************************************************
;                DEFCOMMAND  FOR RESTORE RELATION                                  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC load-relation) (relation directory)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'load-relation)
     (FORMAT NIL "  ~S"
      (CONS
        'load-relation
        (ARGLIST
          'load-relation))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default *ui-relation*
    :type (
      :documentation "Name of the relation to be loaded."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (
      :documentation "Name of the directory in which it is saved."
                :sexp))
  :label "Give parameters for LOAD RELATION:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to load a relation from a given directory."
      :keys ((#\SUPER-L #\SUPER-R)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'LOAD-RELATION relation (LIST 'dir directory))))
  (LOAD-RELATION relation (LIST 'dir directory)))
;**************************************************************************
;                DEFCOMMAND  FOR SAVE DATABASE                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC save-database) (database directory)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'save-database)
     (FORMAT NIL "  ~S"
      (CONS
        'save-database
        (ARGLIST
          'save-database))))
      :arguments (:user-supplied (:label "Database Name:"
    :default *ui-database*
    :type (:documentation
       "Name of the database to be saved."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (:documentation
      "Name of the directory to write to."
      :sexp))
  :label "Give parameters for SAVE DATABASE:")
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Used to save a database on a given directory."
      :keys ((#\SUPER-S #\HYPER-D)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'SAVE-DATABASE database (LIST 'dir directory))))
  (SAVE-DATABASE database (LIST 'dir directory)))
;**************************************************************************
;                DEFCOMMAND  FOR SAVE ENVIRONMENT                                  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC save-environment) (environment directory)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'save-environment)
     (FORMAT NIL "  ~S"
      (CONS
        'save-environment
        (ARGLIST
          'save-environment))))
      :arguments (:user-supplied (:label "Environment Name:"
    :default nil
    :type (:documentation
       "Name of the environment to be saved."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (:documentation
      "Name of the directory to write to."
      :sexp))
  :label "Give parameters for SAVE environment:")
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Used to save an environment on a given directory."
      :keys ((#\SUPER-S #\SUPER-E)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'SAVE-ENVIRONMENT environment (LIST 'dir directory))))
  (SAVE-ENVIRONMENT environment (LIST 'dir directory)))
;**************************************************************************
;                DEFCOMMAND  FOR SAVE RELATION                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC save-relation) (relation directory type save
     &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'save-relation)
     (FORMAT NIL "  ~S"
      (CONS
        'save-relation
        (ARGLIST
          'save-relation))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default *ui-relation*
    :type (
      :documentation "Name of the relation to be saved."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (
      :documentation "Name of the directory to write to."
      :sexp))
   (:label "Type of SAVE:"
    :default *ui-type*
    :type (:documentation "Save type. It can be either XLD or COMMAND." ;mrr 03.31.87
     :sexp))
   (:label "Must Save:"
    :default nil
    :type (:documentation "Save the relation even if the relation has not been modified." :BOOLEAN))
 :label "Give parameters for SAVE RELATION:")
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Used to save a relation on a given directory."
      :keys ((#\SUPER-S #\SUPER-R)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'SAVE-RELATION relation
      (SETQ keywords (LIST 'type type 'dir directory
     'save save)))))
  (SAVE-RELATION relation keywords))
;**************************************************************************
;                DEFCOMMAND  FOR SAVE TRANSACTION                                  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC save-transaction) (transaction directory pathname
     &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'save-transaction)
     (FORMAT NIL "  ~S"
      (CONS
        'save-transaction
        (ARGLIST
          'save-transaction))))
      :arguments (:user-supplied (:label "Transaction Name:"
    :default *ui-transaction*
    :type (
      :documentation "Name of the transaction to be saved."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (
      :documentation "Name of the directory to write to."
      :sexp))
   (:label "Pathname:"
    :default *ui-file*
    :type (:documentation
     "The name of the file into which the transaction forms will be stored. It defaults to <transaction>.lisp"
     :SEXP))
 :label "Give parameters for SAVE TRANSACTION:")
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Used to save a transaction on a given directory."
      :keys ((#\SUPER-S #\SUPER-T)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'SAVE-TRANSACTION transaction
      (SETQ keywords (LIST 'path pathname 'dir directory)))))
  (SAVE-TRANSACTION transaction keywords))

;**************************************************************************
;                DEFCOMMAND  FOR DEFINE IMPLEMENTATION                             *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-implementation) (implementation doc
   &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'define-implementation)
     (FORMAT NIL "  ~S"
      (CONS
        'define-implementation
        (ARGLIST 'define-implementation))))
      :arguments (:user-supplied (:label "Implementation Name:"
    :default nil
    :type (:documentation
      "Name of the implementation. Implementation-dependent routines are expected to be defined by the user."
      :sexp))
   (:label "Documentation:"
    :default *ui-doc*
    :type (:documentation
      "Documentation for the implementation."
      :string))
  :label "Give parameters for DEFINE IMPLEMENTATION:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define an implementation."
      :keys ((#\SUPER-D #\SUPER-I)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'DEFINE-IMPLEMENTATION implementation
      (SETQ keywords (LIST 'doc doc
     )))))
  (DEFINE-IMPLEMENTATION implementation keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE INDEX                                      *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-index) (relation-name index-name key-attributes storage-structure priority
  doc &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'define-index)
     (FORMAT NIL "  ~S"
      (CONS
        'define-index
        (ARGLIST 'define-index))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default nil
    :type (:documentation
      "Name of the relation upon which the index will be defined."
      :sexp))
   (:label "Index Name:"
    :default nil
    :type (:documentation
      "Name of the index to be defined."
      :string))
   (:label "Key Attributes:"
    :default nil
    :type (:documentation
      "List of attribute names which form the key for this index."
      :sexp))
   (:label "Storage Structure:"
    :default "AVL"
    :type (:documentation
      "The storage structure used to define the index."
      :string))
   (:label "Priority:"
    :default 10
    :type (:documentation
      "A numerical value which indicates the priority given to this index. 1 is the highest priority."
      :number))
   (:label "Documentation:"
    :default *ui-doc*
    :type (:documentation
      "Documentation for the index."
      :string))
  :label "Give parameters for DEFINE INDEX:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define a secondary index on a relation."
      :keys ((#\SUPER-D #\HYPER-I)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'DEFINE-INDEX relation-name
      (SETQ keywords (LIST 'name index-name 'key key-attributes 'sto storage-structure
     'priority priority 'doc doc
     )))))
  (DEFINE-INDEX relation-name keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY INDEX                                      *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-index) (relation-name index-name new-index-name
  key-attributes storage-structure priority
  doc &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-index)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-index
        (ARGLIST 'modify-index))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default nil
    :type (:documentation
      "Name of the relation upon which the index to be modified is defined."
      :sexp))
   (:label "Index Name:"
    :default nil
    :type (:documentation
      "Name of the index to be modified."
      :string))
   (:label "New Index Name:"
    :default nil
    :type (:documentation
      "New name of the index."
      :string))
   (:label "Key Attributes:"
    :default nil
    :type (:documentation
      "List of attribute names which form the key for this index."
      :sexp))
   (:label "Storage Structure:"
    :default nil
    :type (:documentation
      "The storage structure used to define the index."
      :string))
   (:label "Priority:"
      :default 10
    :type (:documentation
      "A numerical value which indicates the priority given to this index. 1 is the highest priority."
      :number))
   (:label "Documentation:"
    :default *ui-doc*
    :type (:documentation
      "Documentation for the index."
      :string))
  :label "Give parameters for DEFINE INDEX:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to define a secondary index on a relation."
      :keys ((#\SUPER-M #\HYPER-I)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'MODIFY-INDEX relation-name index-name
      (SETQ keywords (LIST 'new-name new-index-name 'key key-attributes 'sto storage-structure
     'priority priority 'doc doc
     )))))
  (MODIFY-INDEX relation-name index-name keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE STORAGE-STRUCTURE                          *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-storage-structure) (storage-structure doc
   &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'define-storage-structure)
     (FORMAT NIL "  ~S"
      (CONS
        'define-storage-structure
        (ARGLIST 'define-storage-structure))))
      :arguments (:user-supplied (:label "Storage structure name:"
    :default nil
    :type (:documentation
      "Name of the storage structure. Storage-structure-dependent routines are expected to be defined by the user."
      :sexp))
   (:label "Documentation:"
    :default nil
    :type (:documentation
      "Documentation for the storage structure."
      :string))
  :label "Give parameters for DEFINE STORAGE STRUCTURE:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define a storagestructure."
      :keys ((#\SUPER-D #\SUPER-S)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'DEFINE-STORAGE-STRUCTURE storage-structure
      (SETQ keywords (LIST 'doc doc
     )))))
  (DEFINE-STORAGE-STRUCTURE storage-structure keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE DOMAIN                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-domain) (domain def doc format
   &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'define-domain)
     (FORMAT NIL "  ~S"
      (CONS
        'define-domain
        (ARGLIST 'define-domain))))
      :arguments (:user-supplied (:label "Domain Name:"
    :default nil
    :type (:documentation
      "Name of the domain. Domain predicate is expected to be defined prior to this."
      :sexp))
   (:label "Default value:"
    :default nil
    :type (:documentation
     "Default value for this domain."
     :sexp))
   (:label "Documentation:"
    :default nil
    :type (:documentation
      "Documentation for the domain."
      :string))
   (:label "Default width :"
    :default nil
    :type (:documentation
      "The default width to be used for this domain."
      :sexp))
  :label "Give parameters for DEFINE DOMAIN:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define a domain."
      :keys (#\SUPER-HYPER-D))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'DEFINE-DOMAIN domain
      (SETQ keywords (LIST 'default def
     'doc doc
     'format format)))))
  (DEFINE-DOMAIN domain keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY DOMAIN                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-domain) (domain def doc format
   &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-domain)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-domain
        (ARGLIST 'modify-domain))))
      :arguments (:user-supplied (:label "Domain Name:"
    :default nil
    :type (:documentation
      "Name of the domain to be modified."
      :sexp))
   (:label "Default value:"
    :default nil
    :type (:documentation
     "New default value for this domain."
     :sexp))
   (:label "Documentation:"
    :default nil
    :type (:documentation
      "New documentation for the domain."
      :string))
   (:label "Default width :"
    :default nil
    :type (:documentation
      "The new default width to be used for this domain."
      :sexp))
  :label "Give parameters for MODIFY DOMAIN:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to modify a domain."
      :keys ((#\SUPER-M #\SUPER-D)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
  (LIST 'MODIFY-DOMAIN domain
      (SETQ keywords (LIST 'default def
     'doc doc
     'format format)))))
  (MODIFY-DOMAIN domain keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE TRANSACTION                                *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-transaction) (transaction forms dir path
      &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'define-transaction)
     (FORMAT NIL "  ~S"
      (CONS
        'define-transaction
        (ARGLIST 'define-transaction))))
      :arguments (:user-supplied (:label "Transaction Name:"
    :default *ui-transaction*
    :type (:documentation
      "Name of the transaction."
      :sexp))
   (:label "Database calls:"
    :default nil
    :type (:documentation
     "A list of database calls."
     :sexp))
   ,*ucl-dir*
   (:label "Pathname :"
    :default *ui-file*
    :type (:documentation
      "The default file in which it will be saved."
      :SEXP))
  :label "Give parameters for DEFINE TRANSACTION:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define a transaction."
      :keys ((#\SUPER-D #\SUPER-T)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'DEFINE-TRANSACTION transaction forms
      (SETQ keywords (LIST 'dir dir
     'path path)))))
  (DEFINE-TRANSACTION transaction forms keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY TRANSACTION                                *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-transaction) (transaction dir path
      &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-transaction)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-transaction
        (ARGLIST 'modify-transaction))))
      :arguments (:user-supplied (:label "Transaction Name:"
    :default *ui-transaction*
    :type (:documentation
      "Name of the transaction to be modified."
      :sexp))
   (:label "Directory:"
    :default *ui-directory*
    :type (:documentation
      "Default directory in which it can be found, if not in memory."
      :SEXP))
   (:label "Pathname :"
    :default *ui-file*
    :type (:documentation
      "The default file in which it can be found, if not in memory."
      :SEXP))
  :label "Give parameters for MODIFY TRANSACTION:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to modify a transaction."
      :keys ((#\SUPER-M #\SUPER-T)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'MODIFY-TRANSACTION transaction
      (SETQ keywords (LIST 'dir dir
     'path path)))))
  (MODIFY-TRANSACTION transaction keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE DATABASE                                   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-database) (database directory doc env
     &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'define-database)
     (FORMAT NIL "  ~S"
      (CONS
        'define-database
        (ARGLIST 'define-database))))
      :arguments (:user-supplied (:label "Database Name:"
    :default nil
    :type (:documentation
      "Name of the database."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (:documentation
     "Name of the save directory for this database."
     :sexp))
   (:label "Documentation:"
    :default *ui-doc*
    :type (:documentation
      "Documentation for the database."
      :string))
   (:label "Environment:"
    :default nil
    :type (:documentation
      "Name of the environment to be used to replace the default settings."
      :sexp))
  :label "Give parameters for DEFINE DATABASE:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define a database in a given directory."
      :keys ((#\SUPER-D #\SUPER-D)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'DEFDB database
      (SETQ keywords (LIST 'dir directory
     'doc doc
     'environment env)))))
  (DEFDB database keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY DATABASE                                   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-database) (database new-database directory doc
     &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-database)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-database
        (ARGLIST 'modify-database))))
      :arguments (:user-supplied (:label "Database Name:"
    :default nil
    :type (:documentation
      "Name of the database."
      :sexp))
   (:label "New Database Name:"
    :default nil
    :type (:documentation
      "If the database is to be renamed specify the new name."
      :sexp))
   (:label "Directory Name:"
    :default NIL
    :type (:documentation
     "To change the save directory for this database specify a new directory."
     :sexp))
   (:label "Documentation:"
    :default NIL
    :type (:documentation
      "New documentation for the database."
      :string))
  :label "Give parameters for MODIFY DATABASE:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to modify the features a database."
      :keys ((#\SUPER-M #\HYPER-D)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'MODIFY-DATABASE database
      (SETQ keywords (LIST 'database-name new-database
      'dir directory
     'doc doc
     )))))
  (MODIFY-DATABASE database keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY ATTRIBUTE                                  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-attribute) (relation attr new-attr def doc format
     &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-attribute)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-attribute
        (ARGLIST 'modify-attribute))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default *ui-relation*
    :type (:documentation
      "Name of the relation."
      :sexp))
   (:label "Attribute Name:"
    :default nil
    :type (:documentation
      "Name of the attribute."
      :sexp))
   (:label "New Attribute Name:"
    :default nil
    :type (:documentation
      "If the attribute is to be renamed specify the new name."
      :sexp))
   (:label "Default Value:"
    :default NIL
    :type (:documentation
     "To change the default value of this attribute specify a new value."
     :sexp))
   (:label "Documentation:"
    :default NIL
    :type (:documentation
      "New documentation for the attribute."
      :string))
   (:label "Default width :"
    :default nil
    :type (:documentation
      "The new default width to be used for this attribute."
      :sexp))
  :label "Give parameters for MODIFY ATTRIBUTE:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to modify the features a attribute."
      :keys ((#\SUPER-M #\SUPER-A)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'MODIFY-ATTRIBUTE relation attr
      (SETQ keywords (LIST 'attribute-name new-attr
     'def def
     'doc doc 'format format
     )))))
  (MODIFY-ATTRIBUTE relation attr keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY VIEW *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-view) (view def doc
     &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-view)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-view
        (ARGLIST 'modify-view))))
      :arguments (:user-supplied (:label "View Name:"
    :default NIL
    :type (:documentation
      "Name of the view."
      :sexp))
   (:label "View Definition:"
    :default nil
    :type (:documentation
      "New definition of the view."
      :sexp))
   (:label "Documentation:"
    :default NIL
    :type (:documentation
      "New documentation for the view."
      :string))
  :label "Give parameters for MODIFY VIEW:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to modify the features a view."
      :keys ((#\SUPER-M #\SUPER-V)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'MODIFY-VIEW view
      (SETQ keywords (LIST
     'view-def def
     'view-doc doc
     )))))
  (MODIFY-VIEW view keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY RELATION                                   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-relation) (rel new-rel add-att del-att ren-att
     imp sto format key dir doc
     &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-relation)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-relation
        (ARGLIST 'modify-relation))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default *ui-relation*
    :type (:documentation
      "Name of the Relation."
      :sexp))
   (:label "New Relation Name:"
    :default nil
    :type (:documentation
      "If the relation is to be renamed specify the new name."
      :sexp))
   (:label "Add attributes:"
    :default NIL
    :type (:documentation
     "Specify a list of attribute-descriptor pairs for attributes to be added to this relation."
     :sexp))
   (:label "Delete attributes:"
    :default NIL
    :type (:documentation
     "Specify a list of attributes in this relation which are to be deleted."
     :sexp))
   (:label "Rename attributes:"
    :default NIL
    :type (:documentation
     "To rename some of the attributes provide a list of the form (<old-attribute new-attribute>)."
     :sexp))
   (:label "Implementation Type:"
    :default NIL
    :type (:documentation
     "To change the implementation type of this relation specify a new value."
     :sexp))
   (:label "Storage structure:"
    :default NIL
    :type (:documentation
     "To change the storage structure of this relation specify a new value."
     :sexp))
   (:label "Format:"
    :default NIL
    :type (:documentation
     "To change the format for this relation specify a new format as a list of values."
     :sexp))
   (:label "Key:"
    :default NIL
    :type (:documentation
     "To change the key for this relation specify a new key as a list of attributes."
     :sexp))
   (:label "Directory Name:"
    :default NIL
    :type (:documentation
        "To change the save directory for this relation specify a new directory."
     :sexp))
   (:label "Documentation:"
    :default NIL
    :type (:documentation
      "New documentation for the relation."
      :string))
  :label "Give parameters for MODIFY RELATION:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to modify the features a relation."
      :keys ((#\SUPER-M #\SUPER-R)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'MODIFY-RELATION rel
      (SETQ keywords (LIST 'relation new-rel
     'add-attributes add-att
     'delete-attributes del-att
     'rename-attributes ren-att
     'imp imp
     'sto sto
     'format format
     'key key
     'doc doc
     'dir dir
     )))))
  (MODIFY-RELATION rel keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE ENVIRONMENT                                *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-environment) (environment save dir err par-check
        rel-imp rel-sto status sys-imp
        sys-sto val-check warn
        &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'define-environment)
     (FORMAT NIL "  ~S"
      (CONS
        'define-environment
        (ARGLIST 'define-environment))))
      :arguments (:user-supplied (:label "Environment Name:"
    :default nil
    :type (:documentation
      "Name of the environment."
      :sexp))
   (:label "Auto save:"
    :default nil
    :type (:documentation
     "Automatically saves all the modified relations after each function." :boolean))
   ,*ucl-dir*
   (:label "Errors:"
    :default T
    :type (:documentation
      "Controls the printing of the error messages."
      :boolean))
   (:label "Parameter Checking:"
    :default T
    :type (:documentation
      "Controls the checking of the parameters."
      :boolean))
   (:label "Relation Implementation:"
    :default *ui-imp*
    :type (:documentation
      "Default implementation of the user relations."
      :sexp))
   (:label "Relation storage structure:"
    :default *ui-ss*
    :type (:documentation
      "Default storage structure for the user relations."
      :sexp))
   (:label "Status:"
    :default T
    :type (:documentation
      "Controls the printing of the status messages."
      :boolean))
   (:label "System Implementation:"
    :default nil
    :type (:documentation
      "Default implementation of the system relations. Can not change this when a database is active."
      :sexp))
   (:label "System storage structure:"
    :default nil
    :type (:documentation
      "Default storage structure for the system relations. Can not change this when a database is active."
      :sexp))
   (:label "Validity Checking:"
    :default T
    :type (:documentation
      "Controls the checking of the values during insertion and modification for validity."
      :boolean))
   (:label "Warnings:"
    :default T
     :type (:documentation
      "Controls the printing of the warning messages."
      :boolean))
  :label "Give parameters for DEFINE ENVIRONMENT:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define an environment in a given directory."
      :keys ((#\SUPER-D #\SUPER-E)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'DEFENV environment
      (SETQ keywords (IF *active-db*
   (LIST 'auto-save save 'para par-check
     'dir dir 'rel-imp rel-imp 'rel-sto
     rel-sto 'errors err 'status status
     'validity val-check 'warnings warn)
        (LIST 'auto-save save 'para par-check
     'dir dir 'rel-imp rel-imp 'rel-sto
     rel-sto 'errors err 'status status
     'sys-imp sys-imp 'sys-sto sys-sto
     'validity val-check 'warnings warn))))))
  (DEFENV environment keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE RELATION                                   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-relation) (relation attr-des tup
     dir doc key imp ss &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'define-relation)
     (FORMAT NIL "  ~S"
      (CONS
        'define-relation
        (ARGLIST 'define-relation))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default nil
    :type (:documentation
     "Name of the relation to be defined."
     :sexp))
   ,*ucl-attr-desc*
   ,*ucl-format*
   ,*ucl-dir*
   ,*ucl-doci*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
     :label "Give parameters for DEFINE RELATION:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "used to define a relation."
      :keys ((#\SUPER-D #\SUPER-R)))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(LIST 'DEFREL
      relation attr-des
      (SETQ keywords
    (LIST 'tuple-format tup 'dir dir 'doc doc
  'key key 'imp imp 'sto ss)))))
  (DEFREL relation attr-des keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE VIEW *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-view) (viewname view-definition doc)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'define-view)
     (FORMAT NIL "  ~S"
      (CONS
        'define-view
        (ARGLIST 'define-view))))
      :arguments (:user-supplied (:label "View Name:"
    :default nil
    :type (:documentation
       "Specify a name for the view."
     :sexp))
   (:label "View Definition:"
    :default *ui-viewdef*
    :type (:documentation
       "Specify a definition for the view."
     :sexp))
   (:label "View Documentation:"
    :default nil
    :type (:documentation
       "Specify documentation for the view."
     :sexp))
 :label "Give parameters for DEFINE VIEW:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define a view."
      :keys ((#\SUPER-D #\SUPER-V)))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(LIST 'DEFVIEW viewname view-definition doc)))
  (DEFVIEW viewname view-definition doc))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE ATTRIBUTE                                  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-attribute) (relation-name attr-des key
      &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'define-attribute)
     (FORMAT NIL "  ~S"
      (CONS
        'define-attribute
        (ARGLIST 'define-attribute))))
      :arguments (:user-supplied (:label "Relation name: "
    :default *ui-relation*
    :type (:documentation
       "The name of the relation to which new attributes are to be added." :SEXP))
   ,*ucl-attr-desc*
   (:label "Key: "
    :default nil
    :type (:documentation
       "New key for the relation if it is to be different from the previous value. Specify a list of attributes."
       :SEXP))
 :label "Give parameters for DEFINE ATTRIBUTE:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to add attributes to relations."
      :keys ((#\SUPER-D #\SUPER-A)))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(LIST 'DEFINE-ATTRIBUTE relation-name attr-des
      (SETQ keywords (LIST 'key key)))))
  (DEFINE-ATTRIBUTE relation-name attr-des keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY TUPLES                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-tuples) (relation where-clause attributes values
     &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-tuples)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-tuples
        (ARGLIST 'modify-tuples))))
      :arguments (:user-supplied (:label "Relation: "
    :default *ui-relation*
    :type (:documentation
       "Specify the relation whose tuples are to be modified."
     :sexp))
   ,*ucl-where*
   (:label "Attributes: "
    :default *ui-attributes*
    :type (:documentation
       "Specify a list of attributes in the above relation to be modified." :sexp))
   (:label "Values: "
    :default *ui-values*
    :type (:documentation
       "Specify a corresponding list of values to modify the above attributes." :sexp))
 :label "Give parameters for MODIFY TUPLES ==>")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to modify tuples in a relation."
      :keys ((#\SUPER-M #\HYPER-M)))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(LIST 'MODIFY relation (SETQ keywords (LIST 'where where-clause
       'attr attributes
       'values values)))))
  (MODIFY relation keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DELETE TUPLES                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC delete-tuples) (relation where-clause)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'delete-tuples)
     (FORMAT NIL "  ~S"
      (CONS
        'delete-tuples
        (ARGLIST 'delete-tuples))))
      :arguments (:user-supplied (:label "Relation: "
    :default *ui-relation*
    :type (:documentation
       "Specify a relation whose tuples are to be deleted."
     :sexp))
   (:label "Where clause: "
    :default nil
    :type (:documentation
       "Deletes the tuples which satisfy this condition."
     :sexp))
 :label "Give parameters for DELETE TUPLES ==>")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to delete tuples in a relation."
      :keys (#\HYPER-D))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(LIST 'DELETE-TUPLES relation (LIST 'where where-clause))))
  (DELETE-TUPLES  relation (LIST 'where where-clause)))
;**************************************************************************
;                DEFCOMMAND  FOR RETRIEVE TUPLES                                   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC retrieve-tuples) (relation attributes where-clause
     into dir doc key imp sto
     qprint to-file sort
     format wide number print
     tuples qsort stream unique index-name
     &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'retrieve)
       (FORMAT NIL "  ~S"
      (CONS
        'retrieve
        (ARGLIST 'retrieve))))
      :arguments (:user-supplied ,*ucl-retrieve-rel*
   ,*ucl-attributes*
   ,*ucl-where*
   ,*ucl-into*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-qprint*
   ,*ucl-out*
   ,*ucl-sort*
   ,*ucl-format*
   ,*ucl-wide*
   ,*ucl-num*
   ,*ucl-print*
   ,*ucl-tuples*
   ,*ucl-quick-sort*
   ,*ucl-stream*
   ,*ucl-unique*
    ,*ucl-index-name*
 :label "Give parameters for RETRIEVE TUPLES ==>")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to Retrieve tuples in a relation."
      :keys (#\HYPER-R))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'RETRIEVE
      relation
      (SETQ keywords
    (LIST 'project
   (IF (EQUAL attributes T)
       NIL
     attributes)
   'where where-clause 'into into
   'dir dir 'doc doc 'key key 'imp imp 'sto sto
    'qprint (NOT qprint) 'output-to-file to-file
   'sort sort 'format format
   'wide wide 'num number
   'print print 'tuples tuples
   'quick-sort qsort 'stream stream
   'unique unique 'index-name index-name)))))
  (RETRIEVE relation keywords))
;**************************************************************************
;                DEFCOMMAND  FOR SELECT TUPLES                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC select) (relation where-clause
     into dir doc key imp sto
     qprint to-file sort
     format wide number print
     tuples qsort stream unique index-name
     &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'select-tuples)
     (FORMAT NIL "  ~S"
      (CONS
        'select-tuples
        (ARGLIST 'select-tuples))))
      :arguments (:user-supplied ,*ucl-retrieve-rel*
   ,*ucl-where*
   ,*ucl-into*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-qprint*
   ,*ucl-out*
   ,*ucl-sort*
   ,*ucl-format*
   ,*ucl-wide*
   ,*ucl-num*
   ,*ucl-print*
   ,*ucl-tuples*
   ,*ucl-quick-sort*
   ,*ucl-stream*
   ,*ucl-unique*
   ,*ucl-index-name*
 :label "Give parameters for SELECT TUPLES ==>")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to Select tuples in a relation."
      :keys ((#\SUPER-R #\SUPER-S)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'SELECT-TUPLES
      relation
      (SETQ keywords
    (LIST
   'where where-clause 'into into
   'dir dir 'doc doc 'key key 'imp imp 'sto sto
   'qprint (NOT qprint) 'output-to-file to-file
   'sort sort 'format format
   'wide wide 'num number
   'print print 'tuples tuples
   'quick-sort qsort 'stream stream
   'unique unique 'index-name index-name)))))
  (RETRIEVE relation (APPEND (LIST 'project nil) keywords)))
;**************************************************************************
;                DEFCOMMAND  FOR PROJECT TUPLES                                    *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC Project) (relation attributes
      into dir doc key imp sto
      qprint to-file sort
      format wide number print tuples
      qsort stream unique
      &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'project)
     (FORMAT NIL "  ~S"
      (CONS
        'project
        (ARGLIST
          'project))))
      :arguments (:user-supplied ,*ucl-retrieve-rel*
   ,*ucl-attributes*
   ,*ucl-into*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-qprint*
   ,*ucl-out*
   ,*ucl-sort*
   ,*ucl-format*
   ,*ucl-wide*
   ,*ucl-num*
   ,*ucl-print*
   ,*ucl-tuples*
   ,*ucl-quick-sort*
   ,*ucl-stream*
   ,*ucl-unique*
 :label "Give parameters for PROJECT TUPLES ==>")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to Project tuples in a relation."
      :keys ((#\SUPER-R #\SUPER-P)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'PROJECT
      relation
      (SETQ keywords
    (LIST 'project (IF (EQUAL attributes T)
   nil
        attributes)
    'into into 'dir dir 'doc doc 'key key 'imp imp 'sto sto
    'qprint (NOT qprint) 'output-to-file to-file
    'sort sort 'format format
    'wide wide 'num number 'print print 'tuples tuples
    'quick-sort qsort 'stream stream 'unique unique)))))
  (RETRIEVE relation (APPEND (LIST 'where t) keywords)))
;**************************************************************************
;                DEFCOMMAND  FOR COMMIT TRANSACTION                                *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC commit-transaction) (trans dir path &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'commit-transaction)
     (FORMAT NIL "  ~S"
      (CONS
        'commit-transaction
        (ARGLIST
          'commit-transaction))))
      :arguments (:user-supplied (:label "Name of the transaction :"
    :default *ui-transaction*
    :type (:documentation
       "The name of an existing transaction." :SEXP))
   (:label "Name of the directory:"
    :default *ui-directory*
    :type (:documentation
       "Name of the directory which contains the transaction file, if the transaction is not in the memory." :SEXP))
   (:label "Pathname:"
    :default *ui-file*
    :type (:documentation
    "If the transaction is not in memory, provide the pathname for the transaction file. It defaults to <transaction>.lisp." :SEXP))
 :label "Give parameters for COMMIT TRANSACTION")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Commit a transaction - execute all the database calls in it."
      :keys ((#\SUPER-T #\SUPER-C)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'COMMIT-TRANSACTION trans (SETQ keywords
         (LIST 'dir dir
        'path path)))))
  (COMMIT-TRANSACTION trans keywords)
)
;**************************************************************************
;                DEFCOMMAND  FOR JOIN        *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC join) (into from project where
      tuples format dir doc key imp sto
             print unique &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'join)
     (FORMAT NIL "  ~S"
      (CONS
        'join
        (ARGLIST
          'join))))
      :arguments (:user-supplied (:label "Output relation :"
    :default *ui-join-into*
    :type (:documentation
       "If not provided, the result of JOIN is stored in a temporary relation unless only the resultant tuples are requested." :SEXP))
   (:LABEL "FROM :"
    :DEFAULT *ui-from*
    :TYPE (:DOCUMENTATION
     "Specify a list of two relations to be joined." :SEXP))
   (:label "Project :"
    :default NIL
    :type (:documentation
       "This gives the attributes in the output relation. Example: (rel1.* a3 (rel2.a1 a4)) ==> All the attributes in rel1, attribute A3 of rel2 and atribute A1 of rel2 renamed as A4." :SEXP))
   (:label "Where :"
    :default *ui-over*
    :type (:documentation
     "The join clause using the theta-operators. It is a where clause consisting of attributes from the relations being joined." :SEXP))
   (:label "Tuples?"
    :default NIL
    :type (:documentation
     "Specify if the resultant tuples be returned rather than inserted in a relation. The following parameters can be ignored if this is true."
     :boolean))
   ,*ucl-format*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-print*
   ,*ucl-unique*
 :label "Give parameters for JOIN")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to join relations."
      :keys (#\SUPER-J))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'JOIN 'from from
      (SETQ keywords (LIST 'project project
     'into into
     'tuples tuples
     'format format
     'dir dir
     'doc doc
     'key key
     'imp imp
     'sto sto
     'print print
     'where where 'unique unique)))))
  (JOIN-INTERNAL (APPEND (LIST 'from from) keywords))
)
;**************************************************************************
;                DEFCOMMAND  FOR DESTROY DATABASE                                 *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC destroy-database) (database disk &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-database)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-database
        (ARGLIST
          'destroy-database))))
      :arguments (:user-supplied (:label "Database Name:"
    :default nil
    :type (:documentation
       "Name of the database to be destroyed." :SEXP))
   (:label "Delete from the DISK:"
    :default NIL
    :type (:documentation
     "IF YES all the files pertaining to this database are deleted but NOT EXPUNGED." :BOOLEAN))
 :label "Give parameters for DESTROY DATABASE:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy databases"
      :keys ((#\SUPER-K #\SUPER-D)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-DATABASE database
      (SETQ keywords (LIST 'disk disk)))))
  (DESTROY-DATABASE database keywords)
)
;**************************************************************************
;                DEFCOMMAND  FOR DESTROY DOMAIN                                    *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC destroy-domain) (domain)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-domain)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-domain
        (ARGLIST
          'destroy-domain))))
      :arguments (:user-supplied (:label "Domain Name:"
    :default nil
    :type (:documentation
       "Name of the domain to be destroyed." :SEXP))
 :label "Give parameters for DESTROY DOMAIN:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy domains."
      :keys (#\SUPER-HYPER-K))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-DOMAIN domain)))
  (DESTROY-DOMAIN domain))
;**************************************************************************
;                DEFCOMMAND  FOR DESTROY IMPLEMENTATION                            *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC destroy-implementation) (implementation)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-implementation)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-implementation
        (ARGLIST
          'destroy-implementation))))
      :arguments (:user-supplied (:label "Implementation Name:"
    :default nil
    :type (:documentation
       "Name of the implementation to be destroyed." :SEXP))
 :label "Give parameters for DESTROY IMPLEMENTATION:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy implementations."
      :keys ((#\SUPER-K #\SUPER-I)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-IMPLEMENTATION implementation)))
  (DESTROY-IMPLEMENTATION implementation))
;**************************************************************************
;                DEFCOMMAND  FOR DESTROY INDEX                            *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC DESTROY-INDEX) (relation-name index-name)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-index)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-index
        (ARGLIST
          'destroy-index))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default nil
    :type (:documentation
       "Name of the relation on which the index to be destroyed is defined." :SEXP))
   (:label "Index Name:"
    :default nil
    :type (:documentation
       "Name of the index to be destroyed." :SEXP))
     :label "Give parameters for DESTROY INDEX:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy indices."
      :keys ((#\SUPER-K #\HYPER-I)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-INDEX relation-name index-name)))
  (DESTROY-INDEX relation-name index-name))
;**************************************************************************
;                DEFCOMMAND  FOR DESTROY STORAGE STRUCTURE                         *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC destroy-storage-structure) (storage-structure)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-storage-structure)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-storage-structure
        (ARGLIST
          'destroy-storage-structure))))
      :arguments (:user-supplied (:label "Storage structure name:"
    :default nil
    :type (:documentation
       "Name of the storage structure to be destroyed." :SEXP))
 :label "Give parameters for DESTROY STORAGE STRUCTURE:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy storage structures."
      :keys ((#\SUPER-K #\SUPER-S)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-STORAGE-STRUCTURE storage-structure)))
  (DESTROY-STORAGE-STRUCTURE storage-structure))
;**************************************************************************
;                DEFCOMMAND  FOR DESTROY VIEW                                      *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC destroy-view) (view)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-view)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-view
        (ARGLIST
          'destroy-view))))
      :arguments (:user-supplied (:label "View name:"
    :default nil
    :type (:documentation
       "Name of the view to be destroyed."
       :SEXP))
 :label "Give parameters for DESTROY VIEW:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy views."
      :keys ((#\SUPER-K #\SUPER-V)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-VIEW view)))
  (DESTROY-VIEW view))
;**************************************************************************
;                DEFCOMMAND  FOR DESTROYREL   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC destroy-relation) (relation disk &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-relation)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-relation
        (ARGLIST
          'destroy-relation))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default nil
    :type (:documentation
       "Name of the relation to be destroyed." :SEXP))
   (:label "Delete from the DISK:"
    :default NIL
    :type (:documentation
     "IF YES the file corresponding to this relation is deleted but NOT EXPUNGED." :BOOLEAN))
 :label "Give parameters for DESTROY RELATION:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy relations"
      :keys ((#\SUPER-K #\SUPER-R)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-RELATION
      relation (SETQ keywords (LIST 'disk disk)))))
  (DESTROY-RELATION relation keywords)
)
;**************************************************************************
;                DEFCOMMAND  FOR DESTROY ATTRIBUTE                                 *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC destroy-attribute) (relation attr key &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-attribute)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-attribute
        (ARGLIST
          'destroy-attribute))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default nil
    :type (:documentation
       "Name of the relation from which attributes are to be destroyed." :SEXP))
   (:label "Attributes:"
    :default nil
    :type (:documentation
       "List of attributes to destroy." :SEXP))
   (:label "Key:"
    :default NIL
    :type (:documentation
     "New key for the relation if it is to be different from the previous value or if any of the key attributes are destroyed." :SEXP))
 :label "Give parameters for DESTROY ATTRIBUTE:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy attributes from relations"
      :keys ((#\SUPER-K #\SUPER-A)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-ATTRIBUTE relation (SETQ keywords (LIST 'attr attr
      'key key)))))
  (DESTROY-ATTRIBUTE relation keywords)
)
;**************************************************************************
;                DEFCOMMAND  FOR SET UNION   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC union) (from into tuples format
       dir doc key imp sto print unique
       &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'relation-union)
     (FORMAT NIL "  ~S"
       (CONS
        'relation-union
        (ARGLIST
          'relation-union))))
      :arguments (:user-supplied (:label "List of two relations:"
    :default NIL
    :type (:documentation
     "List of the names of two relations which will take part in the relation union operation. The attributes to be projected and a where clause can be specified for each relation using keywords. For instance, (REL1 (PROJECT <attr> WHERE <where-claue>) REL2 (WHERE <where-clause> PROJECT <attr>))." :SEXP))
   ,*ucl-into*
   (:label "Tuples?"
    :default NIL
    :type (:documentation
     "Specify if the resultant tuples be returned rather than inserted in a relation. The following parameters can be ignored if this is true."
     :boolean))
   ,*ucl-format*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-print*
   ,*ucl-unique*
  :LABEL "Parameters for the set-union of two relations")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to form union of two compatible relations"
      :keys ((#\SUPER-O #\SUPER-U)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'RELATION-UNION
      (SETQ keywords (LIST 'into into
     'from from 'tuples tuples
     'format format 'dir dir 'doc doc
     'key key 'imp imp 'sto sto
     'print print 'unique unique)))))
  (RELATION-UNION keywords))
;**************************************************************************
;                DEFCOMMAND  FOR SET DIFFERENCE                                    *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC difference) (from into tuples format
       dir doc key imp sto print unique
       &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'relation-difference)
     (FORMAT NIL "  ~S"
      (CONS
        'relation-difference
        (ARGLIST
          'relation-difference))))
      :arguments (:user-supplied (:label "List of two relations:"
    :default NIL
    :type (:documentation
     "List of the names of two relations which will take part in the relation difference operation. The attributes to be projected and a where clause can be specified for each relation using keywords. For instance, (REL1 (PROJECT <attr> WHERE <where-claue>) REL2 (WHERE <where-clause> PROJECT <attr>))." :SEXP))
   ,*ucl-into*
   (:label "Tuples?"
    :default NIL
    :type (:documentation
     "Specify if the resultant tuples be returned rather than inserted in a relation. The following parameters can be ignored if this is true."
     :boolean))
   ,*ucl-format*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-print*
   ,*ucl-unique*
  :LABEL "Parameters for the set-difference of two relations")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to form difference of two compatible relations"
      :keys ((#\SUPER-O #\SUPER-D)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'RELATION-DIFFERENCE
      (SETQ keywords (LIST 'into into
     'from from 'tuples tuples
     'format format 'dir dir 'doc doc
     'key key 'imp imp 'sto sto
     'print print 'unique unique)))))
  (RELATION-DIFFERENCE keywords))
;**************************************************************************
;                DEFCOMMAND  FOR SET INTERSECTION                                  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC intersection) (from into tuples format
       dir doc key imp sto print unique
       &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'relation-intersection)
     (FORMAT NIL "  ~S"
      (CONS
        'relation-intersection
        (ARGLIST
          'relation-intersection))))
      :arguments (:user-supplied (:label "List of two relations:"
    :default NIL
    :type (:documentation
     "List of the names of two relations which will take part in the relation intersection operation. The attributes to be projected and a where clause can be specified for each relation using keywords. For instance, (REL1 (PROJECT <attr> WHERE <where-claue>) REL2 (WHERE <where-clause> PROJECT <attr>))." :SEXP))
   ,*ucl-into*
   (:label "Tuples?"
    :default NIL
    :type (:documentation
     "Specify if the resultant tuples be returned rather than inserted in a relation. The following parameters can be ignored if this is true."
     :boolean))
   ,*ucl-format*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-print*
   ,*ucl-unique*
  :LABEL "Parameters for the set-intersection of two relations")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to form intersection of two compatible relations"
      :keys ((#\SUPER-O #\SUPER-I)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'RELATION-INTERSECTION
      (SETQ keywords (LIST 'into into
     'from from 'tuples tuples
     'format format 'dir dir 'doc doc
     'key key 'imp imp 'sto sto
     'print print 'unique unique)))))
  (RELATION-INTERSECTION keywords))
;**************************************************************************
;                DEFCOMMAND  FOR AVERAGE     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC average) (relation attribute unique where by tuples
      &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'average)
     (FORMAT NIL "  ~S"
      (CONS
        'average
        (ARGLIST
          'average))))
      :arguments (:user-supplied (:label "Relation name:"
    :default *ui-relation*
    :type (:documentation
     "Name of the relation which contains the attribute to be averaged." :SEXP))
   ,*ucl-count-attr*
   ,*ucl-count-unique*
      ,*ucl-where*
   ,*ucl-by*
   ,*ucl-tuples*
  :LABEL "Parameters for average:")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to compute the average of the attribute values in a relation."
      :keys ((#\SUPER-O #\SUPER-A)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'AVERAGE relation attribute
      (SETQ keywords (LIST 'unique unique
     'where where 'by by 'tuples tuples)))))
  (AVERAGE relation attribute keywords))
;**************************************************************************
;                DEFCOMMAND  FOR SUM         *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC sum) (relation attribute unique where by tuples
  &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'sum)
     (FORMAT NIL "  ~S"
      (CONS
        'sum
        (ARGLIST
          'sum))))
      :arguments (:user-supplied (:label "Relation name:"
    :default *ui-relation*
    :type (:documentation
     "Name of the relation which contains the attribute to be summed." :SEXP))
   ,*ucl-count-attr*
   ,*ucl-count-unique*
   ,*ucl-where*
   ,*ucl-by*
   ,*ucl-tuples*
  :LABEL "Parameters for sum:")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to compute the sum of the attribute values in a relation."
      :keys ((#\SUPER-O #\SUPER-S)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'SUM relation attribute
      (SETQ keywords (LIST 'unique unique 'by by 'tuples tuples
     'where where)))))
  (SUM relation attribute keywords))
;**************************************************************************
;                DEFCOMMAND  FOR SIZE        *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC size) (relation unique where by tuples &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'size)
     (FORMAT NIL "  ~S"
      (CONS
        'size
        (ARGLIST
          'size))))
      :arguments (:user-supplied (:label "Relation name:"
    :default *ui-relation*
    :type (:documentation
     "Name of the relation whose size is required." :SEXP))
   ,*ucl-count-unique*
   ,*ucl-where*
   ,*ucl-by*
   ,*ucl-tuples*
  :LABEL "Parameters for size:")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to compute the size of the relation."
      :keys (#\SUPER-HYPER-S))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'SIZE relation
      (SETQ keywords (LIST 'unique unique 'by by 'tuples tuples
     'where where)))))
  (SIZE relation keywords))
;**************************************************************************
;                DEFCOMMAND  FOR COUNT-RTMS     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC count) (relation attribute unique where by tuples
         &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'count-rtms)
     (FORMAT NIL "  ~S"
      (CONS
        'count-rtms
        (ARGLIST
          'count-rtms))))
      :arguments (:user-supplied (:label "Relation name:"
    :default *ui-relation*
    :type (:documentation
     "Name of the relation which contains the attribute to be used to find the number of tuples." :SEXP))
   ,*ucl-count-attr*
   ,*ucl-count-unique*
   ,*ucl-where*
   ,*ucl-by*
   ,*ucl-tuples*
  :LABEL "Parameters for count:")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to compute the count of the attribute values in a relation."
      :keys ((#\SUPER-O #\SUPER-C)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'COUNT-RTMS relation attribute
      (SETQ keywords (LIST 'unique unique 'by by 'tuples tuples
     'where where)))))
  (COUNT-RTMS relation attribute keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MAXIMUM     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC maximum) (relation attribute where by tuples
      &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'maximum)
     (FORMAT NIL "  ~S"
      (CONS
        'maximum
        (ARGLIST
          'maximum))))
      :arguments (:user-supplied (:label "Relation name:"
    :default *ui-relation*
    :type (:documentation
     "Name of the relation which contains the attribute to be maximumd." :SEXP))
   ,*ucl-count-attr*
   ,*ucl-where*
   ,*ucl-by*
   ,*ucl-tuples*
  :LABEL "Parameters for maximum:")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to compute the maximum of the attribute values in a relation."
      :keys ((#\SUPER-O #\SUPER-M)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'MAXIMUM relation attribute
      (SETQ keywords (LIST 'where where 'by by 'tuples tuples)))))
  (MAXIMUM relation attribute keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MINIMUM     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC minimum) (relation attribute where by tuples
      &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'minimum)
     (FORMAT NIL "  ~S"
      (CONS
        'minimum
        (ARGLIST
          'minimum))))
      :arguments (:user-supplied (:label "Relation name:"
    :default *ui-relation*
    :type (:documentation
     "Name of the relation which contains the attribute to be minimumd." :SEXP))
   ,*ucl-count-attr*
   ,*ucl-where*
   ,*ucl-by*
   ,*ucl-tuples*
  :LABEL "Parameters for minimum:")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to compute the minimum of the attribute values in a relation."
      :keys (#\SUPER-HYPER-M))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'MINIMUM relation attribute
      (SETQ keywords (LIST 'where where 'by by 'tuples tuples)))))
  (MINIMUM relation attribute keywords))
;**************************************************************************
;                DEFCOMMAND  FOR HELP DBMS OBJECT                                  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC inspect-dbms-object) (object)
            `(:description "Information on any database object"
      :arguments (:user-supplied (:label "Database Object:"
    :default *ui-object*
    :type (:documentation
     "Specify a database object (COMMAND / RELATION / ATTRIBUTE)."
     :sexp))
  :LABEL "Help on the database object ->")
      :menus help
      :documentation "Used to inspect any database object."
      :keys (#\CONTROL-HELP))
  (SEND *output-window* :append-item
(FORMAT nil "(INSPECT-DBMS-OBJECT '~S)" object))
  (HELP-OBJECT object))
;**************************************************************************
;                DEFCOMMAND  FOR REFRESH OUTPUT WINDOW                             *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC clear-output-window) ()
    `(:description "Clear the entire output window"
      :menus display
      :keys (#\CLEAR-SCREEN))
  (SEND *output-window* :set-items nil)
  (FUNCALL *OUTPUT-WINDOW* :SCROLL-TO
   (- 2 (W:SHEET-NUMBER-OF-INSIDE-LINES *OUTPUT-WINDOW*))
   :RELATIVE))
;**************************************************************************
;                DEFCOMMAND  FOR SCROLL DOWN *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC scroll-forward) ()
   `(:description "scrolling forward in the output-window"
     :menus display
     :keys (#\CONTROL-V))
  (FUNCALL *OUTPUT-WINDOW* :SCROLL-TO
   (- (W:SHEET-NUMBER-OF-INSIDE-LINES *OUTPUT-WINDOW*) 2)
   :RELATIVE))
;**************************************************************************
;                DEFCOMMAND  FOR SCROLL UP   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC scroll-backward) ()
   `(:description "scrolling backward in the output-window"
     :menus display
     :keys (#\META-V))
  (FUNCALL *OUTPUT-WINDOW* :SCROLL-TO
   (- 2 (W:SHEET-NUMBER-OF-INSIDE-LINES *OUTPUT-WINDOW*))
   :RELATIVE))
;**************************************************************************
;                DEFCOMMAND  FOR SCROLL TO THE TOP                                 *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC scroll-to-top) ()
   `(:description "scrolling to the top in the output-window"
     :menus display
     :keys (#\META-<))
  (SEND *OUTPUT-WINDOW* :put-item-in-window
(SEND *OUTPUT-WINDOW* :item-of-number 0)))
;**************************************************************************
;                DEFCOMMAND  FOR SCROLL TO THE BOTTOM                              *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC scroll-to-bottom) ()
   `(:description "scrolling to the bottom in the output-window"
     :menus display
     :keys (#\META->))
  (SEND *OUTPUT-WINDOW* :put-last-item-in-window))
;**************************************************************************
;                DEFCOMMAND  FOR SCROLL TO A RELATION                              *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC scroll-to-a-relation) (relation &aux index)
     `(:description "Scroll to a particular relation"
       :arguments (:user-supplied (:label "Relation Name:"
    :default *ui-relation*
    :type (:documentation
     "Name of the relation to scroll to:"
     :sexp))
   :label "Scroll to the relation ==>")
       :menus display
       :keys (#\CONTROL-R))
  (IF (AND (SETQ index (GETP relation :index))
   (< index (LENGTH (SEND *output-window* :items))))
      (SEND *output-window* :put-item-in-window
    (SEND *output-window* :item-of-number index))
    (FORMAT *typeout-window* "~%The relation ~S is not in the output-window"
    relation)))
;**************************************************************************
;                DEFCOMMAND  FOR SEND OUTPUT TO A FILE                             *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC send-output-to-file) (file &AUX pathname)
     `(:description "Send the contents of the output window to a file."
       :arguments (:user-supplied (:label "File name:"
       :default *ui-file*
    :type (:documentation
     "Name of the file to send the output to:" :sexp))
   :label "Send the output window contents to:")
       :menus display
       :keys (#\HYPER-F))
  (UNWIND-PROTECT
      (SETQ pathname (CAR (ERRSET
  (OPEN (SETQ pathname file) :characters t
                     :direction :output        ;mrr 04.09.87
                     :if-does-not-exist :create) nil)))
    (IF (null pathname)
(FORMAT *typeout-window* "~S is a bad file." file)
      (MAPCAR (FUNCTION (LAMBDA (line &AUX item)
       (COND ((OR (STRINGP line)
  (NUMBERP line)
  (SYMBOLP line))
      (PRINC line pathname))
     ((LISTP line)
      (DOLIST (element line)
(COND ((OR (STRINGP element)
    (NUMBERP element)
    (SYMBOLP element))
       (PRINC element pathname))
      ((NULL (LISTP element)) nil)
      ((NULL (EQUAL (CAR element) :item1))
       (PRINC (CAR element) pathname))
      (T (SETQ item (CADR element))
  (PRINC
    (IF (LISTP item)
        (CAR item)
      item)
    pathname)
  )))))
       (TERPRI pathname)))
      (LISTARRAY (SEND *output-window* :items)))))
  (IF pathname
      (CLOSE pathname)))
;**************************************************************************
;                DEFCOMMAND  FOR INTRODUCTION                                      *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC introduction) ()
    `(:description "Introduction to this interface."
      :menus help
      :keys (#\META-HELP))
  (HELP))

;**************************************************************************
;                DEFCOMMAND  FOR SUB-MENU DBMS HELP                                *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC help) ()
    `(:description "Introduction to the interface. Help on any database object (COMMAND / RELATION / ATTRIBUTE)."
      :documentation "Introduction to the interface. Help on any database object (COMMAND/RELATION/ATTRIBUTE)."
      :menus system-menu)
  (LET ((command (SEND SELF :submenu-choose *help-submenu*)))
    (IF command (SEND command :execute SELF))))
;**************************************************************************
;                DEFCOMMAND  FOR SUB-MENU DBMS COMMANDS                            *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC command-menu) ()
    `(:description "Select a database command from a menu. A choose-variable-values window will be presented to get the arguments for that command."
      :documentation "Select a database command from a menu. A choose-variable-values window will be presented to get the arguments for that command."
      :menus system-menu
      :keys (#\mouse-r-1))
  (LET ((command (SEND SELF :submenu-choose *command-submenu*)))
    (IF command (SEND command :execute SELF))))
;**************************************************************************
;                DEFCOMMAND  FOR SUB-MENU DISPLAY COMMANDS                         *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC display) ()
    `(:description "Select an item from a menu to scroll in the output window."
      :documentation "Select an item from a menu to scroll in the output window."
      :menus system-menu)
  (LET ((command (SEND SELF :submenu-choose *display-submenu*)))
    (IF command (SEND command :execute SELF))))
;**************************************************************************
;                DEFCOMMAND  FOR EXIT        *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC kill) ()
     `(:description "To exit the interface by killing the process."
      :documentation "To exit the interface by killing the process."
      :menus system-menu
      :keys (#\SUPER-END))
  (SEND dbms-frame1 :kill)
  (SETQ dbms-frame1 nil))
;**************************************************************************
;                DEFCOMMAND  FOR QUIT        *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC exit) ()
     `(:description "To exit the interface by burying it."
      :documentation "To exit the interface by burying it."
      :menus system-menu
      :keys (#\END))
  (SEND dbms-frame1 :bury))
;**************************************************************************
;              Build the command table       *
;**************************************************************************
(SETQ dbms-comtab (MAKE-INSTANCE 'UCL:COMMAND-TABLE
     :name "Database command table"
     :documentation "database help"))
(UCL:BUILD-COMMAND-TABLE 'dbms-comtab 'dbms-rc
 '(help command-menu display kill exit
   delete-tuples destroy-attribute
                 destroy-database destroy-relation
  destroy-domain destroy-implementation destroy-index
  destroy-storage-structure destroy-view
  modify-database modify-transaction
  modify-domain modify-relation
  modify-attribute modify-index modify-view modify-tuples
   union intersection difference join
         retrieve-tuples select project
 commit-transaction average sum count size maximum
 minimum
   define-view define-database define-relation
         define-attribute define-environment
 define-implementation define-storage-structure
 define-domain define-transaction define-index
 attach-relation detach-relation insert-tuples
         load-database load-relation load-environment
   maptuple print-relation save-database save-relation
         save-environment save-transaction
         active-database environment-status
 rename-attribute rename-relation
 rename-database mapt abort-transaction
 begin-transaction  end-transaction
   inspect-dbms-object introduction
   scroll-forward clear-output-window
   scroll-to-top scroll-to-bottom
   scroll-backward scroll-to-a-relation send-output-to-file))
;**************************************************************************
;            Init method to define the submenus COMMAND-MENU HELP                 *
;            DISPLAY as part of the system menu.                                  *
;**************************************************************************

(DEFMETHOD (dbms-rc :after :init) (&rest ignore)
  (declare (special command-menu))
  (SETQ
   *help-submenu*  (MAKE-INSTANCE 'W:menu
   :pop-up t
   :dynamic t
   :superior W:mouse-sheet
                                  :item-list-pointer 'help)
   *command-submenu*  (MAKE-INSTANCE 'W:menu
      :pop-up t
      :dynamic t
      :multicolumn t
                                     :superior W:mouse-sheet
                                     :column-spec-list command-menu)
   *display-submenu* (MAKE-INSTANCE 'W:menu
        :pop-up t
     :dynamic t
                                    :superior W:mouse-sheet
                                    :item-list-pointer 'display))
  (SETQ *menupane* (SEND SELF :get-pane 's-m-pane))
  (SEND *menupane* :set-item-list-pointer 'system-menu)
  (SEND *menupane* :update-item-list)
  (SETQ *interaction* (FUNCALL self :get-pane 'i-pane)
      *output-window* (FUNCALL self :get-pane 'o-pane))
  (SEND self :set-selection-substitute rtms:*interaction*)
  )
;**************************************************************************
;  Build the submenus.                       *
;**************************************************************************
(UCL:BUILD-MENU 'system-menu 'dbms-rc
:item-list-order '(help kill command-menu exit display))
(UCL:BUILD-MENU 'help 'dbms-rc :item-list-order
'(introduction inspect-dbms-object))
(UCL:BUILD-MENU 'display 'dbms-rc :item-list-order
'(scroll-to-top scroll-backward clear-output-window
  send-output-to-file
  scroll-to-a-relation ucl:display-command-tables
  ucl:edit-command-tables scroll-forward scroll-to-bottom))
(PUTPROP 'command-menu '(dbms-comtab) 'ucl:items)
(UCL:BUILD-MENU 'command-menu 'dbms-rc
:item-list-order
'(define-database define-relation define-view define-attribute
  define-index define-environment define-domain define-transaction
  define-implementation define-storage-structure
  attach-relation detach-relation
  load-database load-relation load-environment
  insert-tuples delete-tuples modify-tuples
  modify-database modify-relation modify-attribute
  modify-index modify-domain modify-transaction modify-view
                  destroy-database destroy-relation destroy-attribute
  destroy-domain destroy-implementation destroy-index
  destroy-storage-structure destroy-view
  retrieve-tuples join union intersection difference
  select project commit-transaction
  average sum size count maximum minimum
  print-relation save-database save-relation
  save-environment save-transaction maptuple mapt
  active-database environment-status
  rename-attribute rename-relation
  rename-database abort-transaction
  begin-transaction end-transaction)
:column-list-order
'(("Definition" :FONT FONTS:hl12bi)
  ("Manipulation" :FONT FONTS:hl12bi)
  ("Operators" :FONT FONTS:hl12bi)
  ("Other Features" :FONT FONTS:hl12bi)))
;**************************************************************************
;         Define the variable to hold the instance of the application flavor.      *
;**************************************************************************
(SETQ dbms-frame1 nil)
;**************************************************************************
;         Method used to get input from submenus.                                  *
;**************************************************************************
(DEFMETHOD (dbms-rc :submenu-choose) (submenu)
  (LET ((sup (SEND submenu :superior)))
    (UNWIND-PROTECT
      (PROGN
(SEND (CAR (SEND *interaction* :blinker-list)) :set-visibility NIL)
(SEND submenu :set-superior W:mouse-sheet)
(SEND submenu :choose))
      (SEND submenu :set-superior sup)
      (SEND (CAR (SEND *interaction* :blinker-list)) :set-visibility :blink)
      )))
;**************************************************************************
;                              Some initializations                                *
;**************************************************************************
(DEFMETHOD (dbms-rc :before :command-loop) ()
    (SETQ *typeout-window* (FUNCALL *output-window* :typeout-window))
    (SEND *interaction* :clear-screen)
    (SEND *output-window* :clear-screen)
    (SEND *typeout-window* :set-io-buffer
  (SEND *interaction* :io-buffer)))
;**************************************************************************
;    Method to be executed before each time it enters the command-loop. Used       *
;    to refresh the output window if its typeout window is exposed.                *
;**************************************************************************

(DEFMETHOD (dbms-rc :before :fetch-and-execute) (&rest ignore)
  (DECLARE (SPECIAL ch))
  (IF (SEND *typeout-window* :active-p)
      (PROGN
(FORMAT *typeout-window* "~%")
(FORMAT *typeout-window* "~%")
(FORMAT *typeout-window*
w:*remove-typeout-standard-message*)   ;mrr 04.07.87
(SETQ ch (FUNCALL dbms-frame1 :any-tyi))
(SEND *output-window* :flush-typeout))))
;(SEND dbms-frame1 :set-basic-help '(help))
;(SEND dbms-frame1 :set-print-function 'NEW-PRINT)
(DEFUN NEW-PRINT (x &AUX ch)
  (IF (SEND *typeout-window* :active-p)
      (PROGN
(FORMAT *typeout-window* "~%")
(FORMAT *typeout-window* "~%")
(FORMAT *typeout-window*
w:*remove-typeout-standard-message*)   ;mrr 04.07.87
(SETQ ch (FUNCALL dbms-frame1 :any-tyi))
(SEND *output-window* :flush-typeout)))
  (SEND *output-window* :append-item (FORMAT nil "~S" x)))
(DEFMETHOD (dbms-rc :before :execute-command) (&rest ignore)
;  (setq ucl:inhibit-results-print? T)
  (IF (EQ ucl:input-mechanism 'ucl:typein)
      (SEND *output-window* :append-item (FORMAT nil "~S" -))))
(DEFMETHOD (dbms-rc :after :execute-command) (&rest ignore &AUX ch)
  (IF (SEND *typeout-window* :active-p)
      (PROGN
(FORMAT *typeout-window* "~%")
(FORMAT *typeout-window* "~%")
(FORMAT *typeout-window*
w:*remove-typeout-standard-message*)   ;mrr
(SETQ ch (FUNCALL dbms-frame1 :any-tyi))
(SEND *output-window* :flush-typeout)))
  '(MAPC #'(LAMBDA (val)
    (IF val
(PROGN
  (SEND *output-window* :append-item (FORMAT NIL "~S" val))
  (SEND *output-window* :put-last-item-in-window))))
//)
  )

;**************************************************************************
;      Sets the I/O streams the appropriate panes in the interface.                *
;**************************************************************************
(DEFMETHOD (dbms-rc :designate-io-streams) ()
  (DECLARE (special *standard-output* error-output debug-io
    *terminal-io*))
  (SETQ *terminal-io* *interaction*
*standard-output* *interaction*
error-output *typeout-window*
debug-io *typeout-window*))
;**************************************************************************
;      The function to be called from lisp-listener to get use the interface.      *
;**************************************************************************
(COMPILE-FLAVOR-METHODS dbms-rc)
(DEFUN Interface (&rest ignore)
  (IF (W:FIND-WINDOW-OF-FLAVOR 'RTMS:dbms-rc)
      dbms-frame1
    (SETQ dbms-frame1 (W:MAKE-WINDOW 'RTMS:dbms-rc)))
  (SEND dbms-frame1 :expose)
  (SEND *interaction* :select))
;**************************************************************************
;             Add the database interface to the system keys and system menu      *
;**************************************************************************
(DEFUN CREATE-KEYS ()
  (W:ADD-SYSTEM-KEY #\D 'RTMS:dbms-rc
     "Rtms Interface"
     '(RTMS:interface))
  (W:ADD-TO-SYSTEM-MENU-COLUMN :PROGRAMS
    "RTMS" '(RTMS:interface) "Rtms interface"))
(CREATE-KEYS)
;**************************************************************************
;              Function used to scroll down in the output window.                  *
;**************************************************************************
(DEFUN scroll-to-bottom ()
  (SEND *output-window* :append-item " ")
  (SEND *output-window* :put-last-item-in-window)
  (FUNCALL *OUTPUT-WINDOW* :SCROLL-TO
   (- (W:SHEET-NUMBER-OF-INSIDE-LINES *OUTPUT-WINDOW*) 2)
   :RELATIVE))
;**************************************************************************
;           Function used to print items in the output window.                     *
;**************************************************************************
(DEFUN DBMS-PRINTER (line arg stream item-no)
  (LET (item)  ;item was declared special locally in Rel 2 -mrr
  arg
  item-no
  (COND ((STRINGP line) (PRINC line stream))
((NUMBERP line) (PRINC line stream))
((SYMBOLP line) (PRINC line stream))
((LISTP line)
   (DOLIST (element line)
     (COND ((STRINGP element) (PRINC element stream))
   ((SYMBOLP element) (PRINC element stream))
   ((NUMBERP element) (PRINC element stream))
   ((NULL (LISTP element)) nil)
   ((NULL (EQUAL (CAR element) :item1))
    (IF (STRINGP (CAR element))
(PRINC (CAR element) stream)
(PRIN1 (CAR element) stream)))
   (T (SETQ item (CADR element))
      (FUNCALL stream :item1 item (CADDR element)
       #'(LAMBDA (item stream)
    (PRINC
      (IF (LISTP item)
   (CAR item)
        item)
      stream))))))))))
;**************************************************************************
;               Functions used to provide help on line-area scrolling.            *
;**************************************************************************
(DEFUN HELP-LINE-AREA (line &AUX item)
  (COND ((OR (STRINGP line)
     (NUMBERP line)
     (SYMBOLP line))
 (PRINC line *TYPEOUT-WINDOW*))
((LISTP line)
   (DOLIST (element line)
     (COND ((OR (STRINGP element)
(NUMBERP element)
(SYMBOLP element))
    (PRINC element *TYPEOUT-WINDOW*))
   ((NULL (LISTP element)) nil)
   ((NULL (EQUAL (CAR element) :item1))
    (IF (STRINGP (CAR element))
(PRINC (CADR element) *typeout-window*)
(PRIN1 (CADR element) *typeout-window*)))
   (T (SETQ item (CADR element))
      (PRINC
(IF (LISTP item)
    (CADR item)
  item)
*TYPEOUT-WINDOW*)
      )))
   (FORMAT *typeout-window* "~%")
   (FORMAT *typeout-window* "~%")
   (FORMAT *typeout-window*
w:*remove-typeout-standard-message*)   ;mrr 04.07.87
   (SEND dbms-frame1 :any-tyi)
   (SEND *output-window* :flush-typeout))))
(DEFUN HELP-LINE-AREA-DEL (line &AUX items item-number mod-relation
             mod-attributes num)
  (SETQ item-number (SEND *output-window* :number-of-item line))
  (MAPC (FUNCTION (LAMBDA (rel &AUX numbers)
  (IF (AND
(SETQ numbers (GETP (READ-FROM-STRING
      (STRING-APPEND *pkg-name*
       (CAR rel)))
    'items))
(>= item-number (CAR numbers))
(<= item-number (CADR numbers))
)
      (PROGN
(SETQ num numbers)
(SETQ mod-relation (READ-FROM-STRING
      (STRING-APPEND *pkg-name* (CAR rel))) ;mrr 04.06.87
      mod-attributes (CADR rel))
))))
(QTRIEVE 'system-relation
 *system-relation-attributes*
 '(relation-name attributes)
 *system-relation-key*
 t))
  (IF mod-relation
      (PROGN
(IF (W:MOUSE-CONFIRM "Delete the indicated tuple?")
    (PROGN
      (DOLIST (element line)
(IF (LISTP element)
    (SETQ items (APPEND items (CDR element)))))
      (IF (>
    (CADR
      (MULTIPLE-VALUE-LIST
      (DELETE-TUPLES mod-relation
      'where (CONS 'AND
    (MAPCAR (FUNCTION (LAMBDA (attr val)
       (LIST 'EQUAL (READ-FROM-STRING (STRING attr))
      `(QUOTE
         ,(READ-FROM-STRING val)))))
     mod-attributes
     items)))))
    0)
  (PROGN
    (SEND *output-window* :delete-item item-number)
    (PUTP mod-relation
     (LIST (CAR num) (- (CADR num) 1))
     'items)))
      )))))
(DEFUN HELP-LINE-AREA-MOD (line
   &AUX items item-number attribute-vars mod-tuple
   mod-relation mod-attributes blanks tuple-format tuple)
  (BLOCK nil
  (SETQ item-number (SEND *output-window* :number-of-item line))
  (MAPC (FUNCTION (LAMBDA (rel &AUX numbers)
  (IF (AND
(SETQ numbers (GETP (READ-FROM-STRING
      (STRING-APPEND *pkg-name*
       (CAR rel)))
    'items))
(>= item-number (CAR numbers))
(<= item-number (CADR numbers))
)
      (SETQ mod-relation (READ-FROM-STRING     ;mrr 04.06.87
      (STRING-APPEND *pkg-name* (CAR rel)))
    mod-attributes (CADR rel)))))
(QTRIEVE 'system-relation
 *system-relation-attributes*
 '(relation-name attributes)
 *system-relation-key*
 t))
  (IF mod-relation
      (PROGN
(DOLIST (element line)
  (IF (LISTP element)
      (PROGN
(SETQ tuple-format (APPEND tuple-format
     (LIST (LENGTH (CAR element)))))
(SETQ items (APPEND items (CDR element))))))
(SETQ blanks
      (MAKE-ARRAY
(+ 1 (LENGTH mod-attributes)
   (APPLY (FUNCTION +) tuple-format)) :type 'art-string
:initial-value 32))
(SETQ attribute-vars
      (MAPCAR (FUNCTION (LAMBDA (attr)
       (READ-FROM-STRING (STRING-APPEND "MOD" attr))))
      mod-attributes))
(MAPC (FUNCTION (LAMBDA (attr val)
                  (SET attr (READ-FROM-STRING val))))
      attribute-vars
      items)
(SETQ *line-area-values-modifiedp* nil)
(IF (CATCH 'abort
      (W:CHOOSE-VARIABLE-VALUES
(MAPCAR (FUNCTION (LAMBDA (var attr)
     (LIST var (STRING attr))))
attribute-vars
mod-attributes)
:label (FORMAT nil "Modify the relation: ~S" mod-relation)
:function 'line-area-domain-check
:margin-choices '("Do It" ("Abort" (THROW 'abort T)))))        ;mrr 04.06.87
    (setq  *line-area-values-modifiedp* NIL))
(IF *line-area-values-modifiedp*
    (PROGN
      (SETQ tuple (MAPCAR (FUNCTION (LAMBDA (x)
        `(QUOTE ,(SYMBOL-VALUE x))))
   attribute-vars))
      (SETQ mod-tuple
    (CAR (PRINT-TUPLE (LIST
  (MAPCAR (FUNCTION (LAMBDA (x)
        (eval `,x)))
   tuple))
       tuple-format nil T blanks nil)))
      (IF (>
    (CADR
      (MULTIPLE-VALUE-LIST
      (MODIFY mod-relation 'attributes mod-attributes
'values tuple
'where (CONS 'AND
        (MAPCAR (FUNCTION (LAMBDA (attr val)
      (LIST 'EQUAL (READ-FROM-STRING
       (STRING-APPEND *pkg-name*    ;mrr
        (STRING attr)))
     `(QUOTE
        ,(READ-FROM-STRING
           val)))))
         mod-attributes
         items)))))
    0)
  (PROGN
     (SEND *output-window* :delete-item item-number)
    (SEND *output-window* :insert-item item-number mod-tuple)))))))))
(DEFUN line-area-domain-check (&rest ignore)
  (BLOCK nil
;Later on, we will have to take the arguments window, variable, old-value, and
;new-value (see page 195. bottom) inorder to do the domain check for this attribute
;But we probably should not do the domain checking here because it will be done
;anyway in the modify routine.
(SETQ *line-area-values-modifiedp* T)
(RETURN nil)))


(DEFUN HELP-MODIFY (relation &rest ignore
    &AUX qtrieve)
  (DECLARE (SPECIAL new-rel relation
    old-att  new-add new-del new-ren old-add old-del old-ren
    old-imp  new-imp
    old-sto  new-sto
    old-key  new-key
    old-dir  new-dir
    old-doc  new-doc
    old-tup  new-tup))
  (BLOCK nil
    (SETQ qtrieve (CADR (GET-RELATION
relation
'(attributes save-directory doc tuple-format
  implementation-type storage-structure key)
T)))
    (COND ((NULL (CADR qtrieve))
   (IF *provide-error-messages*
       (FORMAT *STANDARD-OUTPUT*
     "~%ERROR - Relation ~s does not exist in the database ~s"
     relation *active-db*))
   (RETURN NIL)))
  (SETQ old-att (FIRST qtrieve)
old-dir (SECOND qtrieve)
old-doc (THIRD qtrieve)
old-tup (FOURTH qtrieve)
old-imp (FIFTH qtrieve)
old-sto (SIXTH qtrieve)
old-key (SEVENTH qtrieve) old-add NIL old-del NIL old-ren NIL)
  (SETQ new-dir old-dir new-doc old-doc new-rel relation
new-tup old-tup new-imp old-imp new-sto old-sto new-key old-key
new-add old-add new-del old-del new-ren old-ren)
  (IF (NOT (CATCH 'abort
     (W:CHOOSE-VARIABLE-VALUES
       `(
 (new-rel "Relation Name"
  :documentation "To change the relation name." :SEXP)
 ,(FORMAT nil "     Attributes: ~S" old-att)
 (new-add "Add attributes"
  :documentation "To add attributes specify attribute descriptor pair." :SEXP)
 (new-del "Delete attributes"
  :documentation "To delete attributes, specify a list of the attributes." :SEXP)
 (new-ren "Rename attributes"
  :documentation "To rename attributes, specify a list of the type <(old new)>." :SEXP)
 " "
 (new-imp "Implementation-type"
  :documentation "To change the type of implementation."
  :SEXP)
 (new-sto "Storage Structure"
  :documentation
  "To change the type of storage structure." :SEXP)
 (new-key "Key"
  :documentation "To change the key attributes."
  :SEXP)
 (new-doc "Documentation"
  :documentation "To change the relation documentation."
  :SEXP)
 (new-dir "Save Directory"
  :documentation
  "To change the directory in which this relation can be saved."
  :SEXP)
 (new-tup "Tuple format"
  :documentation
  "To change the format in printing the relation."
  :SEXP))
       :label (FORMAT nil "Change the features of ~S" relation)
       :margin-choices '("Do It" ("Abort" (THROW 'abort T))))))       ;mrr 04.06.87
      (PROGN
(SETQ qtrieve NIL)
(MAPC #'(LAMBDA (old new key)
  (IF (NOT (EVAL `(*EQUALP ,old ,new)))
      (SETQ qtrieve (APPEND qtrieve (LIST key (eval `,new))))))
      '(relation old-add old-del old-ren
 old-dir old-doc old-tup old-imp old-sto old-key)
      '(new-rel new-add new-del new-ren
new-dir new-doc new-tup new-imp new-sto new-key)
      '(rel add-attr delete-attr rename-attr dir doc format imp sto key))
(IF qtrieve
    (MODIFY-RELATION relation qtrieve)))))
  )


;;; fragments

;; me of the database to be loaded."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (
      :documentation "Name of the directory in which it is stored."
      :sexp))
 :label "Give parameters for LOAD DATABASE:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to load database from a given directory."
      :keys ((#\SUPER-L #\SUPER-D)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'LOAD-DATABASE database (LIST 'dir directory))))
  (LOAD-DATABASE database (LIST 'dir directory)))



;;**************************************************************************
;                DEFCOMMAND  FOR RESTORE ENVIRONMENT                               *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC load-environment) (environment directory)
`(:description ,(STRING-APPEND (DOCUMENTATION 'load-environment)
     (FORMAT NIL "  ~S"
      (CONS
        'load-environment
        (ARGLIST
          'load-environment))))
      :arguments (:user-supplied (:label "Environment Name:"
    :default *ui-database*
    :type (
      :documentation "Name of the environment to be loaded."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (
      :documentation "Name of the directory in which it is stored."
      :sexp))
 :label "Give parameters for LOAD ENVIRONMENT:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to load environment from a given directory."
      :keys ((#\SUPER-L #\SUPER-E)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'LOAD-ENVIRONMENT environment (LIST 'dir directory))))
  (LOAD-ENVIRONMENT environment (LIST 'dir directory)))
;**************************************************************************
;                DEFCOMMAND  FOR RESTORE RELATION                                  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC load-relation) (relation directory)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'load-relation)
     (FORMAT NIL "  ~S"
      (CONS
        'load-relation
        (ARGLIST
          'load-relation))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default *ui-relation*
    :type (
      :documentation "Name of the relation to be loaded."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (
      :documentation "Name of the directory in which it is saved."
                :sexp))
  :label "Give parameters for LOAD RELATION:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to load a relation from a given directory."
      :keys ((#\SUPER-L #\SUPER-R)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'LOAD-RELATION relation (LIST 'dir directory))))
  (LOAD-RELATION relation (LIST 'dir directory)))
;**************************************************************************
;                DEFCOMMAND  FOR SAVE DATABASE                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC save-database) (database directory)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'save-database)
     (FORMAT NIL "  ~S"
      (CONS
        'save-database
        (ARGLIST
          'save-database))))
      :arguments (:user-supplied (:label "Database Name:"
    :default *ui-database*
    :type (:documentation
       "Name of the database to be saved."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (:documentation
      "Name of the directory to write to."
      :sexp))
  :label "Give parameters for SAVE DATABASE:")
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Used to save a database on a given directory."
      :keys ((#\SUPER-S #\HYPER-D)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'SAVE-DATABASE database (LIST 'dir directory))))
  (SAVE-DATABASE database (LIST 'dir directory)))
;**************************************************************************
;                DEFCOMMAND  FOR SAVE ENVIRONMENT                                  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC save-environment) (environment directory)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'save-environment)
     (FORMAT NIL "  ~S"
      (CONS
        'save-environment
        (ARGLIST
          'save-environment))))
      :arguments (:user-supplied (:label "Environment Name:"
    :default nil
    :type (:documentation
       "Name of the environment to be saved."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (:documentation
      "Name of the directory to write to."
      :sexp))
  :label "Give parameters for SAVE environment:")
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Used to save an environment on a given directory."
      :keys ((#\SUPER-S #\SUPER-E)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'SAVE-ENVIRONMENT environment (LIST 'dir directory))))
  (SAVE-ENVIRONMENT environment (LIST 'dir directory)))
;**************************************************************************
;                DEFCOMMAND  FOR SAVE RELATION                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC save-relation) (relation directory type save
     &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'save-relation)
     (FORMAT NIL "  ~S"
      (CONS
        'save-relation
        (ARGLIST
          'save-relation))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default *ui-relation*
    :type (
      :documentation "Name of the relation to be saved."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (
      :documentation "Name of the directory to write to."
      :sexp))
   (:label "Type of SAVE:"
    :default *ui-type*
    :type (:documentation "Save type. It can be either XLD or COMMAND." ;mrr 03.31.87
     :sexp))
   (:label "Must Save:"
    :default nil
    :type (:documentation "Save the relation even if the relation has not been modified." :BOOLEAN))
 :label "Give parameters for SAVE RELATION:")
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Used to save a relation on a given directory."
      :keys ((#\SUPER-S #\SUPER-R)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'SAVE-RELATION relation
      (SETQ keywords (LIST 'type type 'dir directory
     'save save)))))
  (SAVE-RELATION relation keywords))
;**************************************************************************
;                DEFCOMMAND  FOR SAVE TRANSACTION                                  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC save-transaction) (transaction directory pathname
     &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'save-transaction)
     (FORMAT NIL "  ~S"
      (CONS
        'save-transaction
        (ARGLIST
          'save-transaction))))
      :arguments (:user-supplied (:label "Transaction Name:"
    :default *ui-transaction*
    :type (
      :documentation "Name of the transaction to be saved."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (
      :documentation "Name of the directory to write to."
      :sexp))
   (:label "Pathname:"
    :default *ui-file*
    :type (:documentation
     "The name of the file into which the transaction forms will be stored. It defaults to <transaction>.lisp"
     :SEXP))
 :label "Give parameters for SAVE TRANSACTION:")
      :menus ((command-menu :COLUMN "Other Features"))
      :documentation "Used to save a transaction on a given directory."
      :keys ((#\SUPER-S #\SUPER-T)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'SAVE-TRANSACTION transaction
      (SETQ keywords (LIST 'path pathname 'dir directory)))))
  (SAVE-TRANSACTION transaction keywords))

;**************************************************************************
;                DEFCOMMAND  FOR DEFINE IMPLEMENTATION                             *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-implementation) (implementation doc
   &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'define-implementation)
     (FORMAT NIL "  ~S"
      (CONS
        'define-implementation
        (ARGLIST 'define-implementation))))
      :arguments (:user-supplied (:label "Implementation Name:"
    :default nil
    :type (:documentation
      "Name of the implementation. Implementation-dependent routines are expected to be defined by the user."
      :sexp))
   (:label "Documentation:"
    :default *ui-doc*
    :type (:documentation
      "Documentation for the implementation."
      :string))
  :label "Give parameters for DEFINE IMPLEMENTATION:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define an implementation."
      :keys ((#\SUPER-D #\SUPER-I)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'DEFINE-IMPLEMENTATION implementation
      (SETQ keywords (LIST 'doc doc
     )))))
  (DEFINE-IMPLEMENTATION implementation keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE INDEX                                      *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-index) (relation-name index-name key-attributes storage-structure priority
  doc &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'define-index)
     (FORMAT NIL "  ~S"
      (CONS
        'define-index
        (ARGLIST 'define-index))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default nil
    :type (:documentation
      "Name of the relation upon which the index will be defined."
      :sexp))
   (:label "Index Name:"
    :default nil
    :type (:documentation
      "Name of the index to be defined."
      :string))
   (:label "Key Attributes:"
    :default nil
    :type (:documentation
      "List of attribute names which form the key for this index."
      :sexp))
   (:label "Storage Structure:"
    :default "AVL"
    :type (:documentation
      "The storage structure used to define the index."
      :string))
   (:label "Priority:"
    :default 10
    :type (:documentation
      "A numerical value which indicates the priority given to this index. 1 is the highest priority."
      :number))
   (:label "Documentation:"
    :default *ui-doc*
    :type (:documentation
      "Documentation for the index."
      :string))
  :label "Give parameters for DEFINE INDEX:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define a secondary index on a relation."
      :keys ((#\SUPER-D #\HYPER-I)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'DEFINE-INDEX relation-name
      (SETQ keywords (LIST 'name index-name 'key key-attributes 'sto storage-structure
     'priority priority 'doc doc
     )))))
  (DEFINE-INDEX relation-name keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY INDEX                                      *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-index) (relation-name index-name new-index-name
  key-attributes storage-structure priority
  doc &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-index)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-index
        (ARGLIST 'modify-index))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default nil
    :type (:documentation
      "Name of the relation upon which the index to be modified is defined."
      :sexp))
   (:label "Index Name:"
    :default nil
    :type (:documentation
      "Name of the index to be modified."
      :string))
   (:label "New Index Name:"
    :default nil
    :type (:documentation
      "New name of the index."
      :string))
   (:label "Key Attributes:"
    :default nil
    :type (:documentation
      "List of attribute names which form the key for this index."
      :sexp))
   (:label "Storage Structure:"
    :default nil
    :type (:documentation
      "The storage structure used to define the index."
      :string))
   (:label "Priority:"
      :default 10
    :type (:documentation
      "A numerical value which indicates the priority given to this index. 1 is the highest priority."
      :number))
   (:label "Documentation:"
    :default *ui-doc*
    :type (:documentation
      "Documentation for the index."
      :string))
  :label "Give parameters for DEFINE INDEX:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to define a secondary index on a relation."
      :keys ((#\SUPER-M #\HYPER-I)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'MODIFY-INDEX relation-name index-name
      (SETQ keywords (LIST 'new-name new-index-name 'key key-attributes 'sto storage-structure
     'priority priority 'doc doc
     )))))
  (MODIFY-INDEX relation-name index-name keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE STORAGE-STRUCTURE                          *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-storage-structure) (storage-structure doc
   &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'define-storage-structure)
     (FORMAT NIL "  ~S"
      (CONS
        'define-storage-structure
        (ARGLIST 'define-storage-structure))))
      :arguments (:user-supplied (:label "Storage structure name:"
    :default nil
    :type (:documentation
      "Name of the storage structure. Storage-structure-dependent routines are expected to be defined by the user."
      :sexp))
   (:label "Documentation:"
    :default nil
    :type (:documentation
      "Documentation for the storage structure."
      :string))
  :label "Give parameters for DEFINE STORAGE STRUCTURE:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define a storagestructure."
      :keys ((#\SUPER-D #\SUPER-S)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'DEFINE-STORAGE-STRUCTURE storage-structure
      (SETQ keywords (LIST 'doc doc
     )))))
  (DEFINE-STORAGE-STRUCTURE storage-structure keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE DOMAIN                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-domain) (domain def doc format
   &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'define-domain)
     (FORMAT NIL "  ~S"
      (CONS
        'define-domain
        (ARGLIST 'define-domain))))
      :arguments (:user-supplied (:label "Domain Name:"
    :default nil
    :type (:documentation
      "Name of the domain. Domain predicate is expected to be defined prior to this."
      :sexp))
   (:label "Default value:"
    :default nil
    :type (:documentation
     "Default value for this domain."
     :sexp))
   (:label "Documentation:"
    :default nil
    :type (:documentation
      "Documentation for the domain."
      :string))
   (:label "Default width :"
    :default nil
    :type (:documentation
      "The default width to be used for this domain."
      :sexp))
  :label "Give parameters for DEFINE DOMAIN:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define a domain."
      :keys (#\SUPER-HYPER-D))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'DEFINE-DOMAIN domain
      (SETQ keywords (LIST 'default def
     'doc doc
     'format format)))))
  (DEFINE-DOMAIN domain keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY DOMAIN                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-domain) (domain def doc format
   &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-domain)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-domain
        (ARGLIST 'modify-domain))))
      :arguments (:user-supplied (:label "Domain Name:"
    :default nil
    :type (:documentation
      "Name of the domain to be modified."
      :sexp))
   (:label "Default value:"
    :default nil
    :type (:documentation
     "New default value for this domain."
     :sexp))
   (:label "Documentation:"
    :default nil
    :type (:documentation
      "New documentation for the domain."
      :string))
   (:label "Default width :"
    :default nil
    :type (:documentation
      "The new default width to be used for this domain."
      :sexp))
  :label "Give parameters for MODIFY DOMAIN:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to modify a domain."
      :keys ((#\SUPER-M #\SUPER-D)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
  (LIST 'MODIFY-DOMAIN domain
      (SETQ keywords (LIST 'default def
     'doc doc
     'format format)))))
  (MODIFY-DOMAIN domain keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE TRANSACTION                                *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-transaction) (transaction forms dir path
      &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'define-transaction)
     (FORMAT NIL "  ~S"
      (CONS
        'define-transaction
        (ARGLIST 'define-transaction))))
      :arguments (:user-supplied (:label "Transaction Name:"
    :default *ui-transaction*
    :type (:documentation
      "Name of the transaction."
      :sexp))
   (:label "Database calls:"
    :default nil
    :type (:documentation
     "A list of database calls."
     :sexp))
   ,*ucl-dir*
   (:label "Pathname :"
    :default *ui-file*
    :type (:documentation
      "The default file in which it will be saved."
      :SEXP))
  :label "Give parameters for DEFINE TRANSACTION:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define a transaction."
      :keys ((#\SUPER-D #\SUPER-T)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'DEFINE-TRANSACTION transaction forms
      (SETQ keywords (LIST 'dir dir
     'path path)))))
  (DEFINE-TRANSACTION transaction forms keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY TRANSACTION                                *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-transaction) (transaction dir path
      &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-transaction)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-transaction
        (ARGLIST 'modify-transaction))))
      :arguments (:user-supplied (:label "Transaction Name:"
    :default *ui-transaction*
    :type (:documentation
      "Name of the transaction to be modified."
      :sexp))
   (:label "Directory:"
    :default *ui-directory*
    :type (:documentation
      "Default directory in which it can be found, if not in memory."
      :SEXP))
   (:label "Pathname :"
    :default *ui-file*
    :type (:documentation
      "The default file in which it can be found, if not in memory."
      :SEXP))
  :label "Give parameters for MODIFY TRANSACTION:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to modify a transaction."
      :keys ((#\SUPER-M #\SUPER-T)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'MODIFY-TRANSACTION transaction
      (SETQ keywords (LIST 'dir dir
     'path path)))))
  (MODIFY-TRANSACTION transaction keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE DATABASE                                   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-database) (database directory doc env
     &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'define-database)
     (FORMAT NIL "  ~S"
      (CONS
        'define-database
        (ARGLIST 'define-database))))
      :arguments (:user-supplied (:label "Database Name:"
    :default nil
    :type (:documentation
      "Name of the database."
      :sexp))
   (:label "Directory Name:"
    :default *ui-directory*
    :type (:documentation
     "Name of the save directory for this database."
     :sexp))
   (:label "Documentation:"
    :default *ui-doc*
    :type (:documentation
      "Documentation for the database."
      :string))
   (:label "Environment:"
    :default nil
    :type (:documentation
      "Name of the environment to be used to replace the default settings."
      :sexp))
  :label "Give parameters for DEFINE DATABASE:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define a database in a given directory."
      :keys ((#\SUPER-D #\SUPER-D)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'DEFDB database
      (SETQ keywords (LIST 'dir directory
     'doc doc
     'environment env)))))
  (DEFDB database keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY DATABASE                                   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-database) (database new-database directory doc
     &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-database)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-database
        (ARGLIST 'modify-database))))
      :arguments (:user-supplied (:label "Database Name:"
    :default nil
    :type (:documentation
      "Name of the database."
      :sexp))
   (:label "New Database Name:"
    :default nil
    :type (:documentation
      "If the database is to be renamed specify the new name."
      :sexp))
   (:label "Directory Name:"
    :default NIL
    :type (:documentation
     "To change the save directory for this database specify a new directory."
     :sexp))
   (:label "Documentation:"
    :default NIL
    :type (:documentation
      "New documentation for the database."
      :string))
  :label "Give parameters for MODIFY DATABASE:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to modify the features a database."
      :keys ((#\SUPER-M #\HYPER-D)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'MODIFY-DATABASE database
      (SETQ keywords (LIST 'database-name new-database
      'dir directory
     'doc doc
     )))))
  (MODIFY-DATABASE database keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY ATTRIBUTE                                  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-attribute) (relation attr new-attr def doc format
     &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-attribute)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-attribute
        (ARGLIST 'modify-attribute))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default *ui-relation*
    :type (:documentation
      "Name of the relation."
      :sexp))
   (:label "Attribute Name:"
    :default nil
    :type (:documentation
      "Name of the attribute."
      :sexp))
   (:label "New Attribute Name:"
    :default nil
    :type (:documentation
      "If the attribute is to be renamed specify the new name."
      :sexp))
   (:label "Default Value:"
    :default NIL
    :type (:documentation
     "To change the default value of this attribute specify a new value."
     :sexp))
   (:label "Documentation:"
    :default NIL
    :type (:documentation
      "New documentation for the attribute."
      :string))
   (:label "Default width :"
    :default nil
    :type (:documentation
      "The new default width to be used for this attribute."
      :sexp))
  :label "Give parameters for MODIFY ATTRIBUTE:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to modify the features a attribute."
      :keys ((#\SUPER-M #\SUPER-A)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'MODIFY-ATTRIBUTE relation attr
      (SETQ keywords (LIST 'attribute-name new-attr
     'def def
     'doc doc 'format format
     )))))
  (MODIFY-ATTRIBUTE relation attr keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY VIEW *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-view) (view def doc
     &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-view)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-view
        (ARGLIST 'modify-view))))
      :arguments (:user-supplied (:label "View Name:"
    :default NIL
    :type (:documentation
      "Name of the view."
      :sexp))
   (:label "View Definition:"
    :default nil
    :type (:documentation
      "New definition of the view."
      :sexp))
   (:label "Documentation:"
    :default NIL
    :type (:documentation
      "New documentation for the view."
      :string))
  :label "Give parameters for MODIFY VIEW:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to modify the features a view."
      :keys ((#\SUPER-M #\SUPER-V)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'MODIFY-VIEW view
      (SETQ keywords (LIST
     'view-def def
     'view-doc doc
     )))))
  (MODIFY-VIEW view keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY RELATION                                   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-relation) (rel new-rel add-att del-att ren-att
     imp sto format key dir doc
     &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-relation)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-relation
        (ARGLIST 'modify-relation))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default *ui-relation*
    :type (:documentation
      "Name of the Relation."
      :sexp))
   (:label "New Relation Name:"
    :default nil
    :type (:documentation
      "If the relation is to be renamed specify the new name."
      :sexp))
   (:label "Add attributes:"
    :default NIL
    :type (:documentation
     "Specify a list of attribute-descriptor pairs for attributes to be added to this relation."
     :sexp))
   (:label "Delete attributes:"
    :default NIL
    :type (:documentation
     "Specify a list of attributes in this relation which are to be deleted."
     :sexp))
   (:label "Rename attributes:"
    :default NIL
    :type (:documentation
     "To rename some of the attributes provide a list of the form (<old-attribute new-attribute>)."
     :sexp))
   (:label "Implementation Type:"
    :default NIL
    :type (:documentation
     "To change the implementation type of this relation specify a new value."
     :sexp))
   (:label "Storage structure:"
    :default NIL
    :type (:documentation
     "To change the storage structure of this relation specify a new value."
     :sexp))
   (:label "Format:"
    :default NIL
    :type (:documentation
     "To change the format for this relation specify a new format as a list of values."
     :sexp))
   (:label "Key:"
    :default NIL
    :type (:documentation
     "To change the key for this relation specify a new key as a list of attributes."
     :sexp))
   (:label "Directory Name:"
    :default NIL
    :type (:documentation
        "To change the save directory for this relation specify a new directory."
     :sexp))
   (:label "Documentation:"
    :default NIL
    :type (:documentation
      "New documentation for the relation."
      :string))
  :label "Give parameters for MODIFY RELATION:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to modify the features a relation."
      :keys ((#\SUPER-M #\SUPER-R)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'MODIFY-RELATION rel
      (SETQ keywords (LIST 'relation new-rel
     'add-attributes add-att
     'delete-attributes del-att
     'rename-attributes ren-att
     'imp imp
     'sto sto
     'format format
     'key key
     'doc doc
     'dir dir
     )))))
  (MODIFY-RELATION rel keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE ENVIRONMENT                                *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-environment) (environment save dir err par-check
        rel-imp rel-sto status sys-imp
        sys-sto val-check warn
        &AUX keywords)
    `(:description ,(STRING-APPEND (DOCUMENTATION 'define-environment)
     (FORMAT NIL "  ~S"
      (CONS
        'define-environment
        (ARGLIST 'define-environment))))
      :arguments (:user-supplied (:label "Environment Name:"
    :default nil
    :type (:documentation
      "Name of the environment."
      :sexp))
   (:label "Auto save:"
    :default nil
    :type (:documentation
     "Automatically saves all the modified relations after each function." :boolean))
   ,*ucl-dir*
   (:label "Errors:"
    :default T
    :type (:documentation
      "Controls the printing of the error messages."
      :boolean))
   (:label "Parameter Checking:"
    :default T
    :type (:documentation
      "Controls the checking of the parameters."
      :boolean))
   (:label "Relation Implementation:"
    :default *ui-imp*
    :type (:documentation
      "Default implementation of the user relations."
      :sexp))
   (:label "Relation storage structure:"
    :default *ui-ss*
    :type (:documentation
      "Default storage structure for the user relations."
      :sexp))
   (:label "Status:"
    :default T
    :type (:documentation
      "Controls the printing of the status messages."
      :boolean))
   (:label "System Implementation:"
    :default nil
    :type (:documentation
      "Default implementation of the system relations. Can not change this when a database is active."
      :sexp))
   (:label "System storage structure:"
    :default nil
    :type (:documentation
      "Default storage structure for the system relations. Can not change this when a database is active."
      :sexp))
   (:label "Validity Checking:"
    :default T
    :type (:documentation
      "Controls the checking of the values during insertion and modification for validity."
      :boolean))
   (:label "Warnings:"
    :default T
     :type (:documentation
      "Controls the printing of the warning messages."
      :boolean))
  :label "Give parameters for DEFINE ENVIRONMENT:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define an environment in a given directory."
      :keys ((#\SUPER-D #\SUPER-E)))
  (SEND *output-window*
:append-item
(FORMAT nil "~S"
(LIST 'DEFENV environment
      (SETQ keywords (IF *active-db*
   (LIST 'auto-save save 'para par-check
     'dir dir 'rel-imp rel-imp 'rel-sto
     rel-sto 'errors err 'status status
     'validity val-check 'warnings warn)
        (LIST 'auto-save save 'para par-check
     'dir dir 'rel-imp rel-imp 'rel-sto
     rel-sto 'errors err 'status status
     'sys-imp sys-imp 'sys-sto sys-sto
     'validity val-check 'warnings warn))))))
  (DEFENV environment keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE RELATION                                   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-relation) (relation attr-des tup
     dir doc key imp ss &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'define-relation)
     (FORMAT NIL "  ~S"
      (CONS
        'define-relation
        (ARGLIST 'define-relation))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default nil
    :type (:documentation
     "Name of the relation to be defined."
     :sexp))
   ,*ucl-attr-desc*
   ,*ucl-format*
   ,*ucl-dir*
   ,*ucl-doci*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
     :label "Give parameters for DEFINE RELATION:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "used to define a relation."
      :keys ((#\SUPER-D #\SUPER-R)))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(LIST 'DEFREL
      relation attr-des
      (SETQ keywords
    (LIST 'tuple-format tup 'dir dir 'doc doc
  'key key 'imp imp 'sto ss)))))
  (DEFREL relation attr-des keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE VIEW *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-view) (viewname view-definition doc)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'define-view)
     (FORMAT NIL "  ~S"
      (CONS
        'define-view
        (ARGLIST 'define-view))))
      :arguments (:user-supplied (:label "View Name:"
    :default nil
    :type (:documentation
       "Specify a name for the view."
     :sexp))
   (:label "View Definition:"
    :default *ui-viewdef*
    :type (:documentation
       "Specify a definition for the view."
     :sexp))
   (:label "View Documentation:"
    :default nil
    :type (:documentation
       "Specify documentation for the view."
     :sexp))
 :label "Give parameters for DEFINE VIEW:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to define a view."
      :keys ((#\SUPER-D #\SUPER-V)))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(LIST 'DEFVIEW viewname view-definition doc)))
  (DEFVIEW viewname view-definition doc))
;**************************************************************************
;                DEFCOMMAND  FOR DEFINE ATTRIBUTE                                  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC define-attribute) (relation-name attr-des key
      &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'define-attribute)
     (FORMAT NIL "  ~S"
      (CONS
        'define-attribute
        (ARGLIST 'define-attribute))))
      :arguments (:user-supplied (:label "Relation name: "
    :default *ui-relation*
    :type (:documentation
       "The name of the relation to which new attributes are to be added." :SEXP))
   ,*ucl-attr-desc*
   (:label "Key: "
    :default nil
    :type (:documentation
       "New key for the relation if it is to be different from the previous value. Specify a list of attributes."
       :SEXP))
 :label "Give parameters for DEFINE ATTRIBUTE:")
      :menus ((command-menu :COLUMN "Definition"))
      :documentation "Used to add attributes to relations."
      :keys ((#\SUPER-D #\SUPER-A)))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(LIST 'DEFINE-ATTRIBUTE relation-name attr-des
      (SETQ keywords (LIST 'key key)))))
  (DEFINE-ATTRIBUTE relation-name attr-des keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MODIFY TUPLES                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC modify-tuples) (relation where-clause attributes values
     &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'modify-tuples)
     (FORMAT NIL "  ~S"
      (CONS
        'modify-tuples
        (ARGLIST 'modify-tuples))))
      :arguments (:user-supplied (:label "Relation: "
    :default *ui-relation*
    :type (:documentation
       "Specify the relation whose tuples are to be modified."
     :sexp))
   ,*ucl-where*
   (:label "Attributes: "
    :default *ui-attributes*
    :type (:documentation
       "Specify a list of attributes in the above relation to be modified." :sexp))
   (:label "Values: "
    :default *ui-values*
    :type (:documentation
       "Specify a corresponding list of values to modify the above attributes." :sexp))
 :label "Give parameters for MODIFY TUPLES ==>")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to modify tuples in a relation."
      :keys ((#\SUPER-M #\HYPER-M)))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(LIST 'MODIFY relation (SETQ keywords (LIST 'where where-clause
       'attr attributes
       'values values)))))
  (MODIFY relation keywords))
;**************************************************************************
;                DEFCOMMAND  FOR DELETE TUPLES                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC delete-tuples) (relation where-clause)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'delete-tuples)
     (FORMAT NIL "  ~S"
      (CONS
        'delete-tuples
        (ARGLIST 'delete-tuples))))
      :arguments (:user-supplied (:label "Relation: "
    :default *ui-relation*
    :type (:documentation
       "Specify a relation whose tuples are to be deleted."
     :sexp))
   (:label "Where clause: "
    :default nil
    :type (:documentation
       "Deletes the tuples which satisfy this condition."
     :sexp))
 :label "Give parameters for DELETE TUPLES ==>")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to delete tuples in a relation."
      :keys (#\HYPER-D))
  (SEND *output-window* :append-item (FORMAT nil "~S"
(LIST 'DELETE-TUPLES relation (LIST 'where where-clause))))
  (DELETE-TUPLES  relation (LIST 'where where-clause)))
;**************************************************************************
;                DEFCOMMAND  FOR RETRIEVE TUPLES                                   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC retrieve-tuples) (relation attributes where-clause
     into dir doc key imp sto
     qprint to-file sort
     format wide number print
     tuples qsort stream unique index-name
     &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'retrieve)
       (FORMAT NIL "  ~S"
      (CONS
        'retrieve
        (ARGLIST 'retrieve))))
      :arguments (:user-supplied ,*ucl-retrieve-rel*
   ,*ucl-attributes*
   ,*ucl-where*
   ,*ucl-into*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-qprint*
   ,*ucl-out*
   ,*ucl-sort*
   ,*ucl-format*
   ,*ucl-wide*
   ,*ucl-num*
   ,*ucl-print*
   ,*ucl-tuples*
   ,*ucl-quick-sort*
   ,*ucl-stream*
   ,*ucl-unique*
    ,*ucl-index-name*
 :label "Give parameters for RETRIEVE TUPLES ==>")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to Retrieve tuples in a relation."
      :keys (#\HYPER-R))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'RETRIEVE
      relation
      (SETQ keywords
    (LIST 'project
   (IF (EQUAL attributes T)
       NIL
     attributes)
   'where where-clause 'into into
   'dir dir 'doc doc 'key key 'imp imp 'sto sto
    'qprint (NOT qprint) 'output-to-file to-file
   'sort sort 'format format
   'wide wide 'num number
   'print print 'tuples tuples
   'quick-sort qsort 'stream stream
   'unique unique 'index-name index-name)))))
  (RETRIEVE relation keywords))
;**************************************************************************
;                DEFCOMMAND  FOR SELECT TUPLES                                     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC select) (relation where-clause
     into dir doc key imp sto
     qprint to-file sort
     format wide number print
     tuples qsort stream unique index-name
     &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'select-tuples)
     (FORMAT NIL "  ~S"
      (CONS
        'select-tuples
        (ARGLIST 'select-tuples))))
      :arguments (:user-supplied ,*ucl-retrieve-rel*
   ,*ucl-where*
   ,*ucl-into*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-qprint*
   ,*ucl-out*
   ,*ucl-sort*
   ,*ucl-format*
   ,*ucl-wide*
   ,*ucl-num*
   ,*ucl-print*
   ,*ucl-tuples*
   ,*ucl-quick-sort*
   ,*ucl-stream*
   ,*ucl-unique*
   ,*ucl-index-name*
 :label "Give parameters for SELECT TUPLES ==>")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to Select tuples in a relation."
      :keys ((#\SUPER-R #\SUPER-S)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'SELECT-TUPLES
      relation
      (SETQ keywords
    (LIST
   'where where-clause 'into into
   'dir dir 'doc doc 'key key 'imp imp 'sto sto
   'qprint (NOT qprint) 'output-to-file to-file
   'sort sort 'format format
   'wide wide 'num number
   'print print 'tuples tuples
   'quick-sort qsort 'stream stream
   'unique unique 'index-name index-name)))))
  (RETRIEVE relation (APPEND (LIST 'project nil) keywords)))
;**************************************************************************
;                DEFCOMMAND  FOR PROJECT TUPLES                                    *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC Project) (relation attributes
      into dir doc key imp sto
      qprint to-file sort
      format wide number print tuples
      qsort stream unique
      &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'project)
     (FORMAT NIL "  ~S"
      (CONS
        'project
        (ARGLIST
          'project))))
      :arguments (:user-supplied ,*ucl-retrieve-rel*
   ,*ucl-attributes*
   ,*ucl-into*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-qprint*
   ,*ucl-out*
   ,*ucl-sort*
   ,*ucl-format*
   ,*ucl-wide*
   ,*ucl-num*
   ,*ucl-print*
   ,*ucl-tuples*
   ,*ucl-quick-sort*
   ,*ucl-stream*
   ,*ucl-unique*
 :label "Give parameters for PROJECT TUPLES ==>")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to Project tuples in a relation."
      :keys ((#\SUPER-R #\SUPER-P)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'PROJECT
      relation
      (SETQ keywords
    (LIST 'project (IF (EQUAL attributes T)
   nil
        attributes)
    'into into 'dir dir 'doc doc 'key key 'imp imp 'sto sto
    'qprint (NOT qprint) 'output-to-file to-file
    'sort sort 'format format
    'wide wide 'num number 'print print 'tuples tuples
    'quick-sort qsort 'stream stream 'unique unique)))))
  (RETRIEVE relation (APPEND (LIST 'where t) keywords)))
;**************************************************************************
;                DEFCOMMAND  FOR COMMIT TRANSACTION                                *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC commit-transaction) (trans dir path &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'commit-transaction)
     (FORMAT NIL "  ~S"
      (CONS
        'commit-transaction
        (ARGLIST
          'commit-transaction))))
      :arguments (:user-supplied (:label "Name of the transaction :"
    :default *ui-transaction*
    :type (:documentation
       "The name of an existing transaction." :SEXP))
   (:label "Name of the directory:"
    :default *ui-directory*
    :type (:documentation
       "Name of the directory which contains the transaction file, if the transaction is not in the memory." :SEXP))
   (:label "Pathname:"
    :default *ui-file*
    :type (:documentation
    "If the transaction is not in memory, provide the pathname for the transaction file. It defaults to <transaction>.lisp." :SEXP))
 :label "Give parameters for COMMIT TRANSACTION")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Commit a transaction - execute all the database calls in it."
      :keys ((#\SUPER-T #\SUPER-C)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'COMMIT-TRANSACTION trans (SETQ keywords
         (LIST 'dir dir
        'path path)))))
  (COMMIT-TRANSACTION trans keywords)
)
;**************************************************************************
;                DEFCOMMAND  FOR JOIN        *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC join) (into from project where
      tuples format dir doc key imp sto
             print unique &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'join)
     (FORMAT NIL "  ~S"
      (CONS
        'join
        (ARGLIST
          'join))))
      :arguments (:user-supplied (:label "Output relation :"
    :default *ui-join-into*
    :type (:documentation
       "If not provided, the result of JOIN is stored in a temporary relation unless only the resultant tuples are requested." :SEXP))
   (:LABEL "FROM :"
    :DEFAULT *ui-from*
    :TYPE (:DOCUMENTATION
     "Specify a list of two relations to be joined." :SEXP))
   (:label "Project :"
    :default NIL
    :type (:documentation
       "This gives the attributes in the output relation. Example: (rel1.* a3 (rel2.a1 a4)) ==> All the attributes in rel1, attribute A3 of rel2 and atribute A1 of rel2 renamed as A4." :SEXP))
   (:label "Where :"
    :default *ui-over*
    :type (:documentation
     "The join clause using the theta-operators. It is a where clause consisting of attributes from the relations being joined." :SEXP))
   (:label "Tuples?"
    :default NIL
    :type (:documentation
     "Specify if the resultant tuples be returned rather than inserted in a relation. The following parameters can be ignored if this is true."
     :boolean))
   ,*ucl-format*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-print*
   ,*ucl-unique*
 :label "Give parameters for JOIN")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to join relations."
      :keys (#\SUPER-J))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'JOIN 'from from
      (SETQ keywords (LIST 'project project
     'into into
     'tuples tuples
     'format format
     'dir dir
     'doc doc
     'key key
     'imp imp
     'sto sto
     'print print
     'where where 'unique unique)))))
  (JOIN-INTERNAL (APPEND (LIST 'from from) keywords))
)
;**************************************************************************
;                DEFCOMMAND  FOR DESTROY DATABASE                                 *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC destroy-database) (database disk &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-database)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-database
        (ARGLIST
          'destroy-database))))
      :arguments (:user-supplied (:label "Database Name:"
    :default nil
    :type (:documentation
       "Name of the database to be destroyed." :SEXP))
   (:label "Delete from the DISK:"
    :default NIL
    :type (:documentation
     "IF YES all the files pertaining to this database are deleted but NOT EXPUNGED." :BOOLEAN))
 :label "Give parameters for DESTROY DATABASE:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy databases"
      :keys ((#\SUPER-K #\SUPER-D)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-DATABASE database
      (SETQ keywords (LIST 'disk disk)))))
  (DESTROY-DATABASE database keywords)
)
;**************************************************************************
;                DEFCOMMAND  FOR DESTROY DOMAIN                                    *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC destroy-domain) (domain)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-domain)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-domain
        (ARGLIST
          'destroy-domain))))
      :arguments (:user-supplied (:label "Domain Name:"
    :default nil
    :type (:documentation
       "Name of the domain to be destroyed." :SEXP))
 :label "Give parameters for DESTROY DOMAIN:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy domains."
      :keys (#\SUPER-HYPER-K))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-DOMAIN domain)))
  (DESTROY-DOMAIN domain))
;**************************************************************************
;                DEFCOMMAND  FOR DESTROY IMPLEMENTATION                            *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC destroy-implementation) (implementation)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-implementation)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-implementation
        (ARGLIST
          'destroy-implementation))))
      :arguments (:user-supplied (:label "Implementation Name:"
    :default nil
    :type (:documentation
       "Name of the implementation to be destroyed." :SEXP))
 :label "Give parameters for DESTROY IMPLEMENTATION:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy implementations."
      :keys ((#\SUPER-K #\SUPER-I)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-IMPLEMENTATION implementation)))
  (DESTROY-IMPLEMENTATION implementation))
;**************************************************************************
;                DEFCOMMAND  FOR DESTROY INDEX                            *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC DESTROY-INDEX) (relation-name index-name)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-index)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-index
        (ARGLIST
          'destroy-index))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default nil
    :type (:documentation
       "Name of the relation on which the index to be destroyed is defined." :SEXP))
   (:label "Index Name:"
    :default nil
    :type (:documentation
       "Name of the index to be destroyed." :SEXP))
     :label "Give parameters for DESTROY INDEX:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy indices."
      :keys ((#\SUPER-K #\HYPER-I)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-INDEX relation-name index-name)))
  (DESTROY-INDEX relation-name index-name))
;**************************************************************************
;                DEFCOMMAND  FOR DESTROY STORAGE STRUCTURE                         *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC destroy-storage-structure) (storage-structure)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-storage-structure)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-storage-structure
        (ARGLIST
          'destroy-storage-structure))))
      :arguments (:user-supplied (:label "Storage structure name:"
    :default nil
    :type (:documentation
       "Name of the storage structure to be destroyed." :SEXP))
 :label "Give parameters for DESTROY STORAGE STRUCTURE:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy storage structures."
      :keys ((#\SUPER-K #\SUPER-S)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-STORAGE-STRUCTURE storage-structure)))
  (DESTROY-STORAGE-STRUCTURE storage-structure))
;**************************************************************************
;                DEFCOMMAND  FOR DESTROY VIEW                                      *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC destroy-view) (view)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-view)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-view
        (ARGLIST
          'destroy-view))))
      :arguments (:user-supplied (:label "View name:"
    :default nil
    :type (:documentation
       "Name of the view to be destroyed."
       :SEXP))
 :label "Give parameters for DESTROY VIEW:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy views."
      :keys ((#\SUPER-K #\SUPER-V)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-VIEW view)))
  (DESTROY-VIEW view))
;**************************************************************************
;                DEFCOMMAND  FOR DESTROYREL   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC destroy-relation) (relation disk &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-relation)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-relation
        (ARGLIST
          'destroy-relation))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default nil
    :type (:documentation
       "Name of the relation to be destroyed." :SEXP))
   (:label "Delete from the DISK:"
    :default NIL
    :type (:documentation
     "IF YES the file corresponding to this relation is deleted but NOT EXPUNGED." :BOOLEAN))
 :label "Give parameters for DESTROY RELATION:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy relations"
      :keys ((#\SUPER-K #\SUPER-R)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-RELATION
      relation (SETQ keywords (LIST 'disk disk)))))
  (DESTROY-RELATION relation keywords)
)
;**************************************************************************
;                DEFCOMMAND  FOR DESTROY ATTRIBUTE                                 *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC destroy-attribute) (relation attr key &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'destroy-attribute)
     (FORMAT NIL "  ~S"
      (CONS
        'destroy-attribute
        (ARGLIST
          'destroy-attribute))))
      :arguments (:user-supplied (:label "Relation Name:"
    :default nil
    :type (:documentation
       "Name of the relation from which attributes are to be destroyed." :SEXP))
   (:label "Attributes:"
    :default nil
    :type (:documentation
       "List of attributes to destroy." :SEXP))
   (:label "Key:"
    :default NIL
    :type (:documentation
     "New key for the relation if it is to be different from the previous value or if any of the key attributes are destroyed." :SEXP))
 :label "Give parameters for DESTROY ATTRIBUTE:")
      :menus ((command-menu :COLUMN "Manipulation"))
      :documentation "Used to destroy attributes from relations"
      :keys ((#\SUPER-K #\SUPER-A)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'DESTROY-ATTRIBUTE relation (SETQ keywords (LIST 'attr attr
      'key key)))))
  (DESTROY-ATTRIBUTE relation keywords)
)
;**************************************************************************
;                DEFCOMMAND  FOR SET UNION   *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC union) (from into tuples format
       dir doc key imp sto print unique
       &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'relation-union)
     (FORMAT NIL "  ~S"
       (CONS
        'relation-union
        (ARGLIST
          'relation-union))))
      :arguments (:user-supplied (:label "List of two relations:"
    :default NIL
    :type (:documentation
     "List of the names of two relations which will take part in the relation union operation. The attributes to be projected and a where clause can be specified for each relation using keywords. For instance, (REL1 (PROJECT <attr> WHERE <where-claue>) REL2 (WHERE <where-clause> PROJECT <attr>))." :SEXP))
   ,*ucl-into*
   (:label "Tuples?"
    :default NIL
    :type (:documentation
     "Specify if the resultant tuples be returned rather than inserted in a relation. The following parameters can be ignored if this is true."
     :boolean))
   ,*ucl-format*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-print*
   ,*ucl-unique*
  :LABEL "Parameters for the set-union of two relations")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to form union of two compatible relations"
      :keys ((#\SUPER-O #\SUPER-U)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'RELATION-UNION
      (SETQ keywords (LIST 'into into
     'from from 'tuples tuples
     'format format 'dir dir 'doc doc
     'key key 'imp imp 'sto sto
     'print print 'unique unique)))))
  (RELATION-UNION keywords))
;**************************************************************************
;                DEFCOMMAND  FOR SET DIFFERENCE                                    *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC difference) (from into tuples format
       dir doc key imp sto print unique
       &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'relation-difference)
     (FORMAT NIL "  ~S"
      (CONS
        'relation-difference
        (ARGLIST
          'relation-difference))))
      :arguments (:user-supplied (:label "List of two relations:"
    :default NIL
    :type (:documentation
     "List of the names of two relations which will take part in the relation difference operation. The attributes to be projected and a where clause can be specified for each relation using keywords. For instance, (REL1 (PROJECT <attr> WHERE <where-claue>) REL2 (WHERE <where-clause> PROJECT <attr>))." :SEXP))
   ,*ucl-into*
   (:label "Tuples?"
    :default NIL
    :type (:documentation
     "Specify if the resultant tuples be returned rather than inserted in a relation. The following parameters can be ignored if this is true."
     :boolean))
   ,*ucl-format*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-print*
   ,*ucl-unique*
  :LABEL "Parameters for the set-difference of two relations")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to form difference of two compatible relations"
      :keys ((#\SUPER-O #\SUPER-D)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'RELATION-DIFFERENCE
      (SETQ keywords (LIST 'into into
     'from from 'tuples tuples
     'format format 'dir dir 'doc doc
     'key key 'imp imp 'sto sto
     'print print 'unique unique)))))
  (RELATION-DIFFERENCE keywords))
;**************************************************************************
;                DEFCOMMAND  FOR SET INTERSECTION                                  *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC intersection) (from into tuples format
       dir doc key imp sto print unique
       &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'relation-intersection)
     (FORMAT NIL "  ~S"
      (CONS
        'relation-intersection
        (ARGLIST
          'relation-intersection))))
      :arguments (:user-supplied (:label "List of two relations:"
    :default NIL
    :type (:documentation
     "List of the names of two relations which will take part in the relation intersection operation. The attributes to be projected and a where clause can be specified for each relation using keywords. For instance, (REL1 (PROJECT <attr> WHERE <where-claue>) REL2 (WHERE <where-clause> PROJECT <attr>))." :SEXP))
   ,*ucl-into*
   (:label "Tuples?"
    :default NIL
    :type (:documentation
     "Specify if the resultant tuples be returned rather than inserted in a relation. The following parameters can be ignored if this is true."
     :boolean))
   ,*ucl-format*
   ,*ucl-dir*
   ,*ucl-doco*
   ,*ucl-key*
   ,*ucl-imp*
   ,*ucl-sto*
   ,*ucl-print*
   ,*ucl-unique*
  :LABEL "Parameters for the set-intersection of two relations")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to form intersection of two compatible relations"
      :keys ((#\SUPER-O #\SUPER-I)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'RELATION-INTERSECTION
      (SETQ keywords (LIST 'into into
     'from from 'tuples tuples
     'format format 'dir dir 'doc doc
     'key key 'imp imp 'sto sto
     'print print 'unique unique)))))
  (RELATION-INTERSECTION keywords))
;**************************************************************************
;                DEFCOMMAND  FOR AVERAGE     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC average) (relation attribute unique where by tuples
      &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'average)
     (FORMAT NIL "  ~S"
      (CONS
        'average
        (ARGLIST
          'average))))
      :arguments (:user-supplied (:label "Relation name:"
    :default *ui-relation*
    :type (:documentation
     "Name of the relation which contains the attribute to be averaged." :SEXP))
   ,*ucl-count-attr*
   ,*ucl-count-unique*
      ,*ucl-where*
   ,*ucl-by*
   ,*ucl-tuples*
  :LABEL "Parameters for average:")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to compute the average of the attribute values in a relation."
      :keys ((#\SUPER-O #\SUPER-A)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'AVERAGE relation attribute
      (SETQ keywords (LIST 'unique unique
     'where where 'by by 'tuples tuples)))))
  (AVERAGE relation attribute keywords))
;**************************************************************************
;                DEFCOMMAND  FOR SUM         *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC sum) (relation attribute unique where by tuples
  &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'sum)
     (FORMAT NIL "  ~S"
      (CONS
        'sum
        (ARGLIST
          'sum))))
      :arguments (:user-supplied (:label "Relation name:"
    :default *ui-relation*
    :type (:documentation
     "Name of the relation which contains the attribute to be summed." :SEXP))
   ,*ucl-count-attr*
   ,*ucl-count-unique*
   ,*ucl-where*
   ,*ucl-by*
   ,*ucl-tuples*
  :LABEL "Parameters for sum:")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to compute the sum of the attribute values in a relation."
      :keys ((#\SUPER-O #\SUPER-S)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'SUM relation attribute
      (SETQ keywords (LIST 'unique unique 'by by 'tuples tuples
     'where where)))))
  (SUM relation attribute keywords))
;**************************************************************************
;                DEFCOMMAND  FOR SIZE        *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC size) (relation unique where by tuples &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'size)
     (FORMAT NIL "  ~S"
      (CONS
        'size
        (ARGLIST
          'size))))
      :arguments (:user-supplied (:label "Relation name:"
    :default *ui-relation*
    :type (:documentation
     "Name of the relation whose size is required." :SEXP))
   ,*ucl-count-unique*
   ,*ucl-where*
   ,*ucl-by*
   ,*ucl-tuples*
  :LABEL "Parameters for size:")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to compute the size of the relation."
      :keys (#\SUPER-HYPER-S))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'SIZE relation
      (SETQ keywords (LIST 'unique unique 'by by 'tuples tuples
     'where where)))))
  (SIZE relation keywords))
;**************************************************************************
;                DEFCOMMAND  FOR COUNT-RTMS     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC count) (relation attribute unique where by tuples
         &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'count-rtms)
     (FORMAT NIL "  ~S"
      (CONS
        'count-rtms
        (ARGLIST
          'count-rtms))))
      :arguments (:user-supplied (:label "Relation name:"
    :default *ui-relation*
    :type (:documentation
     "Name of the relation which contains the attribute to be used to find the number of tuples." :SEXP))
   ,*ucl-count-attr*
   ,*ucl-count-unique*
   ,*ucl-where*
   ,*ucl-by*
   ,*ucl-tuples*
  :LABEL "Parameters for count:")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to compute the count of the attribute values in a relation."
      :keys ((#\SUPER-O #\SUPER-C)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'COUNT-RTMS relation attribute
      (SETQ keywords (LIST 'unique unique 'by by 'tuples tuples
     'where where)))))
  (COUNT-RTMS relation attribute keywords))
;**************************************************************************
;                DEFCOMMAND  FOR MAXIMUM     *
;**************************************************************************
(UCL:DEFCOMMAND (DBMS-RC maximum) (relation attribute where by tuples
      &AUX keywords)
            `(:description ,(STRING-APPEND (DOCUMENTATION 'maximum)
     (FORMAT NIL "  ~S"
      (CONS
        'maximum
        (ARGLIST
          'maximum))))
      :arguments (:user-supplied (:label "Relation name:"
    :default *ui-relation*
    :type (:documentation
     "Name of the relation which contains the attribute to be maximumd." :SEXP))
   ,*ucl-count-attr*
   ,*ucl-where*
   ,*ucl-by*
   ,*ucl-tuples*
  :LABEL "Parameters for maximum:")
      :menus ((command-menu :COLUMN "Operators"))
      :documentation "Used to compute the maximum of the attribute values in a relation."
      :keys ((#\SUPER-O #\SUPER-M)))
  (SEND *output-window* :append-item
(FORMAT nil "~S"
(LIST 'MAXIMUM relation attribute
      (SETQ keywords (LIST 'where where 'by by 'tuples tuples)))))
  (MAXIMUM relation attribute keywords))



;; fragments


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
   UNIQUE               - If T, only unique tuples will be part of the resultant relation.  (RELATION-UNION &REST KEYWORD-LIST &KEY &OPTIONAL FROM INTO DIRECTORY D

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
   UNIQUE               - If T, only unique tuples will be part of the resultant relation.  (RELATION-DIFFERENCE &REST KEYWORD-LIST &KEY &OPTIONAL FROM INTO DIRECTORY DOCUMENTATION FORMAT IMPLEMENTATION-TYPE STORAGE-STRUCTURE KEY PRINT TUPLES UNIQUE &ALLOW-OTHER-KEYS)

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
   UNIQUE               - If T, only unique tuples will be part of the resultant relation.  (RELATION-INTERSECTION &REST KEYWORD-LIST &KEY &OPTIONAL FROM INTO DIRECTORY DOCUMENTATION FORMAT IMPLEMENTATION-TYPE STORAGE-STRUCTURE KEY PRINT TUPLES UNIQUE &ALLOW-OTHER-KEYS)

   RELATION-NAME  - Name of the relation.
   ATTRIBUTE-NAME - Name of the attribute whose average is to be found.
   UNIQUE         - If T, only unique values will be used.
   WHERE          - If a selection criterion is provided, only the satisfying values will be used.
   BY             - Name of the attribute to group average of the above attribute by.
   TUPLES         - If T, the resultant values will be returned rather than printed out as a table.  (AVERAGE RELATION-NAME ATTRIBUTE-NAME &REST KEYWORD-LIST &KEY &OPTIONAL UNIQUE WHERE BY TUPLES &ALLOW-OTHER-KEYS)



   RELATION-NAME  - Name of the relation.
   ATTRIBUTE-NAME - Name of the attribute whose sum is to be found.
   UNIQUE         - If T, only unique values will be used.
   WHERE          - If a selection criterion is provided, only the satisfying values will be used.
   BY             - Name of the attribute to group sum of the above attribute by.
   TUPLES         - If T, the resultant values will be returned rather than printed out as a table.
