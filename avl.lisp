;;; -*- Mode:Common-Lisp; Package:RTMS; Base:10; Fonts:(*CODE-FONT* *COMMENT-FONT* *STRING-FONT*) -*-
;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved.
;;; AVL
;;;
;;; This file contains the following Explorer extensions to CommonLisp Standard as Indicated in the June 1985 Explorer Lisp
;;; Reference
;;;     firstn
;;;     nleft
;;;     errset
;;;
;;; The following function contains flavor references and thus are incompatable with CommonLisp. Their removal will not
;;; effect the functionality of RTMS.
;;;     process-subtree-flavor
;;;     process-subtree-flavor-left
;;;     insert-flavor-avl
;;;     insert-avl-flavor
;;;     flavor-avl-key-traversal
;;;

(defun process-subtree-list (sub-tree termination-clause temp-attribute-list mode
			     &aux results tuples)
  (cond ((and (not (null sub-tree)) (not (string-equal mode "TERMINATE")))
	 (cond ((super-fast-eval-where (first sub-tree) temp-attribute-list termination-clause)
		(setf results (nconc (process-subtree-list-left (third sub-tree)) results))
		(setf results (append (first sub-tree) results))
		(multiple-value-setq (tuples mode)
		  (process-subtree-list (fourth sub-tree) termination-clause temp-attribute-list mode))
		(setf results (append tuples results)))
	       (t
		(multiple-value-setq (tuples mode)
		  (process-subtree-list (third sub-tree) termination-clause temp-attribute-list mode))
		(setf results (append tuples results))
		(setf mode "TERMINATE")))))
  (values results mode))

(defun process-subtree-flavor (sub-tree termination-clause temp-attribute-list mode
			       &aux results tuples)
  (cond ((and (not (null sub-tree)) (not (string-equal mode "TERMINATE")))
	 (cond ((super-fast-eval-where (project-flavor (first sub-tree) temp-attribute-list temp-attribute-list)
				       temp-attribute-list termination-clause)
		(setf results (nconc (process-subtree-flavor-left (third sub-tree) temp-attribute-list)
				     results))
		(setf results (append (project-flavor (first sub-tree) temp-attribute-list temp-attribute-list)
				      results))
		(multiple-value-setq (tuples mode)
		  (process-subtree-flavor (fourth sub-tree) termination-clause
					  temp-attribute-list mode))
		(setf results (append tuples results)))
	       (t
		(multiple-value-setq (tuples mode)
		  (process-subtree-flavor (third sub-tree) termination-clause
					  temp-attribute-list mode))
		(setf results (append tuples results))
		(setf mode "TERMINATE")))))
  (values results mode))

(defun process-subtree-struct (sub-tree termination-clause temp-attribute-list mode relation-name
			       &aux results tuples)
  (cond ((and (not (null sub-tree))(not (string-equal mode "TERMINATE")))
	 (cond ((super-fast-eval-where (project-struct (first sub-tree) temp-attribute-list temp-attribute-list
						       relation-name)
				       temp-attribute-list termination-clause)
		(setf results (nconc (process-subtree-struct-left (third sub-tree) temp-attribute-list
								  relation-name) results))
		(setf results (append (project-struct (first sub-tree) temp-attribute-list temp-attribute-list
						      relation-name)
				      results))
		(multiple-value-setq (tuples mode)
		  (process-subtree-struct (fourth sub-tree) termination-clause
					  temp-attribute-list mode relation-name))
		(setf results (append tuples results)))
	       (t
		(multiple-value-setq (tuples mode)
		  (process-subtree-struct (third sub-tree) termination-clause
					  temp-attribute-list mode relation-name))
		(setf results (append tuples results))
		(setf mode "TERMINATE")))))
  (values results mode))


(defun process-subtree-list-left (sub-tree &aux results)
  (cond ((car sub-tree)
	 (if (third sub-tree)
	     (setf results (append (process-subtree-list-left (third sub-tree)) results)))
	 (setf results (append (first sub-tree) results))
	 (if (fourth sub-tree)
	     (setf results (append (process-subtree-list-left (fourth sub-tree)) results)))
	 results)))

(defun process-subtree-flavor-left (sub-tree attribute-list &aux results)
  (cond ((car sub-tree)
	 (cond ((third sub-tree)
		(setf results (append (process-subtree-flavor-left (third sub-tree) attribute-list) results))))
	 (setf results (append (project-flavor (first sub-tree) attribute-list attribute-list) results))
	 (cond ((fourth sub-tree)
		(setf results (append (process-subtree-flavor-left (fourth sub-tree) attribute-list) results))))
	 results)))

(defun process-subtree-struct-left (sub-tree attribute-list relation-name &aux results)
  (cond ((car sub-tree)
	 (cond ((third sub-tree)
		(setf results (append (process-subtree-struct-left (third sub-tree) attribute-list
								   relation-name) results))))
	 (setf results (append (project-struct (first sub-tree) attribute-list attribute-list relation-name)
			       results))
	 (cond ((fourth sub-tree)
		(setf results (append (process-subtree-struct-left (fourth sub-tree) attribute-list
								   relation-name) results))))
	 results)))

(defun retrieve-avl  (relation-name attribute-list project-list key where-clause key-value imp index-name
		      &aux all-tuples (tuples nil) domains temp-attribute-list termination-condition mode
			results)
  ;;
  ;;  Note that keyed retrieval from an AVL tree does not guarantee sorted order when there are multiple parts.
  ;;
  (cond ((null key-value)
	 (setf tuples (nreverse (eval-where (avl-inorder-traversal (getp index-name 'entry-point))
					    attribute-list where-clause imp relation-name)))
	 (setf temp-attribute-list attribute-list))
	(t
	 ;;
	 ;;  The key passed is of the form :
	 ;;            (list-of-key-attributes-present-the-key
	 ;;             list-of-starting-values
	 ;;             list-of-ending-values)
	 ;;
	 (setf domains (caar (funcall
			      (find-symbol (concatenate 'string "RETRIEVE-"
							*system-relation-base-implementation*
							"-" *system-relation-storage-structure*) *pkg-string*)
			      'system-relation *system-relation-attributes* '("DOMAINS") *system-relation-key*
			      `(string-equal relation-name ,(string-upcase relation-name))
			      nil 'system-relation)))
	 (setf all-tuples (getp index-name 'entry-point)
	       key (firstn (length (caar key-value)) key)
	       domains (car (project-list (list domains) attribute-list key)))
	 ;;
	 ;;  Take each key extracted from the WHERE clause and retrieve the tuples selected by the individual keys.
	 ;;
	 ;;
	 (do ((key% (first key-value) (cdr key%))
	      (beginning-value% (second key-value) (cdr beginning-value%))
	      (termination-clause% (third key-value) (cdr termination-clause%)))
	     ((null key%) tuples)
	   (cond ((equal (caar termination-clause%) t)
		  (setf termination-condition t))
		 (t
		  (setf termination-condition (list 'lep (caar key%) (caar termination-clause%)))))
	   ;;
	   ;;  Since EVAL-WHERE has to be called once per node while searching, the process can be sped up by separating
	   ;; the eval overhead from the actual operation. The call to prereq initializes all subsequent calls to FAST-EVAL-WHERE.
	   ;;
	   (multiple-value-setq (termination-condition temp-attribute-list)
	     (eval-where-prereq termination-condition attribute-list relation-name))
	   (progv temp-attribute-list nil
	     (multiple-value-setq (results mode)
	       (funcall (find-symbol (concatenate 'string (string-upcase imp) "-AVL-KEY-TRAVERSAL")
				     *pkg-string*)
			all-tuples (car key%) domains (car beginning-value%) termination-condition "LOCATE"
			temp-attribute-list relation-name)))
	   (setf tuples (append (eval-where  results (convert-attributes temp-attribute-list) where-clause
					     "LIST" relation-name)
				tuples)))
	 (setf tuples (nreverse tuples))))
  (setf tuples (project-list tuples temp-attribute-list (unconvert-attributes project-list)))
  tuples)


(defun extract-key-avl (attribute-list key-list domains where-clause package-name
			&aux key-domains (opt-func-list nil) result opt-func)
  ;;
  ;;  This should be done before any of the extract key functions are called in retrieve
  ;;
  (setf attribute-list (unconvert-attributes attribute-list package-name)
	key-list (unconvert-attributes key-list package-name)
	domains (unconvert-attributes domains package-name))
  (cond ((not (or (not (listp where-clause)) (equal where-clause '(t))))
	 (setf key-domains (car (project-list (list domains) attribute-list key-list)))
	 ;;
	 ;;  Determine if the controllin function of the subclause is one of the optimization function which RTMS is aquainted.
	 ;;
	 (cond ((listp where-clause)
		;;
		;;  Obtain a list of the optimization functions which are known to RTMS at this time. The values come back from
		;; retrieve as a list of lists. Remove the extra set of lists from the values. This list will be used in all of the
		;; optimization functions.
		;;
		(setf opt-func-list
		      (funcall
		       (find-symbol (concatenate 'string "RETRIEVE-" *system-relation-base-implementation*
						 "-" *system-relation-storage-structure*) *pkg-string*)
		       'system-optfunc *system-optfunc-attributes* '(symbol-name optimize-function)
		       *system-optfunc-key* (list 'string-equal 'storage-structure-type "AVL") nil
		       'system-optfunc)))
	       (t
		(setf opt-func-list nil)))
	 (cond ((setf opt-func (cadr (assoc (car where-clause) opt-func-list :test 'string-equal)))
		(cond ((string-equal opt-func "OPT-AVL-AND")
		       (setf result (opt-avl-and attribute-list key-list where-clause opt-func-list)))
		      ((string-equal opt-func "OPT-AVL-EQUAL")
		       (setf result (opt-avl-equal attribute-list key-list where-clause opt-func-list)))
		      ((string-equal opt-func "OPT-AVL-GT")
		       (setf result (opt-avl-gt attribute-list key-list where-clause opt-func-list)))
		      ((string-equal opt-func "OPT-AVL-LT")
		       (setf result (opt-avl-lt attribute-list key-list where-clause opt-func-list)))
		      ((string-equal opt-func "OPT-AVL-OR")
		       (setf result (opt-avl-or attribute-list key-list where-clause opt-func-list)))
		      (t
		       (setf result (funcall (find-symbol opt-func *pkg-string*)
					     attribute-list key-list where-clause opt-func-list))))
		;;
		;;  Now that the key has been formed, further processing is required to identify any possible
		;; overlapping in multiple keys
		;;
		(cond ((string-equal (car result) "k")
		       (setf result (append (list (second result)) (list (third result)) (list (fifth result))))
		       (setf result (reduce-avl-key key-list key-domains (first result) (second result)
						    (third result))))
		      (t
		       (setf result nil))))
	       (t
		(setf result nil))))
	(t
	 (setf result nil)))
  result)


(defun insert-list-avl (relation-name attribute-list tuples key-list index-name)
  (let (domain-key-list domain-list key new-element tree)
    (cond ((member (string-upcase relation-name) *system-relations* :test 'string-equal)
	   (setf domain-key-list (eval (read-from-string (concatenate 'string *pkg-string* ":*"
								      (string-upcase relation-name)
								      "-KEY-DOMAINS*")))))
	  (t
	   (setf domain-list (caar (qtrieve 'system-relation *system-relation-attributes* '(domains)
					    *system-relation-key*
					    (list 'string-equal 'relation-name (string-upcase relation-name))))
		 domain-key-list (car (project-list (list domain-list) attribute-list key-list)))))
    (setf key (project-list tuples attribute-list key-list)
	  tree (getp index-name 'entry-point))
    (do ((tuple% tuples (cdr tuple%))
	 (key% key (cdr key%)))
	((null tuple%) t)
      (setf *bf* 1
	    *node-inserted* nil
	    *equal-inserted* nil)
      (setf new-element (cons (list (car tuple%)) (append (list 0) (list nil) (list nil)))
	    tree (insert-avl-list new-element tree (car key%) key-list attribute-list domain-key-list nil
				  index-name)))
    (putp index-name tree 'entry-point)
    tuples))

(defun insert-flavor-avl (relation-name attribute-list tuples key-list index-name)
  (let (domain-key-list domain-list flavor-tuples key new-element tree
			(string-relation-name (string-upcase relation-name)))
    (cond ((member (string-upcase relation-name) *system-relations* :test 'string-equal)
	   (setf domain-key-list (eval (read-from-string (concatenate 'string *pkg-string* ":*"
								      string-relation-name "-KEY-DOMAINS*")))))
	  (t
	   (setf domain-list (caar (qtrieve 'system-relation *system-relation-attributes* '(domains)
					    *system-relation-key*
					    (list 'string-equal 'relation-name (string-upcase relation-name))))
		 domain-key-list (car (project-list (list domain-list) attribute-list key-list)))))
    (setf key (project-list tuples attribute-list key-list)
	  tree (getp index-name 'entry-point))
    ;;
    ;; Insert one tuple at a time into the AVL tree
    ;;
    (setf attribute-list (unconvert-attributes attribute-list *pkg-name*)
	  relation-name (read-from-string (concatenate 'string *pkg-name* string-relation-name)))
    (setf flavor-tuples (mapcar (function
				 (lambda (tuple keyval &aux %tuple)
				  ;;
				  ;;Insert the tuple into the hash table using heap formation for collisions.
				  ;;Form the instance to be stored in the hash table.
				  ;;
				  (setf %tuple (make-instance relation-name))
				  (do ((tuple tuple (cdr tuple))
				       (attribute-list attribute-list (cdr attribute-list)))
				      ((null tuple) %tuple)
				    (set-in-instance %tuple (car attribute-list) (car tuple)))
				  (setf *bf* 1
					*node-inserted* nil
					*equal-inserted* nil)
				  ;;
				  ;;  Form the new element in a form suitable for insertion
				  ;;
				  (setf new-element (cons (list %tuple)
							  (append (list 0) (list nil) (list nil)))
					tree (insert-avl-flavor new-element tree keyval key-list
								attribute-list domain-key-list nil
								index-name))
				  %tuple))
				tuples key))
    (putp index-name tree 'entry-point)
    flavor-tuples))

(defun insert-struct-avl (relation-name attribute-list tuples key-list index-name)
  (let (domain-key-list domain-list key new-element relation-macro struct-tuples tree
			(string-relation-name (string relation-name)))
    (cond ((member string-relation-name *system-relations* :test 'string-equal)
	   (setf domain-key-list (eval (read-from-string (concatenate 'string *pkg-string* ":*"
								      string-relation-name
								      "-KEY-DOMAINS*")))))
	  (t
	   (setf domain-list (caar (qtrieve 'system-relation *system-relation-attributes* '(domains)
					    *system-relation-key*
					    (list 'string-equal 'relation-name string-relation-name)))
		 domain-key-list (car (project-list (list domain-list) attribute-list key-list)))))
    (setf key (project-list tuples attribute-list key-list)
	  tree (getp index-name 'entry-point)
	  relation-macro (read-from-string (concatenate 'string *pkg-name* "MAKE-" string-relation-name)))
    ;;
    ;; Insert one tuple at a time into the AVL tree
    ;;
    (setf attribute-list (unconvert-attributes attribute-list *pkg-name*))
    (setf relation-name (read-from-string (concatenate 'string *pkg-name* string-relation-name)))
    (setf attribute-list
	  (mapcar #'(lambda (attr)
		      (read-from-string (concatenate 'string ":" string-relation-name (string-upcase attr))))
		  attribute-list))
    (setf struct-tuples (mapcar (function
				 (lambda (tuple keyval &aux %tuple attribute-value)
				  ;;
				  ;;Insert the tuple into the hash table using heap formation for collissions.
				  ;;Form the instance to be stored in the hash table.
				  ;;
				  (do ((tuple tuple (cdr tuple))
				       (attribute-list attribute-list (cdr attribute-list)))
				      ((null tuple) %tuple)
				    (push `(quote ,(car tuple)) attribute-value)
				    (push (car attribute-list) attribute-value)
				    (setf %tuple (eval `(,relation-macro ,@attribute-value))))
				  (setf *bf* 1
					*node-inserted* nil
					*equal-inserted* nil)
				  ;;
				  ;;  Form the new element in a form suitable for insertion
				  ;;
				  (setf new-element (cons (list %tuple)
							  (append (list 0) (list nil) (list nil)))
					tree (insert-avl-struct new-element tree keyval key-list
								attribute-list domain-key-list nil
								relation-name))
				  %tuple))
				tuples key))
    (putp index-name tree 'entry-point)
    struct-tuples))

(defun insert-avl-list (new-element tree key key-list attribute-list domain-key-list rebalancep relation-name
			&aux comparison-operator current-node-key mod-tree)
  relation-name
  (cond ((equal tree nil)
	 (setf rebalancep t
	       tree new-element))
	(t
	 ;;
	 ;;  This next line is the only accessor dependant part of the entire function. This call needs to be moved
	 ;; into another function to prevent the calling of FUNCALL READ-FROM-STRING nonsence from occuring many times.
	 ;;
	 (setf current-node-key (car (project-list (list (caar tree)) attribute-list key-list)))
	 (setf comparison-operator (node-compare key current-node-key domain-key-list))
	 (cond ((equal comparison-operator 'less-than)
		(multiple-value-setq (mod-tree rebalancep)
		  (insert-avl-list new-element (caddr tree) key key-list attribute-list domain-key-list
				   rebalancep relation-name))
		(rplaca (cddr tree) mod-tree)
		(cond (rebalancep
		       (multiple-value-setq (tree rebalancep)
			 (left-balance tree rebalancep)))))
	       ((equal comparison-operator 'greater-than)
		(multiple-value-setq (mod-tree rebalancep)
		  (insert-avl-list new-element (cadddr tree) key key-list attribute-list domain-key-list
				   rebalancep relation-name))
		(rplaca (cdddr tree) mod-tree)
		(cond (rebalancep
		       (multiple-value-setq (tree rebalancep)
			 (right-balance tree rebalancep)))))
	       ((equal comparison-operator 'equal)
		(multiple-value-setq (tree rebalancep)
		  (insert-avl-equal tree new-element rebalancep))))))
  (values tree rebalancep))


(defun insert-avl-flavor (new-element tree key key-list attribute-list domain-key-list rebalancep relation-name
			  &aux comparison-operator current-node-key mod-tree)
  (cond ((equal tree nil)
	 (setf rebalancep t
	       tree new-element))
	(t
	 ;;
	 ;;  This next line is the only accessor dependant part of the entire function. This call needs to be moved
	 ;; into another function to prevent the calling of FUNCALL READ-FROM-STRING nonsence from occuring many times.
	 ;;
	 (setf current-node-key (car (project-flavor (list (caar tree))attribute-list key-list relation-name)))
	 (setf comparison-operator (node-compare key current-node-key domain-key-list))
	 (cond ((equal comparison-operator 'less-than)
		(multiple-value-setq (mod-tree rebalancep)
		  (insert-avl-flavor new-element (caddr tree) key key-list attribute-list domain-key-list
				     rebalancep relation-name))
		(rplaca (cddr tree) mod-tree)
		(cond (rebalancep
		       (multiple-value-setq (tree rebalancep)
			 (left-balance tree rebalancep)))))
	       ((equal comparison-operator 'greater-than)
		(multiple-value-setq (mod-tree rebalancep)
		  (insert-avl-flavor new-element (cadddr tree) key key-list attribute-list domain-key-list
				     rebalancep relation-name))
		(rplaca (cdddr tree) mod-tree)
		(cond (rebalancep
		       (multiple-value-setq (tree rebalancep)
			 (right-balance tree rebalancep)))))
	       ((equal comparison-operator 'equal)
		(multiple-value-setq (tree rebalancep)
		  (insert-avl-equal tree new-element rebalancep))))))
  (values tree rebalancep))

(defun insert-avl-struct (new-element tree key key-list attribute-list domain-key-list rebalancep relation-name
			  &aux comparison-operator current-node-key mod-tree)
  (cond ((equal tree nil)
	 (setf rebalancep t
	       tree new-element))
	(t
	 ;;
	 ;;  This next line is the only accessor dependant part of the entire function. This call needs to be moved
	 ;; into another function to prevent the calling of FUNCALL READ-FROM-STRING nonsence from occuring many times.
	 ;;
	 (setf current-node-key (car (project-struct (list (caar tree)) attribute-list key-list
						     relation-name))
	       comparison-operator (node-compare key current-node-key domain-key-list))
	 (cond ((equal comparison-operator 'less-than)
		(multiple-value-setq (mod-tree rebalancep)
		  (insert-avl-struct new-element (caddr tree) key key-list attribute-list domain-key-list
				     rebalancep relation-name))
		(rplaca (cddr tree) mod-tree)
		(cond (rebalancep
		       (multiple-value-setq (tree rebalancep)
			 (left-balance tree rebalancep)))))
	       ((equal comparison-operator 'greater-than)
		(multiple-value-setq (mod-tree rebalancep)
		  (insert-avl-struct new-element (cadddr tree) key key-list attribute-list domain-key-list
				     rebalancep relation-name))
		(rplaca (cdddr tree) mod-tree)
		(cond (rebalancep
		       (multiple-value-setq (tree rebalancep)
			 (right-balance tree rebalancep)))))
	       ((equal comparison-operator 'equal)
		(multiple-value-setq (tree rebalancep)
		  (insert-avl-equal tree new-element rebalancep))))))
  (values tree rebalancep))

(defun insert-avl-equal (tree new-element rebalancep)
  (rplaca tree (append (car new-element) (car tree)))
  (setf rebalancep nil)
  (values tree rebalancep))

(defun left-balance (tree rebalancep
		     &aux left-branch right-branch)
  (cond ((equal (cadr tree) -1)
	 (rplaca (cdr tree) 0)
	 (setf rebalancep nil))
	((equal (cadr tree) 0)
	 (rplaca (cdr tree) 1))
	((equal (cadr tree) 1)
	 (setf left-branch (caddr tree))
	 (cond ((equal (cadr left-branch) 1)
		;;
		;;  LL rotation
		;;
		(rplaca (cddr tree) (cadddr left-branch))
		(rplaca (cdddr left-branch) tree)
		(rplaca (cdr tree) 0)
		(setf tree left-branch)
		)
	       (t
		;;
		;;  LR rotation
		;;
		(setf right-branch (cadddr left-branch))
		(rplaca (cdddr left-branch) (caddr right-branch))
		(rplaca (cddr right-branch) left-branch)
		(rplaca (cddr tree) (cadddr right-branch))
		(rplaca (cdddr right-branch) tree)
		(cond ((equal (cadr right-branch) 1)
		       (rplaca (cdr tree) -1))
		      (t
		       (rplaca (cdr tree) 0)))
		(cond ((equal (cadr right-branch) -1)
		       (rplaca (cdr left-branch) 1))
		      (t
		       (rplaca (cdr left-branch) 0)))
		(setf tree right-branch)))
	 (rplaca (cdr tree) 0)
	 (setf rebalancep nil)))
  (values tree rebalancep))

(defun right-balance (tree rebalancep &aux left-branch right-branch)
  (cond ((equal (cadr tree) 1)
	 (rplaca (cdr tree) 0)
	 (setf rebalancep nil))
	((equal (cadr tree) 0)
	 (rplaca (cdr tree) -1))
	((equal (cadr tree) -1)
	 (setf right-branch (cadddr tree))
	 (cond ((equal (cadr right-branch) -1)
		(rplaca (cdddr tree) (caddr right-branch))
		(rplaca (cddr right-branch) tree)
		(rplaca (cdr tree) 0)
		(setf tree right-branch))
	       (t
		(setf left-branch (caddr right-branch))
		(rplaca (cddr right-branch) (cadddr left-branch))
		(rplaca (cdddr left-branch) right-branch)
		(rplaca (cdddr tree) (caddr left-branch))
		(rplaca (cddr left-branch) tree)
		(cond ((equal (cadr left-branch) -1)
		       (rplaca (cdr tree) 1))
		      (t
		       (rplaca (cdr tree) 0)))
		(cond ((equal (cadr left-branch) 1)
		       (rplaca (cdr right-branch) -1))
		      (t
		       (rplaca (cdr right-branch) 0)))
		(setf tree left-branch)))
	 (rplaca (cdr tree) 0)
	 (setf rebalancep nil)))
  (values tree rebalancep))


(defun avl-inorder-traversal (avl-tree &aux tuple-list)
  (cond ((car avl-tree)
	 (if (third avl-tree)
	     (setf tuple-list (append (avl-inorder-traversal (third avl-tree))tuple-list)))
	 (setf tuple-list (append (first avl-tree) tuple-list))
	 (if (fourth avl-tree)
	     (setf tuple-list (append (avl-inorder-traversal (fourth avl-tree)) tuple-list)))
	 tuple-list)))

(defun avl-node-traversal (avl-tree &aux tuple-list tuple)
  (cond ((null (car avl-tree)) avl-tree)
	(t
	 (setf tuple (avl-node-traversal (third avl-tree)))
	 (if tuple
	     (setf tuple-list (cons tuple tuple-list)))
	 (if (first avl-tree)
	     (setf tuple-list (cons (print (first avl-tree)) tuple-list)))
	 (setf tuple (avl-node-traversal (fourth avl-tree)))
	 (if tuple
	     (setf tuple-list (cons tuple tuple-list)))
	 tuple-list)))

(defun list-avl-key-traversal (all-tuples key-attributes domains beginning-value termination-clause mode
			       temp-attribute-list relation-name
			       &aux comparison-operator current-node-key-value temp-results tuples results)
  ;;
  ;;  Locate the node where the search will begin
  ;;
  (cond ((and (not (equal all-tuples nil))
	      (or (string-equal mode 'locate) (string-equal mode 'locate-stage-2)))
	 (setf current-node-key-value (car (project-list (list (caar all-tuples)) temp-attribute-list
							 key-attributes)))
	 (cond ((equal (car beginning-value) t)
		(setf comparison-operator 'less-than))
	       (t
		(setf comparison-operator(node-compare beginning-value  current-node-key-value domains))))
	 (cond ((string-equal comparison-operator 'equal)
		;;
		;;  Found a node that is equal to the current tuple as far as the key goes. This might not however be the only
		;; node in the tree which is equivalent with the current key value. This is because the key used in the retrieval
		;; may not be the complete key of the relation. Because of this, must continue to travel along the left path until
		;; the node is no longer equal.
		;;
		;;
		;;  The current node is equal to the beginning condition, continue down the left branch until this condition
		;; no longer exists.
		;;
		(setf mode  "LOCATE-STAGE-2")
		(multiple-value-setq (results mode)
		  (list-avl-key-traversal (caddr all-tuples) key-attributes domains beginning-value
					  termination-clause mode temp-attribute-list relation-name))
		(cond ((not (string-equal mode 'terminate))
		       ;;
		       ;;  Need to process the right subtree -- must return mode, results
		       ;;
		       (cond ((setf temp-results (super-fast-eval-where (first all-tuples) temp-attribute-list
									termination-clause))
			      (setf results (append temp-results results))
			      (multiple-value-setq (tuples mode)
				(process-subtree-list (fourth all-tuples) termination-clause
						      temp-attribute-list mode))
			      (setf results (append tuples results)))
			     ((first all-tuples)
			      (setf mode "TERMINATE"))))))
	       ((and (equal comparison-operator 'less-than) (string-equal mode 'locate))
		(multiple-value-setq (results mode)
		  (list-avl-key-traversal (caddr all-tuples) key-attributes domains beginning-value
					  termination-clause mode temp-attribute-list relation-name))
		(cond ((not (string-equal mode 'terminate))
		       ;;
		       ;;  Need to process the right subtree
		       ;;
		       (cond ((setf temp-results (super-fast-eval-where (first all-tuples)
									temp-attribute-list termination-clause))
			      (setf results (append temp-results results))
			      (multiple-value-setq (tuples mode)
				(process-subtree-list (fourth all-tuples) termination-clause temp-attribute-list
						      mode))
			      (setf results (append tuples results)))
			     ((first all-tuples)
			      (setf mode "TERMINATE"))))))
	       ((and (equal comparison-operator 'greater-than) (string-equal mode 'locate))
		(multiple-value-setq (results mode)
		  (list-avl-key-traversal (cadddr all-tuples) key-attributes domains beginning-value
					  termination-clause mode temp-attribute-list relation-name))
		;;
		;;  Check the current node for satisfaction of the termination clause if it does process the right subtree else
		;; terminate
		;;
		(cond ((and (not (and (not (string-equal mode 'terminate))
				      (super-fast-eval-where (first  all-tuples) temp-attribute-list
							     termination-clause)))
			    (first all-tuples))
		       (setf mode "TERMINATE")))))))
  (values results mode))

(defun flavor-avl-key-traversal (all-tuples key-attributes domains beginning-value termination-clause mode
				 temp-attribute-list relation-name
				 &aux comparison-operator current-node-key-value temp-results tuples results )
  ;;
  ;;  Locate the node where the search will begin
  ;;
  (cond ((and (not (equal all-tuples nil))
	      (or (string-equal mode 'locate) (string-equal mode 'locate-stage-2)))
	 (setf current-node-key-value (car (project-flavor (list (caar all-tuples)) temp-attribute-list
							   key-attributes relation-name)))
	 (cond ((equal (car beginning-value) t)
		(setf comparison-operator 'less-than))
	       (t
		(setf comparison-operator (node-compare beginning-value  current-node-key-value domains))))
	 (cond ((string-equal comparison-operator 'equal)
		;;
		;;  Found a node that is equal to the current tuple as far as the key goes. This might not however be the only
		;; node in the tree which is equivalent with the current key value. This is because the key used in the retrieval
		;; may not be the complete key of the relation. Because of this, must continue to travel along the left path until
		;; the node is no longer equal.
		;;
		;;
		;;  The current node is equal to the beginning condition, continue down the left branch until this condition
		;; no longer exists.
		;;
		(setf mode  "LOCATE-STAGE-2")
		(multiple-value-setq (results mode)
		  (flavor-avl-key-traversal (caddr all-tuples) key-attributes domains beginning-value
					    termination-clause mode temp-attribute-list relation-name))
		(cond ((not (string-equal mode 'terminate))
		       ;;
		       ;;  Need to process the right subtree -- must return mode, results
		       ;;
		       (cond ((setf temp-results (super-fast-eval-where (project-flavor (first all-tuples)
											temp-attribute-list
											temp-attribute-list
											relation-name)
									temp-attribute-list
									termination-clause))
			      (setf results (append temp-results results))
			      (multiple-value-setq (tuples mode)
				(process-subtree-flavor (fourth all-tuples) termination-clause
							temp-attribute-list mode))
			      (setf results (append tuples results)))
			     ((first all-tuples)
			      (setf mode "TERMINATE"))))))
	       ((and (equal comparison-operator 'less-than) (string-equal mode 'locate))
		(multiple-value-setq (results mode)
		  (flavor-avl-key-traversal (caddr all-tuples) key-attributes domains beginning-value
					    termination-clause mode temp-attribute-list relation-name))
		(cond ((not (string-equal mode 'terminate))
		       ;;
		       ;;  Need to process the right subtree
		       ;;
		       (cond ((setf temp-results (super-fast-eval-where (project-flavor (first all-tuples)
											temp-attribute-list
											temp-attribute-list
											relation-name)
									temp-attribute-list
									termination-clause))
			      (setf results (append temp-results results))
			      (multiple-value-setq (tuples mode)
				(process-subtree-flavor (fourth all-tuples) termination-clause
							temp-attribute-list mode))
			      (setf results (append tuples results)))
			     ((first all-tuples)
			      (setf mode "TERMINATE"))))))
	       ((and (equal comparison-operator 'greater-than) (string-equal mode 'locate))
		(multiple-value-setq (results mode)
		  (flavor-avl-key-traversal (cadddr all-tuples) key-attributes domains beginning-value
					    termination-clause mode temp-attribute-list relation-name))
		;;
		;;  Check the current node for satisfaction of the termination clause if it does process the right subtree else
		;; terminate
		;;
		(cond ((and (not (and (not (string-equal mode 'terminate))
				      (super-fast-eval-where (project-flavor (first  all-tuples)
									     temp-attribute-list
									     temp-attribute-list
									     relation-name)
							     temp-attribute-list termination-clause)))
			    (first all-tuples))
		       (setf mode "TERMINATE")))))))
  (values results mode))


(defun struct-avl-key-traversal (all-tuples key-attributes domains beginning-value termination-clause mode
				 temp-attribute-list relation-name
				 &aux results string-temp-attributes)
  (multiple-value-setq (termination-clause temp-attribute-list)
    (eval-where-prereq termination-clause (convert-attributes temp-attribute-list) relation-name))
  (setf string-temp-attributes (convert-attributes temp-attribute-list))
  (progv temp-attribute-list nil
    (multiple-value-setq (results temp-attribute-list mode)
      (struct-avl-key-traversal-2 all-tuples (convert-attributes key-attributes) domains beginning-value
				  termination-clause mode temp-attribute-list string-temp-attributes
				  relation-name))))

(defun project-struct-fast (tuples attribute-list project-list relation-name)
  attribute-list relation-name
  (mapcar (function (lambda (tuple)
	    (mapcar (function (lambda (attr)
		      (funcall attr tuple)))
		    project-list)))
	  tuples))

(defun struct-avl-key-traversal-2 (all-tuples key-attributes domains beginning-value termination-clause mode
				   temp-attribute-list string-temp-attributes relation-name
				   &aux comparison-operator current-node-key-value temp-results tuples results )
  ;;
  ;;  Locate the node where the search will begin
  ;;
  (cond ((and (not (equal all-tuples nil))
	      (or (string-equal mode 'locate)
		  (string-equal mode 'locate-stage-2)))
	 (setf current-node-key-value (car (project-struct (list (caar all-tuples)) string-temp-attributes
							   key-attributes relation-name)))
	 (cond ((equal (car beginning-value) t)
		(setf comparison-operator 'less-than))
	       (t
		(setf comparison-operator
		      (node-compare beginning-value  current-node-key-value domains))))
	 (cond ((string-equal comparison-operator 'equal)
		;;
		;;  Found a node that is equal to the current tuple as far as the key goes. This might not however be the only
		;; node in the tree which is equivalent with the current key value. This is because the key used in the retrieval
		;; may not be the complete key of the relation. Because of this, must continue to travel along the left path until
		;; the node is no longer equal.
		;;
		;;
		;;  The current node is equal to the beginning condition, continue down the left branch until this condition
		;; no longer exists.
		;;
		(setf mode  "LOCATE-STAGE-2")
		(multiple-value-setq (results mode)
		  (struct-avl-key-traversal-2 (caddr all-tuples) key-attributes domains beginning-value
					      termination-clause mode temp-attribute-list
					      string-temp-attributes relation-name))
		(cond ((not (string-equal mode 'terminate))
		       ;;
		       ;;  Need to process the right subtree -- must return mode, results
		       ;;
		       (cond ((setf temp-results (super-fast-eval-where (project-struct (first all-tuples)
											string-temp-attributes
											string-temp-attributes
											relation-name)
									temp-attribute-list termination-clause))
			      (setf results (append temp-results results))
			      (multiple-value-setq (tuples mode)
				(process-subtree-struct (fourth all-tuples) termination-clause
							temp-attribute-list mode relation-name))
			      (setf results (append tuples results)))
			     ((first all-tuples)
			      (setf mode "TERMINATE"))))))
	       ((and (equal comparison-operator 'less-than) (string-equal mode 'locate))
		(multiple-value-setq (results mode)
		  (struct-avl-key-traversal-2 (caddr all-tuples) key-attributes domains beginning-value
					      termination-clause mode temp-attribute-list
					      string-temp-attributes relation-name))
		(cond ((not (string-equal mode 'terminate))
		       ;;
		       ;;  Need to process the right subtree
		       ;;
		       (cond ((setf temp-results (super-fast-eval-where (project-struct (first all-tuples)
											string-temp-attributes
											string-temp-attributes
											relation-name)
									temp-attribute-list termination-clause))
			      (setf results (append temp-results results))
			      (multiple-value-setq (tuples mode)
				(process-subtree-struct (fourth all-tuples) termination-clause
							temp-attribute-list mode relation-name))
			      (setf results (append tuples results)))
			     ((first all-tuples)
			      (setf mode "TERMINATE"))))))
	       ((and (equal comparison-operator 'greater-than) (string-equal mode 'locate))
		(multiple-value-setq (results mode)
		  (struct-avl-key-traversal-2 (cadddr all-tuples) key-attributes domains beginning-value
					      termination-clause mode temp-attribute-list
					      string-temp-attributes relation-name))
		;;
		;;  Check the current node for satisfaction of the termination clause if it does process the right subtree else
		;; terminate
		;;
		(cond ((and (not (and (not (string-equal mode 'terminate))
				      (super-fast-eval-where (project-struct (first  all-tuples)
									     string-temp-attributes
									     string-temp-attributes
									     relation-name)
							     temp-attribute-list termination-clause)))
			    (first all-tuples))
		       (setf mode "TERMINATE")))))))
  (values results mode))


;;; This routine compares the two nodes which have the specified domains. It will return EQUAL, LESS-THAN or GREATER-THAN, indicating
;;; the relationship of the first node to the second.
(defun node-compare (key-list-1 key-list-2 key-domain-list
		     &aux comparison-operator val1 val2)
  (setf comparison-operator 'equal)
  ;;
  ;;  It is assumed that the key lists are of the same length, this should
  ;; always be the case.
  ;;
  (do ((key1 key-list-1 (cdr key1))
       (key2 key-list-2 (cdr key2))
       (domain key-domain-list (cdr domain)))
      ((or (null key1)
	   (not (equal comparison-operator 'equal))) comparison-operator)
    ;;
    ;;  This section of the code has been rewritten to handle the domains which are defined by
    ;; RTMS explicitly. This loss in generality was necessitated by the lack of speed caused
    ;; by the FUNCALLing for each comparison.
    ;;
    (cond ((string-equal (car domain) "NUMBERP")
	   (cond ((= (car key1)(car key2))
		  )
		 ((< (car key1) (car key2))
		  (setf comparison-operator 'less-than))
		 (t
		  (setf comparison-operator 'greater-than))))
	  ((string-equal (car domain) "STRINGP")
	   (cond ((string-equal (car key1) (car key2))
		  )
		 ((string-lessp (car key1) (car key2))
		  (setf comparison-operator 'less-than))
		 (t
		  (setf comparison-operator 'greater-than))))
	  ((string-equal (car domain) "ATOM")
	   (setf val1 (car key1))
	   (setf val2 (car key2))
	   (cond ((and (numberp val1) (numberp val2))
		  (cond ((= val1 val1)
			 (setf comparison-operator 'equal))
			((< val1 val2)
			 (setf comparison-operator 'less-than))
			(t
			 (setf comparison-operator 'greater-than))))
		 ((and (stringp val1) (stringp val2))
		  (cond ((string-equal val1 val2)
			 (setf comparison-operator 'equal))
			((string-lessp val1 val2)
			 (setf comparison-operator 'less-than))
			(t
			 (setf comparison-operator 'greater-than))))
		 ((numberp val2)
		  (setf comparison-operator 'greater-than))
		 ((numberp val1)
		  (setf comparison-operator 'less-than))
		 (t
		  (setf val1 (string val1)
			val2 (string val2))
		  (cond ((string-equal val1 val2)
			 (setf comparison-operator 'equal))
			((string-lessp val1 val2)
			 (setf comparison-operator 'less-than))
			(t
			 (setf comparison-operator 'greater-than))))))
	  ((string-equal (car domain) "LISTP")
	   (cond ((equal-listp (car key1) (car key2))
		  )
		 ((lt-listp (car key1) (car key2))
		  (setf comparison-operator 'less-than))
		 (t
		  (setf comparison-operator 'greater-than))))
	  ((string-equal (car domain) "ANYP")
	   (cond ((equal-anyp (car key1) (car key2))
		  )
		 ((lt-anyp (car key1) (car key2))
		  (setf comparison-operator 'less-than))
		 (t
		  (setf comparison-operator 'greater-than))))
	  ;;
	  ;;  This section can be made faster by having a function called COMPARE-domainname which will return EQUAL, LESS-THAn or
	  ;; GREATER-THAN. This restricts the FUNCALLing to a single call.
	  ;;
	  (t
	   (cond ((not (funcall (find-symbol (concatenate 'string "EQUAL-" (string-upcase (car domain)))
					     *pkg-string*) (car key1) (car key2)))
		  (cond ((funcall (find-symbol (concatenate 'string "LT-" (string-upcase (car domain)))
					       *pkg-string*) (car key1) (car key2))
			 (setf comparison-operator 'less-than))
			(t
			 (setf comparison-operator 'greater-than))))))))
  comparison-operator)

'(defun compare-numberp (val1 val2)
  (block nil
    (cond ((= val1 val2)
	   (return 'equal))
	  ((< val1 val2)
	   (return 'less-than))
	  (t
	   (return 'greater-than)))))

;;; This function will process the rest of the where clause and determine if an avl key may be extracted. If so, the AVL key is returned.
;;; The results returned from this function, as well as all optimization functions, are in the following form :
;;;  (stat-character
;;;   list--of-lists-of-key-attributes-used
;;;   list-of-beginning-values
;;;   list-of-non-key-attributes-present
;;;   list-of-termination-predicates)
;;;   where
;;;  stat-character - is one of the following :
;;;    k - a valid key has been formed
;;;    n - no key can be formed from where
;;;        clause (ever).
;;;    p - it is still possible to form a
;;;        but this sub-clause will not
;;;        contribute to it.
;;;    c - constant sub-clause - nothing to
;;;        add to the key.
(defun opt-avl-and (attribute-list key-list where-clause opt-func-list
		    &aux do-continue opt-func (result '("p" (nil) (nil) (nil) (nil))) (where-aux-list nil)
		      (where-key-list nil) (where-val-list nil) (where-endval-list nil) element-index
		      ref-endval ref-key ref-val resultant-endval resultant-key resultant-val
		      temp-endval temp-key temp-val)
  ;;
  ;; Loop until all of the sub-clauses of the where-clause have been processed.
  ;;
  (do ((where-clause (cdr where-clause) (cdr where-clause)))
      ((or (null where-clause) (string-equal (car result) "n")) result)
    (cond ((listp (car where-clause))
	   ;;
	   ;;  The element is a list, must be a function call. Call the appropriate optimization function if one exists. Add the results from
	   ;; the function to the current results. The optimization functions which are defined by RTMS initially are used as selections in
	   ;; the following COND clause to improve the speed of calling these functions. If the function in the sub-clause is not from this
	   ;; group, the function name must be formed before it can be invoked.
	   ;;
	   (cond ((setf opt-func (cadr (assoc (caar where-clause) opt-func-list :test 'string-equal)))
		  (cond ((string-equal opt-func "OPT-AVL-AND")
			 (setf result (opt-avl-and attribute-list key-list (car where-clause) opt-func-list)))
			((string-equal opt-func "OPT-AVL-EQUAL")
			 (setf result (opt-avl-equal attribute-list key-list (car where-clause) opt-func-list)))
			((string-equal opt-func "OPT-AVL-GT")
			 (setf result (opt-avl-gt attribute-list key-list (car where-clause) opt-func-list)))
			((string-equal opt-func "OPT-AVL-LT")
			 (setf result (opt-avl-lt attribute-list key-list (car where-clause) opt-func-list)))
			((string-equal opt-func "OPT-AVL-OR")
			 (setf result (opt-avl-or attribute-list key-list (car where-clause) opt-func-list)))
			(t
			 (setf result (funcall (find-symbol opt-func *pkg-string*) attribute-list key-list
					       (car where-clause) opt-func-list))))
		  ;;
		  ;;  Add the results to the results lists
		  ;;
		  (setf where-key-list (append where-key-list (car (second result)))
			where-val-list (append where-val-list (car (third result)))
			where-aux-list (append where-aux-list (car (fourth result)))
			where-endval-list (append where-endval-list (car (fifth result)))))
		 ;;
		 ;;  The function which drives this sub-clause is not defined to RTMS as a function which can be optimized, therefore
		 ;; no key can be extracted and processing can be stopped.
		 ;;
		 (t
		  (setf result '("n" (nil) (nil) (nil) (nil))))))))
  ;;
  ;;  The where-clause has been parsed, now let us see what we have
  ;;
  (cond ((and where-key-list where-val-list)
	 ;;
	 ;;  We have a possible key. Several things need to be tested before the final determination can be made.
	 ;;
	 (setf result '(()()()()))
	 (setf do-continue 't)
	 ;;
	 ;;  Must determine if a key attribute is contained in the extracted key more than once. If so, must determine if this
	 ;; key is logically valid (i.e., (AND (< a1 100)(> a1 50))) and combine the occurances in a meaningful manner.
	 ;; (beginning value --> 50 and ending value --> 100) . The possible cases are given below :
	 ;;                  1)  (AND (< a1 100)(> a1 50)) : begval --> 50
	 ;;            endval --> 100
	 ;;   2)  (AND (= a1 50)(> a1 100)) : Logically invalid.
	 ;;   3)  (AND (> a1 50)(> a1 100)) : Logically weak but a key can be returned.
	 ;;           begval --> 100
	 ;;           endval --> T
	 ;;   3)  (AND (< a1 50)(< a1 100)) : Logically weak but a key can be returned.
	 ;;           begval --> T
	 ;;           endval --> 100
	 ;;   4) (AND (< a1 50) (> a1 100)) : Logically invalid. set key to NIL and return laughing.
	 ;;
	 (setf do-continue t
	       resultant-key nil
	       resultant-val nil
	       resultant-endval nil)
	 (do ((key-list% where-key-list (cdr key-list%))
	      (val-list% where-val-list (cdr val-list%))
	      (endval-list% where-endval-list (cdr endval-list%)))
	     ((or (null key-list%) (not do-continue)) t)
	   (setf temp-key (first key-list%)
		 temp-val (first val-list%)
		 temp-endval (first endval-list%)
		 ref-key (first key-list%)
		 ref-val (first val-list%)
		 ref-endval (first endval-list%))
	   (do ((key% (cdr key-list%) (cdr key%))
		(val% (cdr val-list%) (cdr val%))
		(endval% (cdr endval-list%) (cdr endval%)))
	       ((or (null key%) (not do-continue)) t)
	     (cond ((string-equal temp-key (first key%))
		    ;;
		    ;;  A multiple occurance of the same key attribute in the AND clause has been found, process accordingly
		    ;;
		    (cond ((equal temp-val temp-endval)
			   ;;
			   ;;  An equal key has occured, determine if the other key overlaps with it, if not then the user has
			   ;; specified an invalid where clause. SInce one of the keys has the same value for the upper and lower
			   ;; bounds the same, the other key can not modify the key values.
			   ;;
			   (cond ((not (and (gep temp-val (first val%)) (lep temp-val (first endval%))))
				  (setf do-continue nil
					temp-key nil))))
			  ;;
			  ;;  An equal key has occured, determine if the other key overlaps with it, if not then the user has
			  ;; specified an invalid where clause. SInce one of the keys has the same value for the upper and lower
			  ;; bounds the same, the other key can not modify the key values.
			  ;;
			  ((equal (first val%) (first endval%))
			   (cond ((and (gep (first val%) temp-val)
				       (lep (first val%) temp-endval))
				  (setf temp-val (first val%)
					temp-endval (first endval%)))
				 (t
				  (setf do-continue nil
					temp-key nil))))
			  ;;
			  ;;  Both of the keys are less thans having no lower bound, use the smallest value as the upper limit
			  ;;
			  ((and (rtms-equalp temp-val "T")
				(rtms-equalp (first val%) "T"))
			   (cond ((lep (first endval%) temp-endval)
				  (setf temp-endval (first endval%)))))
					;
			  ;;  Both of the keys are greater thans having no upper bound, use the largest value as the upper limit
			  ;;
			  ((and (rtms-equalp temp-endval "T")
				(rtms-equalp (first endval%) "T"))
			   (cond ((gep (first val%) temp-val)
				  (setf temp-val (first val%)))))
			  ;;
			  ;;  The temp-key is a less than and the key% element is greater than, if they over lap set the value of
			  ;; the key to that of the lower limit of the key% element and the upper limit to that of the temp-key
			  ;; element
			  ;;
			  ((rtms-equalp (first endval%) "T")
			   (cond ((rtms-equalp temp-val "T")
				  (cond ((gep temp-endval (first val%))
					 (setf temp-val (first val%)))
					(t
					 (setf temp-key nil
					       do-continue nil))))
				 ((not (gep (first endval%) temp-val))
				  (setf temp-key nil
					do-continue nil))))
			  ;;
			  ;;  The key% element is less than and the temp-key element is either greater than or a range, in either
			  ;; case the test for overlap must be made. In the case that the temp-key element is greater than, the
			  ;; upper limit of the key is set to that of the key% element. If the temp-key element is a range, the
			  ;; upper limit becomes to smaller of the two upper limits.
			  ;;
			  ((rtms-equalp (first val%) "T")
			   (cond ((rtms-equalp temp-endval "T")
				  (cond ((gep (first endval%) temp-val)
					 (setf temp-endval (first endval%)))
					(t
					 (setf temp-key nil
					       do-continue nil))))
				 ((gep (first endval%) temp-val)
				  (cond ((lep (first endval%) temp-endval)
					 (setf temp-endval (first endval%)))))
				 (t
				  (setf temp-key nil
					do-continue nil))))
			  (t
			   (setf temp-key nil
				 do-continue nil)))
		    ;;
		    ;;  Remove the element matched from further processing
		    ;;
		    (setf element-index (- (length key-list%) (length key%))
			  key-list% (append (firstn element-index key-list%) (cdr key%))
			  val-list% (append (firstn element-index val-list%) (cdr val%))
			  endval-list% (append (firstn element-index endval-list%) (cdr endval%))))))
	   (setf resultant-key (append (list temp-key) resultant-key)
		 resultant-val (append (list temp-val) resultant-val)
		 resultant-endval (append (list temp-endval) resultant-endval))
	   (cond ((and (equal (length key-list%) 1)
		       (not (and (equal (first key-list%) ref-key)
				 (equal (first val-list%) ref-val)
				 (equal (first endval-list%) ref-endval))))
		  (setf resultant-key (append key-list% resultant-key)
			resultant-val (append val-list% resultant-val)
			resultant-endval (append endval-list% resultant-endval)))))
	 (setf where-key-list resultant-key
	       where-val-list resultant-val
	       where-endval-list resultant-endval)
	 ;;
	 ;;  Determine which attributes which contigously form the key are present in the sub clause and accumulate the results.
	 ;; It is not required that all of the attributes which make the key are present in the sub clause for a key can be formed
	 ;; from the ones that are there as long as they are in continous order starting with the first attribute.
	 ;;
	 (do ((key-list key-list (cdr key-list)))
	     ((or (null key-list) (null do-continue)) do-continue)
	   (if (setf do-continue (member (car key-list) where-key-list :test 'string-equal))
	       ;;
	       ;;  The next attribute in the key list is contained in the sub clause, locate its associated results and add them to
	       ;; the results list. Proceed in the order given in the key-list until find an key attribute which is not present. At this
	       ;; point the processing stops.
	       ;;
	       (setf result
		     (do ((where-key-list where-key-list (cdr where-key-list))
			  (where-val-list where-val-list (cdr where-val-list))
			  (where-aux-list where-aux-list (cdr where-aux-list))
			  (where-endval-list where-endval-list (cdr where-endval-list)))
			 ((equal (car where-key-list) (car key-list))
			  (if (car key-list)
			      (list (append (car result) (list (car where-key-list)))
				    (append (cadr result) (list (car where-val-list)))
				    (append (caddr result) (list (car where-aux-list)))
				    (append (cadddr result) (list (car where-endval-list))))))))))
	 ;;
	 ;;  If the first attribute from the key is included in the results, then we have a key.
	 ;;
	 (if (equal (caar result) (car key-list))
	     (setf result (append (list "k") (list (list (first result))) (list (list (second result)))
				  (list (list (third result))) (list (list (fourth result)))))
	     (setf result '("n" (nil) (nil) (nil) (nil)))))
	;;
	;;  Constant expression - nothing to add
	;;
	((and (null where-key-list) (null where-aux-list))
	 (setf result '("c" (nil) (nil) (nil) (nil))))
	;;
	;; A key is not possible from this where clause
	;;
	(t
	 (setf result '("n" (nil) (nil) (nil) (nil)))))
  result)


;;; This function will process the rest of the where clause and determine if an avl key may be extracted. If so, the AVL key is returned.
;;; The results returned from this function, as well as all optimization functions, are in the following form :
;;;  (stat-character
;;;   list--of-lists-of-key-attributes-used
;;;   list-of-beginning-values
;;;   list-of-non-key-attributes-present
;;;   list-of-termination-predicates)
;;;   where
;;;  stat-character - is one of the following :
;;;    k - a valid key has been formed
;;;    n - no key can be formed from where
;;;        clause (ever).
;;;    p - it is still possible to form a
;;;        but this sub-clause will not
;;;        contribute to it.
;;;    c - constant sub-clause - nothing to
;;;        add to the key.
(defun opt-avl-equal (attribute-list key-list where-clause opt-func-list
		      &aux opt-func (result '("p" (nil) (nil) (nil) (nil))) (where-aux-list nil)
			(where-key-list nil) (where-val-list nil))
  ;;
  ;;  Process for all sub-clauses of the where-clause
  ;;
  (do ((where-clause (cdr where-clause) (cdr where-clause)))
      ((or (null where-clause) (string-equal (car result) "n")) result)
    (cond
      ;;
      ;;  This clause determines if the current element is an atom constant, if present, it is considered a value
      ;;
      ((and (listp (car where-clause))
	    (equal 'quote (caar where-clause)))
       (if (null where-val-list)
	   (setf where-val-list (cadar where-clause))
	   (setf result (append (list "c") (cdr result)))))
      ;;
      ;;  The element is a list, must be a function call. Call the appropriate optimization function if one exists. Add the result from the
      ;; function to the current results. Currently the return value must be a constant and ther are no optimization functions which are
      ;; currently supported which return a constant. This structure is supported on the chance that some function will be supported in
      ;; the future
      ;;
      ((listp (car where-clause))
       (cond ((setf opt-func (cadr (assoc (caar where-clause) opt-func-list :test 'string-equal)))
	      (cond ((string-equal opt-func "OPT-AVL-AND")
		     (setf result (opt-avl-and attribute-list key-list (car where-clause) opt-func-list)))
		    ((string-equal opt-func "OPT-AVL-EQUAL")
		     (setf result (opt-avl-equal attribute-list key-list (car where-clause) opt-func-list)))
		    ((string-equal opt-func "OPT-AVL-GT")
		     (setf result (opt-avl-gt attribute-list key-list (car where-clause) opt-func-list)))
		    ((string-equal opt-func "OPT-AVL-LT")
		     (setf result (opt-avl-lt attribute-list key-list (car where-clause) opt-func-list)))
		    ((string-equal opt-func "OPT-AVL-OR")
		     (setf result (opt-avl-or attribute-list key-list (car where-clause) opt-func-list)))
		    (t
		     (setf result (funcall (find-symbol opt-func *pkg-string*) attribute-list key-list
					   (car where-clause) opt-func-list))))
	      (if (null where-val-list)
		  (setf where-val-list (car (third result))))
	      ;;
	      ;;  No key may be formed if the key list or the aux list is non-nil. Also, all values returned must be single.
	      ;;
	      (if (or (car (second result)) (car (fourth result)) (cdr (third result)))
		  (setf result '("n" (nil) (nil) (nil) (nil)))))
	     (t
	      (setf result '("n" (nil) (nil) (nil) (nil))))))
      ;;
      ;; The element is a symbol, add the fact to the proper list
      ;;
      ((symbolp (car where-clause))
       (if (member (car where-clause) attribute-list :test 'string-equal)
	   ;;
	   ;;  Symbol is an attribute
	   ;;
	   (if (member (car where-clause) key-list :test 'string-equal)
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
	   ;;
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
	 ;; We have a possible key. The termination condition is the sub-clause evaluated. Since the equal clause allows the inclusion of
	 ;; only one attribute, it must be the first attribute in the key. Otherwise, it could be used as part of the key later on.
	 ;;
	 (if (equal where-key-list (car key-list))
	     (setf result (append (list "k") (list (list (list where-key-list)))
				  (list (list (list where-val-list))) (list (list (list where-aux-list)))
				  (list (list (list where-val-list)))))
	     (setf result (append (list "p") (list (list (list where-key-list)))
				  (list (list (list where-val-list))) (list (list (list where-aux-list)))
				  (list (list (list where-val-list)))))))
	;;
	;;  Determine if the where subclause was a constant
	;;
	((and (null where-key-list) (null where-aux-list))
	 (setf where-clause (append (list 'equal) where-clause))
	 (setf where-val-list (eval where-clause))
	 (setf result (append (list "c") (list (list (list where-key-list)))
			      (list (list (list where-val-list))) (list (list (list where-aux-list)))
			      (list (list (list where-val-list))))))
	;;
	;;  Determine if two attributes were specified, if so this invalidates the possibility of there being a key
	;;
	((or (and (null where-aux-list) (null where-val-list))
	     (and where-key-list where-aux-list))
	 (setf result '("n" (nil) (nil) (nil) (nil))))
	(t
	 (setf result '("p" (nil) (nil) (nil) (nil)))))
  result)

;;; This function will process the rest of the where clause and determine if an avl key may be extracted. If so, the AVL key is returned.
;;; The results returned from this function, as well as all optimization functions, are in the following form :
;;;  (stat-character
;;;   list--of-lists-of-key-attributes-used
;;;   list-of-beginning-values
;;;   list-of-non-key-attributes-present
;;;   list-of-termination-predicates)
;;;   where
;;;  stat-character - is one of the following :
;;;    k - a valid key has been formed
;;;    n - no key can be formed from where
;;;        clause (ever).
;;;    p - it is still possible to form a
;;;        but this sub-clause will not
;;;        contribute to it.
;;;    c - constant sub-clause - nothing to
;;;        add to the key.
(defun opt-avl-gt (attribute-list key-list where-clause opt-func-list
		   &aux result)
  ;;
  ;;  The functionality is the same between this routine and the OPT-AVL-EQUAL except for the termination condition. Therefore, call
  ;; OPT-AVL-EQUAL and then massage the termination condition.
  ;;
  (setf result (opt-avl-equal attribute-list key-list where-clause opt-func-list))
  (cond ((and (car (second result))
	      (or (equal (car result) "k") (equal (car result) "p")))
	 (setf result (append (list (car result)) (list (second result)) (list (third result))
			      (list (fourth result)) (list (list (list t)))))))
  result)

;;; This function will process the rest of the where clause and determine if an avl key may be extracted. If so, the AVL key is returned.
;;; The results returned from this function, as well as all optimization functions, are in the following form :
;;;  (stat-character
;;;   list--of-lists-of-key-attributes-used
;;;   list-of-beginning-values
;;;   list-of-non-key-attributes-present
;;;   list-of-termination-predicates)
;;;   where
;;;  stat-character - is one of the following :
;;;    k - a valid key has been formed
;;;    n - no key can be formed from where
;;;        clause (ever).
;;;    p - it is still possible to form a
;;;        but this sub-clause will not
;;;        contribute to it.
;;;    c - constant sub-clause - nothing to
;;;        add to the key.
(defun opt-avl-lt (attribute-list key-list where-clause opt-func-list
		   &aux result)
  ;;
  ;;  The functionality is the same between this routine and the OPT-AVL-EQUAL except for the beginning condition. Therefore, call
  ;; OPT-AVL-EQUAL and then massage the beginning condition.
  ;;
  (setf result (opt-avl-equal attribute-list key-list where-clause opt-func-list))
  (cond ((or (equal (car result) "k") (equal (car result) "p"))
	 (setf result (append (list (car result)) (list (second result)) (list (list (list t)))
			      (list (fourth result)) (list (fifth result))))))
  result)

;;; This function will process the rest of the where clause and determine if an avl key may be extracted. If so, the AVL key is returned.
;;; The results returned from this function, as well as all optimization functions, are in the following form :
;;;  (stat-character
;;;   list--of-lists-of-key-attributes-used
;;;   list-of-beginning-values
;;;   list-of-non-key-attributes-present
;;;   list-of-termination-predicates)
;;;   where
;;;  stat-character - is one of the following :
;;;    k - a valid key has been formed
;;;    n - no key can be formed from where
;;;        clause (ever).
;;;    p - it is still possible to form a
;;;        but this sub-clause will not
;;;        contribute to it.
;;;    c - constant sub-clause - nothing to
;;;        add to the key.
(defun opt-avl-or (attribute-list key-list where-clause opt-func-list
		   &aux result opt-func (a-result nil))
  ;;
  ;;  Process all of the sub-clauses of the where-clause
  ;;
  (do ((where-clause (cdr where-clause) (cdr where-clause)))
      ((or (null where-clause) (string-equal (car result) "n")) a-result)
    (cond ((listp (car where-clause))
	   ;;
	   ;;  The element is a list, must be a function call. Call the appropriate optimization function if one exists.
	   ;;
	   (cond ((setf opt-func (cadr (assoc (caar where-clause) opt-func-list :test 'string-equal)))
		  (cond ((string-equal opt-func "OPT-AVL-AND")
			 (setf result (opt-avl-and attribute-list key-list (car where-clause) opt-func-list)))
			((string-equal opt-func "OPT-AVL-EQUAL")
			 (setf result (opt-avl-equal attribute-list key-list (car where-clause) opt-func-list)))
			((string-equal opt-func "OPT-AVL-GT")
			 (setf result (opt-avl-gt attribute-list key-list (car where-clause) opt-func-list)))
			((string-equal opt-func "OPT-AVL-LT")
			 (setf result (opt-avl-lt attribute-list key-list (car where-clause) opt-func-list)))
			((string-equal opt-func "OPT-AVL-OR")
			 (setf result (opt-avl-or attribute-list key-list (car where-clause) opt-func-list)))
			(t
			 (setf result (funcall (find-symbol opt-func *pkg-string*) attribute-list key-list
					       (car where-clause) opt-func-list))))
		  (cond ((string-equal (car result) "k")
			 ;;
			 ;;  The sub-clause returned a valid key, add it to the list of keys formed.
			 ;;
			 (setf a-result
			       (list "k" (append (second result) (second a-result))
				     (append (third result) (third a-result))
				     (append (fourth result)(fourth a-result))
				     (append (fifth result) (fifth a-result)))))
			((string-equal (car result) "c"))
			;;
			;;  Result returned from the optimization function was not a constant expression or a valid key, therefore no
			;; key may be formed from this OR sub-clause
			;;
			((or (string-equal (car result) "n") (string-equal (car result) "p"))
			 (setf a-result '("n" (nil) (nil) (nil) (nil))))))
		 (t
		  (setf a-result '("n" (nil) (nil) (nil) (nil))))))
	  ((symbolp (car where-clause)))))
  a-result)


(defun reduce-avl-key (key-attribute-list key-domains extracted-key-attribute-list beg-val-list end-val-list
		       &aux (normalized-key-list nil) (normalized-begval-list nil) (normalized-endval-list nil)
			 reduction-key-list reduction-begval-list reduction-endval-list result index
			 begval1 begval2 endval1 endval2 reference-reductions)
  key-domains
  (block reduce-avl-key
    ;;
    ;;   If there is only one element in the key, there is nothing which can be reduced.
    ;;
    (cond ((<= (length beg-val-list) 1)
	   (return-from reduce-avl-key (append (list extracted-key-attribute-list)
					       (list beg-val-list)
					       (list end-val-list)))))
    ;;
    ;;  This is a test section of this function, it only looks at the first attribute of the key to determine the range of values which
    ;;will be selected.
    ;;
    (do ((begval% beg-val-list (cdr begval%))
	 (endval% end-val-list (cdr endval%)))
	((null begval%) t)
      (setf normalized-key-list (append (list (list (car key-attribute-list))) normalized-key-list)
	    normalized-begval-list (cons (list (caar begval%)) normalized-begval-list)
	    normalized-endval-list (cons (list (caar endval%)) normalized-endval-list)))
    ;;
    ;;  Begin the reduction process
    ;;
    (do ((reductions 1 reductions))
	((equal reductions 0) result)
      (setf reductions 0)
      ;;
      ;;  This loop will take each key element and attempt to combine them with ONE other key element. If this can be done, the
      ;; new combined element is added to the reduction list and the two combining elements are removed from further consideration
      ;; in this pass. If the current element does not combine, it is added to the final results list.
      ;;
      (setf reduction-key-list nil
	    reduction-begval-list nil
	    reduction-endval-list nil)
      ;;
      ;;  The normalized lists are the current working lists
      ;;
      (do ((key% normalized-key-list (cdr key%))
	   (begval% normalized-begval-list (cdr begval%))
	   (endval% normalized-endval-list (cdr endval%)))
	  ((null begval%) t)
	(setf reference-reductions reductions)
	;;
	;;  See if the next element can be combined with any of the remaining key elements
	;;
	;;  1) is the beginning value between the beg and end point of the other or is the ending value between the begin and end
	;;     point of the other, if so combine. T is less than all other values in the beg clause and greater than all other values in
	;;     the end clause. Everything to the right of a T is a T.
	;;  2) Have a domain comparison problem again...for now this will work only for numbers
	;;  3) Does not handle multiple key attributes.
	;;
	(do ((reference-begval% (cdr begval%) (cdr reference-begval%))
	     (reference-endval% (cdr endval%) (cdr reference-endval%)))
	    ((null reference-begval%) t)
	  (setf begval1 (caar begval%)
		begval2 (caar reference-begval%)
		endval1 (caar endval%)
		endval2 (caar reference-endval%))
	  (cond ((and (or (equal begval1 't) (equal endval2 't) (lep begval1 endval2))
		      (or (equal begval2 't) (equal endval1 't) (lep begval2 endval1)))
		 (setf reductions (+ reductions 1))
		 (cond ((equal begval1 't)
			(setf reduction-begval-list (cons (car begval%) reduction-begval-list)))
		       ((equal begval2 't)
			(setf reduction-begval-list (cons (car reference-begval%) reduction-begval-list)))
		       ((lep begval1 begval2)
			(setf reduction-begval-list (cons (car begval%) reduction-begval-list)))
		       (t
			(setf reduction-begval-list (cons (car reference-begval%) reduction-begval-list))))
		 (cond ((equal endval1 't)
			(setf reduction-endval-list (cons (car endval%) reduction-endval-list)))
		       ((equal endval2 't)
			(setf reduction-endval-list (cons (car reference-endval%) reduction-endval-list)))
		       ((gep endval2 endval1)
			(setf reduction-endval-list (cons (car reference-endval%) reduction-endval-list)))
		       (t
			(setf reduction-endval-list (cons (car endval%) reduction-endval-list))))
		 ;;
		 ;;  Remove the element matched. Can not use Remove to do this because it removes all occurances of the value
		 ;; not just the first one found
		 ;;
		 (cond ((equal (setf index (length reference-begval%)) 1)
			(setf begval% (firstn (- (length begval%) 1) begval%)
			      endval% (firstn (- (length endval%) 1) endval%)))
		       (t
			(setf begval% (append (firstn (- (length begval%) index 1) begval%)
					      (nleft index begval%))
			      endval% (append (firstn (- (length endval%) index 1) endval%)
					      (nleft index endval%))))))))
	;;
	;;  If the current key element does not match any of the other key element, insert into reduction list
	;;
	(cond ((equal reference-reductions reductions)
	       (setf reduction-begval-list (cons (car begval%) reduction-begval-list)
		     reduction-endval-list (cons (car endval%) reduction-endval-list)))))
      ;;
      ;;  Initialize for the next pass through the reduction loop, using the results from the last pass through the loop
      ;;
      (setf normalized-begval-list reduction-begval-list
	    normalized-endval-list reduction-endval-list)
      (if (equal (length normalized-begval-list) 1)
	  (setf reductions 0)))
    (return-from reduce-avl-key (append (list (firstn (length normalized-begval-list) normalized-key-list))
					(list normalized-begval-list) (list normalized-endval-list)))))
