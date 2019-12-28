
;;; -*- Mode:Common-Lisp; Package:RTMS; Fonts:(*CODE-FONT* *COMMENT-FONT* *STRING-FONT*); Base:10 -*-
;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved
;;; MISC-USER
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

(defun recover-all ()
  (do ((relations *system-relations* (cdr relations)))
      ((null relations) t)
    (putp (car relations) nil 'entry-point)
    (putp (car relations) nil 'commit-tuples))
  (setf *active-db* nil)
  (if *save-user-id*
      (setf user-id *save-user-id*))
  (setf *restore-operation* nil))

(defun recover-restore ()
  (setf *restore-operation* nil))
