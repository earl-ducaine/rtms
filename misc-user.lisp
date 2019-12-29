;;; -*- Mode:Common-Lisp; Package:RTMS; Base:10 -*-

;;; Copyright (c) by Texas Instruments, Incorporated
;;; All rights reserved
;;; MISC-USER
;;;
;;; This file contains the following Explorer extensions to CommonLisp
;;; as indicated in the June 1985 Explorer Lisp Reference

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
