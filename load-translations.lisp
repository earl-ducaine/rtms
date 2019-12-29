;;;  -*- Mode:Common-Lisp; Base:10 -*-

;;; This file loads the translations for rtms, then sets the system
;;; source file so it knows where to find the information about how to
;;; build rtms.

(load "SYS: SITE; RTMS TRANSLATIONS" :verbose nil)

(si:set-system-source-file "RTMS" "RTMS:RTMS;DEFSYSTEM LISP")
