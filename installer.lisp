;;; -*- Mode:Common-Lisp; Package:INSTALLER; Fonts:(MEDFNT HL12B); Base:10 -*-
;;; Copyright (c) 1985, 1986, 1987 Texas Instruments Incorporated.  All Rights Reserved.

;;; the above is a temporary file attribute line which is replaced on
;;; the tape


;;; Restore File for RTMS Source and Object Product
(MT:DEF-LDT-1 (work-directory)
  ;; describe logical hosts
    (mt:ldt-translations-files
     '("RTMS" . "The file server host for the RTMS source/object files"))

  ;; create RTMS: RTMS; directory
  (mt:ldt-create-directory "RTMS: RTMS;")

  ;; create RTMS: RTMS-PATCH; directory
  (mt:ldt-create-directory "RTMS: RTMS-PATCH;")

  ;; restore remainder of tape
  (mt:ldt-restore-file      "SYS: SITE; RTMS.SYSTEM#>")
  (let ((restore-as-original-p (mt:restore-original-versionS-p)))
    (mt:ldt-no-more-prompts)
    (if restore-as-original-p
(progn
  (mt:ldt-restore-directory "RTMS: RTMS; *.*#*")
          (mt:ldt-restore-directory "RTMS: RTMS-PATCH; *.*#*"))
(progn
  (mt:ldt-restore-directory "RTMS: RTMS; *.*#>")
          (mt:ldt-restore-directory "RTMS: RTMS-PATCH; *.*#>")))
    );let
  (mt:ldt-restore-inp work-directory)


  ;; verify tape except for Restore File and TRANSLATIONS Files
  (mt:rewind)     ; go back to the beginning again
  (mt:space-to-eof)    ; skip over RESTORE-FILE
  (mt:space-to-eof)    ; skip over TRANSLATION-FILE
  (mt:ldt-verify-or-throw "SYS: SITE; RTMS.SYSTEM#>")
  (mt:ldt-verify-or-throw "RTMS: RTMS; *.*#>")
  (mt:ldt-verify-or-throw "RTMS: RTMS-PATCH; *.*#>")
  (mt:ldt-verify-inp-or-throw work-directory))
