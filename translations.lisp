;;; -*- Mode:Common-Lisp; Package:FS; Fonts:(MEDFNT HL12B HL12BI); Base:10 -*-

;;;; RTMS Logical Host Translations

(fs:SET-logical-pathname-host "RTMS"
      :PHYSICAL-HOST (send (fs:get-pathname-host "SYS") :host)
      ;; The default translations are for a top level directory,
      ;; SYS:RTMS; If a different directory is used as the
      ;; main RTMS subdirectory, then the following translations
      ;; must be edited to point to that subdirectory.
;         :TRANSLATIONS '(("RTMS" "RTMS;"))
      )
