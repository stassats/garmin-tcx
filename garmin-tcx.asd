;;; -*- Mode: Lisp -*-

(defsystem garmin-tcx
  :serial t
  :depends-on (cxml-stp "local-time")
  :components ((:file "packages")
               (:file "garmin-tcx")))
