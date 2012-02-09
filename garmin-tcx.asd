;;; -*- Mode: Lisp -*-

(defsystem garmin-tcx
  :serial t
  :depends-on (cxml-stp local-time drakma)
  :components ((:file "packages")
               (:file "garmin-tcx")))
