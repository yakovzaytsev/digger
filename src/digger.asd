;;; -*- Mode: Lisp; Syntax: Common-Lisp; Indent-Tabs-Mode: NIL -*-

(in-package "CL-USER")

(asdf:defsystem :digger
  :serial t
  :version "0.1"
  :depends-on (:cl-utilities)
  :components ((:file "package")
               (:file "util")
               (:file "map")
               (:file "map-update")
               (:file "a-star")))

