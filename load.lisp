;;; -*- Mode: Lisp; Syntax: Common-Lisp; Indent-Tabs-Mode: NIL -*-

(in-package "CL-USER")

(pushnew (make-pathname :directory (pathname-directory *load-truename*))
         asdf:*central-registry*)

(ql:quickload "digger")
