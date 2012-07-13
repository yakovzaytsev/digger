;;; -*- Mode: Lisp; Syntax: Common-Lisp; Indent-Tabs-Mode: NIL -*-

(in-package "CL-USER")

(require 'asdf)

(pushnew (make-pathname :directory (pathname-directory *load-truename*))
         asdf:*central-registry*)

(asdf:oos 'asdf:load-op "digger")
