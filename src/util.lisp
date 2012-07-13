;;; -*- Mode: Lisp; Syntax: Common-Lisp; Indent-Tabs-Mode: NIL -*-

(in-package "DIGGER")

(defmacro cond-it (&body body)
  "Like COND. IT is bound to the passed COND test."
  `(let (it)
     (cond
       ,@(mapcar (lambda (clause)
                   `((setf it ,(car clause)) ,(cadr clause)))
                 ;; uses the fact, that SETF returns the value set
                 body))))
