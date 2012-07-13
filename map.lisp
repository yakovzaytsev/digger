;;; -*- Mode: Lisp; Syntax: Common-Lisp; Indent-Tabs-Mode: NIL -*-

(in-package "DIGGER")

(defun empty-map (n m)
  (make-array `(,n ,m) :element-type '(unsigned-byte 8)))
