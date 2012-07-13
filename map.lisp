;;; -*- Mode: Lisp; Syntax: Common-Lisp; Indent-Tabs-Mode: NIL -*-

(in-package "DIGGER")

(defun empty-map (cols rows)
  (make-array `(,rows ,cols) :element-type '(unsigned-byte 8)))

(defmacro cols (map)
  `(array-dimension ,map 1))

(defmacro rows (map)
  `(array-dimension ,map 0))

