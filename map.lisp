;;; -*- Mode: Lisp; Syntax: Common-Lisp; Indent-Tabs-Mode: NIL -*-

(in-package "DIGGER")

(defun empty-map (cols rows)
  (make-array `(,rows ,cols) :element-type 'character))

(defmacro cols (map)
  `(array-dimension ,map 1))

(defmacro rows (map)
  `(array-dimension ,map 0))

(defun load-map (stream)
  (loop :for line := (read-line stream nil) :while line
        :collect (string-trim " " line) :into lines
        :finally (return (make-array (list (length lines)
                                           (length (car lines)))
                                     :element-type 'character
                                     :initial-contents lines))))
