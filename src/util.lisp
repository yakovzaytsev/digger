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

;; taken from ARNESI
(defun copy-array (array)
  "Returns a fresh copy of ARRAY. The returned array will have
the same dimensions and element-type, will not be displaced and
will have the same fill-pointer as ARRAY.

See http://thread.gmane.org/gmane.lisp.allegro/13 for the
original implementation and discussion."
  (let ((dims (array-dimensions array)))
    (adjust-array (make-array dims
                              :element-type (array-element-type array)
                              :displaced-to array)
                  dims
                  :fill-pointer (and (array-has-fill-pointer-p array)
                                     (fill-pointer array)))))