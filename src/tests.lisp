;;; -*- Mode: Lisp; Syntax: Common-Lisp; Indent-Tabs-Mode: NIL -*-

(in-package "DIGGER")


(defun test-load-map ()
  (assert (equalp (with-input-from-string (map
"######
#. *R#
#  \\.#
#\\ * #
L  .\\#
######")
                                          (load-map map))
                  #2A((#\# #\# #\# #\# #\# #\#)
                      (#\L #\  #\  #\. #\\ #\#)
                      (#\# #\\ #\  #\* #\  #\#)
                      (#\# #\  #\  #\\ #\. #\#)
                      (#\# #\. #\  #\* #\R #\#)
                      (#\# #\# #\# #\# #\# #\#))))
  t)


(defun test-last-is-space ()
  (assert (= 3 (cols (map-from-string "#R ")))))


(defun test-lambdas-count ()
  (assert (= 3 (lambdas-count (map-from-string
"######
#. *R#
#  \\.#
#\\ * #
L  .\\#
######"
                               )))))
