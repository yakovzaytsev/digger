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


