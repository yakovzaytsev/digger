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

(defun test-simple-a* (map)
  (let* ((map (map-from-string map))
         (path (a* map (find-char #\R map) (find-char #\O map))))
    (dolist (node path)
      (setf (aref map (y node) (x node)) #\+))
    (format t "~{~A~%~}" (let (rows)
                           (dotimes (i (rows map) rows)
                             (push (format nil "~{~C~}"
                                           (let (chars)
                                             (dotimes (j (cols map)
                                                         (nreverse chars))
                                               (push (aref map i j) chars))))
                                   rows))))
    path))
