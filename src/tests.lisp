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
         (r-pos (find-char #\R map))
         (l-pos (find-char #\L map)))
    (setf (aref map (y r-pos) (x r-pos)) #\.
          (aref map (y l-pos) (x l-pos)) #\.)
    (let ((path (a* map r-pos l-pos)))
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
      path)))