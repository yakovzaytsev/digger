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
    (setf (map-at map (x r-pos) (y r-pos)) #\.
          (map-at map (x l-pos) (y l-pos)) #\.)
    (let ((path (a* map r-pos l-pos)))
      (dolist (node path)
        (setf (map-at map (x (pos node)) (y (pos node))) (move-char node)))
      (print-map map)
      path)))

(defun move-char (node)
  (ecase (move-dir node)
    (#\W #\+)
    (#\L #\<)
    (#\R #\>)
    (#\D #\v)
    (#\U #\^)))

(defun print-map (map)
  (format t "~{~A~%~}"
          (let (rows)
            (dotimes (i (rows map) rows)
              (push (format nil "~{~C~}"
                            (let (chars)
                              (dotimes (j (cols map) (nreverse chars))
                                (push (map-at map j i) chars))))
                    rows)))))
