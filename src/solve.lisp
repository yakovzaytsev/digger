;;; -*- Mode: Lisp; Syntax: Common-Lisp; Indent-Tabs-Mode: NIL -*-

(in-package "DIGGER")


(defvar *solution*)

(defun print-solution ()
  (when *solution*
    (dolist (path (car *solution*))
      (format t "~{~C~}" (mapcar #'move-dir path)))
    (terpri)
    (finish-output)))

(defun solve (map)
  (let* ((robot (find-char #\R map))
         (lift (find-char #\L map))
         (lambdas (find-lambdas map))
         (c (center-mass goals)))
    (dolist (goal (sort (copy-list goals) #'< :key (lambda (x)
                                                     (manhattan x c))))
      (let* ((path (dfs robot lambdas))
             (last-node (car path))
             (last-map (next-map (map last-node)
                                 (pos last-node) (pos last-node)))
             (final-move (if (find-char #\O last-map)
                             (or (a* (map last-node) (pos last-node) lift)
                                 (list #\A))
                             (list #\A)))
             (whole-path (cons final-move path))
             (score (score whole-path)))
        (when (or (null *solution*)
                  (> score (cdr *solution*)))
          (setf *solution* (cons (reverse whole-path) score)))))))

(defun dfs (start goals)
  )

(defun score (path)
  (let ((multiplier 50)
        score)
    (if (char= #\A (caar path))
        (setf multiplier 25
              path (cdr path)))
    (- (* multiplier (length path) 25)
       (reduce #'+ path :key #'length))))

(defun center-mass (lambdas)
  (/ (reduce #'+ lambdas) (length lambdas)))