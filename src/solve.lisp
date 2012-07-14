;;; -*- Mode: Lisp; Syntax: Common-Lisp; Indent-Tabs-Mode: NIL -*-

(in-package "DIGGER")


(defvar *solution* ())

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
         (path (dfs map robot lambdas))
         (last-node (car (last (car (last path)))))
         (final-move (with-slots (map pos) last-node
                       (if (find-char #\O map)
                           (or (a* map pos lift)
                               (list #\A))
                           (list #\A))))
         (whole-path (append path final-move))
         (score (score whole-path)))
    (when (or (null *solution*)
              (> score (cdr *solution*)))
      (setf *solution* (cons whole-path score)))))

(defun dfs (map start goals)
  "Find first complete (or maximum length) path through all nodes."
  (let ((c (center-mass goals)))
    (dolist (goal (sort (copy-list goals) #'< :key (lambda (x)
                                                     (manhattan x c))))
      (let ((move (a* map start goal)))
        (when move
          (if (cdr goals)
              (let ((remainder (dfs (map (car (last move)))
                                    goal (remove goal goals))))
                (when remainder
                  (return (cons move remainder))))
              (return (list move))))))))


(defun score (path)
  (let ((multiplier 50)
        score)
    (when (eql #\A (car (last path)))
      (setf multiplier 25
            path (butlast path)))
    (- (* multiplier (length path) 25)
       (reduce #'+ path :key #'length))))

(defun center-mass (lambdas)
  (/ (reduce #'+ lambdas) (length lambdas)))