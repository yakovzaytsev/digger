;;; -*- Mode: Lisp; Syntax: Common-Lisp; Indent-Tabs-Mode: NIL -*-

(in-package "DIGGER")


(defvar *solution* ())

(defun print-solution ()
  (when *solution*
    (dolist (path (car *solution*))
      (format t "~{~C~}" (mapcar #'move-dir path)))
    (format t "A")
    (terpri)
    (finish-output)))

(defun solve (map)
  (let* ((robot (find-char #\R map))
         (lift (find-char #\L map))
         (lambdas (find-lambdas map)))
    (setf *solution* nil)
    (dolist (path (dfs map robot lambdas))
      (let* ((last-node (car (last (car (last path)))))
             (final-move (with-slots (map pos) last-node
;                           (if (find-char #\O map)
                               (progn (setf (map-at map (x lift) (y lift))
                                            #\Space)
                                      (or (cdr (a* map pos lift))
                                          '(#\A)))))
;                               '(#\A))))
             (whole-path (append path (list final-move)))
             (score (score whole-path)))
        (when (or (null *solution*)
                  (> score (cdr *solution*)))
          (setf *solution* (cons whole-path score)))))
    *solution*))

(defun dfs (map start goals)
  (let ((c (center-mass goals))
        rez)
    (dolist (goal (sort (copy-list goals) #'<
                        :key (lambda (x) (dist x start c))))
      (let ((move (a* map start goal)))
        (when move
          (let ((tail (dfs1 (map (car (last move)))
                            goal
                            (remove goal goals))))
            (when tail
              (push (cons (cdr move) tail) rez))))))
    (nreverse rez)))

(defun dfs1 (map start goals)
  (let ((c (center-mass goals)))
    (dolist (goal (sort (copy-list goals) #'<
                        :key (lambda (x) (dist x start c))))
      (let ((move (a* map start goal)))
        (if (cdr goals)  ;; some goals remain
            (when move
              (let ((tail (dfs1 (map (car (last move)))
                                goal
                                (remove goal goals))))
                (when tail
                  (return (cons (cdr move) tail)))))
            (return (list (cdr move))))))))

(defun dist (x start center)
  (let ((m (manhattan x start)))
    (* m (+ m (manhattan x center)))))

(defun score (path)
  (let ((bonus 50)
        score)
    (when (eql #\A (caar (last path)))
      (setf bonus 25
            path (butlast path)))
    (- (* (length path) (+ 25 bonus))
       (reduce #'+ path :key #'length))))

(defun center-mass (lambdas)
  (/ (reduce #'+ lambdas) (length lambdas)))