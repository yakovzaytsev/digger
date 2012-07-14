;;; -*- Mode: Lisp; Syntax: Common-Lisp; Indent-Tabs-Mode: NIL -*-

(in-package "DIGGER")

(defvar *lambdas* 0)

(defun move-robot (x y map x1 y1)
  (let ((new-map (copy-map map)))
    (cond ((member (map-at map x1 y1) +soil+)
           (setf (map-at new-map x1 y1) +robot+
                 (map-at new-map x y) +empty+)
           (case (map-at map x1 y1)
             (#\\ (incf *lambdas*) new-map)
             (#\O 'completed)
             (otherwise new-map)))
          ((rock? map x1 y1)
           (symbol-macrolet ((moves-right? `(and (= x1 (1+ x)) (= y1 y)))
                             (moves-left?  `(and (= x1 (1- x)) (= y1 y))))
             (cond ((and moves-right?
                         (< x (- (cols map) 2))
                         (empty? map (+ x 2) y))
                    (setf (map-at new-map x1 y1) +robot+
                          (map-at new-map x y) +empty+
                          (map-at new-map (+ x 2) y) +rock+)
                    new-map)
                   ((and moves-left?
                         (> x 1)
                         (empty? map (- x 2) y))
                    (setf (map-at new-map x1 y1) +robot+
                          (map-at new-map x y) +empty+
                          (map-at new-map (- x 2) y) +rock+)
                    new-map)))))))

(defun execute-command (x y command map)
  "With (X, Y) being location of the Robot execute a COMMAND,
then return updated MAP.
In case of Abort, returns NIL.
If mine is completed returns COMPLETED"
  (case command
    (#\L (move-robot x y map (- x 1) y))
    (#\R (move-robot x y map (+ x 1) y))
    (#\U (move-robot x y map x (+ y 1)))
    (#\D (move-robot x y map x (- y 1)))
    (#\W (copy-map map))
    (#\A nil)))

(defun lambdas-count (map)
  (loop for x from 0 to (- (cols map) 1)
     sum (loop for y from 0 to (- (rows map) 1)
            when (lambda? map x y)
            count it)))
;; alternative: (length (find-lamdas map))

(defmacro no-lambdas-left? (map)
  `(= 0 (lambdas-count ,map)))

(defun update (map x y new-map)
  (setf (map-at new-map x y) (map-at map x y))
  (case (map-at map x y)
    (#\* (case (map-at map x (1- y))
           (#\Space (setf (map-at new-map x y) +empty+
                          (map-at new-map x (1- y)) +rock+))
           (#\* (cond ((and (empty? map (1+ x) y)
                            (empty? map (1+ x) (1- y)))
                       (setf (map-at new-map x y) +empty+
                             (map-at new-map (1+ x) (1- y)) +rock+))
                      ((>= 1 x) (when (and (or (not (empty? map (1+ x) y))
                                               (not (empty? map (1+ x) (1- y))))
                                           (empty? map (1- x) y)
                                           (empty? map (1- x) (1- y)))
                                  (setf (map-at new-map x y) +empty+
                                        (map-at new-map (1- x) (1- y)) +rock+)))))
           (#\\ (when (and (empty? map (1+ x) y)
                           (empty? map (1+ x) (1- y)))
                  (setf (map-at new-map x y) +empty+
                        (map-at new-map (1+ x) (1- y)) +rock+)))))
    (#\L (when (no-lambdas-left? map)
           (setf (map-at new-map x y) +open-lift+)))))

(defun map-update (map)
  "The updated MAP is returned"
  (loop with new-map = (same-size-map map)
     for x from 0 to (1- (cols map))
     do (loop for y from 0 to (1- (rows map))
           do (update map x y new-map))
     finally (return new-map)))