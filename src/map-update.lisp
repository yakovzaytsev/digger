;;; -*- Mode: Lisp; Syntax: Common-Lisp; Indent-Tabs-Mode: NIL -*-

(in-package "DIGGER")

(defun lambdas-count (map)
  (loop for x from 0 to (- (cols map) 1)
     sum (loop for y from 0 to (- (rows map) 1)
           when (lambda? map x y)
           count it)))

(defmacro no-lambdas-left? (map)
  `(= 0 (lambdas-count ,map)))

(defun update (map x y new-map)
  (case (map-at map x y)
    (#\R (case (map-at map x (- y 1))
           (#\ (setf (map-at new-map x y) +empty+
                     (map-at new-map x (- y 1) +rock+)))
           (#\R (cond ((and (empty? (map-at map (+ x 1) y))
                            (empty? (map-at map (+ x 1) (- y 1))))
                         (setf (map-at new-map x y) +empty+
                               (map-at new-map (+ x 1) (- y 1)) +rock+))
                      ((>= 1 x) (when (and (or (not-empty? map (+ x 1))
                                               (not-empty? map (+ x 1) (- y 1)))
                                           (empty? map (- x 1) y)
                                           (empty? map (- x 1 (- y 1))))
                                  (setf (map-at new-map x y) +empty+
                                        (map-at new-map (- x 1) (- y 1) +rock+))))))
           (#\\ (when (and (empty? map (+ x 1) y)
                           (empty? map (+ x 1) (- y 1)))
                  (setf (map-at new-map x y) +empty+
                        (map-at new-map (+ x 1) (- y 1) +rock+))))))
    (#\L (when (no-lambdas-left? map) (setf (map-at new-map x y +open-lift+))))))

(defun map-update (map)
  "The updated MAP is returned"
  (loop with new-map = (same-size-map map)
     for x from 0 to (- (cols map) 1)
     do (loop for y from (- (rows map) 1) downto 1
           do (update map x y new-map))
     finally return new-map))

