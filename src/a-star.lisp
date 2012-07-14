;;; -*- Mode: Lisp; Syntax: Common-Lisp; Indent-Tabs-Mode: NIL -*-

(in-package "DIGGER")


(declaim (inline x y f))

(defstruct (node (:conc-name nil))
  parent
  children
  map
  pos
  g
  h)

(defmethod print-object ((obj node) stream)
   (format stream "<~A,~A-~A-~C>~%"
           (y (pos obj)) (x (pos obj)) (f obj) (move-dir obj)))

(defun f (node)
  (+ (g node) (h node)))

(defun move-dir (node)
  (with-slots (parent pos) node
    (if (null parent)
        #\W
        (let ((parent-pos (pos (parent node))))
          (cond
           ((< (x parent-pos) (x pos)) #\R)
           ((> (x parent-pos) (x pos)) #\L)
           ((< (y parent-pos) (y pos)) #\U)
           ((> (y parent-pos) (y pos)) #\D)
           (t #\W))))))


(defvar *map*)
(defvar *open* ())
(defvar *done* ())
(defvar *goal*)


;; A*

(defun a* (map start goal)
  (let ((*goal* goal)
        *open*
        *done*)
    (push (make-node :pos start
                     :g 0
                     :h (heuristic start)
                     :map map)
          *open*)
    (do ()
        ((null *open*))
      (let ((bestnode (pop *open*)))
        (push bestnode *done*)
        (if (= goal (pos bestnode))
            (return (extract-path bestnode))
            (expand-node bestnode))))))

(defun extract-path (node)
  (do (path
       (n node (parent n)))
      ((null n) path)
    (push n path)))

(defun expand-node (node)
  (macrolet ((add-node (node new-map type)
               (let ((n (gensym)))
                 `(let ((,n ,node))
                    (push ,n (children node))
                    (try-change-parent ,n node ,new-map ,type)))))
    (dolist (next (possible-moves node))
      (destructuring-bind (pos . map) next
        (cond-it
         ((find pos *open* :key #'pos :test #'=)
          (add-node it map 'open))
         ((find pos *done* :key #'pos :test #'=)
          (add-node it map 'done))
         (t (add-node (make-node :pos pos
                                 :g (1+ (g node))
                                 :h (heuristic pos)
                                 :parent node
                                 :map map)
                      map
                      'open)))))))

(defun try-change-parent (node bestnode new-map type)
  (with-slots (parent map g) node
    (when (and (not (eql bestnode parent))
               (> g (1+ (g bestnode))))
      (setf parent bestnode
            map new-map
            g (1+ (g bestnode)))))
  (ecase type
    (open (change-node-prio node))
    (done (propagate-cost node))))

(defun change-node-prio (node)
  (setf *open* (delete node *open*))
  (add-to-openlist node))

(defun add-to-openlist (node)
  (loop :with f := (f node)
        :for tail :on *open*
        :do (when (< f (f (car tail)))
              (setf *open* (append head (cons node tail)))
              (return-from add-to-openlist))
        :collect (car tail) :into head
        :finally (setf *open* (append *open* (list node)))))

(defun propagate-cost (node)
  (dolist (child (children node))
    (with-slots (parent map g pos) child
      (let ((child-map (next-map (map node) (pos node) pos)))
        (when (and (> g (1+ (g node)))
                   (not (rock? child-map (x pos) (y pos))))
          (setf parent node
                map child-map
                g (1+ (g node)))
          (when (member child *open*)
            (change-node-prio child))
          (propagate-cost child))))))

;; map

(defun pt (x y)
  (complex y x))
(defun x (pos)
  (imagpart pos))
(defun y (pos)
  (realpart pos))

(defun heuristic (pos)
  (+ (abs (- (x pos) (x *goal*)))
     (abs (- (y pos) (y *goal*)))))

(defun dir (from to)
  (cond
   ((< (x from) (x to)) #\R)
   ((> (x from) (x to)) #\L)
   ((< (y from) (y to)) #\U)
   ((< (y from) (y to)) #\D)
   (t #\W)))

(defun possible-moves (node)
  (with-slots (pos map) node
    (let (rez)
      (dotimes (i 3)
        (dotimes (j 3)
          (let* ((x1 (+ (x pos) i -1))
                 (y1 (+ (y pos) j -1))
                 (pt1 (pt x1 y1)))
            (when (and (< -1 x1 (cols map))
                       (< -1 y1 (rows map))
                       (or (and (= x1  (x pos))
                                (/= y1 (y pos)))
                           (and (/= x1 (x pos))
                                (= y1  (y pos))))
                       ;; movement allowed?
                       (or (member (map-at map x1 y1) +soil+)
                           ;; moving rock
                           (and (= y1 (y pos))
                                (rock? map x1 y1)
                                (if (> x1 (x pos))
                                    ;; to the right
                                    (and (< x1 (1- (cols map)))
                                         (empty? map (1+ x1) (y pos)))
                                    ;; to the left
                                    (and (> x1 0)
                                         (empty? map (1- x1) (y pos)))))))
              (let ((map1 (next-map map pos pt1)))
                (unless (rock? map1 x1 y1)  ; check for flying rocks
                  (push (cons pt1 map1) rez)))))))
      ;; if no moves available, just wait
      (when (null rez)
        (push (cons pos (next-map map pos pos)) rez))
      rez)))

(defun next-map (map from to)
  (let ((map (execute-command (x from) (y from) (dir from to) map)))
    (when map (map-update map))))

(defun find-char (char map)
  (dotimes (i (rows map))
    (dotimes (j (cols map))
      (when (char= char (map-at map j i))
        (return-from find-char (pt j i))))))

(defun find-lambdas (map)
  (let (rez)
    (dotimes (i (rows map))
      (dotimes (j (cols map))
        (when (char= #\\ (map-at map j i))
          (push (pt j i) rez))))
    rez))
