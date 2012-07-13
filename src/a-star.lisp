;;; -*- Mode: Lisp; Syntax: Common-Lisp; Indent-Tabs-Mode: NIL -*-

(in-package "DIGGER")


(declaim (inline x y f))

(defstruct (node (:conc-name nil))
  parent
  children
  pos
  g
  h)

(defun f (node)
  (+ (g node) (h node)))


(defvar *map*)
(defvar *open* ())
(defvar *done* ())
(defvar *goal*)
(defvar *expanded-count*)


;; A*

(defun a* (map start goal)
  (let ((*expanded-count* 0)
        (*map* map)
        (*goal* goal)
        *open*
        *done*)
    (setf (aref map (y start) (x start)) #\.)
    (push (make-node :pos start
                     :g 0
                     :h (heuristic start))
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
    (push (pos n) path)))


(defun expand-node (node)
  (incf *expanded-count*)
  (macrolet ((add-node (node type)
               `(progn (push ,node (children node))
                       (try-change-parent ,node node ,type))))
    (dolist (next (expand (pos node)))
      (cond-it
       ((find next *open* :key #'pos :test #'=)
        (add-node it 'open))
       ((find next *done* :key #'pos :test #'=)
        (add-node it 'done))
       (t (add-node (make-node :pos next
                               :g (1+ (g node))
                               :h (heuristic next)
                               :parent node)
                    'open))))))

(defun try-change-parent (node bestnode type)
  (with-slots (parent g f h) node
    (when (> g (1+ (g bestnode)))
      (setf parent bestnode
            g (1+ (g bestnode))))
    (ecase type
      (open (change-priority-of-node node))
      (done (propagate-cost node)))))

(defun change-priority-of-node (node)
  (setf *open* (remove node *open*))
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
    (with-slots (parent g f h) child
      (when (> g (1+ (g node)))
        (setf parent node
              g (1+ (g node)))
        (when (member child *open*)
          (change-priority-of-node child *open*))
        (propagate-cost child)))))

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

(defun expand (point)
  (let (rez)
    (dotimes (i 3)
      (dotimes (j 3)
        (let ((x1 (+ (x point) i -1))
              (y1 (+ (y point) j -1)))
          (when (and (< -1 x1 (cols *map*))
                     (< -1 y1 (rows *map*))
                     (or (and (= x1 (x point))
                              (/= y1 (y point)))
                         (and (/= x1 (x point))
                              (= y1 (y point))))
                     (member (aref *map* y1 x1) +soil+))
            (push (pt x1 y1) rez)))))
    rez))

(defun find-char (char map)
  (dotimes (i (rows map))
    (dotimes (j (cols map))
      (when (char= char (aref map i j))
        (return-from find-char (pt j i))))))