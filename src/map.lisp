;;; -*- Mode: Lisp; Syntax: Common-Lisp; Indent-Tabs-Mode: NIL -*-

(in-package "DIGGER")


(defparameter +empty+ #\Space)

(defparameter +rock+ #\*)

(defparameter +open-lift+ #\O "Open Lambda Lift")

(defparameter +lambda+ #\\)

(defparameter +soil+ '(#\\ #\. #\Space))

(defparameter +earth+ #\.)

(defparameter +robot+ #\R)


(defmacro cols (map)
  `(array-dimension ,map 1))

(defmacro rows (map)
  `(array-dimension ,map 0))

(defmacro map-at (map x y)
  `(aref ,map ,y ,x))


(defun empty-map (cols rows)
  (make-array `(,rows ,cols) :element-type 'character))

(defmacro same-size-map (map)
  `(apply #'empty-map (reverse (array-dimensions ,map))))

(defmacro copy-map (map)
  `(copy-array ,map))

(defun empty? (map x y)
  (char= +empty+ (map-at map x y)))

(defun lambda? (map x y)
  (char= +lambda+ (map-at map x y)))

(defun rock? (map x y)
  (char= +rock+ (map-at map x y)))


(defun load-map (stream)
  (apply #'values
         (loop :for line := (read-line stream nil)
               :while (and line (not (= 0 (length line))))
               :collect (string-trim " " line) :into lines
               :finally (return (make-array (list (length lines)
                                                  (length (car lines)))
                                            :element-type 'character
                                            :initial-contents (reverse lines))))
          (loop :for line := (read-line stream nil) :while line
                :collect (parse-integer (subseq (string-trim " " line)
                                                (1+ (position #\Space line)))))))

(defmacro map-from-string (s)
  `(with-input-from-string (stream ,s) (load-map stream)))


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