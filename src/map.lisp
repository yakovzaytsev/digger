;;; -*- Mode: Lisp; Syntax: Common-Lisp; Indent-Tabs-Mode: NIL -*-

(in-package "DIGGER")

(defun empty-map (cols rows)
  (make-array `(,rows ,cols) :element-type 'character))

(defmacro same-size-map (map)
  `(apply #'empty-map (reverse (array-dimensions ,map))))

(defmacro cols (map)
  `(array-dimension ,map 1))

(defmacro rows (map)
  `(array-dimension ,map 0))

(defmacro map-at (map x y)
  `(aref ,map ,y ,x))

(defconstant +empty+ #\Space)

(defconstant +rock+ #\R)

(defconstant +open-lift+ #\O "Open Lambda Lift")

(defconstant +lambda+ #\\)

(defconstant +soil+ '(#\\ #\. #\Space))

(defconstant +earth+ #\.)

(defconstant +robot+ #\R)

(defmacro empty? (map x y)
  `(equalp +empty+ (map-at ,map ,x ,y)))

(defmacro not-empty? (&rest args)
  `(not (empty? ,@args)))

(defmacro lambda? (map x y)
  `(equalp +lambda+ (map-at ,map ,x ,y)))

(defmacro rock? (map x y)
  `(equalp +rock+ (map-at ,map ,x ,y)))

(defun load-map (stream)
  (loop :for line := (read-line stream nil) :while (and line
                                                        (not (= 0 (length line))))
        :collect (string-trim " " line) :into lines
        :finally (return (make-array (list (length lines)
                                           (length (car lines)))
                                     :element-type 'character
                                     :initial-contents (reverse lines)))))

(defmacro map-from-string (s)
  `(with-input-from-string (stream ,s) (load-map stream)))

