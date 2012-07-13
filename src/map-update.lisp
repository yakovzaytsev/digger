;;; -*- Mode: Lisp; Syntax: Common-Lisp; Indent-Tabs-Mode: NIL -*-

(in-package "DIGGER")

(defun update (map x y new-map)
  )

(defun map-update (map)
  "The updated MAP is returned"
  (loop with new-map = (same-size-map map)
     for x from 0 to (- (cols map) 1)
     do (loop for y from (- (rows map) 1) downto 1
           do (update map x y new-map))
     finally return new-map))

