(ql:quickload :split-sequence)
(defun file-get-lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
         while line
         collect (split-sequence:split-sequence #\, line))))
; take an initial point and a
; puzzle component and return
; the points for htat puzzle component
; ((0 . 0) "R55") -> ((0 . 0) (0 . 1)... (0 . 54))
(defun build-points-helper (x-y current-point start end)
  (if (> end start) 
      (loop for i from start to end 
         collect (if (equal x-y #\x) (cons i (cdr current-point)) (cons (car current-point) i)))
      (loop for i from start downto end 
         collect (if (equal x-y #\x) (cons i (cdr current-point)) (cons (car current-point) i)))))
(defun build-points (current-point circuit-point-list)
  (if (> (car current-point) 1000) 
      '()
      (let* ((circuit-point (car circuit-point-list))
             (direction (char circuit-point 0))
             (end-value (parse-integer (subseq circuit-point 1)))
             (x-y (if (member direction '(#\R #\L)) #\x #\y))
             (sign (if (member direction '(#\R #\U)) 1 -1))
             (start (if (equal x-y #\x) (car current-point) (cdr current-point)))
             (end (+ (* end-value sign) start))
             (new-points (build-points-helper x-y current-point start end)))
        (print current-point)
        (print circuit-point)
        (print direction)
        (print end-value)
        (print x-y)
        (print sign)
        (print start)
        (print end)
        (append new-points (build-points (car (last new-points)) (cdr circuit-point-list))))))
(defun find-intersection-points (nodes-1 nodes-2)
  (loop for node in nodes-1
     collect (car (remove-if-not #'(lambda (x) (equal node x)) nodes-2))))
;; (defparameter *nodes1* (build-points (cons 0 0) (car (file-get-lines "d3.txt"))))
;; (defparameter *nodes2* (build-points (cons 0 0) (cadr (file-get-lines "d3.txt"))))
;; (defparameter *intersect* (remove-if #'null (find-intersection-points *nodes1* *nodes2*)))
;; (reduce #'m
