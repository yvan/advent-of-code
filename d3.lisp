(ql:quickload "str")
(defun file-get-lines (filename)
  (with-open-file (stream filename)
    (loop for line = (read-line stream nil)
         while line
       collect (str:split #\, line))))

; take an initial point and a
; puzzle component and return
; the points for that puzzle component
; ((0 . 0) "R55") -> ((0 . 0) (0 . 1)... (0 . 54))
(defun build-points-helper (x-y current-point start end current-steps)
  (print "current steps func start")
  (print current-steps)
  (setq counter 0)
  (if (> end start) 
      (loop for i from start to end 
	 collect (progn (setq counter (1+ counter)) (if (equal x-y #\x)
					    (cons (cons i (cdr current-point)) (+ current-steps counter))
					    (cons (cons (car current-point) i) (+ current-steps counter)))))
      (loop for i from start downto end 
	 collect (progn (setq counter (1+ counter)) (if (equal x-y #\x)
					    (cons (cons i (cdr current-point)) (+ current-steps counter))
					    (cons (cons (car current-point) i) (+ current-steps counter)))))))

(defun find-intersection-points (nodes-1 nodes-2 check-steps)
  (if check-steps
      (loop for node in nodes-1
	 collect (car (remove-if-not #'(lambda (x) (equal (car node) (car x))) nodes-2)))
      (loop for node in nodes-1
	 collect (car (remove-if-not #'(lambda (x) (equal node x)) nodes-2)))))

(defun find-manhattan-distance (intersection)
  (apply #'min (remove 0 (mapcar
			  #'(lambda (node)
			      (+ (abs (caar node)) (abs (cdar node))))
			  intersection))))

(defun find-min-steps (intersection)
  (apply #'min (remove 0 (mapcar
			  #'(lambda (node)
			      (cdr node))
			  intersection))))

(defun build-points (current-point circuit-point-list &optional current-steps)
  (if (not circuit-point-list) 
      '()
      (let* ((circuit-point (car circuit-point-list))
	     (direction (char circuit-point 0))
	     (end-value (parse-integer (subseq circuit-point 1)))
	     (x-y (if (member direction '(#\R #\L)) #\x #\y))
	     (sign (if (member direction '(#\R #\U)) 1 -1))
	     (start (if (equal x-y #\x) (car current-point) (cdr current-point)))
	     (end (+ (* end-value sign) start))
	     (new-points (build-points-helper
			  x-y current-point
			  start
			  end
			  (if current-steps current-steps 0)))
	     (new-steps (if current-steps (cdar (last new-points)) nil)))
	;; (print (format nil "current-point: ~a" current-point))
	;; (print (format nil "circuit-point: ~a" circuit-point))
	;; (print (format nil "direction: ~a" direction))
	;; (print (format nil "x-y: ~a" x-y))
	;; (print (format nil "end-value: ~a" end-value))
	;; (print (format nil "sign: ~a" sign))
	;; (print (format nil "start: ~a" start))
	;; (print (format nil "end: ~a" end))
	;; (print (format nil "new-point: ~a" (car (last new-points))))
	;; (print "")
	(print "x")
	(print new-steps)
	(print "last steps")
	(print (last new-points))
	(print "y")
	(print (cdar (last new-points)))
	(if current-steps
	    (append new-points
		    (build-points
		     (car (car (last new-points)))
		     (cdr circuit-point-list)
		     new-steps))
	    (append new-points
		    (build-points
		     (car (car (last new-points)))
		     (cdr circuit-point-list)
		     nil))))))

(defparameter *nodes1* (build-points (cons 0 0) (car (file-get-lines "data/d3-test.txt")) 0))
(defparameter *nodes2* (build-points (cons 0 0) (cadr (file-get-lines "data/d3-test.txt")) 0))
(defparameter *intersect* (remove-if #'null (find-intersection-points *nodes1* *nodes2* t)))
;; (defparameter *result* (find-manhattan-distance *intersect*))
(defparameter *result2* (find-min-steps *intersect*))

