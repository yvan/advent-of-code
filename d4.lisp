(ql:quickload :rutils)
(ql:quickload :str)

(defparameter *puzzle-input* '(231832 767346))

(defun check-6-digits (input)
  (eql (length input) 6))

(defun check-2-adjacent-digits (input)
  ())
