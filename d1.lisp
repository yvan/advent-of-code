;;read in data
(defun read-file (file)
  (with-open-file (stream file)
    (loop for line = (read-line stream nil)
       while line
       collect (parse-integer line))))
;; calculate fuel cost
(defun calc-fuel-cost (mass)
  (- (floor (/ mass 3)) 2))
;; apply this to the data
(apply #'+ (mapcar #'calc-fuel-cost (read-file "data/d1.txt")))
;; calcualate module fuel
(defun calc-mod-fuel (module-size)
  (let ((module-fuel (calc-fuel-cost module-size)))
    (if (<= module-fuel 0)
        0
        (+ module-fuel (calc-mod-fuel module-fuel)))))
;; calculate additional fuel
(apply #'+ (mapcar #'calc-mod-fuel (read-file "data/d1.txt")))
