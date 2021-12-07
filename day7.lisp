(in-package :aoc-2021)

(defun day7 (input &optional (part1 nil))
  (let ((parsed (run-parser (parse-number-list) input)))
    (iter
      (for pos from (apply #'min parsed) to (apply #'max parsed))
      (for dist = (if part1
                      (lambda (v)
                        (abs (- v pos)))
                      (lambda (v)
			(let ((d (abs (- v pos))))
			  (/ (* d (1+ d)) 2)))))
      (minimizing (reduce #'+ (mapcar dist parsed))))))
