(in-package :aoc-2021)

(defun day7 (input)
  (let ((parsed (run-parser (parse-number-list) input)))
    (iter
      (for pos from (apply #'min parsed) to (apply #'max parsed))
      (minimizing
       (reduce #'+ (mapcar (lambda (v)
			     (abs (- v pos)))
			   parsed))
       into part1)
      (minimizing
       (reduce #'+ (mapcar (lambda (v)
			     (let ((d (abs (- v pos))))
			       (/ (* d (1+ d)) 2)))
			   parsed))
       into part2)
      (finally (return (list part1 part2))))))
