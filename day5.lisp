(in-package :aoc-2021)

(defun parse-file ()
  (parse-lines (parse-list (parse-number-list) (parse-string " -> "))))

(defun day5 (input &optional (part2 nil))
  (let ((parsed (run-parser (parse-file) input))
	(counts (make-hash-table :test 'equal)))
    (iter
      (for (a b) in parsed)
      (for diff = (point- b a))
      (for dir = (point-signum diff))
      (for steps = (1+ (apply #'max (point-abs diff))))
      (when (or part2 (find 0 diff))
	(iter
	  (repeat steps)
	  (for pos first a then (point+ pos dir))
	  (incf (gethash pos counts 0)))))
    (iter
      (for (nil v) in-hashtable counts)
      (counting (>= v 2)))))
