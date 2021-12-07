(in-package :aoc-2021)

(defun parse-file ()
 (parse-list (parse-number) (parse-newline)))

(defun sum-between (nums a b)
  (reduce #'+ (subseq nums a b)))

(defun day1 (input)
  (let ((parsed (run-parser (parse-file) input)))
    (iter
      (with part1 = 0)
      (with part2 = 0)
      
      (for idx from 0)
      (for num in parsed)
      
      (when (>= idx 1)
	(when (> (elt parsed idx)
		 (elt parsed (1- idx)))
	  (incf part1)))

      (when (>= idx 3)
	(when (> (sum-between parsed (- idx 2) (1+ idx))
		 (sum-between parsed (- idx 3) idx))
	  (incf part2)))
      
      (finally (return (list part1 part2))))))
