(in-package :aoc-2021)

(defun parse-file ()
  (parse-lines (parse-list (parse-character "({[<>]})") "")))

(defun closing-char (char)
  (ecase char (#\( #\)) (#\[ #\]) (#\{ #\}) (#\< #\>)))

(defun parse-syntax (chars)
  (iter
    (with closing = ())
    (for char in chars)
    (cond
      ((find char "({[<") (push (closing-char char) closing))
      ((char= char (first closing)) (pop closing))
      (t (leave (list :corrupted char))))
    (finally (return (if (= 0 (length closing))
                         (list :complete)
                         (list :incomplete closing))))))

(defun score-char (char)
  (ecase char (#\) 3) (#\] 57) (#\} 1197) (#\> 25137)))

(defun score-closing (closing)
  (reduce (lambda (acc c)
            (+ (* acc 5)
               (ecase c (#\) 1) (#\] 2) (#\} 3) (#\> 4))))
          closing
          :initial-value 0))

(defun median (scores)
  (setf scores (sort scores #'<))
  (elt scores (floor (length scores) 2)))

(defun day10 (input)
  (iter
    (for line in (run-parser (parse-file) input))
    (for (result item) = (parse-syntax line))
    (case result
      (:corrupted (summing (score-char item) into part1))
      (:incomplete (collecting (score-closing item) into part2-scores)))
    (finally (return (list part1 (median part2-scores))))))
