(in-package :aoc-2021)

(defun parse-file ()
  (parse-number-list))

(defun age-fish (fish)
  (let ((new-fish (make-hash-table)))
    (iter
      (for (timer num-fish) in-hashtable fish)
      (if (= 0 timer)
          (progn
            (incf (gethash 6 new-fish 0) num-fish)
            (incf (gethash 8 new-fish 0) num-fish))
          (incf (gethash (1- timer) new-fish 0) num-fish))
      (finally (return new-fish)))))

(defun day6 (days input)
  (let* ((parsed (run-parser (parse-file) input))
         (fish (make-hash-table)))
    (iter
      (for timer in parsed)
      (incf (gethash timer fish 0)))
    
    (iter
      (repeat days)
      (setf fish (age-fish fish)))
    
    (iter
      (for (nil num) in-hashtable fish)
      (summing num))))
