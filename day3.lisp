(in-package :aoc-2021)

(defun parse-file ()
  (parse-lines (parse-list (parse-digit) "")))

(defun most-common (numbers on-eq)
  (let* ((half-length (/ (length numbers) 2))
         (one-counts (reduce (lambda (a b) (map 'list #'+ a b)) numbers))
         (most-common (mapcar (lambda (c) (cond
                                            ((= c half-length) on-eq)
                                            ((> c half-length) 1)
                                            (t 0)))
                              one-counts)))
    most-common))

(defun remove-at (position value numbers)
  (remove-if (lambda (word) (= (elt word position) value)) numbers))

(defun get-oxygen-rating (position numbers)
  (if (= 1 (length numbers))
      (car numbers)
      (let ((most-common (elt (most-common numbers 1) position)))
        (get-oxygen-rating (1+ position)
                           (remove-at position (- 1 most-common) numbers)))))

(defun get-co2-rating (position numbers)
  (if (= 1 (length numbers))
      (car numbers)
      (let ((most-common (elt (most-common numbers 1) position)))
        (get-co2-rating (1+ position)
                        (remove-at position most-common numbers)))))

(defun day3 (input)
  (let* ((parsed (run-parser (parse-file) input)))
    (* (digits-to-int (get-oxygen-rating 0 parsed))
       (digits-to-int (get-co2-rating 0 parsed)))))
