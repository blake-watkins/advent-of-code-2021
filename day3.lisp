(in-package :aoc-2021)

(defun parse-file ()
  (parse-lines (parse-list (parse-digit) "")))

(defun most-common (numbers)
  (let* ((half-length (/ (length numbers) 2))
         (one-counts (reduce (lambda (a b) (map 'list #'+ a b)) numbers)))
    (mapcar (lambda (c) (cond
                          ((> c half-length) 1)
                          ((= c half-length) :eq)
                          (t 0)))
            one-counts)))

(defun keep-if (position value numbers)
  (remove-if-not (lambda (word) (= (elt word position) value)) numbers))

(defun get-rating (position numbers &key (oxygen-p t))
  (if (= 1 (length numbers))
      (car numbers)
      (let* ((most-common (elt (most-common numbers) position))
             (keep-if (case most-common
                        (:eq (if oxygen-p 1 0))
                        (t (if oxygen-p most-common (- 1 most-common))))))
        (get-rating (1+ position)
                    (keep-if position keep-if numbers)
                    :oxygen-p oxygen-p))))

(defun day3 (input)
  (let* ((parsed (run-parser (parse-file) input))
         (most-common (most-common parsed))
         (gamma (digits-to-int most-common))
         (epsilon (digits-to-int (mapcar (lambda (x) (- 1 x)) most-common))))
    (* gamma epsilon)))

(defun day3-2 (input)
  (let ((parsed (run-parser (parse-file) input)))
    (* (digits-to-int (get-rating 0 parsed))
       (digits-to-int (get-rating 0 parsed :oxygen-p nil)))))
