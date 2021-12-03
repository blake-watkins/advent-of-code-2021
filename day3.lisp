(in-package :aoc-2021)

(defun parse-file ()
  (parse-lines (parse-list (parse-digit) "")))

(defun most-common (numbers)
  (let* ((half-length (/ (length numbers) 2))
         (one-counts (reduce (lambda (a b) (map 'list #'+ a b)) numbers))
         (most-common (mapcar (lambda (c) (cond
                                            ((= c half-length) :eq)
                                            ((> c half-length) 1)
                                            (t 0)))
                              one-counts)))
    most-common))

(defun keep-if (position value numbers)
  (remove-if-not (lambda (word) (= (elt word position) value)) numbers))

(defun get-rating (position numbers &key (oxygen-p t))
  (if (= 1 (length numbers))
      (car numbers)
      (let ((most-common (elt (most-common numbers) position)))
        (case most-common
          (:eq (get-rating (1+ position)
                           (keep-if position (if oxygen-p 1 0) numbers)
                           :oxygen-p oxygen-p))
          (t (get-rating (1+ position)
                         (keep-if position
                                  (if oxygen-p
                                      most-common
                                      (- 1 most-common))
                                  numbers)
                         :oxygen-p oxygen-p))))))

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
