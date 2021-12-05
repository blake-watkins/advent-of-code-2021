(in-package :aoc-2021)

(defun parse-board ()
  (n-of 5 (n-of 5 (parse-until (parse-number)))))

(defun parse-file ()
  (with-monad
    (assign numbers (parse-number-list))
    (assign board (parse-list (parse-board) ""))
    (unit (list numbers board))))

(defun winning-p (board)
  (or
   (some (lambda (row) (every (lambda (n) (eq n :marked)) row)) board)
   (iter
     (for i below (length board))
     (thereis (every (lambda (row) (eq (elt row i) :marked)) board)))))

(defun play (num-called numbers board)
  (when numbers      
    (let* ((called (car numbers))
           (new-board (subst :marked called board)))
      (if (winning-p new-board)
          (list (1+ num-called) called new-board)
          (play (1+ num-called) (cdr numbers) new-board)))))

(defun score (called-number board)
  (let ((sum (reduce (lambda (acc row)
                       (+ acc (reduce #'+ row)))
                     (subst 0 :marked board)
                     :initial-value 0)))
    (* called-number sum)))

(defun day4 (input)
  (destructuring-bind (numbers boards) (run-parser (parse-file) input)
    (iter
      (for board in boards)
      (for result = (play 0 numbers board))      
      (when result
        (destructuring-bind (num-called last-number board) result
          (finding (score last-number board) minimizing num-called into part1)
          (finding (score last-number board) maximizing num-called into part2)))
      (finally (return (list part1 part2))))))
