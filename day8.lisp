(in-package :aoc-2021)

(defun parse-pattern ()
  (with-monad
    (assign word (parse-characters #'lower-case-p))
    (unit (fset:convert 'fset:set word))))

(defun parse-file ()
  (parse-lines
   (parse-list (parse-list (parse-pattern) #\Space) (parse-string " | "))))

(defun day8 (input)
  (let ((parsed (run-parser (parse-file) input)))
    (iter
      (for (patterns words) in parsed)
      (summing (iter
                 (for word in words)
                 (count (member (fset:size word) '(2 3 4 7))))))))

;; PICK and DECODE-PATTERNS are Game monad functions. PICK uses the AMB operator
;; to choose an element from a list that will pass subsequent GUARD calls. 

;; The state in the game monad is the list of the (initially) 10 different patterns.
(defun pick (&key size)
  (with-monad
    (assign remaining (get-state))
    (assign choice (amb remaining))
    (guard (= size (fset:size choice)))
    (set-state (remove choice remaining))
    (unit choice)))

(defun decode-patterns ()
  (with-monad
    (assign one (pick :size 2))
    (assign four (pick :size 4))
    (assign seven (pick :size 3))
    (assign eight (pick :size 7))

    (assign six (pick :size 6))
    (guard (= 1 (num-intersections-with six one)))
    (assign nine (pick :size 6))
    (guard (= 4 (num-intersections-with nine four)))
    (assign zero (pick :size 6))

    (assign three (pick :size 5))
    (guard (= 2 (num-intersections-with three one)))
    (assign two (pick :size 5))
    (guard (= 2 (num-intersections-with two four)))
    (assign five (pick :size 5))

    (unit (list zero one two three four five six seven eight nine))))

(defun day8-2 (input)
  (let ((parsed (run-parser (parse-file) input)))
    (iter
      (for (patterns words) in parsed)
      (for mapping = (first (run-game (decode-patterns) patterns)))
      (summing (digits-to-int
                (mapcar (lambda (w)
                          (fset:lookup (convert-to-map mapping) w))
                        words)
                :base 10)))))

(defun num-intersections-with (a b)
  (fset:size (fset:intersection a b)))

(defun convert-to-map (words)
  (fset:convert 'fset:map
                (iter
                  (for val from 0)
                  (for word in words)
                  (collect (cons word val)))))


