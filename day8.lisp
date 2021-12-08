(in-package :aoc-2021)

(defun parse-pattern ()
  (with-monad
    (assign word (parse-characters #'lower-case-p))
    (unit (fset:convert 'fset:set word))))

(defun parse-file ()
  (parse-lines (parse-list (parse-list (parse-pattern) #\Space) (parse-string " | "))))

(defun decode-words (words)
  (let ((ret (make-hash-table :test 'equal))
        (lengths (make-hash-table)))
    (iter
      (for word in words)
      (cond
        ((member (fset:size word) '(2 3 4 7))
         (case (fset:size word)
           (2 (setf (gethash 1 ret) word))
           (3 (setf (gethash 7 ret) word))
           (4 (setf (gethash 4 ret) word))
           (7 (setf (gethash 8 ret) word))))
        (t (push word (gethash (fset:size word) lengths)))))
    (iter
      (for word in (gethash 6 lengths))
      (cond
        ((= 2 (fset:size (fset:intersection (gethash 7 ret) word)))
         (setf (gethash 6 ret) word))
        ((= 4 (fset:size (fset:intersection (gethash 4 ret) word)))
         (setf (gethash 9 ret) word))
        (t (setf (gethash 0 ret) word))))
    (iter
      (for word in (gethash 5 lengths))
      (cond
        ((= 5 (fset:size (fset:intersection (gethash 6 ret) word)))
         (setf (gethash 5 ret) word))
        ((= 2 (fset:size (fset:intersection (gethash 1 ret) word)))
         (setf (gethash 3 ret) word))
        (t (setf (gethash 2 ret) word))))
    ret))

(defun day8 (input)
  (let ((parsed (run-parser (parse-file) input)))
    (iter
      (for (pattern words) in parsed)
      (for table = (decode-words pattern))
      (summing
       (digits-to-int
        (iter
          (for word in words)
          (collect (iter
                     (for (d w) in-hashtable table)
                     (finding d such-that (eq :equal (fset:compare w word))))))
        :base 10)))))
