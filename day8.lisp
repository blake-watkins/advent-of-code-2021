(in-package :aoc-2021)

(defun parse-file ()
  (parse-lines (with-monad
                 (assign patterns (parse-list (parse-characters #'lower-case-p) #\Space))
                 (parse-string " | ")
                 (assign words (parse-list (parse-characters #'lower-case-p) #\Space))
                 (unit (list patterns words)))))

(defun map-code (str)
  (list ("cg" . 1) ("bcdg" . 4) ("acg" .7) ("abcdefg" . 8) ("abcefg" . ) ("abcdeg" . )
        ("abcfg" . ) ("abdfg" . )))

(defun normalize (str)
  (sort str #'char<))

(defun decode-words (words)
  (let ((ret (make-hash-table :test 'equal)))
    (iter
      (for word in words)
      (setf word (normalize word))
      (case (length word)
        (2 (setf (gethash 1 ret) word))
        (3 (setf (gethash 7 ret) word))
        (4 (setf (gethash 4 ret) word))
        (7 (setf (gethash 8 ret) word))))
    (iter
      (for word in words)
      (setf word (normalize word))
      (when (and (= (length word) 6)
                 (= 1 (count-if (lambda (c) (find c (gethash 1 ret))) (iter (for c in-string word) (collect c)))))
        (setf (gethash 6 ret) word)))
    (iter
      (for word in words)
      (setf word (normalize word))
      (when (and (= (length word) 6)
                 (not (equal word (gethash 6 ret))))
        (if (every (lambda (c) (find c word)) (gethash 4 ret))
            (setf (gethash 9 ret) word)
            (setf (gethash 0 ret) word)
            )))
    (iter
      (for word in words)
      (setf word (normalize word))
      (when (and (= (length word) 5)
                 (= 1 (edit-distance word (gethash 6 ret))))
        (setf (gethash 5 ret) word)))
    (iter
      (for word in words)
      (setf word (normalize word))
      (when (and (= (length word) 5)
                 (not (equal word (gethash 5 ret))))
        (if (every (lambda (c) (find c word)) (gethash 1 ret))
            (setf (gethash 3 ret) word)
            (setf (gethash 2 ret) word))))
    ret))

(defun day8 (input)
  (let ((parsed (run-parser (parse-file) input)))
    (reduce #'+  (iter
                   (for (pattern words) in parsed)
                   (for table = (decode-words pattern))
                   (format t "~a~%" table)
                   (collect (digits-to-int (iter
                                             (for word in words)
                                             (setf word (normalize word))
                                             (format t "~a~%" word)
                                             (collect (iter (for (d w) in-hashtable table) (finding d such-that (equal w word))))) :base 10))))))
