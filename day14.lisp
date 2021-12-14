(in-package :aoc-2021)

(defun parse-file ()
  (with-monad
    (assign polymer (parse-characters #'upper-case-p))
    (n-of 2 (parse-newline))
    (assign templates (parse-lines
                       (with-monad
                         (assign pattern (parse-characters #'upper-case-p))
                         (parse-string " -> ")
                         (assign replacement (parse-character #'upper-case-p))
                         (unit (cons pattern replacement)))))
    (unit (list polymer templates))))

(defun expand (polymer templates)
  (format nil "~{~a~}" (cons (elt polymer 0)
                             (iter
                               (for i below (1- (length polymer)))
                               (collect (cdr (assoc (subseq polymer i (+ i 2)) templates :test 'equal)))
                               (collect (elt polymer (1+ i)))))))
(defun get-pairs (polymer)
  (iter
    (for i below (1- (length polymer)))
    (collect (subseq polymer i (+ i 2)))))

(defun get-map (polymer)
  (iter
    (with ret = (fset:empty-map 0))
    (for pair in (get-pairs polymer))
    (incf (fset:lookup ret pair))
    (finally (return ret))))

(defun expand-map (map templates)
  (iter
    (with ret = (fset:empty-map 0))
    (for pair in-fset map)
    (iter
      (for new-pair in (get-pairs (expand pair templates)))
      (incf (fset:lookup ret new-pair) (fset:lookup map pair)))
    (finally (return ret))))

(defun counts (map orig)
  (let ((ret (fset:empty-map 0)))    
    (iter
      (for pair in-fset map)
      (iter
        (for letter in-string pair)
        (incf (fset:lookup ret letter) (fset:lookup map pair))))
    (incf (fset:lookup ret (elt orig 0)))
    (incf (fset:lookup ret (elt orig (1- (length orig)))))
    ret))

(defun score (map)
  (iter
    (for letter in-fset map)
    (maximizing (fset:lookup map letter) into max)
    (minimizing (fset:lookup map letter) into min)
    (finally (return (floor (- max min) 2)))))

(defun day14-2 (input steps)
  (let* ((parsed (run-parser (parse-file) input)))
    (destructuring-bind (polymer templates) parsed
      (let ((map (get-map polymer)))
        (iter
          (repeat steps)
          (setf map (expand-map map templates)))
        (score (counts map polymer))))))

(defun day14 (input)
  (let* ((parsed (run-parser (parse-file) input)))
    (destructuring-bind (polymer templates) parsed
      (iter
        (repeat 7)
        (format t "~a~%" polymer)
        (setf polymer (expand polymer templates)))
      (iter
        (with ret = (fset:empty-bag))
        (for i below (length polymer))
        (fset:includef ret (elt polymer i))
        (finally (return ret))))))
