(in-package :aoc-2021)

(defun parse-file ()
  (with-monad
    (assign dots (parse-lines (parse-number-list)))
    (parse-newline)
    (parse-newline)
    (assign folds (parse-lines
		   (with-monad
		     (parse-string "fold along ")
		     (assign axis (with-monad
				    (assign char (parse-character "xy"))
				    (parse-character #\=)
				    (unit (if (char= char #\y) :y :x))))
		     (assign amount (parse-number))
		     (unit (list  axis amount)))))
    (unit (list dots  folds))))

(defun fold-along-y (coord)
  (lambda (p)
    (list (first p) (if (> (second p) coord)
			(- (* 2 coord) (second p))
			(second p)))))

(defun fold-along-x (coord)
  (lambda (p)
    (list  (if (> (first p) coord)
	       (- (* 2 coord) (first p))
	       (first p))
	   (second p))))


(defun day13 (input)
  (destructuring-bind (dots folds) (run-parser (parse-file) input)
    (setf dots (fset:convert 'fset:set dots))
    (iter
      (for (axis coord) in folds)
      (setf dots (fset:image (if (eq axis :y)
				 (fold-along-y coord)
				 (fold-along-x coord))
			     dots))
      (finally (return dots)))))

(defun print-code (coords)
  (iter
    (with ((min-x min-y) (max-x max-y)) = (get-min-max coords))
    (for y from min-y to max-y)
    (format t "~{~a~}~%"
	    (iter
	      (for x from min-x  to max-x)
	      (collect (if (fset:contains? coords (list x y)) #\* #\Space))))))
