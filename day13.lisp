(in-package :aoc-2021)

(defun parse-fold ()
  (with-monad
    (parse-string "fold along ")
    (assign axis (with-monad
		   (assign axis (parse-character "xy"))
		   (unit (if (char= axis #\x) :x :y))))		     
    (parse-character #\=)
    (assign amount (parse-number))
    (unit (list axis amount))))

(defun parse-file ()
  (with-monad
    (assign dots (parse-lines (parse-number-list)))
    (parse-newline)
    (parse-newline)
    (assign folds (parse-lines (parse-fold)))
    (unit (list (fset:convert 'fset:set dots) folds))))

(defun reflect-about (coord c)
  (if (> c coord)
      (- (* 2 coord) c)
      c))

(defun fold (dots fold)
  (destructuring-bind (axis coord) fold
    (fset:image
     (lambda (p)
       (destructuring-bind (x y) p
	 (case axis
	   (:x (list (reflect-about coord x) y))
	   (:y (list x (reflect-about coord y))))))
     dots)))

(defun print-code (coords)
  (iter
    (with (max-x max-y) = (fset:reduce (lambda (acc coord)
					 (map 'list #'max acc coord))
				       coords))
    (for y to max-y)
    (format t "~{~a~}~%"
	    (iter
	      (for x to max-x)
	      (collect (if (fset:contains? coords (list x y))
			   #\U+2588 #\Space))))))

(defun day13 (input &key (part 2))
  (destructuring-bind (dots folds) (run-parser (parse-file) input)
    (if (= part 1)
	(progn
	  (setf folds (subseq folds 0 1))
	  (fset:size (fset:reduce #'fold folds :initial-value dots)))
	(print-code (fset:reduce #'fold folds :initial-value dots)))))


