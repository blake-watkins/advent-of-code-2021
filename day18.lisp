(in-package :aoc-2021)

(defun parse-pair ()
  (either (with-monad
            (parse-character #\[)
            (assign x (parse-pair))
            (parse-character #\,)
            (assign y (parse-pair))
            (parse-character #\])
            (unit (list x y)))
          (parse-number)))

;; find-explosion-site :: Tree a -> Maybe Zipper a
(defun find-explosion-site (tree)
  (find-in-tree tree (lambda (tree-node depth)
		       (and (listp tree-node) (= 4 depth)))))

(defun find-split-site (tree)
  (find-in-tree tree (lambda (tree-node depth)
		       (declare (ignore depth))
		       (and (numberp tree-node) (>= tree-node 10)))))


(defun dont-fail (f)
  (lambda (zipper)
    (maybe-mplus
     (funcall f zipper)
     (unit zipper))))

;; add-previous :: Int -> Zipper a -> Maybe Zipper a
(defun add-previous (n)
  (dont-fail
   (concatm (go-prev)
	    (modify (lambda (x) (+ x n)))
	    (go-next))))

(defun add-next (n)
  (dont-fail
   (concatm (go-next)
	    (modify (lambda (x) (+ x n)))
	    (go-prev))))

;; explode :: Tree -> Maybe Tree
(defun explode (tree)
  (with-monad
    (assign explosion-site (find-explosion-site tree))
    (assign ret (let ((pair (zipper-tree explosion-site)))
		  (funcall (concatm 
			    (attach 0)
			    (add-previous (first pair))
			    (add-next (second pair)))
			   explosion-site)))
    (unit (zipper-to-tree ret))))

(defun split (tree)
  (with-monad
    (assign split-site (find-split-site tree))
    (assign ret (let* ((number (zipper-tree split-site))
		       (new-pair (list (floor number 2)
				       (ceiling number 2))))
		  (funcall (attach new-pair) split-site)))
    (unit (zipper-to-tree ret))))

(defun reduce-snailnum-fully (snail-num)
  (iter
    (for exploded = (from-maybe (explode snail-num)))
    (when exploded
      (setf snail-num exploded)
      (next-iteration))
    (for split = (from-maybe (split snail-num)))
    (when split
      (setf snail-num split)
      (next-iteration))
    (while (or exploded split))
    (finally (return snail-num))))

(defun add-snailnums (acc v)
  (reduce-snailnum-fully (list acc v)))

(defun magnitude (snailnum)
  (cond
    ((numberp snailnum) snailnum)
    (t (+ (* 3 (magnitude (first snailnum)))
	  (* 2 (magnitude (second snailnum)))))))

(defun day18 (input)
  (let ((parsed (run-parser (parse-lines (parse-pair)) input)))
    (magnitude (reduce #'add-snailnums parsed))))

(defun day18-2 (input)
  (let ((parsed (run-parser (parse-lines (parse-pair)) input)))
    (iter outer
      (for a in parsed)
      (iter
	(for b in parsed)
	(unless (equal a b))
	(in outer (maximizing
		   (max (magnitude (add-snailnums a b))
			(magnitude (add-snailnums b a)))))))))

