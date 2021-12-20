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

;; Zippers
;; http://learnyouahaskell.com/zippers
;;
;; Maybe monad

;; Given a function f :: Zipper -> Maybe Zipper, this will return a function
;; (dont-fail f) :: Zipper -> Maybe Zipper that will not be :nothing. If f
;; would have failed, then (dont-fail f) will return the zipper originally
;; passed to it, otherwise it will return f's result.
(defun dont-fail (f)
  (lambda (zipper)
    (maybe-mplus
     (funcall f zipper)
     (unit zipper))))

;; add-to-previous :: Int -> Zipper a -> Maybe Zipper a
;; Takes an Int and returns a function taking a Zipper -> Maybe Zipper. When
;; the function is given a zipper it attempts to go to the previous, add the
;; int, then come back. If anything fails this function will return the
;; unaltered zipper, otherwise it will return the altered one.
(defun add-to-previous (n)
  (dont-fail (lambda (explosion-site)
	       (with-zipper explosion-site
		 (go-prev)
		 (modify (lambda (x) (+ x n)))
		 (go-next)))))

(defun add-to-next (n)
  (dont-fail (lambda (explosion-site)
	       (with-zipper explosion-site
		 (go-next)
		 (modify (lambda (x) (+ x n)))
		 (go-prev)))))

;; find-explosion-site :: Tree a -> Maybe Zipper a
;; Uses utility fun FIND-IN-TREE that returns a zipper to the first (in-order)
;; node that satisfies the predicate. Returns :nothing if there aren't any
;; explosion sites.
;; The explosion site must be a pair (list) at depth 4.
(defun find-explosion-site (tree)
  (find-in-tree tree
		(lambda (tree-node depth)
		  (and (listp tree-node) (= 4 depth)))))

;; Takes a tree, returns a Maybe Tree. A tree is returned if there was an
;; explosion, otherwise :nothing is returned.
(defun explode (tree)
  (with-monad
    (assign explosion-site (find-explosion-site tree))
    (assign ret (let ((pair (zipper-tree explosion-site)))
		  (with-zipper explosion-site
		    (attach 0)
		    (add-to-previous (first pair))
		    (add-to-next (second pair)))))
    (unit (zipper-to-tree ret))))

;; A split site is a single number that is >= 10. 
(defun find-split-site (tree)
  (find-in-tree tree
		(lambda (tree-node depth)
		  (declare (ignore depth))
		  (and (numberp tree-node) (>= tree-node 10)))))

;; Split takes a tree and returns a Maybe Tree. If there was a split the new
;; tree is returned, otherwise :nothing is returned. 
(defun split (tree)
  (with-monad
    (assign split-site (find-split-site tree))
    (assign ret (let* ((number (zipper-tree split-site))
		       (new-pair (list (floor number 2)
				       (ceiling number 2))))
		  (funcall (attach new-pair) split-site)))
    (unit (zipper-to-tree ret))))

;; Fully reduces snailnum by exploding and splitting. 
(defun snailnum-reduce (snailnum)
  (iter
    (for exploded = (from-maybe (explode snailnum)))
    (when exploded
      (setf snailnum exploded)
      (next-iteration))
    (for split = (from-maybe (split snailnum)))
    (when split
      (setf snailnum split)
      (next-iteration))
    (while (or exploded split))
    (finally (return snailnum))))

;; Add two snailnums. Make a list and then reduce it.
(defun snailnum+ (a b)
  (snailnum-reduce (list a b)))

(defun snailnum-magnitude (snailnum)
  (cond
    ((numberp snailnum) snailnum)
    (t (+ (* 3 (snailnum-magnitude (first snailnum)))
	  (* 2 (snailnum-magnitude (second snailnum)))))))

(defun day18 (input)
  (let ((parsed (run-parser (parse-lines (parse-pair)) input)))
    (snailnum-magnitude (reduce #'snailnum+ parsed))))

(defun day18-2 (input)
  (let ((parsed (run-parser (parse-lines (parse-pair)) input)))
    (iter outer
      (for a in parsed)
      (iter
	(for b in parsed)
	(unless (equal a b))
	(in outer (maximizing
		   (max (snailnum-magnitude (snailnum+ a b))
			(snailnum-magnitude (snailnum+ b a)))))))))

