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

;;; MAYBE MONAD

(defun maybe-unit (x)
  (list :just x))

(defun maybe-bind (ma f)
  (if (eq (car ma) :just)
      (funcall f (second ma))
      (list :nothing)))

(defun maybe-fail ()
  (list :nothing))

(defun maybe-mplus (ma mb)
  (if (eq (first ma) :nothing)
      mb
      ma))

(defun maybe-mzero ()
  (maybe-fail))

(defmacro run-maybe (m)
  `(let ((aoc:*unit-function* 'maybe-unit)
         (aoc:*bind-function* 'maybe-bind))
     ,m))

(defmacro from-maybe (ma &optional (default nil))
  `(let ((aoc:*unit-function* 'maybe-unit)
         (aoc:*bind-function* 'maybe-bind))
     (if (eq (car ,ma) :just)
	 (second ,ma)
	 ,default)))

;; Haskell Control-Monad-Loops.html

;; Monad m => [a -> m a] -> a -> m a
(defun concatm (&rest monads)
  (lambda (value)
    (if (null monads)
	(unit value)      
	(with-monad
	  (assign new-value (funcall (car monads) value))
	  (funcall (apply #'concatm (cdr monads)) new-value)))))

;; Monad m => (a -> Bool) -> (a -> m a) -> a -> m a
(defun iterate-untilm (pred f)
  (lambda (value)
    (if (funcall pred value)
	(unit value)
	(funcall (concatm f (iterate-untilm pred f)) value))))

;;; ZIPPER
;; http://learnyouahaskell.com/zippers
;; Zipper is a two element list - first element is the focus of the structure,
;; second element is the information needed to move around as a list of breadcrumbs

;; Zipper information
(defun zipper-tree (zipper)
  (first zipper))

(defun zipper-crumbs (zipper)
  (second zipper))

(defun zipper-depth (zipper)
  "How deep is the zipper. Zero is the root level."
  (length (zipper-crumbs zipper)))

(defun zipper-rootp (zipper)
  (null (zipper-crumbs zipper)))

(defun zipper-leafp (zipper)
  (numberp (zipper-tree zipper)))

(defun zipper-siblingp (type)
  "TYPE should be :LEFT or :RIGHT. Returns t if zipper is at that type of child"
  (lambda (zipper)
    (let ((crumbs (zipper-crumbs zipper)))
      (and crumbs (eq type (caar crumbs))))))

;; Zipper movement.
;; All movement return results in a Maybe. Will return :nothing if moved off the
;; tree

;; goLeft :: Zipper a -> Maybe (Zipper a)
(defun go-left ()
  (lambda (zipper)
    (if (zipper-leafp zipper)
	(maybe-fail)      
	(destructuring-bind ((left right) crumbs) zipper
          (unit (list left (cons (list :left right) crumbs)))))))

(defun go-right ()
  (lambda (zipper)
    (if (zipper-leafp zipper)
	(maybe-fail)
	(destructuring-bind ((left right) crumbs) zipper
          (unit (list right (cons (list :right left) crumbs)))))))

(defun go-up ()
  (lambda (zipper)
    (if (zipper-rootp zipper)
	(maybe-fail)
	(destructuring-bind (tree ((crumb-type crumb) . crumbs)) zipper
          (if (eq crumb-type :left)
              (unit (list (list tree crumb) crumbs))
              (unit (list (list crumb tree) crumbs)))))))

(defun go-topmost ()
  (iterate-untilm #'zipper-rootp (go-up)))

(defun go-leftmost ()
  (iterate-untilm #'zipper-leafp (go-left)))

(defun go-rightmost ()
  (iterate-untilm #'zipper-leafp (go-right)))

(defun go-right-sibling ()
  "Go to the closest right sibling in the tree. Fails if none exists. "
  (concatm (iterate-untilm (zipper-siblingp :left) (go-up)) (go-up) (go-right)))

(defun go-left-sibling ()
  "Go to the closest left sibling in the tree. Fails if none exists. "
  (concatm (iterate-untilm (zipper-siblingp :right) (go-up)) (go-up) (go-left)))

(defun go-next ()
  "Go to next inorder leaf of the tree. Fails if none exists. "
  (concatm (go-right-sibling) (go-leftmost)))

(defun go-prev ()
  "Go to previous inorder leaf of the tree. Fails is none exists. "
  (concatm (go-left-sibling) (go-rightmost)))

;; Zipper modification functions

;; modify :: (a -> b) -> Zipper a -> Maybe Zipper b
(defun modify (f)
  "Returns zipper with a the value at the focus modified by the function if at a leaf. Otherwise fails. "
  (lambda (zipper)
    (if (zipper-leafp zipper)
	(unit (list (funcall f (zipper-tree zipper))
		    (zipper-crumbs zipper)))
	(maybe-fail))))

(defun attach (new-tree)
  (lambda (zipper)
    (unit (list new-tree (zipper-crumbs zipper)))))

;; find-explosion-site :: Zipper a -> Maybe Zipper a
(defun find-explosion-site (zipper)
  (cond
    ((zipper-leafp zipper) (maybe-fail))
    ((= 4 (zipper-depth zipper)) (unit zipper))
    (t (maybe-mplus
	(with-monad
	  (assign left (funcall (go-left) zipper))
	  (find-explosion-site left))
	(with-monad
	  (assign right (funcall (go-right) zipper))
	  (find-explosion-site right))))))

(defun find-split-site (zipper)
  (if (and (zipper-leafp zipper) (>= (zipper-tree zipper) 10))
      (unit zipper)
      (maybe-mplus
       (with-monad
	 (assign left (funcall (go-left) zipper))
	 (find-split-site left))
       (with-monad
	 (assign right (funcall (go-right) zipper))
	 (find-split-site right)))))

(defun dont-fail (f)
  (lambda (zipper)
    (maybe-mplus
     (funcall f zipper)
     (unit zipper))))

;; add-previous :: Int -> Zipper a -> Maybe Zipper a
(defun add-previous (n)
  (concatm (go-prev)
	   (modify (lambda (x) (+ x n)))
	   (go-next)))
(defun add-next (n)
  (concatm (go-next)
	   (modify (lambda (x) (+ x n)))
	   (go-prev)))

(defun explode ()
  (lambda (zipper)
    (with-monad
      (assign explosion-site (find-explosion-site zipper))
      (let ((tree (zipper-tree explosion-site)))
	(funcall (concatm 
		  (attach 0)
		  (dont-fail (add-previous (first tree)))
		  (dont-fail (add-next (second tree))))
		 explosion-site)))))

(defun split ()
  (lambda (zipper)
    (with-monad
      (assign split-site (find-split-site zipper))
      (let ((tree (zipper-tree split-site)))
	(funcall (attach (list (floor tree 2) (ceiling tree 2)))
		 split-site)))))

(defun reduce-snailnum ()
  (lambda (zipper)
    (funcall (concatm
	      (go-topmost)
	      (lambda (zipper)
		(maybe-mplus
		 (funcall (explode) zipper)
		 (funcall (split) zipper)))
	      (go-topmost))
	     zipper)))

(defun reduce-snailnum-fully (snail-num)
  (iter
    (for reduced = (from-maybe (funcall (reduce-snailnum)
					(list snail-num nil))))
    (while reduced)
    (setf snail-num (first reduced))
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

