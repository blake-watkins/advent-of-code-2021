(in-package :aoc-2021)

(defun parse-file ()
  (with-monad
    (assign polymer (parse-list (parse-upper-case) ""))
    (n-of 2 (parse-newline))
    (assign templates (parse-lines
                       (with-monad
                         (assign pattern (parse-list (parse-upper-case) ""))
                         (parse-string " -> ")
                         (assign replacement (parse-upper-case))
                         (unit (cons pattern replacement)))))
    (unit (list polymer templates))))

;; Use cached internal recursive function. Base case is zero steps, in which case
;; return a bag with just the start and end characters. Otherwise, find the character
;; that goes between start and end (from the template) and add the bag of characters
;; from the start to the middle character and the bag from the middle to the end. The
;; middle character was counted twice, so remove a copy.
(defun expand (steps start end templates)
  "Expand the polymer between the characters START and END for STEPS steps. Return a bag of the number of each element."
  (let ((cache (fset:empty-map)))
    (labels ((expand-rec (steps start end)
               (let ((cached (fset:lookup cache (list steps start end))))
                 (cond
                   (cached cached) 
                   ((= 0 steps) (fset:bag start end))  
                   (t
                    (let* ((mid (cdr (assoc (list start end) templates :test 'equal)))
                           (bag (fset:less
                                 (fset:bag-sum
                                  (expand-rec (1- steps) start mid)
                                  (expand-rec (1- steps) mid end))
                                 mid)))
                      (fset:includef cache (list steps start end) bag)
                      bag))))))
      (expand-rec steps start end))))

;; For each pair in the polymer, expand it to get the bag of characters between the
;; start and end. Add the bags together. All of the elements in the middle of the
;; polymer have been added twice, so remove one of each from the bag. 
(defun count-elements (steps polymer templates)
  (iter
    (with ret = (fset:empty-bag))
    (for (start end) on polymer)
    (when end
      (setf ret (fset:bag-sum ret (expand steps start end templates))))    
    (finally
     (let ((repeated (subseq polymer 1 (1- (length polymer)))))
       (return (fset:bag-difference ret (fset:convert 'fset:bag repeated)))))))

(defun score (bag)
  (iter
    (for (nil m) in-fset-bag bag)    
    (maximizing m into max)
    (minimizing m into min)
    (finally (return (- max min)))))

(defun day14 (input &key (part 2))
  (let* ((parsed (run-parser (parse-file) input)))
    (destructuring-bind (polymer templates) parsed
      (score (count-elements (if (= part 1) 10 40) polymer templates)))))
