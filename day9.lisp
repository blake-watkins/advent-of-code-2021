(in-package :aoc-2021)

(defun parse-file ()
  (parse-lines (parse-list (parse-digit) "")))

(defun neighbours (p map)
  (remove-if-not
   (lambda (p) (destructuring-bind (a b) p
                 (and (<= 0 a (1- (length map)))
                      (<= 0 b (1- (length (first map)))))))
   (mapcar (lambda (d) (map 'list #'+  p d))
           '((1 0) (-1 0) (0 1) (0 -1)))))

(defun height (p map)
  (elt (elt map (first p)) (second p)))

(defun fill-map (from map)
  (let ((seen (fset:empty-set))
        (queue (fset:seq from)))
    (iter
      (while (> (fset:size queue) 0))
      (for cur = (fset:first queue))
      (for height = (height cur map))
      (setf queue (fset:less-first queue))
      (when (not (fset:contains? seen cur))
        (fset:includef seen cur)
        (iter
          (for n in (neighbours cur map))
          (for height-n = (height n map))
          (when (and (/= height-n 9)
                     (> height-n height))
            (setf queue (fset:with-last queue n))))))
    (fset:size seen)))

(defun get-sinks (map)
  (iter
    (with sinks = '())
    (for r below (length map))
    (iter
      (for c below (length (first map)))
      (for height = (height (list r c) map))
      (for neighbours = (neighbours (list r c) map))
      (when (every (lambda (n) (< height (height n map)))
                   (neighbours (list r c) map))
        (setf sinks (cons (list r c) sinks))))
    (finally (return sinks))))

(defun day9 (input)
  (let* ((parsed (run-parser (parse-file) input))
         (sinks (get-sinks parsed))
         (part1 (iter
                  (for sink in sinks)
                  (summing (1+ (height sink parsed))))))

    (let ((sizes (iter
                   (for sink in sinks)
                   (collect (fill-map sink parsed)))))
      (setf sizes (sort sizes #'>))
      (list part1 (reduce #'* (subseq sizes 0 3))))))
