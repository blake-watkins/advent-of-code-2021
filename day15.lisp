(in-package :aoc-2021)

(defun parse-file ()
  (parse-lines (parse-list (parse-digit) "")))

(defun neighbours (p map)
  (remove-if-not (lambda (p) (and (<= 0 (first p)  (1- (length map)))
				  (<= 0 (second p) (1- (length (first map))))))
             (mapcar (lambda (d) (map 'list #'+ d p))
                     '((1 0) (0 1) (-1 0) (0 -1)))))

(defun risk (cur map)
  (elt (elt map (first cur)) (second cur)))

(defun total-risk (cur map)
  (let ((cache (fset:empty-map)))
    (labels ((total-risk-rec (cur map)
               (cond
                 ((fset:domain-contains? cache cur)
                  (fset:lookup cache cur))
                 ((null (neighbours cur map)) (risk cur map))
                 (t 
                  (let ((ret
                          (+ (reduce #'min
                                     (mapcar
                                      (lambda (n) (total-risk-rec n map))
                                      (neighbours cur map)))
                             (risk cur map))))
                    (fset:includef cache cur ret)
                    ret)))))
      (total-risk-rec cur map))))


(defun day15 (input)
  (let ((map (run-parser (parse-file) input))
        (ret (fset:empty-map)))
    (dijkstra '(0 0)
              (lambda (vertex parent distance)
		(declare (ignore parent))
                (fset:includef ret vertex distance))
              (lambda (v)
                (mapcar (lambda (n) (list n (risk n map)))
                        (neighbours v map))))
    ret))
