(in-package :aoc-2021)

(defun parse-map ()
  (parse-lines (one-or-more (with-monad
                              (assign char (parse-character ">v."))
                              (unit (ecase char
                                      (#\> :east)
                                      (#\v :south)
                                      (#\. :empty)))))))

(defun get-map (input)
  (let ((parsed (run-parser (parse-map) input)))
    (iter
      (with ret = (fset:empty-map :empty))
      (for r below (length parsed))
      (for row in parsed)
      (iter
        (for c below (length row))
        (for square in row)
        (unless (eq square :empty)
          (fset:includef ret (list r c) square)))
      (finally (return (list ret
                             (list (length parsed)
                                   (length (first parsed)))))))))

(defun adjacent (square type dimensions)
  (mapcar #'mod
          (point+ square (if (eq type :east) '(0 1) '(1 0)))
          dimensions))

(defun step-type (type map dimensions)
  (iter
    (with ret = (fset:empty-map :empty))
    (with changed = nil)
    (for square in-fset map)
    (for square-type = (fset:lookup map square))
    (cond
      ((eq type square-type)
       (let ((adjacent (adjacent square type dimensions)))
         (if (eq (fset:lookup map adjacent) :empty)
             (progn
               (fset:includef ret adjacent type)
               (setf changed t))
             (fset:includef ret square type))))
      (t (fset:includef ret square square-type)))
    (finally (return (list ret changed)))))

(defun step-map (map dimensions)
  (destructuring-bind (map-2 east-changedp) (step-type :east map dimensions)
    (destructuring-bind (map-3 south-changedp) (step-type :south map-2 dimensions)
      (list map-3 (or east-changedp south-changedp)))))

(defun day25 (input)
  (destructuring-bind (map dimensions) (get-map input)
    (iter
      (for steps from 1)
      (for (new-map changedp) = (step-map map dimensions))
      (while changedp)
      (setf map new-map)
      (finally (return steps)))))
