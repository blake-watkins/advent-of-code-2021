(in-package :aoc-2021)

(defun parse-cave ()
  (either (with-monad
            (assign cave (parse-characters #'upper-case-p))
            (unit (list :big cave)))
          (with-monad
            (assign cave (parse-characters #'lower-case-p))
            (unit (list :little cave)))))

(defun parse-file ()
  (parse-lines (parse-list (parse-cave) "-")))

(defun num-paths-from (cave path caves)
  (if (equal cave '(:little "end"))
      1
      (iter
        (for neighbour in-fset (fset:lookup caves cave))
        (for type = (first neighbour))
        (for visited-p = (fset:find neighbour path))
        
        (when (or (eq type :big)
                  (not visited-p))
          (summing (num-paths-from neighbour
                                   (fset:with-last path cave)
                                   caves))))))

(defun num-paths-from-2 (cave path double-visit caves)
  (if (equal cave '(:little "end"))
      1
      (iter
        (for neighbour in-fset (fset:lookup caves cave))
        (for (type name) = neighbour)
        (for visited-p = (fset:find neighbour path))

        (when (or (eq type :big)
                  (not visited-p)
                  (and (not double-visit)
                       (not (equal name "start"))))
          (summing (num-paths-from-2 neighbour
                                     (fset:with-last path cave)
                                     (or double-visit
                                         (and visited-p
                                              (eq type :little)))
                                     caves))))))

(defun get-caves (parsed)
  (iter
    (with ret = (fset:empty-map (fset:empty-set)))
    (for (from to) in parsed)
    (setf (fset:lookup ret from) (fset:with (fset:lookup ret from) to))
    (setf (fset:lookup ret to) (fset:with (fset:lookup ret to) from))
    (finally (return ret))))

(defun day12 (input)
  (let* ((parsed (run-parser (parse-file) input))
         (caves (get-caves parsed))
         (part1 (num-paths-from '(:little "start")
                                (fset:empty-seq)
                                caves))
         (part2 (num-paths-from-2 '(:little "start")
                                  (fset:empty-seq)
                                  nil
                                  caves)))
    (list part1 part2)))
