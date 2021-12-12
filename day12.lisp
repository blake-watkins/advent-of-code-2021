(in-package :aoc-2021)

(defun parse-cave ()
  (with-monad
    (assign cave (parse-characters #'alpha-char-p))
    (unit (list (if (upper-case-p (elt cave 0)) :big :little) cave))))

(defun parse-file ()
  (parse-lines (parse-list (parse-cave) "-")))

(defun num-paths-from (cave path double-visit caves)
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
    (fset:adjoinf (fset:lookup ret from) to)
    (fset:adjoinf (fset:lookup ret to) from)
    (finally (return ret))))

(defun day12 (input &optional (part1 nil))
  (let* ((parsed (run-parser (parse-file) input))
         (caves (get-caves parsed)))
    (num-paths-from '(:little "start") (fset:empty-seq) part1 caves)))
