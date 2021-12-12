(in-package :aoc-2021)

(defun parse-file ()
  (parse-lines (parse-list (parse-characters #'alpha-char-p) "-")))

(defun big-cave-p (cave)
  (upper-case-p (elt cave 0)))

(defun num-paths-from (cave path allow-repeat caves)
  (if (equal cave "end")
      1
      (iter
        (for neighbour in-fset (fset:lookup caves cave))
        (for visited-p = (fset:find neighbour path))

        (when (or (big-cave-p neighbour)
                  (not visited-p)
                  (and allow-repeat (not (equal neighbour "start"))))
          (sum (num-paths-from neighbour
                               (fset:with-last path cave)
                               (and allow-repeat
                                    (or (big-cave-p neighbour)
                                        (not visited-p)))
                               caves))))))

(defun get-caves (parsed)
  (iter
    (with ret = (fset:empty-map (fset:empty-set)))
    (for (from to) in parsed)
    (fset:adjoinf (fset:lookup ret from) to)
    (fset:adjoinf (fset:lookup ret to) from)
    (finally (return ret))))

(defun day12 (input &key (part 2))
  (let* ((parsed (run-parser (parse-file) input))
         (caves (get-caves parsed)))
    (num-paths-from "start" (fset:empty-seq) (= part 2) caves)))
