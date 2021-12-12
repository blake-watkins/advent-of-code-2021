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
        (for (neighbour-type neighbour) in-fset (fset:lookup caves cave))
        (when (or (eq neighbour-type :big)
                  (not (fset:find (list neighbour-type neighbour)path)))
          (summing (num-paths-from (list neighbour-type neighbour)
                                   (fset:push-last path cave)
                                   caves))))))

(defun num-paths-from-2 (cave path double-small caves)
  (if (equal cave '(:little "end"))
      1
      (iter
        (for (neighbour-type neighbour) in-fset (fset:lookup caves cave))
        (let ((num-visited (fset:count-if
                            (lambda (p)
                              (equal p (list neighbour-type neighbour)))
                            path)))
          (when (or (eq neighbour-type :big)
                    (= num-visited 0)
                    (and (= num-visited 1)
                         (not double-small)
                         (not (string-equal neighbour "start"))))
            (summing (num-paths-from-2 (list neighbour-type neighbour)
                                       (fset:with-last path cave)
                                       (or double-small
                                           (and (eq neighbour-type :little)
                                                (= num-visited 1)))
                                       caves)))))))


(defun day12 (input)
  (let* ((parsed (run-parser (parse-file) input))
         (caves (iter
                  (with ret = (fset:empty-map (fset:empty-set)))
                  (for (from to) in parsed)
                  (setf ret (fset:with ret from
                                       (fset:with (fset:lookup ret from) to)))
                  (setf ret (fset:with ret to
                                       (fset:with (fset:lookup ret to) from)))
                  (finally (return ret)))))

    (num-paths-from-2 '(:little "start")
                      (fset:empty-seq)
                      nil
                      caves)))
