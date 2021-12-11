(in-package :aoc-2021)

(defun parse-file ()
  (parse-lines (parse-list (parse-digit) "")))

(defun neighbours (p map)
  (remove-if-not
   (lambda (p) (gethash p map))
   (mapcar (lambda (d) (map 'list #'+  p d))
           '((1 0) (-1 0) (0 1) (0 -1) (1 1) (-1 1) (1 -1) (-1 -1)))))

(defun increase (map)
  (iter
    (with ret = (make-hash-table :test 'equal))
    (with to-flash = (fset:empty-seq))
    (for (octopus brightness) in-hashtable map)
    (setf (gethash octopus ret) (1+ brightness))
    (when (> (1+ brightness) 9)
      (fset:push-last to-flash octopus))
    (finally (return (list ret to-flash)))))

(defun flash (map to-flash)
  (iter
    (with flashed = (fset:empty-set))
    (while (> (fset:size to-flash) 0))
    (for flashing = (fset:pop-first to-flash))
    (when (not (fset:contains? flashed flashing))
      (mapc (lambda (p)
              (let ((new (1+ (gethash p map))))
                (setf (gethash p map) new)
                (when (> new 9)
                  (fset:push-last to-flash p))))
            (neighbours flashing map)))
    (fset:includef flashed flashing)
    (finally
     (iter
       (for flashed-o in-fset flashed)
       (setf (gethash flashed-o map) 0))
     (return (list map (fset:size flashed))))))

(defun day11 (input)
  (let* ((parsed (run-parser (parse-file) input))
         (ht (iter
               (with ret = (make-hash-table :test 'equal))
               (for r below (length parsed))
               (iter
                 (for c below (length (first parsed)))
                 (setf (gethash (list r c) ret) (elt (elt parsed r) c)))
               (finally (return ret)))))
    (iter
      (with map = ht)
      (for num-steps from 1)
      (for (new-map to-flash) = (increase map))
      (for (new-map2 flashes) = (flash new-map to-flash))      
      (setf map new-map2)
      (until (= (hash-table-count map) flashes))
      (finally (return num-steps)))))
