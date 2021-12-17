(in-package :aoc-2021)

(defun parse-range ()
  (with-monad
    (parse-lower-case)
    (parse-character #\=)
    (parse-list (parse-number) (parse-string ".."))))

(defun parse-file ()
  (with-monad
    (parse-string "target area: ")
    (parse-list (parse-range) ", ")))

(defun within (point target)
  (every (lambda (coord range)
           (<= (first range) coord (second range)))
         point
         target))

(defun fire (velocity target)
  (iter    
    (with pos = '(0 0))
    (maximizing (second pos) into max-height)
    (until (or (within pos target)
               (< (second pos) (y-min target))
               (> (first pos) (x-max target))))
    (setf pos (point+ pos velocity))
    (setf velocity (point+ velocity (list (- (signum (first velocity))) -1)))    
    (finally (return (if (within pos target) max-height nil)))))

(defun x-max (target) (cadar target))
(defun y-min (target) (caadr target))

(defun day17 (input)
  (let ((target (run-parser (parse-file) input)))
    (iter outer
      (for x-vel from 1 to (x-max target))
      (iter
        (for y-vel from (y-min target) to (- (y-min target)))
        (for result = (fire (list x-vel y-vel) target))
        (when result
          (in outer (maximizing result into part1))
          (in outer (counting result into part2))))
      (finally (return-from outer (list part1 part2))))))
