(in-package :aoc-2021)

(defun parse-file ()
  (with-monad
    (parse-string "target area: x=")
    (assign x (parse-list (parse-number) (parse-string "..")))
    (parse-string ", y=")
    (assign y (parse-list (parse-number) (parse-string "..")))
    (unit (list x y))))

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
