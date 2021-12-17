(in-package :aoc-2021)

(defun parse-file ()
  (with-monad
    (parse-string "target area: x=")
    (assign x (parse-list (parse-number) (parse-string "..")))
    (parse-string ", y=")
    (assign y (parse-list (parse-number) (parse-string "..")))
    (unit (list x y))))

(defun within (point target)
  (every (lambda (coord range) (<= (first range) coord (second range)))
         point
         target))

(defun fire (init-velocity target)
  (iter    
    (with pos = '(0 0))
    (with velocity = init-velocity)
    (until (or (within pos target)
               (< (second pos) (reduce #'min (second target)))))
    (maximizing (second pos) into max-height)
    (setf pos (point+ pos velocity))
    (setf velocity (point+ velocity (list (- (signum (first velocity)))
                                          -1)))
    
    (finally (return (if (within pos target) max-height nil)))))

(defun day17 (input)
  (let ((target (run-parser (parse-file) input)))
    (iter      
      (for x-vel from 1 to 300)
      (for res = (iter
         (for y-vel from -500 to 500)
         (for result = (fire (list x-vel y-vel) target))
         (when result
           (counting result))))
      (when res
        (summing res)))))
