(in-package :aoc-2021)

(defun parse-file ()
  (parse-lines
   (with-monad
     (assign dir (parse-keyword))
     (parse-space)
     (assign amount (parse-number))
     (unit (list dir amount)))))

(defun day2 (input)
  (let ((parsed (run-parser (parse-file) input)))
    (iter
      (with pos = 0)
      (with depth = 0)
      (for (dir amount) in parsed)
      (case dir
        (:forward (incf pos amount))
        (:up (decf depth amount))
        (:down (incf depth amount)))
      (finally (return (* pos depth))))))

(defun day2 (input)
  (let ((parsed (run-parser (parse-file) input)))
    (iter
      (with pos = 0)
      (with depth = 0)
      (with aim = 0)
      (for (dir amount) in parsed)
      (case dir
        (:forward
         (incf pos amount)
         (incf depth (* aim amount)))
        (:up
         (decf aim amount))
        (:down
         (incf aim amount)))
      (finally (return (* pos depth))))))
