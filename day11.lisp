(in-package :aoc-2021)

(defun parse-file ()
  (parse-lines (parse-list (parse-digit) "")))

(defun neighbours (p map)
  (remove-if-not
   (lambda (p) (fset:domain-contains? map p))
   (mapcar (lambda (d) (map 'list #'+  p d))
           '((1 0) (-1 0) (0 1) (0 -1) (1 1) (-1 1) (1 -1) (-1 -1)))))

(def-state (map flashed flashes))

(defun get-neighbours (octopus)
  (with-monad
    (assign map (get-map))
    (unit (neighbours octopus map))))

(defun set-brightness (octopus op)
  "Either increment octopus brightness if OP is :INC, or set to zero if OP is :RESET. Returns new brightness. "
  (with-monad
    (assign map (get-map))
    (let ((brightness (ecase op
                        (:reset 0)
                        (:inc (1+ (fset:lookup map octopus))))))
      (with-monad
        (set-map (fset:with map octopus brightness))
        (unit brightness)))))

(defun reset-flashed ()
  "Reset the brightness of flashed octopuses. Clear the set of flashed octopuses. Increase and return the count of flashes. "
  (with-monad
    (assign flashed (get-flashed))
    (mapfsetm_ (lambda (octopus)
                 (set-brightness octopus :reset))
              flashed)
    (set-flashed (fset:empty-set))

    (assign flashes (get-flashes))
    (let ((num-flashed (fset:size flashed)))
      (with-monad
        (set-flashes (+ flashes num-flashed))
        (unit num-flashed)))))

(defun increase (octopus)
  "Increase octopus brightness. Flash the octopus if it's more than 9. "
  (with-monad
    (assign brightness (set-brightness octopus :inc))
    (whenm (> brightness 9)
      (flash octopus))))

(defun flash (octopus)
  "If the octopus hasn't already flashed, flash it and increase its neighbours. "
  (with-monad
    (assign flashed (get-flashed))
    (whenm (not (fset:contains? flashed octopus))
      (with-monad
        (set-flashed (fset:with flashed octopus))
        (assign neighbours (get-neighbours octopus))
        (mapm_ #'increase neighbours)))))

(defun step-octopuses ()
  "Increase all octopuses, flashing when necessary. Reset flashed. Return number of flashed. "
  (with-monad
    (assign map (get-map))
    (mapfsetm_ #'increase map)
    (reset-flashed)))

(defun step-octopuses-until (steps flashes-target)
  "Step octopuses until flashes-target octopuses flash. Return number of steps. "
  (with-monad
    (assign flashes (step-octopuses))
    (if (= flashes flashes-target)
        (unit (1+ steps))
        (step-octopuses-until (1+ steps) flashes-target))))

(defun day11 (input &optional (part1 nil))
  (let* ((parsed (run-parser (parse-file) input))
         (map (map-from-list-list parsed)))
    (run-state
     (if part1
         (with-monad
           (n-of 100 (step-octopuses))
           (get-flashes))
         (step-octopuses-until 0 (fset:size map)))
     (make-state :map map
                 :flashed (fset:empty-set)
                 :flashes 0))))


