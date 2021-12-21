(in-package :aoc-2021)

;; Return the place in the circle you'd be if you started at N and moved X
;; places forward. 
(defun place (n x)
  (1+ (mod (+ (1- n) x) 10)))

;; Returns the score of rolling a deterministic dice three times if it has
;; already been rolled ROLLS times. 
(defun roll-three (rolls)
  (iter
    (repeat 3)
    (summing (1+ (mod rolls 100)))
    (incf rolls)))

(defun day21 (pos other-pos &optional (score 0) (other-score 0) (rolls 0))
  (cond
    ((>= other-score 1000) (* score rolls))
    (t (let ((new-pos (place pos (roll-three rolls))))
         (day21 other-pos new-pos other-score (+ score new-pos) (+ rolls 3))))))

;; wins returns a two element list with the number of wins for both players.
;; Base cases: If the player's score is >= 21, the player has a win. If the
;;   other player's score is >= 21, the other player has a win.
;; Recursive case: The three rolls of the die can move the player any number
;;   from 3 - 9 squares forward. There is one universe where it moves forward
;;   3 ((1 1 1)), three where it moves forward 4 ((1 1 2) (1 2 1) (2 1 1)) etc.
;;   Find the number of wins from the other player's perspective from each of
;;   those positions and multiply by the number of universes it happened in.
;;   Add up all those results then reverse the answer since wins for the other
;;  player are losses for the player. 
(defun wins (pos other-pos &optional (score 0) (other-score 0))
  (cond
    ((>= score 21) (list 1 0))
    ((>= other-score 21) (list 0 1))
    (t (reverse
        (apply #'map 'list #'+
               (iter
                 (for (next-inc universes) in
                      '((3 1) (4 3) (5 6) (6 7) (7 6) (8 3) (9 1)))
                 (for next-square = (place pos next-inc))
                 (for other-wins = (wins other-pos next-square
                                         other-score (+ score next-square)))
                 (collect (mapcar (lambda (x) (* x universes))
                                  other-wins))))))))

(defun day21-2 (pos-1 pos-2)
  (reduce #'max (wins pos-1 pos-2)))

;; Memoize arguments, decreases part 2 runtime ~100x.
(defun memo (f)
  (let ((cache (fset:empty-map)))
    (lambda (&rest args)
      (let ((cached (fset:lookup cache args)))
        (if cached
            cached
            (let ((ret (apply f args)))
              (fset:includef cache args ret)
              ret))))))
(setf (symbol-function 'wins) (memo #'wins))
