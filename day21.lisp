(in-package :aoc-2021)

(defun place (n x)
  (1+ (mod (+ (1- n) x) 10)))

(defun memo (f)
  (let ((cache (fset:empty-map)))
    (lambda (&rest args)
      (let ((cached (fset:lookup cache args)))
        (if cached
            cached
            (let ((ret (apply f args)))
              (fset:includef cache args ret)
              ret))))))

(setf (symbol-function 'ways) (memo #'ways))
(defun ways (a-start b-start a-pos b-pos turn a-score b-score)
;;  (format t "a-pos ~a b-pos ~a turn ~a a-score ~a b-score ~a~%" a-pos b-pos turn a-score b-score)
  (let ((ret (cond
               ((or (< a-score 0) (< b-score 0)) 0)
               ((= a-score 0)
                (if (and (= b-score 0)
                         (= a-start a-pos)
                         (= b-start b-pos)
                         (eq turn :a))
                    1 0))
               (t
                (if (eq turn :a)
                    (iter
                      (for last-square-inc from 1)
                      (for last-square = (place b-pos last-square-inc))
                      (for universes in '(1 3 6 7 6 3 1))
                      (for ways = (* (ways a-start b-start
                                           a-pos last-square
                                           :b
                                           a-score (- b-score b-pos))
                                     universes))
                      (summing ways))
                    (iter
                      (for last-square-inc from 1)
                      (for last-square = (place a-pos last-square-inc))
                      (for universes in '(1 3 6 7 6 3 1))
                      (for ways = (* (ways a-start b-start
                                           last-square b-pos
                                           :a
                                           (- a-score a-pos) b-score)
                                     universes))
                      (summing ways)))))))
;;    (format t "a-pos ~a b-pos ~a turn ~a a-score ~a b-score ~a => ~a~%" a-pos b-pos turn a-score b-score ret)
    ret))
(defun ways (start pos score other)
  (let ((cache (fset:empty-map)))
    (labels ((ways-rec (start pos score other)
               (let ((cached (fset:lookup cache (list start pos score other))))
                 (if cached
                     cached
                     (let ((ret (ways-int (start pos score other))))
                       (setf )
                       ())))
               )
             (ways-int (start pos score turn other)               
               (cond
                 ((< score 0) 0)
                 ((= score 0) (if (= pos start) 1 0))
                 (t )))))))

(defun wins (a-start b-start player)
  (iter outer
    (for winner-score from 21 to 30)
    (iter
      (for loser-score from 0 to 20)
      (iter
        (for winner-pos from (- winner-score 20) to 10)
        (iter
          (for loser-pos from 1 to 10)
          (in outer (summing (ways a-start
                                   b-start
                                   (if (eq player :a)
                                       winner-pos
                                       loser-pos)
                                   (if (eq player :a)
                                       loser-pos
                                       winner-pos)
                                   (if (eq player :a) :b :a)
                                   (if (eq player :a)
                                       winner-score
                                       loser-score)
                                   (if (eq player :a)
                                       loser-score
                                       winner-score)))))))))
(defun day21 (players)
  (let ((die 1)
        (dice-rolls 0))
    (flet ((next-roll ()
             (let ((ret die))
               (incf dice-rolls)
               (setf die (if (= die 100) 1 (1+ die)))
               ret)))
      (iter
        (with player-scores = (fset:empty-map 0))
        (with player-positions =
              (iter
                (with ret = (fset:empty-map))
                (for p from 0)
                (for pos in players)
                (fset:includef ret p
                               (fset:with (iter
                                            (with ret = (fset:empty-map))
                                            (for i below 10)
                                            (fset:includef ret i 0)
                                            (finally (return ret)))
                                          pos
                                          1))
                (finally (return ret))))
        (with cur-player = 0)
        (let ((pos-inc (+ (fset:lookup player-positions cur-player)
                          (next-roll) (next-roll) (next-roll))))
          (setf pos-inc (1+ (mod (1- pos-inc) 10)))
          (setf player-positions (fset:with player-positions cur-player pos-inc))
          (incf (fset:lookup player-scores cur-player) pos-inc))
        (until (iter
                 (for player in-fset player-scores)
                 (thereis (>= (fset:lookup player-scores player) 1000))))

        (setf cur-player (mod (1+ cur-player) (length players)))
        (finally
         (return (iter
                   (for player in-fset player-scores)
                   (for score = (fset:lookup player-scores player))
                   (finding (* score dice-rolls) such-that (< score 1000)))))))))
