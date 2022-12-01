(in-package :aoc-2021)

(defun parse-file ()
  (parse-lines (one-or-more (parse-character "# .ABCD"))))

(defun get-map (input)
  (iter
    (with ret = (fset:empty-map))
    (for r from 0)
    (for row in (run-parser (parse-file) input))
    (iter
      (for c from 0)
      (for char in row)
      (when (find char '(#\. #\A #\B #\C #\D))
        (fset:includef ret (list r c) (ecase char
					(#\. :open)
                                        (#\A :a)
                                        (#\B :b)
                                        (#\C :c)
                                        (#\D :d)))))
    (finally (return ret))))

(defparameter *hallway* '((1 1) (1 2) (1 4) (1 6) (1 8) (1 10) (1 11)))

(defparameter *prohibited* '((1 3) (1 5) (1 7) (1 9)))

(defparameter *costs* '((:a . 1) (:b . 10) (:c . 100) (:d . 1000)))

(defparameter *targets* '((:a . (2 3)) (:b . (2 5)) (:c . (2 7)) (:d . (2 9))))

;; evaluate either of these two progn's to set up for part 1/2. then evaluate
;; *room-map* and *move-tree*
(progn
  (defparameter *input* "#############
#...........#
###A#D#B#D###
  #D#C#B#A#
  #D#B#A#C#
  #B#C#A#C#
  #########")

  (defparameter *rooms* '((:a . ((5 3) (4 3) (3 3) (2 3)))
                          (:b . ((5 5) (4 5) (3 5) (2 5)))
                          (:c . ((5 7) (4 7) (3 7) (2 7)))
                          (:d . ((5 9) (4 9) (3 9) (2 9)))))

  (defparameter *map* (get-map "#############
#...........#
###.#.#.#.###
  #.#.#.#.#
  #.#.#.#.#
  #.#.#.#.#
  #########")))

(progn
    (defparameter *input* "#############
#...........#
###A#D#B#D###
  #B#C#A#C#
  #########")

  (defparameter *rooms* '((:a . ((3 3) (2 3)))
                          (:b . ((3 5) (2 5)))
                          (:c . ((3 7) (2 7)))
                          (:d . ((3 9) (2 9)))))

  (defparameter *map* (get-map "#############
#...........#
###.#.#.#.###
  #.#.#.#.#
  #########")))

;; precompute tree of moves from each position
(defparameter *move-tree*
  (let ((tree (make-hash-table :test 'equal)))
    (labels ((move-tree-rec (current last)
	       (let ((neighbours
		       (fset:filter
			(lambda (neighbour)
			  (fset:domain-contains? *map* neighbour))
			(mapcar (lambda (diff)
				  (map 'list #'+ diff current))
				'((1 0) (-1 0) (0 1) (0 -1))))))
		 (setf (gethash current tree) neighbours)
		 (iter
		   (for neighbour in neighbours)
		   (unless (equal neighbour last)
		     (move-tree-rec neighbour current))))))
      (move-tree-rec '(1 1) nil)
      tree)))

;; precompute each room's type
(defparameter *room-map*
  (let ((ret (make-hash-table :test 'equal)))
    (iter
      (for room in *hallway*)
      (if (find room *prohibited* :test 'equal)
	  (setf (gethash room ret) :prohibited)
	  (setf (gethash room ret) :hallway)))
    (iter
      (for (bug-type . pod-rooms) in *rooms*)
      (iter
	(for room in pod-rooms)
	(setf (gethash room ret) bug-type)))
    ret))

;; Return t if the pod for BUG-TYPE is full, the topmost empty square if it's
;; partially filled with the correct type of bugs, or nil if there are other bugs
;; there. 
(defun pure-pod (bug-type bugs)
  (let ((ret (iter
	       (for room in (cdr (assoc bug-type *rooms* :test 'equal)))
	       (for occupant = (fset:lookup bugs room))
	       (finding room such-that (or (null occupant)
					   (not (eq bug-type occupant)))))))
    (cond
      ((null ret) t)
      ((null (fset:lookup bugs ret)) ret)
      (t nil))))

(defun reachable (pos bugs)
  (labels ((reachable-rec (current last distance)
	     (iter
	       (for neighbour in (gethash current *move-tree*))		 
	       (unless (or (and last (equal neighbour last))
			   (fset:lookup bugs neighbour))
		 (appending
		  (cons (cons neighbour (1+ distance))
			(reachable-rec neighbour current (1+ distance))))))))
    (fset:convert 'fset:map (reachable-rec pos nil 0))))

;; if in own pod, can move to any reachable hallway (- prohibited) if you're not
;; at the bottom or on top of the same as you
;; in in hallway, can move to bottom of own pod if it's reachable and (empty or
;; has same as you
;; if in other pod, can move to any reachable hallway
(defun get-moves (pos bugs)
  (let ((bug-type (fset:lookup bugs pos))
	(room-type (gethash pos *room-map*)))
    (cond
      ((eq room-type bug-type);own room
       (if (pure-pod bug-type bugs)
	   (fset:empty-map)
	   (fset:filter (lambda (square dist)
			  (declare (ignore dist))
			  (eq (gethash square *room-map*) :hallway))
			(reachable pos bugs))))
      ((eq room-type :hallway)
       (let ((pure-pod (pure-pod bug-type bugs)))
	 (if (not pure-pod)
	     (fset:empty-map)
	     (fset:filter (lambda (square dist)
			    (declare (ignore dist))
			    (equal pure-pod square))
			  (reachable pos bugs)))))
      (t (fset:filter (lambda (square dist)
			  (declare (ignore dist))
			  (eq (gethash square *room-map*) :hallway))
			(reachable pos bugs))))))
(defun find-bugs (map)
  (iter
    (with ret = (fset:empty-map))
    (for (r c) in-fset map)
    (when (find (fset:lookup map (list r c)) '(:a :b :c :d))
      (fset:includef ret (list r c) (fset:lookup map (list r c))))
    (finally (return ret))))

(defun move-bug (from to bugs)
  (let ((from-bug (fset:lookup bugs from)))
    (fset:with (fset:less bugs from) to from-bug)))

(defun next-states (bugs)
  (iter outer
    (for bug in-fset bugs)
    (iter
      (with moves = (get-moves bug bugs))
      (for pos in-fset moves)
      (for dist = (fset:lookup moves pos))
      (in outer
          (collect (list (move-bug bug pos bugs)
                         (* dist
                            (cdr (assoc (fset:lookup bugs bug) *costs*)))))))))

(defun finished (bugs)
  "Return t if all bugs are in their own pods. "
  (iter
    (for bug in-fset bugs)
    (always (eq (fset:lookup bugs bug)
		(gethash bug *room-map*)))))

(defun in-own-pod (bug-pos bugs)
  (find bug-pos (assoc (fset:lookup bugs bug-pos) *rooms*) :test 'equal))

;; manhattan distance to top of pod - can often underestimate but that's okay
(defun heuristic (bugs)
  (iter
    (for bug in-fset bugs)
    (summing
     (if (in-own-pod bug bugs)
         0
         (* (manhattan bug (cdr (assoc (fset:lookup bugs bug) *targets*)))
            (cdr (assoc (fset:lookup bugs bug) *costs*)))))))

(defun print-map (bugs)
  (format t "~{~{~a~}~%~}~%"
          (iter
            (for r below 7)
            (collect
                (iter
                  (for c below 13)
                  (cond
                    ((fset:domain-contains? bugs (list r c))
                     (collect (symbol-name (fset:lookup bugs (list r c)))))
                    ((fset:domain-contains? *map* (list r c))
                     (collect #\.))
                    (t (collect #\#))))))))

(define-condition found (condition) 
  ((value :initarg :value :reader value)))

(defun day23 (input)
  (let ((bugs (find-bugs (get-map input))))
    (handler-case 
      (a-star
       bugs
       (lambda (vertex parent distance)
	 (declare (ignore parent))
	 (when (finished vertex)
	   (signal 'found :value distance)))
       #'next-states
       #'heuristic)
    (found (obj)
      (format t "~a~%" (value obj))))))
