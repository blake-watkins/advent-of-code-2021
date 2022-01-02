(in-package :aoc-2021)

(defun parse-range ()
  (parse-list (parse-number) ".."))

(defun parse-file ()
  (parse-lines (with-monad
                 (assign state (parse-keyword))
                 (assign ranges (n-of 3 (parse-until (parse-range))))
                 (unit (list state ranges)))))

;; CLASSES

;; Region is a superclass representing either a single box or a collection of
;; boxes in a oct-tree.
(defclass region ()
  ((region :initarg :region :accessor region)))

;; Box is a cuboid of space all with the same state.
(defclass box (region)
  ((bottom-left :initarg :bottom-left :accessor bottom-left)
   (top-right :initarg :top-right :accessor top-right)
   (state :initarg :state :accessor state)))

;; Node has 2^d children - d is the dimension of the region. Each child is either
;; a box or another node and is fully contained in node. 
(defclass node (region)
  ((children :initarg :children :accessor children)))


;; BOX FUNCTIONS

(defun box-size (box)
  (let ((diff (point- (bottom-left box) (top-right box))))
    (reduce #'(lambda (acc v) (* acc (abs v)))
            diff
            :initial-value 1)))

(defun box-intersection (box-1 box-2)
  "Return intersection of BOX-1 and BOX-2. Return NIL if there is none. "
  (let* ((max-of-mins
           (map 'list #'max (bottom-left box-1) (bottom-left box-2)))
         (min-of-maxs
           (map 'list #'min (top-right box-1) (top-right box-2)))
         (ret (make-instance 'box
                             :bottom-left max-of-mins
                             :top-right min-of-maxs
                             :state (state box-2))))
    (if (> (box-size ret) 0) ret nil)))

(defun box-intersectp (box-1 box-2)
  "Return T if the two boxes intersect, NIL otherwise. "
  (labels ((intersect (b-1 t-1 b-2 t-2)
             (or (<= b-1 b-2 (1- t-1))
                 (<= b-1 (1- t-2) (1- t-1))
                 (<= b-2 b-1 (1- t-2))
                 (<= b-2 (1- t-1) (1- t-2)))))
    (every #'intersect
           (bottom-left box-1)
           (top-right box-1)
           (bottom-left box-2)
           (top-right box-2))))

(defun box-contains (box-1 box-2)
  "Return T if BOX-2 is fully contained in BOX-1. "
  (labels ((contains (b-1 t-1 b-2 t-2)
             (and (<= b-1 b-2 (1- t-1))
                  (<= b-1 (1- t-2) (1- t-1)))))
    (every #'contains
           (bottom-left box-1)
           (top-right box-1)
           (bottom-left box-2)
           (top-right box-2))))

(defun box-midpoint (box)
  "Return the midpoint of the box (rounded 'down' if a dimension is even). "
  (mapcar (lambda (bottom-left top-right)
            (floor (+ bottom-left top-right) 2))
          (bottom-left box)
          (top-right box)))

(defun box-corners (box)
  "Return a list of all 2^d corners of the box."
  (labels ((box-corners-rec (a b)
             (cond
               ((or (null a) (null b)) nil)
               ((or (null (cdr a)) (null (cdr b)))
                (list (list (car a)) (list (car b))))
               (t
                (let ((corners (box-corners-rec (cdr a) (cdr b))))
                  (append
                   (mapcar (lambda (coords) (cons (car a) coords)) corners)
                   (mapcar (lambda (coords) (cons (car b) coords)) corners)))))))
    (box-corners-rec (bottom-left box) (top-right box))))

(defun box-clamp (box point)
  "Clamp point so that it lies within BOX. "
  (flet ((clamp-internal (v min-val max-val)
           (max min-val (min v max-val))))
    (map 'list #'clamp-internal point (bottom-left box) (top-right box))))

(defun box-split (box split-point)
  "Split box at split-point into up to 2^d other boxes."
  (let ((ret (empty-set)))
    ;; Build a list of the bottom left and top right coords in ACC-BL ACC-TR. 
    ;; A, B, M hold the original box coordinates and midpoint.
    (labels ((box-split-rec (a b m acc-bl acc-tr)
               (if (or  (null a) (null b) (null m))
                   (includef ret
			     (make-instance 'box
					    :bottom-left (reverse acc-bl)
					    :top-right (reverse acc-tr)
					    :state (state box)))
                   (progn
		     ;; Make a recursive call for the bottom and top halves
		     ;; of each dimension if those halves are bigger than 0
                     (when (>= (- (car m) (car a)) 1)
                       (box-split-rec (cdr a) (cdr b) (cdr m)
                                      (cons (car a) acc-bl)
                                      (cons (car m) acc-tr)))
                     (when (>= (- (car b) (car m)) 1)
                       (box-split-rec (cdr a) (cdr b) (cdr m)
                                      (cons (car m) acc-bl)
                                      (cons (car b) acc-tr)))))))
      (box-split-rec (bottom-left box)
                     (top-right box)
                     (box-clamp box split-point)
                     ()
                     ()))
    ret))


;; ADD-BOX
;; Add a box behind existing boxes to a region / box / node.

(defgeneric add-box (region box))

(defmethod add-box ((region region) (new-box box))
  (setf (region region) (add-box (region region) new-box)))


(defun closest-corner-to-midpoint (midpoint-of corners-of)
  (let* ((corners (box-corners corners-of))
         (midpoint (box-midpoint midpoint-of))
         (enclosing-corners (box-corners midpoint-of))
         (distances (mapcar (lambda (corner)
                              (list corner (manhattan corner midpoint)))
                            (fset:filter (lambda (corner)
                                           (not (find corner enclosing-corners
                                                      :test 'equal)))
                                    corners))))
    (car (first (sort distances #'< :key #'second)))))

(defun remove-empties (boxes)
  (fset:filter (lambda (box)
                 (not (eq (state box) :empty)))
               boxes))

(defmethod add-box ((orig-region box) (new-box box))
  (if (box-contains new-box orig-region)
      (if (eq (state orig-region) :empty)
          (make-instance 'box
                         :bottom-left (bottom-left orig-region)
                         :top-right (top-right orig-region)
                         :state (state new-box))
          orig-region)
      (if (and (eq (state orig-region) :empty)
               (box-intersectp new-box orig-region))
          (let ((split-point (closest-corner-to-midpoint orig-region new-box)))
            (add-box (make-instance 'node
                                    :children (box-split orig-region split-point))
                     new-box))
          orig-region)))

(defmethod add-box ((orig-region node) (new-box box))
  (make-instance 'node
                 :children (fset:image (lambda (child)
                                         (add-box child new-box))
                                       (children orig-region))))


;; COUNT-TYPE
;; Count pixels of the given type in the region / box / node

(defgeneric count-type (region type))

(defmethod count-type ((region region) type)
  (count-type (region region) type))

(defmethod count-type ((box box) type)
  (if (eq (state box) type) (box-size box) 0))

(defmethod count-type ((node node) type)
  (iter
    (for child in-fset (children node))
    (summing (count-type child type))))


;; FSET:COMPARE
;; Compare boxes by their coordinates lexicographically. If coordinates match
;; and the state is the same the boxes are :EQUAL, otherwise they're :UNEQUAL.
(defmethod fset:compare ((box-1 box) (box-2 box))
  (flet ((get-numbers (box)
           (list (bottom-left box) (top-right box))))
    (let ((compare (fset:compare (get-numbers box-1) (get-numbers box-2))))
      (if (eq compare :equal)
          (if (eq (state box-1) (state box-2)) :equal :unequal)
          compare))))


;; INITIALIZE-INSTANCE
(defmethod initialize-instance :after ((box box) &key)
  (let ((mins (map 'list #'min (bottom-left box) (top-right box)))
        (maxs (map 'list #'max (bottom-left box) (top-right box))))
    (setf (bottom-left box) mins)
    (setf (top-right box) maxs)))


;; PRINT-OBJECT 
(defmethod print-object ((region region) stream)
  (print-object (region region) stream))

(defmethod print-object ((box box) stream)
  (let ((*print-pretty* nil))
    (print-unreadable-object (box stream :type t)
      (format stream "~9d ~a ~a ~a"
	      (box-size box)
	      (bottom-left box)
	      (top-right box)
              (state box)))))

(defmethod print-object ((node node) stream)
  (let ((*print-pretty* nil))
    (print-unreadable-object (node stream :type t)
      (format stream "Node~% ~{~a~%~}"
	      (iter (for child in-fset (children node)) (collect child))))))


(defun get-boxes (input)
  "Return a FSET:SEQ containing boxes from input string INPUT in reverse order. "
  (iter
    (with parsed = (run-parser (parse-file) input))
    (with ret = (fset:empty-seq))
    (for (state ranges) in parsed)
    (let ((box (make-instance 'box :bottom-left (mapcar #'first ranges)
                                   :top-right (mapcar #'1+
                                                      (mapcar #'second ranges))
                                   :state state)))
      (setf ret (fset:with-first ret box)))
    (finally (return ret))))

(defun bounding-box (boxes)
  "Return an :EMPTY box that encloses all of the boxes in BOXES. "
  (let* ((min (fset:reduce (lambda (acc bl) (map 'list #'min acc bl))
                           (fset:image #'bottom-left boxes)))
         (max (fset:reduce (lambda (acc tr) (map 'list #'max acc tr))
                           (fset:image #'top-right boxes)))
         (box (make-instance 'box
                   :bottom-left min
                   :top-right max
                   :state :empty)))
    box))

(defparameter *i* "on x=-45..7,y=-17..27,z=5..49
on x=-47..6,y=-17..30,z=-24..26")


(defun day22-2 (input)
  (let* ((boxes (get-boxes input))
         (region (bounding-box boxes)))
    (setf boxes (fset:filter (lambda (box)
                               (box-contains region box))
                             boxes))
    (iter
      (for box in-fset boxes)
      (setf region (add-box region box))
      (finally (return (count-type region :on))))))

(defun process (state map point ranges)
  (if (null ranges)
      (fset:includef map (reverse point) state)
      (iter
        (with range = (car ranges))
        (for i from (first range) to (second range))
        (setf map (process state map (cons i point) (cdr ranges)))))
  map)

(defun day22 (input)
  (let ((parsed (run-parser (parse-file) input))
        (map (fset:empty-map :off)))
    (iter
      (for (state ranges) in parsed)
      (when (every (lambda (range)
                     (and (<= -50 (first range) 50)
                          (<= -50 (second range) 50)))
                   ranges)
        (setf map (process state map '() ranges))))

    (iter
      (for coord in-fset map)
      (counting (eq (fset:lookup map coord) :on)))))
