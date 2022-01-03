(in-package :aoc-2021)

(defun parse-range ()
  (parse-list (parse-number) ".."))

(defun parse-file ()
  (parse-lines (with-monad
                 (assign state (parse-keyword))
                 (assign ranges (n-of 3 (parse-until (parse-range))))
                 (unit (list state ranges)))))

;; CLASSES

;; Box is a cuboid of space all with the same state.
(defclass box ()
  ((bottom-left :initarg :bottom-left :accessor bottom-left)
   (top-right :initarg :top-right :accessor top-right)
   (state :initarg :state :accessor state)))

;; Node is a collection of up to 2^d children - d is the dimension of the region.
;; Each child is either a box or another node and is fully contained in node. 
(defclass node ()
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
    ;; A, B, S hold the original box coordinates and splitpoint.
    (labels ((box-split-rec (a b s acc-bl acc-tr)
               (if (or  (null a) (null b) (null s))
                   (includef ret
			     (make-instance 'box
					    :bottom-left (reverse acc-bl)
					    :top-right (reverse acc-tr)
					    :state (state box)))
                   (progn
		     ;; Make a recursive call for the bottom and top halves
		     ;; of each dimension if those halves are bigger than 0
                     (when (>= (- (car s) (car a)) 1)
                       (box-split-rec (cdr a) (cdr b) (cdr s)
                                      (cons (car a) acc-bl)
                                      (cons (car s) acc-tr)))
                     (when (>= (- (car b) (car s)) 1)
                       (box-split-rec (cdr a) (cdr b) (cdr s)
                                      (cons (car s) acc-bl)
                                      (cons (car b) acc-tr)))))))
      (box-split-rec (bottom-left box)
                     (top-right box)
                     (box-clamp box split-point)
                     ()
                     ()))
    ret))


;; ADD-BOX
;; Add a box behind existing boxes.

(defgeneric add-box (region box))

(defun split-corner (enclosing-box split-box)
  "Find a corner to split on. This is a corner of the intersection between the two boxes that is not already a corner of the enclosing box. "
  (let* ((intersection (box-intersection enclosing-box split-box))
         (enclosing-corners (box-corners enclosing-box))
         (corners (box-corners intersection)))
    (car (fset:filter (lambda (corner)
                             (not (find corner enclosing-corners :test 'equal)))
                           corners))))

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
          (let ((split-point (split-corner orig-region new-box)))
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
;; Count pixels of the given type in the box / node

(defgeneric count-type (region type))

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
    (let ((box (make-instance 'box
                              :bottom-left (mapcar #'first ranges)
                              :top-right (mapcar #'1+ (mapcar #'second ranges))
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

(defun day22 (input &optional (part 2))
  (let ((boxes (get-boxes input)))
    (when (= part 1)
      (let ((initialization-region (make-instance 'box
                                                  :bottom-left '(-50 -50 -50)
                                                  :top-right '(51 51 51)
                                                  :state :empty)))        
        (setf boxes (fset:filter (lambda (box)
                                   (box-contains initialization-region box))
                                 boxes))))
    (let ((region (bounding-box boxes)))
      (iter
        (for box in-fset boxes)
        (setf region (add-box region box))
        (finally (return (count-type region :on)))))))
