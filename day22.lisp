(in-package :aoc-2021)

(defun parse-range ()
  (parse-list (parse-number) ".."))

(defun parse-file ()
  (parse-lines (with-monad
                 (assign state (parse-keyword))
                 (assign ranges (n-of 3 (parse-until (parse-range))))
                 (unit (list state ranges)))))

(defun make-box-from-input (state ranges)
  (let ((bl (mapcar #'first ranges))        
        (tr (mapcar #'second ranges)))
    (make-instance 'box :state state :bottom-left bl :top-right tr)))



(defun get-split-points (box-1 box-2)
  (mapcar (lambda (points)
            (sort points #'<))
          (map 'list #'list
               (bottom-left box-1)
               (top-right box-1)
               (bottom-left box-2)
               (top-right box-2))))

(defun get-boxes-from-input (parsed)
  (iter
    (with ret = (fset:empty-seq))
    (for (state ranges) in parsed)
    (setf ret (fset:with-first ret (make-box-from-input state ranges)))
    (finally (return ret))))

(defun day22-2 (input)
  (let* ((parsed (run-parser (parse-file) input))
         (boxes (get-boxes-from-input parsed)))
    (iter
      (with intersections = (list boxes))
      (for last = (first intersections))
      (for next = (iter
                    (with next2 = (fset:empty-set))
                    (for (a b) in (pairs (fset:convert 'list last)))
                    (let ((intersection (box-intersection a b)))
                      (when intersection
                        (fset:includef next2 intersection)))
                    (finally (return next2))))
      (until (fset:empty? next))
      (format t "~a~%" (fset:size next))
      (setf intersections (cons next intersections))
      (finally (iter
                 (for intersection in (reverse intersections))
                 (for i from 0)
                 (format t "~a ~a~%" i (fset:size  intersection)))))))

(defun get-region (boxes)
  (let ((min (fset:reduce (lambda (acc bl) (map 'list #'min acc bl))
                          (fset:image #'bottom-left boxes)))
        (max (fset:reduce (lambda (acc tr) (map 'list #'max acc tr))
                          (fset:image #'top-right boxes))))
    (make-instance 'box
                   :bottom-left min
                   :top-right max
                   :state :empty)))
(defun day22-3 (input)
  (let* ((parsed (run-parser (parse-file) input))
         (boxes (get-boxes-from-input parsed)))
    (iter
      (with region = (get-region boxes))
      (for box in-fset boxes)
      (setf region (add-box region box))
      (finally (return (count-type region :on))))))

(defclass region ()
  ((region :initarg :region :accessor region)))

(defgeneric add-box (region box))

(defmethod add-box ((region region) (new-box box))
  (setf (region region) (add-box (region region) new-box)))

(defmethod add-box ((orig-region box) (new-box box))
  (if (box-contains new-box orig-region)
      (if (eq (state orig-region) :empty)
          (make-instance 'box
                         :bottom-left (bottom-left orig-region)
                         :top-right (top-right orig-region)
                         :state (state new-box))
          orig-region)
      (if (box-intersect new-box orig-region)
          (add-box (make-instance 'node :children (box-split orig-region))
                   new-box)
          orig-region)))

(defmethod add-box ((orig-region node) (new-box box))
  (make-instance 'node
                 :children (fset:image (lambda (child)
                                         (add-box child new-box))
                                       (children orig-region))))

(defgeneric count-type (region type))

(defmethod count-type ((box box) type)
  (if (eq (state box) type)
      (box-size box)
      0))

(defmethod count-type ((node node) type)
  (iter
    (for child in-fset (children node))
    (summing (count-type child type))))

(defclass box (region)
  ((bottom-left :initarg :bottom-left :accessor bottom-left)
   (top-right :initarg :top-right :accessor top-right)
   (state :initarg :state :accessor state)))

(defclass node (region)
  ((children :initarg :children :accessor children)))





(defmethod initialize-instance :after ((box box) &key)
  (let ((mins (map 'list #'min (bottom-left box) (top-right box)))
        (maxs (map 'list #'max (bottom-left box) (top-right box))))
    (setf (bottom-left box) mins)
    (setf (top-right box) maxs)))

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
(defun vector- (a b)
  (map 'list #'- a b))

(defun vector+ (a b)
  (map 'list #'+ a b))

(defun vector* (a k)
  (mapcar  #'(lambda (c) (* c k)) a))

(defun vector-floor (a k)
  (mapcar #'(lambda (c) (floor c k)) a))

(defun box-size (box)
  (let ((diff (vector- (bottom-left box) (top-right box))))
    (reduce #'(lambda (acc v) (* acc (abs v)))
            diff
            :initial-value 1)))

(defun box-split-splitpoints (box split-points)
  (let ((ret (empty-set)))
    ;; Build a list of the bottom left and top right coords in ACC-BL ACC-TR. 
    ;; A, B, M hold the original box coordinates and midpoint.
    (labels ((box-split-rec (a b m acc-bl acc-tr)
               (if (or  (null a) (null b) (null m))
                   (let ((new-box (make-instance 'box
					    :bottom-left (reverse acc-bl)
					    :top-right (reverse acc-tr)
                                            :state (state box))))
                     (when (box-contains box new-box)
                       (includef ret new-box)))
                   (progn
		     ;; Make a recursive call for the bottom and top halves
		     ;; of each dimension if those halves are bigger than 0
                     (iter
                       (with last = (car a))
                       (for split-point in (car m))
                       (when (>= (- split-point last) 1)
                         (box-split-rec (cdr a) (cdr b) (cdr m)
                                        (cons last acc-bl)
                                        (cons split-point acc-tr)))
                       (setf last split-point)
                       (finally 
                        (when (>= (- (car b) split-point) 1)
                          (box-split-rec (cdr a) (cdr b) (cdr m)
                                         (cons split-point acc-bl)
                                         (cons (car b) acc-tr)))))))))
      (box-split-rec (bottom-left box)
                     (top-right box)
                     split-points
                     ()
                     ()))
    ret))


(defun box-intersection (box-1 box-2)
  (let* ((max-of-mins (map 'list
                           #'max (bottom-left box-1) (bottom-left box-2)))
         (min-of-maxs (map 'list
                           #'min (top-right box-1) (top-right box-2)))
         (ret (make-instance 'box
                             :bottom-left max-of-mins
                             :top-right min-of-maxs
                             :state (state box-2))))
    (if (> (box-size ret) 0)
        ret
        nil)))

;; does box-1 intersect with box-2
(defun box-intersect (box-1 box-2)
  (labels ((intersect (b-1 t-1 b-2 t-2)
             (or (<= b-1 b-2 (1- t-1))
                 (<= b-2 (1- t-1) (1- t-2)))))
    (every #'intersect
           (bottom-left box-1)
           (top-right box-1)
           (bottom-left box-2)
           (top-right box-2))))

;; does box-1 contain box-2?
(defun box-contains (box-1 box-2)
  (labels ((contains (b-1 t-1 b-2 t-2)
             (and (<= b-1 b-2 (1- t-1))
                  (<= b-1 (1- t-2) (1- t-1)))))
    (every #'contains
           (bottom-left box-1)
           (top-right box-1)
           (bottom-left box-2)
           (top-right box-2))))


(defun box-split (box)
  "Split box into 2^d other boxes where d is the dimension of the box."
  (let ((midpoint (vector-floor (vector+ (bottom-left box) (top-right box)) 2))
        (ret (empty-set)))
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
                     midpoint
                     ()
                     ()))
    ret))

(defmethod fset:compare ((box-1 box) (box-2 box))
  (flet ((get-numbers (box)
           (list (bottom-left box) (top-right box))))
    (fset:compare (get-numbers box-1) (get-numbers box-2))))



(defparameter *i* "on x=-45..7,y=-17..27,z=5..49
on x=-47..6,y=-17..30,z=-24..26")


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
