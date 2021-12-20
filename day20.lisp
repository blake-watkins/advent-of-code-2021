(in-package :aoc-2021)

(defun parse-file ()
  (with-monad
    (assign algorithm (parse-lines (parse-characters "#.")))
    (n-of 2(parse-newline))
    (assign input (parse-lines (parse-characters "#.")))
    (unit (list (apply #'concatenate 'string algorithm) input))))

(defun calc-pixel (r c default map transforms)
  (iter
    (with idx = 0)
    (for pr from (1- r) to (1+ r))
    (iter
      (for pc from (1- c) to (1+ c))
      (let ((val (fset:lookup map (list pr pc))))
        (setf idx (+ (* idx 2) (if val val default)))))
    (finally (return (fset:lookup transforms idx)))))

(defun day20 (input)
  (destructuring-bind (algorithm input) (run-parser (parse-file) input)
    (let ((map (fset:empty-map))
          (transforms (fset:empty-map))
          (min-r 0)
          (min-c 0)
          (max-r (1- (length input)))
          (max-c (1- (length (first input)))))
      (iter
        (for i from 0)
        (for c in-string algorithm)
        (fset:includef transforms i (if (char= c #\.) 0 1)))

      (iter
        (for r below (length input))
        (iter
          (for char in-string (elt input r))
          (for c from 0)
          (fset:includef map (list r c) (if (char= char #\.) 0 1))))
      
      (iter
        (repeat 50)
        (for default first 0 then (- 1 default))
        (for new-map = (fset:empty-map))
        
        (iter
          (for r from (1- min-r) to (1+ max-r))
          (iter
            (for c from (1- min-c) to (1+ max-c))
            (fset:includef new-map
                           (list r c)
                           (calc-pixel r c default map transforms))))
        (setf map new-map)
        (decf min-r)
        (decf min-c)
        (incf max-r)
        (incf max-c))

      (fset:count-if (lambda (k) (= (fset:lookup map k) 1)) map))))

(defun print-map (map)
  (iter
    (for r from -5 to 5)
    (format t "~{~a~}~%"
            (iter
              (for c from -5 to 5)
              (for val = (fset:lookup map (list r c)))
              (if val
                  (collect (if (= val 1) #\# #\.))
                  (collect #\Space))))))
