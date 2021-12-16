(in-package :aoc-2021)


(defun parse-file ()
  (parse-list (parse-character "0123456789ABCDEF") ""))

(defun map-chars (char)
  (case char
    (#\0 "0000")
    (#\1 "0001")
    (#\2 "0010")
    (#\3 "0011")
    (#\4 "0100")
    (#\5 "0101")
    (#\6 "0110")
    (#\7 "0111")
    (#\8 "1000")
    (#\9 "1001")
    (#\A "1010")
    (#\B "1011")
    (#\C "1100")
    (#\D "1101")
    (#\E "1110")
    (#\F "1111")))


(defun parse-bit ()
  (parse-digit))

(defun parse-number-field  (len)
  (with-monad
    (assign bits (n-of len (parse-digit)))
    (unit (digits-to-int bits :base 2))))

(defun parse-encoded-number (&optional (acc 0))
  (with-monad
    (assign bits (n-of 5 (parse-digit)))
    (let ((new-acc (+ (* acc 16) (digits-to-int (subseq bits 1)))))
      (if (= 0 (elt bits 0))
          (unit (list :num new-acc))
          (parse-encoded-number new-acc)))))

(defun map-operator (op)
  (case op
    (0 :sum)
    (1 :product)
    (2 :min)
    (3 :max)
    (5 :gt)
    (6 :lt)
    (7 :eq)))

(defun parse-operator (op)
  (with-monad
    (assign length-type-id (parse-digit))
    (if (= length-type-id 0)
        (with-monad
          (assign length (parse-number-field 15))
          (assign sub-packet-data (n-of length (parse-character "01")))
          (unit (list op
                      (run-parser (parse-packets)
                                  (format nil "~{~a~}" sub-packet-data)))))
        (with-monad
          (assign length (parse-number-field 11))
          (assign sub-packets (n-of length (parse-packet)))
          (unit (list op sub-packets))))))

(defun parse-packets ()
  (parse-list (parse-packet) ""))
(defun parse-packet ()
  (with-monad
    (assign version (parse-number-field 3))
    (assign type-id (parse-number-field 3))
    (assign packet
            (if (= type-id 4)
                (parse-encoded-number)
                (parse-operator (map-operator type-id))))
    (unit (cons version packet))))

(defun add-versions (packet)
  (cond
    ((eq :num (cadr packet)) (car packet))
    (t (+ (car packet) (reduce #'+ (mapcar #'add-versions (caddr packet)))))))


(defun eval-packet (packet)
  (destructuring-bind (ver op sub-packets) packet

    (declare (ignore ver))

    (case op
      (:num sub-packets)
      (:sum (reduce #'+ (mapcar #'eval-packet sub-packets)))
      (:product (reduce #'* (mapcar #'eval-packet sub-packets)))
      (:min (reduce #'min (mapcar #'eval-packet sub-packets)))
      (:max (reduce #'max (mapcar #'eval-packet sub-packets)))
      (:gt (if (> (eval-packet (first sub-packets)) (eval-packet (second sub-packets)))
               1 0))
      (:lt (if (< (eval-packet (first sub-packets)) (eval-packet (second sub-packets)))
               1 0))
      (:eq (if (= (eval-packet (first sub-packets)) (eval-packet (second sub-packets)))
               1 0)))))
(defun day16 (input)
  (let ((input (format nil "~{~a~}" (mapcar #'map-chars (run-parser (parse-file) input)))))
    (run-parser (parse-packet) input)))
