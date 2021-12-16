(in-package :aoc-2021)

(defun parse-file ()
  (with-monad
    (assign chars (parse-list (parse-alphanumeric) ""))
    (unit (format nil "~{~a~}" (mapcar #'map-chars chars)))))

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

(defun map-operator (op)
  (case op
    (0 :sum)
    (1 :product)
    (2 :min)
    (3 :max)
    (4 :num)
    (5 :gt)
    (6 :lt)
    (7 :eq)))

(defun parse-bit ()
  (parse-digit))

(defun parse-number-field  (len)
  (with-monad
    (assign bits (n-of len (parse-bit)))
    (unit (digits-to-int bits :base 2))))

(defun parse-number-packet (&optional (acc 0))
  (with-monad
    (assign bits (n-of 5 (parse-bit)))
    (let ((new-acc (+ (* acc 16) (digits-to-int (subseq bits 1)))))
      (if (= 0 (elt bits 0))
          (unit new-acc)
          (parse-number-packet new-acc)))))

(defun parse-operator-packet ()
  (with-monad
    (assign length-type-id (parse-bit))
    (if (= length-type-id 0)
        (with-monad
          (assign length (parse-number-field 15))
          (assign sub-packets (n-of length (parse-bit)))
          (unit (run-parser (parse-packets) (format nil "~{~a~}" sub-packets))))
        (with-monad
          (assign length (parse-number-field 11))
          (assign sub-packets (n-of length (parse-packet)))
          (unit sub-packets)))))

(defun parse-packets ()
  (parse-list (parse-packet) ""))

(defun parse-packet ()
  (with-monad
    (assign version (parse-number-field 3))
    (assign type-id (parse-number-field 3))
    (assign packet
            (if (= type-id 4)
                (parse-number-packet)
                (parse-operator-packet)))
    (unit (list version (map-operator type-id) packet))))

(defun add-versions (packet)
  (cond
    ((eq :num (second packet)) (first packet))
    (t (+ (first packet) (reduce #'+ (mapcar #'add-versions (third packet)))))))

(defun eval-packet (packet)
  (destructuring-bind (ver opcode data) packet
    (declare (ignore ver))
    (if (eq opcode :num)
      data
      (let* ((op (case opcode
                   (:sum #'+)
                   (:product #'*)
                   (:min #'min)
                   (:max #'max)
                   (:gt #'>)
                   (:lt #'<)
                   (:eq #'=)))
             (ret (reduce op (mapcar #'eval-packet data))))
        (if (find opcode '(:gt :lt :eq))
            (if ret 1 0)
            ret)))))

(defun day16 (input &key (part 2))
  (let* ((bits (run-parser (parse-file) input))
         (packet (run-parser (parse-packet) bits)))
    (if (= part 1)
        (add-versions packet)
        (eval-packet packet))))
