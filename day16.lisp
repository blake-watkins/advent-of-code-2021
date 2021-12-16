(in-package :aoc-2021)

;; Parse the hex characters. The format string converts each digit into four
;; binary digits with leading zeroes and concatenates them all into a big string.
(defun parse-file ()
  (with-monad
    (assign values (parse-list (parse-digit :base 16) ""))
    (unit (format nil "~{~4,'0B~}" values))))

(defun parse-bit ()
  (parse-digit :base 2))

(defun parse-number-field  (length)
  (with-monad
    (assign bits (n-of length (parse-bit)))
    (unit (digits-to-int bits :base 2))))

(defun parse-number-data (&optional (acc 0))
  (with-monad
    (assign more-bits-p (parse-bit))
    (assign value (parse-number-field 4))
    (let ((new-acc (+ (* acc 16) value)))
      (if (= 0 more-bits-p)
          (unit new-acc)
          (parse-number-data new-acc)))))

(defun parse-operator-data ()
  (with-monad
    (assign length-type-id (parse-bit))
    (if (= length-type-id 0)
        (with-monad
          (assign length (parse-number-field 15))
          (parse-subparser length (parse-list (parse-packet) "")))
        (with-monad
          (assign length (parse-number-field 11))
          (n-of length (parse-packet))))))

(defun parse-type ()
  (with-monad
    (assign type-id (parse-number-field 3))
    (unit (aref #(+ * min max :num > < =) type-id))))

(defun parse-packet ()
  (with-monad
    (assign version (parse-number-field 3))
    (assign type (parse-type))
    (assign data (if (eq type :num)
                     (parse-number-data)
                     (parse-operator-data)))
    (unit (list version type data))))

(defun operator-type (type-id)
  )

(defun add-versions (packet)
  (destructuring-bind (version opcode data) packet
    (if (eq :num opcode)
        version
        (+ version (reduce #'+ (mapcar #'add-versions data))))))

(defun eval-packet (packet)
  (destructuring-bind (version opcode data) packet
    (declare (ignore version))
    (if (eq :num opcode)
        data
        (let ((ret (reduce opcode (mapcar #'eval-packet data))))
          (if (find opcode '(> < =))
              (if ret 1 0)
              ret)))))

(defun day16 (input &key (part 2))
  (let* ((bit-string (run-parser (parse-file) input) )
         (packet (run-parser (parse-packet) bit-string)))
    (if (= part 1)
        (add-versions packet)
        (eval-packet packet))))
