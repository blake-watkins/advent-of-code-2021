(in-package :aoc-2021)

(defun parse-instruction ()
  (with-monad
    (assign opcode (parse-keyword))
    (parse-space)
    (assign operands (parse-list (either (parse-number)
                                         (parse-keyword)) #\Space))
    (unit (list opcode operands))))

(defun program (instructions)
  (if (null instructions)
      (with-monad
        (assign state (get-state))
        (guard (= 0 (fset:lookup state :z))))
      (destructuring-bind (opcode operands) (car instructions)
        (with-monad
          (op opcode operands)
          (program (cdr instructions))))))

(defun day24 (input)
  (let ((parsed (run-parser (parse-lines (parse-instruction)) input)))
    (run-game (program parsed) (fset:empty-map 0))))

(defun get-operand (operand)
  (if (numberp operand)
      (unit operand)
      (with-monad
        (assign state (get-state))
        (unit (fset:lookup state operand)))))

(defun op (opcode operands)
  (let ((reg (first operands))
        (operand (second operands)))
    (with-monad
      (assign state (get-state))
      (assign reg-value (get-operand reg))
      (assign operand-value (get-operand operand))
      (ecase opcode
        (:inp
         (with-monad
           (assign choice (amb '(9 8 7 6 5 4  3 2 1)))
           (set-state (fset:with state reg choice))
           (unit choice)))
        (:mul (set-state (fset:with state reg (* reg-value operand-value))))
        (:add (set-state (fset:with state reg (+ reg-value operand-value))))
        (:div
         (with-monad
           (guard (/= operand-value 0))
           (set-state (fset:with state reg (truncate reg-value operand-value)))))
        (:mod
         (with-monad
           (guard (>= reg-value 0))
           (guard (> operand-value 0))
           (set-state (fset:with state reg (mod reg-value operand-value)))))
        (:eql
         (set-state (fset:with state reg (if (= reg-value operand-value) 1 0))))))))
