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
        (guard (= 0 (fset:lookup state :z)))
        (unit (reverse (fset:lookup state :inputs))))
      (destructuring-bind (opcode operands) (car instructions)
        (with-monad
          (op opcode operands)
          (program (cdr instructions))))))

(defun analyse (parsed)
  (let ((parts (iter
                 (for i from 0 below (length parsed) by 18)
                 (collect (subseq parsed i (+ i 18))))))
    (mapcar (lambda (part)
              (list (cadadr (elt part 5))
                    (cadadr (elt part 15))))
            parts)))

(defun day24 (input)
  (let* ((parsed (run-parser (parse-lines (parse-instruction)) input))
         (analysis (analyse parsed)))
    (run-game (program parsed) (fset:with
                                (fset:with
                                 (fset:with (fset:empty-map 0)
                                            :inputs '())
                                 :analysis analysis)
                                :stack '()))))
 
(defun get-operand (operand)
  (if (numberp operand)
      (unit operand)
      (with-monad
        (assign state (get-state))
        (unit (fset:lookup state operand)))))

(defun set-choice (reg choice)
  (with-monad
    (assign state (get-state))
    (progn
      (fset:includef state reg choice)
      (fset:includef state :inputs (cons choice (fset:lookup state :inputs)))
      (set-state state))
    (unit choice)))

(defun push-stack (choice)
  (with-monad
    (assign state (get-state))
    (let ((analysis (fset:lookup state :analysis)))
      (fset:includef state :analysis (cdr analysis))
      (fset:includef state :stack (cons (+ choice (cadar analysis))
                                        (fset:lookup state :stack)))
      (set-state state))))

(defun pop-stack ()
  (with-monad
    (assign state (get-state))
    (let ((stack (fset:lookup state :stack))
          (analysis (fset:lookup state :analysis)))
      (fset:includef state :analysis (cdr analysis))
      (fset:includef state :stack (cdr stack))
      (with-monad
        (set-state state)
        (unit (list (car stack) (caar analysis)))))))

(defun op-input (reg)
  (with-monad
    (assign state (get-state))
    (assign choice (amb '(1 2 3 4 5 6 7 8 9)))
    (set-choice reg choice)
    (if (plusp (caar (fset:lookup state :analysis)))
        (push-stack choice)
        (with-monad
          (assign target (pop-stack))
          (guard (= choice (apply #'+ target)))))
    ))

(defun op (opcode operands)
  (let ((reg (first operands))
        (operand (second operands)))
    (with-monad
      (assign state (get-state))
      (assign reg-value (get-operand reg))
      (assign operand-value (get-operand operand))
      (ecase opcode
        (:inp (op-input reg))
        (:mul (set-state (fset:with state reg (* reg-value operand-value))))
        (:add (set-state (fset:with state reg (+ reg-value operand-value))))
        (:div
         (with-monad
           (guard (/= operand-value 0))
           (set-state
            (fset:with state reg (truncate reg-value operand-value)))))
        (:mod
         (with-monad
           (guard (>= reg-value 0))
           (guard (> operand-value 0))
           (set-state (fset:with state reg (mod reg-value operand-value)))))
        (:eql
         (set-state
          (fset:with state reg (if (= reg-value operand-value) 1 0))))))))
