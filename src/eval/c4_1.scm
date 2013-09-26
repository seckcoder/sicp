
(define (list-of-values-left-to-right exps env)
  (if (null? exps)
    '()
    (let ((first-value (seck-eval (first-operand exps) env)))
      (cons first-value
            (list-of-values-left-to-right (rest-operands exps) env)))))

;; right-to-left is just the same
