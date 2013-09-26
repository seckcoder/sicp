; just like this style
(define (init-operation-table)
  (put 'quote text-of-quotation)
  (put 'set! eval-assignment)
  (put 'if eval-if)
  ...
  )

(define (seck-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        (else
          (let ((op (get (car exp))))
            (cond (op (op exp env))
                  ((application? exp)
                   ...)
                  (else
                    (error ...)))))))
