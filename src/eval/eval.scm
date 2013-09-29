(load "./utils/dict.scm")
(load "./utils/functional.scm")
(load "./utils/guile.scm")
(load "eval-base.scm")

(define (seck-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) ; evaluated when define procedure
         (make-procedure (lambda-params exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (seck-eval (cond->if exp) env))
        ((and? exp) (seck-eval (and->if exp) env))
        ((or? exp) (seck-eval (or->if exp) env))
        ((let? exp) (seck-eval (let->combination exp) env))
        ((letstar? exp) (seck-eval (let*->nested-let exp) env))
        ((letrec? exp) (seck-eval (letrec->let exp) env))
        ((for? exp) (seck-eval (for->let exp) env))
        ((application? exp)
         ; evaluated when call procedure
         (seck-apply (seck-eval (operator exp) env)
                     (list-of-values (operand exp) env)))
        (else
          (error 'seck-eval "eval failed to recognize expression" exp))))

; redefines make-procedure
(define (make-procedure params body-seq env)
  (list 'procedure
        params
        (scan-out-defines body-seq #f)
        env))

; fetch a list of values, eval them and return results as list
(define (list-of-values exps env)
  (if (null? exps)
    '()
    (cons (seck-eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))
(define (seck-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure procedure arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             ; Here, we eval the expression in
             ; a new env which extends procedure env(the
             ; env in which procedure is defined). This 
             ; ensures lexical scoping.
             ; If we evaluate the procedure bodyin
             ; the env that seck-apply is called, then
             ; it will be dynamic scoping.
             (procedure-env procedure))))
        (else
          (error
            'seck-apply
            "Unknown procedure type" procedure))))

(define (eval-if exp env)
  (if (seck-eval (if-predicate exp) env)
    (seck-eval (if-consequent exp) env)
    (seck-eval (if-alternative exp) env)))
(define (eval-definition exp env)
  (define-variable! (definition-var exp)
                    (seck-eval (definition-value exp) env)
                    env))
(define (eval-assignment exp env)
  (set-variable-value! (assignment-var exp)
                       (seck-eval (assignment-value exp) env)
                       env))

(define (eval-sequence exps env)
  (cond ((null? exps)
         (error 'eval-sequence "no expression in code in block"))
        (( last-exp? exps)
         (seck-eval (first-exp exps) env))
        (else
          (seck-eval (first-exp exps) env)
          (eval-sequence (rest-exps exps) env))
        ))

; proc-body is a sequence of expressions to be evaluated
(define (scan-out-defines proc-body do-scan-out)
  (let ((definitions (filter definition? proc-body))
        (non-definitions (filter (compose not definition?)
                                 proc-body)))
    (if (and do-scan-out
             (not (null? definitions)))
      (list (make-let (map (lambda (exp)
                             (list (definition-var exp)
                                   ;; Here, we use '', since
                                   ;; we want to pass a (quote symbol) to the evaluator,
                                   ;; but not a symbol to the evaluator. The outer quote
                                   ;; will be used to protect (quote symbol) in
                                   ;; the naive scheme evaluator.
                                   ''*unassigned*))
                           definitions)
                      (append (map (lambda (exp)
                                     (list 'set!
                                           (definition-var exp)
                                           (definition-value exp)))
                                   definitions)
                              non-definitions)))
      proc-body)))
(define (init-test-env)
  (let ((new-env (make-env global-env)))
    (seck-eval '(define a 3) new-env)
    (seck-eval '(set! a 4) new-env)
    new-env))

(define test-let-exp '(let ((x 1)
                            (y 2))
                        (+ x y)))
(define test-named-let-exp '(let fib-iter ((a 1)
                                           (b 0)
                                           (count 10))
                              (if (= count 0)
                                b
                                (fib-iter (+ a b) a (- count 1)))))

(define (test-eval)
  (let ((new-env (init-test-env)))
    (assert (= (seck-eval '(+ 1 2) global-env) 3))
    (assert (equal? (seck-eval '(cons 1 2) global-env) (cons 1 2)))
    (assert (= (seck-eval 'a new-env) 4))
    (assert (seck-eval '(and (= a 4) (< a 5) (> a 2)) new-env))
    (assert (seck-eval '(or (= a 5) (> a 5) (> a 2)) new-env))
    (seck-eval '(define (op a b)
                  (if (> a b)
                    (- a b)
                    (- b a)))
               new-env)
    (seck-eval '(define (op2 a b)
                  (cond ((> a b)
                         (- a b))
                        ((> b a)
                         (- b a))
                        (else
                          (+ a b))))
               new-env)
    (assert (equal? (seck-eval '(op 1 2) new-env)
                    1))
    (assert (equal? (seck-eval '(op2 2 2) new-env)
                    4))
    (assert (equal? (seck-eval '(let ((a 3)
                                      (b 4))
                                  (cons a b))
                               new-env)
                    (cons 3 4)))
    (assert (equal? (seck-eval '(let* ((x 1)
                                       (y (+ x a))
                                       (z (+ x y))
                                       )
                                  (+ x y z))
                               new-env)
                    12))
    (assert (equal? (seck-eval '(let fib-iter ((a 1)
                                               (b 0)
                                               (count 10))
                                  (if (= count 0)
                                    b
                                    (fib-iter (+ a b)
                                              a
                                              (- count 1))))
                               new-env)
                    55))
    )
  (seck-eval '((lambda ()
                 (define (foo a) a)
                 (foo 3)
                 )
               )
             global-env)
  'pass)


(define letrec-exp '(letrec ((even?
                               (lambda (n)
                                 (if (= n 0)
                                   true
                                   (odd? (- n 1)))))
                             (odd?
                               (lambda (n)
                                 (if (= n 0)
                                   false
                                   (even? (- n 1))))))
                      (even? x)))

(define (test-letrec)
  (seck-eval '(define (f x)
                (letrec ((even?
                           (lambda (n)
                             (if (= n 0)
                               true
                               (odd? (- n 1)))))
                         (odd?
                           (lambda (n)
                             (if (= n 0)
                               false
                               (even? (- n 1))))))
                  (even? x)))
             global-env)
  (seck-eval '(f 3) global-env)
  )
