(load "./dict.scm")

(define (seck-eval exp env)
  (cond ((application? exp)
         (seck-apply (seck-eval (operator exp) env)
                     (list-of-values (operand exp) env)))))

(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    #f))

(define application? pair?)
(define (operator exp) (car exp))
(define (operand exp) (cdr exp))
(define (first-operand exps) (car exps))
(define (rest-operands exps) (cdr exps))
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
             (procedure-environment procedure))))
        (else
          (error
            'seck-apply
            "Inknown procedure type" procedure))))

; @(environment)

(define (make-env base)
  (let ((local-env (list (make-dict) base)))
    (define (cur-env-var-dict) (car local-env))
    (define (base-env) (cadr local-env))

    (define (lookup var)
      (let ((ret (dict-lookup (cur-env-var-dict) var)))
        (let ((found (car ret))
              (value (cadr ret)))
          (cond (found value)
                ((null? (base-env))
                 (error 'lookup "variable not defined" var))
                (else
                  (((base-env) 'lookup) var))))))

    (define (insert! var value)
      (dict-insert! (cur-env-var-dict) var value))

    (define (set! var value)
      (if (dict-set! (cur-env-var-dict) var value)
        value
        (error 'set! "setting an unbound value for " var)))

    (define (dispatch action)
      (cond ((eq? action 'lookup) lookup)
            ((eq? action 'insert) insert!)
            (else
              (error 'make-env-dispatch "unknown action" action))))
    dispatch))

(define (lookup-variable-value var env)
  ((env 'lookup) var))

(define (extend-environment vars values base-env)
  (let ((new-env (make-env base-env)))
    (define (iter vars values)
      (cond ((and (null? vars)
                  (null? values))
             new-env)
            ((or (null? vars)
                 (null? values))
             (error 'extend-environment "vars and values should have equal lenght"))
            (else
              ((new-env 'insert) (car vars) (car values))
              (iter (cdr vars) (cdr values)))))
    (iter vars values)))

(define (define-variable! var value env)
  ((env 'insert) var value))

(define (set-variable-value! var value env)
  ((env 'insert) var value))

(define the-empty-env '())

(define (setup-environment)
  (extend-environment (primitive-names)
                              (primitive-values)
                              the-empty-env))

(define global-env (setup-environment))

; @(primitives)

(define primitive-procedures
  (list (list 'car car)
        (list 'cdr cdr)
        (list 'cons cons)
        (list 'null? null?)
        (list '+ +)
        (list '- -)
        (list '* *)
        (list '/ /)
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define primitive-vars
  (list (list '#t #t)
        (list '#f #f)
        ))

(define (primitive-var-names)
  (map car
       primitive-vars))

(define (primitive-var-values)
  (map cadr
       primitive-vars))

(define (primitive-names)
  (append (primitive-procedure-names)
          (primitive-var-names)))

(define (primitive-values)
  (append (primitive-procedure-objects)
          (primitive-var-values)))

(define (primitive-procedure? proc)
  (tagged-list? proc 'primitive))

(define (apply-primitive-procedure proc args)
  (apply proc args))

; @(procedures)

(define (compound-procedure? proc)
  (tagged-list? proc 'procedure))

(define (make-procedure params body env)
  (list 'procedure params body env))

(define (procedure-parameters proc)
  (cadr proc))

(define (procedure-body proc)
  (caddr proc))

(define (procedure-env proc)
  (cadddr proc))
