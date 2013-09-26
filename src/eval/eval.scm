(load "./utils/dict.scm")

(define (assert v)
  (if (not v)
    (error 'assert "failed")))

(define (typeof v)
  (cond ((boolean? v)
         'boolean)
        ((number? v)
         'number)
        ((string? v)
         'string)
        ((symbol? v)
         'symbol)
        ((null? v)
         'null)
        ((pair? v)
         'pair)
        (else
          'unknown)))

(define (seck-eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp)
         (make-procedure (lambda-params exp)
                         (lambda-body exp)
                         env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (seck-eval (cond->if exp) env))
        ((and? exp) (seck-eval (and->if exp) env))
        ((or? exp) (seck-eval (or->if exp) env))
        ((application? exp)
         (seck-apply (seck-eval (operator exp) env)
                     (list-of-values (operand exp) env)))
        (else
          (error 'seck-eval "eval failed to recognize expression" exp))))

; @(tools)
(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    #f))

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

; @(expression)

(define (first-exp exps)
  (car exps))

(define (rest-exps exps)
  (cdr exps))

(define (last-exp? exps)
  (and (pair? exps)
       (null? (cdr exps))))

(define (begin? exp)
  (tagged-list? exp 'begin))

(define (begin-actions exp)
  (cdr exp))

; implicit begin
(define (sequence->exp seq)
  (cond ((null? seq) seq)
        ((last-exp? seq) (first-exp seq))
        (else
          (make-begin seq))))

(define (make-begin seq)
  (cons 'begin seq))

; @(self-evaluating)
(define (self-evaluating? exp)
  (cond ((number? exp) #t)
        ((string? exp) #t)
        (else #f)))

(define (variable? exp)
  (symbol? exp))


; @(quote)

(define (quoted? exp)
  (tagged-list? exp 'quote))

(define (text-of-quotation exp)
  (cadr exp))

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
        (list '> >)
        (list '< <)
        (list '= =)
        ))

(define (primitive-procedure-names)
  (map car
       primitive-procedures))

(define (primitive-procedure-objects)
  (map (lambda (proc) (list 'primitive (cadr proc)))
       primitive-procedures))

(define (primitive-proc-impl proc)
  (cadr proc))

(define primitive-vars
  (list (list 'true #t)
        (list 'false #f)
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

(define (make-procedure params body-seq env)
  (list 'procedure params body-seq env))

(define (procedure-parameters proc)
  (cadr proc))

(define (procedure-body proc)
  (caddr proc))

(define (procedure-env proc)
  (cadddr proc))

(define (eval-sequence exps env)
  (if (last-exp? exps)
    (seck-eval (first-exp exps) env)
    (eval-sequence (rest-exps) env)))

; @(assignment)

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-var exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))

(define (eval-assignment exp env)
  (set-variable-value! (assignment-var exp)
                       (seck-eval (assignment-value exp) env)
                       env))

; @(defintion)

(define (definition? exp)
  (tagged-list? exp 'define))

(define (definition-var exp)
  (if (symbol? (cadr exp))
    (cadr exp)
    (caadr exp)))

(define (definition-value exp)
  (if (symbol? (cadr exp))
    (caddr exp)
    (make-lambda (cdadr exp)
                 (cddr exp))))

(define (eval-definition exp env)
  (define-variable! (definition-var exp)
                    (seck-eval (definition-value exp) env)
                    env))

; @(if)

(define (make-if predicate consequent . args)
  (if (null? args)
    (list 'if predicate consequent 'false)
    (list 'if predicate consequent (car args))))

(define (if? exp)
  (tagged-list? exp 'if))

(define (if-predicate exp)
  (cadr exp))

(define (if-consequent exp)
  (caddr exp))

(define (if-alternative exp)
  (if (not (null? (cdddr exp)))
    (cadddr exp)
    'false))

(define (eval-if exp env)
  (if (seck-eval (if-predicate exp) env)
    (seck-eval (if-consequent exp) env)
    (seck-eval (if-alternative exp) env)))

; @(cond)
(define (cond? exp)
  (tagged-list? exp 'cond))

(define (cond-firstmatch matches)
  (car matches))
(define (cond-restmatches matches)
  (cdr matches))
(define (cond-lastmatch matches)
  (null? (cdr matches)))
(define (cond-elsematch? match)
  (eq? (car match) 'else))

(define (cond-match-predicate match)
  (car match))

(define (cond-match-actions match)
  (cdr match))

(define (cond->if exp)
  (define (expand matches)
    (if (null? matches)
      'false
      (let ((first (cond-firstmatch matches))
            (rest (cond-restmatches matches)))
        (if (cond-elsematch? first)
          (if (null? rest)
            (make-if 'true (sequence->exp (cond-match-actions first)))
            (error 'cond "else is not last match"))
          (make-if (cond-match-predicate first)
                   (sequence->exp (cond-match-actions first))
                   (expand rest))))))
  (expand (cdr exp)))

; @(and/or)
(define (and? exp)
  (tagged-list? exp 'and))

(define (or? exp)
  (tagged-list? exp 'or))

(define (and->if exp)
  (define (expand exps)
    (if (null? exps)
      (make-if 'true 'true)
      (make-if (car exps)
               (expand (cdr exps))
               'true)))
  (expand (cdr exp)))

(define (or->if exp)
  (define (expand exps)
    (if (null? exps)
      (make-if 'true 'false)
      (make-if (car exps)
               'true
               (expand (cdr exps)))))
  (expand (cdr exp)))

; @(lambda)

(define (lambda? exp)
  (tagged-list? exp 'lambda))

(define (make-lambda params body-seq)
  (cons 'lambda (cons params body-seq)))

(define (lambda-params exp)
  (cadr exp))

(define (lambda-body exp)
  (cddr exp))


; @(application)
(define application? pair?)
(define (operator exp) (car exp))
(define (operand exp) (cdr exp))
(define (first-operand exps) (car exps))
(define (rest-operands exps) (cdr exps))
; fetch a list of values, eval them and return results as list
(define (list-of-values exps env)
  (if (null? exps)
    '()
    (cons (seck-eval (first-operand exps) env)
          (list-of-values (rest-operands exps) env))))
(define (seck-apply procedure arguments)
  (cond ((primitive-procedure? procedure)
         (apply-primitive-procedure (primitive-proc-impl procedure) arguments))
        ((compound-procedure? procedure)
         (eval-sequence
           (procedure-body procedure)
           (extend-environment
             (procedure-parameters procedure)
             arguments
             (procedure-env procedure))))
        (else
          (error
            'seck-apply
            "Inknown procedure type" procedure))))

; @(global env)
(define (setup-environment)
  (extend-environment (primitive-names)
                      (primitive-values)
                      the-empty-env))

(define global-env (setup-environment))

(define (test-eval)
  (assert (= (seck-eval '(+ 1 2) global-env) 3))
  (assert (equal? (seck-eval '(cons 1 2) global-env) (cons 1 2)))
  (let ((new-env (make-env global-env)))
    (seck-eval '(define a 3) new-env)
    (seck-eval '(set! a 4) new-env)
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
    ))
