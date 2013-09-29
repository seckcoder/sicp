(define (assert-eq a b eqproc)
  (if (not (eqproc a b))
    (error 'assert-eq a " and " b " is not equal")
    #t))

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

; @(tools)
(define (tagged-list? exp tag)
  (if (pair? exp)
    (eq? (car exp) tag)
    #f))

(define (unassigned? value)
  (eq? value '*unassigned*))

; @(environment)

(define (make-env base)
  (let ((local-env (list (make-dict) base)))
    (define (cur-env-var-dict) (car local-env))
    (define (base-env) (cadr local-env))

    (define (lookup var)
      (let ((ret (dict-lookup (cur-env-var-dict) var)))
        (let ((found (car ret))
              (value (cadr ret)))
          (cond ((and found
                      (not (unassigned? value)))
                 value)
                (found
                  (error 'lookup "variabled looked up is unassigned"))
                ((empty-env? (base-env))
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
             (error 'extend-environment "procedure arguments not match"))
            (else
              ((new-env 'insert) (car vars) (car values))
              (iter (cdr vars) (cdr values)))))
    (iter vars values)))

(define (define-variable! var value env)
  ((env 'insert) var value))

(define (set-variable-value! var value env)
  ((env 'insert) var value))

(define the-empty-env '())
(define (empty-env? env)
  (null? env))

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

(define (sequence->exp-2 predicate actions)
  (if (eq? (car actions) '=>)
    ; transforms to '(cadr (= a 3)) if predicate is (= a 3)
    ; Note predicate will evaluated twice in the conditions!
    (list (cadr actions)
          predicate)  
    (sequence->exp actions)))

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
        (list 'print print)
        (list 'map map)
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
  (apply (primitive-proc-impl proc) args))

; @(procedures)

(define (compound-procedure? proc)
  (tagged-list? proc 'procedure))

(define (make-procedure params body-seq env)
  (list 'procedure
        params
        body-seq
        env))

(define (procedure-parameters proc)
  (cadr proc))

(define (procedure-body proc)
  (caddr proc))

(define (procedure-env proc)
  (cadddr proc))

; @(assignment)

(define (assignment? exp)
  (tagged-list? exp 'set!))

(define (assignment-var exp)
  (cadr exp))

(define (assignment-value exp)
  (caddr exp))


; @(defintion)

(define (make-definition name value)
  (list 'define name value))
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
        (cond ((cond-elsematch? first) ;else
               (if (null? rest)
                 (make-if 'true (sequence->exp (cond-match-actions first)))
                 (error 'cond "else is not last match")))
              (else ; the other conditions
                (make-if (cond-match-predicate first)
                         (sequence->exp-2 (cond-match-predicate first)
                                          (cond-match-actions first))
                         (expand rest)))))))
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

; @(let)
(define (let? exp)
  (tagged-list? exp 'let))

(define (let-bindings exp)
  (if (namedlet? exp)
    (caddr exp)
    (cadr exp)
    ))

(define (let-body exp)
  (if (namedlet? exp)
    (cdddr exp)
    (cddr exp)
    ))
(define (let-vars exp) (map car (let-bindings exp)))
; if you define let-values here, there will be 
(define (let-vals exp) (map cadr (let-bindings exp)))
(define (namedlet? exp) (symbol? (cadr exp)))
(define (let->name exp) (cadr exp))

(define (let->combination exp)
  (if (namedlet? exp)
    (cons (make-lambda '()
                       (list (make-definition (let->name exp)
                                              (make-lambda (let-vars exp)
                                                           (let-body exp)))
                             (cons (let->name exp)
                                   (let-vals exp))
                             ))
          '())
    (let->lambda (let-vars exp)
                 (let-vals exp)
                 (let-body exp))))

(define (let->lambda vars values body)
  (cons (make-lambda vars body)
        values))

(define (make-let var-bindings body)
  (cons 'let
        (cons var-bindings
              body)))

(define (letstar? exp)
  (tagged-list? exp 'let*))

(define (let*->nested-let exp)
  (define (wrap-let-body vars values)
    (cond ((and (null? vars)
                (null? values))
           (make-let '() (let-body exp)))
          ((or (null? vars)
               (null? values))
           (error 'wrap-let-body "let vars-values length not match"))
          (else
            (make-let (list (list (car vars)
                                  (car values)))
                      (list (wrap-let-body (cdr vars)
                                           (cdr values)))))
          ))
  (wrap-let-body (let-vars exp)
                 (let-vals exp)  ; todo
                 ))


(define (letrec? exp)
  (tagged-list? exp 'letrec))
(define (letrec->let exp)
  (make-let 
    (map (lambda (var)
           (list var ''*assigned*))
         (let-vars exp))
    (append (map (lambda (var value)
                   (list 'set! var value))
                 (let-vars exp)
                 (let-vals exp))
            (let-body exp))))

; @(for)

(define (for? exp)
  (tagged-list? exp 'for))
(define (for-init exp) (cadr exp))
(define (for-can-continue exp) (caddr exp))
(define (for-step exp) (cadddr exp))
(define (for-body exp) (cddddr exp))



; @(application)
(define application? pair?)
(define (operator exp) (car exp))
(define (operand exp) (cdr exp))
(define (first-operand exps) (car exps))
(define (rest-operands exps) (cdr exps))

; @(global env)
(define (setup-environment)
  (extend-environment (primitive-names)
                      (primitive-values)
                      the-empty-env))

(define global-env (setup-environment))
