; a more efficient eval
;

;why analyze can improve performance
     ;for eval without analyze
          ;(define (foo x) x)
          ;everytime, when you want to call foo, (foo x), it will first lookup variable foo, 
          ;which is binder to a lambda, then you will make a procedure from the lambda, and then evaluate the procedure again.
     ;for eval with analyze
          ;(define (foo x) x)
          ;everytime, when you want to call foo, (foo x), it will also lookup variable foo, which returns an analyzed procedure: proc,
          ;and by call proc: (proc env), it directly returns the procedure(without remake it), which improves the performance.

(load "eval-base.scm")
(define (seck-eval exp env)
  ((analyze exp) env))

(define (analyze exp)
  (cond ((self-evaluating? exp)
         (analyze-self-evaluating exp))
        ((quoted? exp) (analyze-quoted exp))
        ((variable? exp) (analyze-variable exp))
        ((assignment? exp) (analyze-assignment exp))
        ((definition? exp) (analyze-definition exp))
        ((if? exp) (analyze-if exp))
        ((lambda? exp) (analyze-lambda exp))
        ((begin? exp) (analyze-sequence (begin-actions exp)))
        ((cond? exp) (analyze (cond->if exp)))
        ((let? exp) (analyze (let->combination exp)))
        ((application? exp) (analyze-application exp))
        (else
          (error 'analyze "unknown expression type" exp))))

(define (analyze-self-evaluating exp)
  (lambda (env) exp))

(define (analyze-quoted exp)
  (let ((qval (text-of-quotation exp)))
    (lambda (env) qval)))

(define (analyze-quoted exp)
  (let ((text (quoted-text exp)))
    (lambda (env)
      text)))

(define (analyze-variable exp)
  (lambda (env)
    (lookup-variable-value exp env)))

(define (analyze-definition exp)
  (let ((var (definition-var exp))
        (vproc (analyze (definition-value exp))))
    (lambda (env)
      (define-variable! var (vproc env) env)
      'ok)))

(define (analyze-if exp)
  (let ((pproc (analyze (if-predicate exp)))
        (cproc (analyze (if-consequent exp)))
        (aproc (analyze (if-alternative exp))))
    (lambda (env)
      (if (pproc env)
        (cproc env)
        (aproc env)))))

(define (analyze-lambda exp)
  (let ((params (lambda-params exp))
        (body-proc (analyze-sequence (lambda-body exp))))
    (lambda (env) (make-procedure params body-proc env))))

(define (analyze-sequence exp)
  (define (combine proc1 proc2)
    (lambda (env)
      (proc1 env)
      (proc2 env)))
  (define (combine-seq-proc seq-proc)
    ; (println "combine-seq-proc")
    (if (null? (cdr seq-proc))
      (car seq-proc)
      (combine (car seq-proc)
               (combine-seq-proc (cdr seq-proc)))))
  (let ((seq-proc (map analyze exp)))
    (if (null? seq-proc)
      (error 'analyze-sequence "no expression in code block")
      (combine-seq-proc seq-proc))))

(define (analyze-sequence-text exps)
  (define (sequentially proc1 proc2)
    (lambda (env) (proc1 env) (proc2 env)))
  (define (loop first-proc rest-procs)
    ; (println "loop")
    (if (null? rest-procs)
        first-proc
        (loop (sequentially first-proc (car rest-procs))
              (cdr rest-procs))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (loop (car procs) (cdr procs))))

(define (analyze-sequence-alyssa exps)
  (define (execute-sequence procs env)
    (cond ((null? (cdr procs))
           ((car procs) env))
          (else ((car procs) env)
                (execute-sequence (cdr procs) env))))
  (let ((procs (map analyze exps)))
    (if (null? procs)
        (error "Empty sequence -- ANALYZE"))
    (lambda (env) (execute-sequence procs env))))

(define (analyze-application exp)
  (let ((fproc (analyze (operator exp)))
        (aprocs (map analyze (operand exp))))
    (lambda (env)
      (execute-application (fproc env)
                           (map (lambda (aproc)
                                  (aproc env))
                                aprocs)))))

(define (execute-application proc args)
  (cond ((primitive-procedure? proc)
         (apply-primitive-procedure proc args))
        ((compound-procedure? proc)
         ((procedure-body proc)
          (extend-environment (procedure-parameters proc)
                              args
                              (procedure-env proc))))
        (else
          (error
            'execute-applicaiton
            "unknown procedure type"
            proc))))

(define (analyze-assignment exp)
  (let ((var (assignment-var exp))
        (vproc (analyze (assignment-value exp))))
    (lambda (env)
      (set-variable-value! var (vproc env) env))))

(define (test-analyze)
  (seck-eval '(define (fact n)
                (if (= n 1)
                  1
                  (* (fact (- n 1))
                     n)))
             global-env)
  (println (seck-eval '(fact 3) global-env))
  )

(define (test-let)
  (seck-eval '(let ((a 1)
                    (b 2))
                (set! a (* a b))
                (+ a b))
             global-env))

(define seq1 '((set! a b)
               (cons a b)
               (cons a b)
               (cons a b)
               (cons a b)))

(define seq2 '((cons a b)))
