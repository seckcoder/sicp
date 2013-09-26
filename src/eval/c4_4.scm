
; and as special forms
; the same goes for or
(define (eval-and exp env)
  (define (iter exps)
    (cond ((null? exp) #t)
          ((not (seck-eval (car exp) env)) #f)
          (else
            (iter (cdr exp) env))))
  (iter (cdr exp)))


; see and/or as derived expressions in eval.scm
