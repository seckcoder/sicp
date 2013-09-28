(load "eval.scm")

(define (test-scoping)
  ; if it's lexical scoping, it will return 7
  ; but if it's dynamic scoping, it will return 8
  (let ((new-env (make-env global-env)))
    (seck-eval '(define a 4)
               new-env)
    (seck-eval '(define (foo)
                  (+ a 3))
               new-env)
    (seck-eval '(define (bar)
                  (define a 5)
                  (foo))
               new-env)
    (println (seck-eval '(bar)
                        new-env))
    ))
