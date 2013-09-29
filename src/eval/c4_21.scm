(load "eval.scm")

(define fact-exp '((lambda (n)
                     ((lambda (fact)
                        (fact fact n))
                      (lambda (ft k)
                        (if (= k 1)
                          1
                          (* k (ft ft (- k 1)))))))
                   10))

; a)
; (println (seck-eval fact-exp global-env))


(define fib-exp '((lambda (n)
                    ((lambda (fib)
                       (fib fib 0 1 0)
                       )
                     (lambda (fib a b k)
                       (if (= k n)
                         b
                         (fib fib b (+ a b) (+ 1 k))))
                     ))
                  10))

; (println (seck-eval fib-exp global-env))

; b)

(define f-exp '(define (f x)
                 ((lambda (even? odd?)
                    (even? even? odd? x))
                  (lambda (ev? od? n)
                    (if (= n 0)
                      true
                      (od? ev? od? (- n 1))))
                  (lambda (ev? od? n)
                    (if (= n 0)
                      false
                      (ev? ev? od? (- n 1)))))))

(seck-eval f-exp global-env)
(println (seck-eval '(f 4) global-env))
