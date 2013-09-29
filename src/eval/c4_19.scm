(load "eval.scm")

; to enable scan-out-defines, try to modify it in eval.scm

(define (demo)
  (seck-eval '(let ((a 1))
                (define (f x)
                  (define b (+ a x))
                  (define a 5)
                  (+ a b))
                (f 10))
             global-env)
  )
(let ((a 1))
  (define (f x)
    (define b (+ a x))
    (define a 5)
    (+ a b))
  (f 10))
