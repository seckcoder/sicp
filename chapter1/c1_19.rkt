#lang racket

#|
Theroy:
suppose v = [a b]', a = 1, b = 0
If T * v = [(bq + aq + ap) (bp + aq)]'
then T = [[(p+q) q]
          [q     p]]

then T' = T * T = [[(p+q)^2 + q^2 (p+q)q + pq]
              [q(p+q) + pq   q^2 + p^2]]

we have: (p+q)q + pq + q^2 + p^2 = (p+q)^2 + q^2
=> p' = p^2 + q^2, q' = 2pq + q^2

vn = T^n * v = (T * T)^(n/2) * v = (T')^(n/2) * v
|#


(require "base.rkt")
(define (fib n)
  (fib-dummy 1 0 0 1 n))

(define (fib-dummy a b p q n)
  (cond ((= n 0) b)
        ((even? n) (fib-dummy a
                              b
                              (+ (square p)
                                 (square q))
                              (+ (square q)
                                 (* 2 p q))
                              (/ n 2)))
        (else (fib-dummy (+ (* b q) (* a q) (* a p))
                         (+ (* b p) (* a q))
                         p
                         q
                         (- n 1)))))

(provide fib)
