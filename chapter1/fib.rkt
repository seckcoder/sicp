#lang racket

(define (fib n)
  (fib-iter 1 0 n))

(define (fib-iter a b n)
  (if (= n 0)
    b
    (fib-iter (+ a b) a (- n 1))))

(provide fib)
