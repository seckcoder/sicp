#lang racket

(require "base.rkt")
(require "prime.rkt")

(define (prime-square-sum a b)
  (filter-sum square
              a
              inc1
              prime?
              b))

(define (reprime-product n)
  (filter-product same
                  2
                  inc1
                  (lambda (k) (= (gcd k n) 1)) 
                  (- n 1)))

(provide prime-square-sum reprime-product)
