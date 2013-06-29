#lang racket

(require "base.rkt")

(define (fast-exp b n)
  (fast-exp-iter 1 b n))

(define (fast-exp-iter product b n)
  (cond ((= n 0) product)
        ((even? n) (fast-exp-iter product
                                  (square b)
                                  (/ n 2)))
        (else (fast-exp-iter (* product b)
                             b
                             (- n 1)))))

(provide fast-exp)
