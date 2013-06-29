#lang racket

(require "base.rkt")
#|(define (fast-exp b n)|#
  ;(fast-exp-impl 1 b n))

;(define (fast-exp-impl product b n)
  ;(display (cons b n))
  ;(cond ((= n 0) 1)
        ;((even? n) (fast-exp-impl product (square b) (/ n 2)))
        #|(else (fast-exp-impl (* product b) b (- n 1)))))|#

(define (fast-exp b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-exp b (/ n 2))))
        (else (* b (fast-exp b (- n 1))))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m))
                                m))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))
(provide fast-exp expmod)
