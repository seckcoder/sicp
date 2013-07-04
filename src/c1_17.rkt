#lang racket
(require "base.rkt")

(define (mult a b)
  (mymulti (min a b) (max a b)))

(define (mymulti a b)
  (cond ((or (= a 0) (= b 0)) 0)
        ((even? a) (double (mymulti (halve a) b)))
        (else (+ b (mymulti (- a 1) b)))))

(provide mult)
