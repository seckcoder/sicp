#lang racket
(require "base.rkt")

(define (mult a b)
  (mymulti 0 (min a b) (max a b)))

(define (mymulti res a b)
  (cond ((or (= a 0) (= b 0)) res)
        ((even? a) (mymulti res
                            (halve a)
                            (double b)))
        (else (mymulti (+ res b)
                       (- a 1)
                       b))))
