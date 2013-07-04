#lang racket

(require "base.rkt")
(require "newton.rkt")
(require "c1_43.rkt")

(define (average-damp f)
  (lambda (x) (average (f x) x)))

(define (damped-nth-root x n)
  (fixed-point-of-transform (lambda (y) (/ x
                                           (fast-exp y (- n 1))))
                            (repeated average-damp (ceiling (log2 n)))
                            1.0))

#|(roots (fast-exp 5 1) 1 average-damp)|#
;(roots (fast-exp 5 5) 5 (repeated average-damp 2))
;(roots (fast-exp 5 9) 9 (repeated average-damp 3))
;(roots (fast-exp 5 18) 18 (repeated average-damp 4))
#|(roots (fast-exp 5 33) 33 (repeated average-damp 5))|#
(damped-nth-root (fast-exp 3 18) 18)
