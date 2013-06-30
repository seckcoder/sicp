#lang racket

(require "base.rkt")
(require "newton.rkt")

(define (average-damp f)
  (lambda (x) (average (f x) x)))

(define (fourth-roots x)
  (fixed-point-of-transform (lambda (y) (/ x (cube y)))
                            average-damp
                            1.0))

(fourth-roots 81)
