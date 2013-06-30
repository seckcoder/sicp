#lang racket

(require "base.rkt")
(require "prime.rkt")

(define (prime-square-sum a b)
  (filter-sum square
              a
              (lambda (v) (+ 1 v))
              prime?
              b))

(provide prime-square-sum)
