#lang racket

(require "base.rkt")
(require "newton.rkt")

(define (cubic a b c)
  (lambda (x) (+ (cube x)
                 (* a (square x))
                 (* b x)
                 c)))

;; there is some problems with this
(newtons-method (cubic 0 0 0) -1.0)
