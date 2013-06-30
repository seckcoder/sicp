#lang racket

(require "base.rkt")
(require "c1_43.rkt")


(define (smooth f)
  (define dx 0.0001)
  (lambda (x) (/ (+ (f (- x dx))
                    (f x)
                    (f (+ x dx)))
                 3.0)))

(define (n-fold-smooth f n)
  ((repeated smooth n) f))

;((smooth square) 2)
;((n-fold-smooth square 2) 2)
