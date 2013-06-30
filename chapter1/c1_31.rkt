#lang racket

(require "base.rkt")

(define (pi-approx n)
  (define (get-numerator k)
    (if (even? k)
      (+ 2 k)
      (get-numerator (+ k 1))))
  (define (get-denominator k)
    (if (even? k)
      (+ 3 k)
      (get-denominator (- k 1))))
  (define (term k)
    (/ (get-numerator k) (get-denominator k)))
  (* (exact->inexact 4) (product term
                                 0
                                 (lambda (k) (+ 1 k))
                                 (- n 1))))

(define (factorial n)
  (product-iter
    (lambda (x) x)
    1
    (lambda (k) (+ 1 k))
    n))

(provide pi-approx factorial)
