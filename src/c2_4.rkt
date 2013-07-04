#lang racket

(define (mycons x y)
  (lambda (m) (m x y)))

(define (mycar z)
  (z (lambda (p q) p)))

(define (mycdr z)
  (z (lambda (p q) q)))

(mycar (mycons 1 2))
(mycdr (mycons 1 2))
