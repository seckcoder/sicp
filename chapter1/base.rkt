#lang racket

(define (square a)
  (* a a))

(define (cube a)
  (* a a a))

(define (even? n)
  (= (remainder n 2) 0))


(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (divides? a b)
  (= (remainder b a) 0))

(provide (all-defined-out))
