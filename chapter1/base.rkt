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

(provide (all-defined-out))
