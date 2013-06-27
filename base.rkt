#lang racket

(define (square a)
  (* a a))

(define (cube a)
  (* a a a))

(provide (all-defined-out))
