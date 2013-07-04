#lang racket

(require "base.rkt")

(define (prime? n)
  (= n (smallest-divisor n)))

(define (next-1 guess)
  (+ 1 guess))
(define (next-2 guess)
  (cond ((= guess 2) 3)
        (else (+ guess 2))))

(define (smallest-divisor n)
  (find-divisor n
                2
                ; replace this as next-2 and run c1_22.rkt to see the
                ; performance difference
                ;next-1))
                next-2))

(define (find-divisor n guess next-gen)
  (cond ((> (square guess) n) n)
        ((divides? guess n) guess)
        (else (find-divisor n (next-gen guess) next-gen))))

(provide prime?)
