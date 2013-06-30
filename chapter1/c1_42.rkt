#lang racket

(require "base.rkt")

(define (mycompose f g)
  (lambda (x) (f (g x))))


((mycompose square inc1) 6)
