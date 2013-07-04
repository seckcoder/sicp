#lang racket

(require "base.rkt")
(define (square-list lst)
  (map square lst))

(square-list '(1 2 3 4))
