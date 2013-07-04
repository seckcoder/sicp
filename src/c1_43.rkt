#lang racket

(require "base.rkt")
(define (repeated f n)
  (if (= n 1)
    f
    (compose1 (repeated f (- n 1))
              f)))

(provide repeated)

;((repeated square 4) 5)
