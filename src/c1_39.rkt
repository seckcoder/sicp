#lang racket

(require "base.rkt")
(require "continued_fraction.rkt")

(define (tan-cf x n)
  (cont-frac-recur (lambda (k) (if (= k 1)
                                 x
                                 (- (square x))))
                   (lambda (k) (- (* 2 k) 1))
                   n))

;; 1.047 ~ pi / 3
(tan-cf (/ 3.14 3) 10)
(tan-cf 0 10)
