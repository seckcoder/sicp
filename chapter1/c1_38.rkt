#lang racket

(require "base.rkt")
(require "continued_fraction.rkt")

;; note k in [1,n]
(define (approx-e)
  (+ (cont-frac-recur (lambda (k) 1.0)
                      ; represent Di with k
                      (lambda (k) (cond ((divides? 3 (- k 2)) (* 2
                                                                 (/ (+ 1 k)
                                                                    3)))
                                        (else 1)))
                      10) 2))

(approx-e)
