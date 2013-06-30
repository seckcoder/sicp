#lang racket

(require "continued_fraction.rkt")

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           11)  ;; 11 is enough to get an approx to 4 decimal places.

(cont-frac-recur (lambda (i) 1.0)
                 (lambda (i) 1.0)
                 11)  ;; 11 is enough to get an approx to 4 decimal places.
