#lang racket


;; from the definition ø: ø^2 = ø + 1. we know ø = 1 + 1/ø.

(require "newton.rkt")

(define (golden-ratio)
  (fixed-point (lambda (ø) (+ 1 (/ 1 ø)))
               (exact->inexact 1)))

(provide golden-ratio)
