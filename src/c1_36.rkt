#lang racket

(require "base.rkt")

(define (good-enough? old-guess new-guess)
  (> 0.01
     (/ (abs (- new-guess old-guess))
        new-guess)))

(define (fixed-point f guess)
  (display guess)
  (newline)
  (let ([new-guess (f guess)])
    (if (good-enough? guess new-guess)
      new-guess
      (fixed-point f new-guess))))

(define (average-dampling f)
  (lambda (x) (average (f x) x)))

(define (xx-fixed)
  ;; uncomment the following line to compare the steps with averge dampling
  ;(fixed-point (lambda (x) (/ (log 1000) (log x)))
  (fixed-point (average-dampling (lambda (x) (/ (log 1000) (log  x))))
               2))

(xx-fixed)
