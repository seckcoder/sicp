#lang racket

(define runtime current-milliseconds)

(define (timer f)
  (let ((startime (runtime)))
    (define value (f))
    (report-time startime)
    value))

(define (report-time starttime)
  (display " *** ")
  (display (- (runtime) starttime)))
