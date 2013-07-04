#lang racket

(require "base.rkt")

(define make-point cons)
(define x-point car)
(define y-point cdr)
(define make-segment cons)
(define start-segment car)
(define end-segment cdr)

(define (length-points p1 p2)
  (sqrt (+ (square (- (x-point p1)
                      (x-point p2)))
           (square (- (y-point p1)
                      (y-point p2))))))

(define (length-segment segment)
  (length-points (start-segment segment)
                 (end-segment segment)))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define (midpoint-segment segment)
  (let* ((start (start-segment segment))
         (stop (end-segment segment)))
    (midpoint start stop)))

(define (midpoint start stop)
  (make-point (average (x-point start)
                       (x-point stop))
              (average (y-point start)
                       (y-point stop))))

(provide (all-defined-out))
