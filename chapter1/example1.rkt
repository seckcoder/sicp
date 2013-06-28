#lang racket
;; 1.1.7 Square Roots by Newton's method


;; mysqrt
(define (mysqrt n)
  (sqrt-iter 1 n))

(define (sqrt-iter guess x)
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (improve guess x)
  (average guess (/ x guess)))

(define (square x)
  (* x x))

(define (average x y)
  (/ (+ x y) 2))
