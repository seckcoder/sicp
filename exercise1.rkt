#lang racket
(require "base.rkt")

;; 1.2
(define result (/ (+ 5 1/2 (- 2 3 (+ 6 1/3))) (* 3 (- 6 2) (- 2 7))))

;; 1.3

(define (square-larger a b c)
  (cond ((and (>= a b c) (>= b a c) (+ (square a) (square b))))
        ((and (>= a c b) (>= c a b) (+ (square a) (square c))))
        (else (+ (square b) (square c)))))

;; 1.6
;; Applicative order will cause sqrt-iter evaluated when it's
;; passed as arguements to new-if
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (average x y)
  (/ (+ x y) 2))


(define (improve guess x)
  (average guess (/ x guess)))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (mysqrt n)
  (sqrt-iter 1.0 n))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))


;; 1.7
;; for small numbers, the computational result is good enough.
;; However, since the guess 0.001 is 10 times of sqrt(0.00000001), which
;; is actually not a good guess.
; (good-enough? 0.001 0.00000001)
;; for large numbers
; (mysqrt 900000000000000000000000000000000000000000000000000000000000000000000000000000000000)

(define (my-new-sqrt n)
  (new-sqrt-iter 1.0 n))

(define (new-sqrt-iter guess x)
  (define new-guess (improve guess x))
  (if (new-good-enough? guess new-guess)
    new-guess
    (new-sqrt-iter new-guess x)))

(define (new-good-enough? guess new-guess)
  (> 0.001
     (/ (abs (- new-guess guess))
        guess)))
#|(sqrt 0.00001)|#
;; this will lead to endless loop
;;(mysqrt 0.00001)
;(my-new-sqrt 0.00001)

;(sqrt 900000000000000000000000000000000000000000000000000000000000000000000000000000000000)
;;(mysqrt 900000000000000000000000000000000000000000000000000000000000000000000000000000000000)
#|(my-new-sqrt 900000000000000000000000000000000000000000000000000000000000000000000000000000000000)|#

;; 1.8

(require "newton.rkt")
(define (cubic-root x)
  (newtons-method (lambda (y) (- (cube x)
                                 x))
                  1.0))

(provide (all-defined-out))
