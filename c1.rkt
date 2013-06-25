#lang racket

;; 1.2
(define result (/ (+ 5 1/2 (- 2 3 (+ 6 1/3))) (* 3 (- 6 2) (- 2 7))))

;; 1.3

(define (square a)
  (* a a))

(define (square-larger a b c)
  (cond ((and (>= a b c) (>= b a c) (+ (square a) (square b))))
        ((and (>= a c b) (>= c a b) (+ (square a) (square c))))
        (else (+ (square b) (square c)))))

;; 1.4
