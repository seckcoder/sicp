#lang racket

(define (f n method)
  (cond ((equal? method "recursive") (f-recur n))
        ((equal? method "iterative") (f-iter n))))

(define (f-recur n)
  (if (< n 3)
    n
    (+ (f-recur (- n 1))
       (* 2 (f-recur (- n 2)))
       (* 3 (f-recur (- n 3))))))

(define (f-iter n)
  (if (< n 3)
    n
    (f-iter-dumy 0 1 2 n)))

(define (f-iter-dumy a b c n)
  (if (< n 3)
    c
    (f-iter-dumy b c (+ (* 3 a)
                        (* 2 b)
                        c) (- n 1))))

(provide f)
