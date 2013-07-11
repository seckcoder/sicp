#lang racket

(define (reverse1 seq)
  (foldr (lambda (x y) (append y (list x)))
         null
         seq))

(define (reverse2 seq)
  (foldl (lambda (x y) (cons x y))
         null 
         seq))

(reverse1 '(1 2 3))
(reverse2 '(1 2 3))
