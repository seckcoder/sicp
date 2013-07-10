#lang racket

(require "functional.rkt")
(define (dot-product v w)
  (foldl + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w)
         (dot-product v w))
       m))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (w)
           (matrix-*-vector cols w))
         m)))

(define (transpose m)
  (accumulate-n cons null m))

(define v1 '(1 2 3))
(define m1 '((1 0 0)
             (0 1 0)
             (0 0 1)))

(define m2 '((1 0 1)
             (1 1 1)
             (0 0 1)))

(dot-product v1 v1)
(matrix-*-vector m1 v1)
(matrix-*-matrix m2 m2)
(transpose m2)
