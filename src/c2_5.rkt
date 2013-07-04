#lang racket
(define (mycons a b)
  (* (expt 2 a)
     (expt 3 b)))

(define (mycar n)
  (if (= (remainder n 2) 0)
    (+ 1 (mycar (/ n 2)))
    0))

(define (mycdr n)
  (if (= (remainder n 3) 0)
    (+ 1 (mycdr (/ n 3)))
    0))

(mycar (mycons 4 3))
(mycdr (mycons 4 3))
