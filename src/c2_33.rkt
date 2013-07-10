#lang racket

(require "base.rkt")

(define (mymap p sequence)
  (foldr (lambda (elem sum)
           (cons (p elem) sum))
         null
         sequence))

(define (myappend seq1 seq2)
  (foldr cons seq2 seq1))

(define (my-length sequence)
  (foldl (lambda (elem sum)
                (+ 1 sum))
              0
              sequence))
(mymap square '(1 2 3))
(myappend '(1 2 3) '(4 5 6))
(my-length '(1 2 3))
