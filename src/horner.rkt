#lang racket


(define (horner-eval x coefficient-sequence)
  ; foldr is equivalent to accumulate
  ; that is accumulate from right of sequence(opposite to foldl)
  (foldr (lambda (elem sum)
           (+ elem
              (* sum x)))
         0
         coefficient-sequence))

(provide horner-eval)
