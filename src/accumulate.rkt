#lang racket

(define (fold-right op initial seq)
  (if (null? seq)
    initial
    (op (car seq)
        (fold-right op initial (cdr seq)))))

(define (fold-left op initial seq)
  (define (iter result rest)
    (if (null? seq)
      null
      (iter (op (car seq) result)
            (cdr rest))))
  (iter initial seq))
