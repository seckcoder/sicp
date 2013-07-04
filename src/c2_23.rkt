#lang racket

(require racket/block)
(define (my-for-each proc lst)
  (if (null? lst)
    null  ;; how to return nothing?
    (block
      (proc (car lst))
      (my-for-each proc (cdr lst)))))


(define (just-print x)
  (display x)
  (display " "))

(define result (my-for-each just-print '(1 2 3 4)))

(for-each just-print '(1 2 3 4))
