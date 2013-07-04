#lang racket

(define (my-last-pair lst)
  (cond ((null? lst) (error "list is null"))
        ((null? (cdr lst)) (list (car lst)))
        (else (my-last-pair (cdr lst)))))

(my-last-pair '(1 2 3 4))
(last-pair '(1 2 3 4))
