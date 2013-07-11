#lang racket
(define (fringe tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

(define x '((1 2) (3 4)))
(fringe x)
(fringe (list x x))
(fringe 3)
