#lang racket

(define (scale-tree tree factor)
  (cond ((null? tree) null)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

(define (scale-tree-another tree factor)
  (map (lambda (sub-tree)
         (if (pair? (sub-tree))
           (scale-tree-another sub-tree factor)
           (* sub-tree factor)))
       tree))
