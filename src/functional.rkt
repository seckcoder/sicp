#lang racket

(define (tree-map f tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (tree-map f sub-tree)
           (f sub-tree)))
       tree))

;; take every corresponding element from each list of seq and feed them to op and combine them with initial.
(define (accumulate-n op initial seq)
  (cond ((null? seq) null)
        ((null? (car seq)) null)
        (else (cons (foldr op initial (map car seq))
                    (accumulate-n op initial (map cdr seq))))))

(provide (all-defined-out))
