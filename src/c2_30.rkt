#lang racket

(require "base.rkt")
(require "functional.rkt")

(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
           (square-tree sub-tree)
           (square sub-tree)))
       tree))

(define (square-tree-another tree)
  (cond ((null? tree) null)
        ((not (pair? tree)) (square tree))
        (else (cons (square-tree-another (car tree))
                    (square-tree-another (cdr tree))))))

(define (square-tree-use-tree-map tree)
  (tree-map square tree))

(define tree '(1 (2 (3 4) 5) (6 7)))
(square-tree tree)
(square-tree-another tree)
(square-tree-use-tree-map tree)
