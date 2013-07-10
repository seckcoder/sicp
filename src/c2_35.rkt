#lang racket

; count number of leaves with foldl
(define (count-leaves tree)
  (foldl (lambda (sub-tree sum)
           (+ sum (cond ((null? sub-tree) 0)
                        ((not (pair? sub-tree)) 1)
                        (else (count-leaves sub-tree)))))
         0
         tree))

(define (count-leaves1 tree)
  (foldl +
         0
         (map (lambda (sub-tree)
                (cond ((null? sub-tree) 0)
                      ((not (pair? sub-tree)) 1)
                      (else (count-leaves1 sub-tree))))
              tree)))

(define x (cons (list 1 2) (list 3 4 5)))
(count-leaves x)
(count-leaves1 x)
