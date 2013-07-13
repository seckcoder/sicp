#lang racket

(require "set_tree.rkt")

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

; O(n)
(define (partial-tree elts n)
  (if (= n 0) (cons null elts)
    (let* ((left-size (quotient (sub1 n) 2))
           (left-result (partial-tree elts left-size))
           (left-tree (car left-result))
           (non-left-elts (cdr left-result))
           (right-size ( - n (add1 left-size)))
           (this-entry (car non-left-elts))
           (right-result (partial-tree (cdr non-left-elts)
                                       right-size))
           (right-tree (car right-result))
           (remaining-elts (cdr right-result)))
      (cons (make-tree this-entry left-tree right-tree)
            remaining-elts))))


;(list->tree '(1 3 5 7 9 11))
(provide list->tree)
