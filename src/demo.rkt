#lang racket


(require "set_tree.rkt")
(require "set_balanced_tree.rkt")

(define tree1 (adjoinlist-set '(7 3 1 5 9 11) null))

(define tree2 (adjoinlist-set '(3 1 7 5 9 11) null))

(define tree3 (adjoinlist-set '(5 3 1 9 7 11) null))

(define tree4 (adjoinlist-set '(10 6 34) null))

(union-set tree3 tree4)
