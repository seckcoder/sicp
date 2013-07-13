#lang racket

(require "huffman_encoding.rkt")

(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                    (make-leaf 'B 2)
                    (make-code-tree (make-leaf 'D 1)
                                    (make-leaf 'C 1)))))

(define sample-bits '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(define sample-message '(A D A B B C A))
(encode sample-message sample-tree)
