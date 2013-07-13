#lang racket

(require "huffman_encoding.rkt")

(define sample-tree (make-huffman-tree '((A 8) (B 3) (C 1) (D 1) (E 1) (F 1) (G 1) (H 1))))
(define freq-pairs '((A 8) (C 1) (D 1) (E 1) (F 1)))

(define sample-message '(B A C A D A E A F A B B A A A G A H)) 

;(encode sample-message sample-tree)
(decode (encode sample-message sample-tree) sample-tree)
