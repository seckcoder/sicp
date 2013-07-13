#lang racket
(require "huffman_encoding.rkt")

(define freq-pairs '((A 2) (BOOM 1) (GET 2) (JOB 2) (NA 16) (SHA 3) (YIP 9) (WAH 1)))

(define tree (make-huffman-tree freq-pairs))

(define message '(GET A JOB SHA NA NA NA NA NA NA NA NA GET A JOB SHA NA NA NA NA NA NA NA NA WAH YIP YIP YIP YIP YIP YIP YIP YIP YIP SHA BOOM))
(display "huffman encoding length: ")
(length (encode message tree))
(newline)
(display "fixed length code length: ")
(* 3 (length message))
