#lang racket
(require "functional.rkt")

; flatmap should receive a proc that returns a list. 
(flatmap (lambda (v)
           (list v v))
         '(1 2 3))

; what it actually does is to cons erase the second level brackets
; Example:
;  ((1 2 3) (2 3 4) (4 5 6)) -> erase second level -> (1 2 3 2 3 4 4 5 6)
(flatmap (lambda (v) v)
         '((1 2 3) (3 2 1)))

(flatmap (lambda (v) v)
         '((1 2 3)))
