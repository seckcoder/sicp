#lang racket
(define (f g)
  (g 2))

(f f)
;; => (f 2)
;; => (2 2)
;; => error.
