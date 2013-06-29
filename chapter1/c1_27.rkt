#lang racket
(require "prime.rkt")
(require "base.rkt")
(require racket/block)

;; a number is carmichael number if it fools the fermal test
;; ie it's not a prime number but it's congruent module to every number a < n
(define (carmichael? n)
  (and (not (prime? n)) (congruent-every? n)))

;; check whether a^n % a = n for every a < n
(define (congruent-every? n)
  (define (iter a)
    (cond ((>= a n) true)
          ((congruent-modulo? n a) (iter (+ a 1)))
          (else false)))
  (iter 2))

(define (test)
  (define (iter lst)
    (if (null? lst)
      (display "Done!\n")
      (block
        (display (car lst))
        (newline)
        (display (carmichael? (car lst)))
        (newline)
        (display "***")
        (iter (cdr lst)))))
  (iter '(561 1105 1729 2465 2821 6601)))

;; the test shows that those numbers are carmichael number.
(test)
