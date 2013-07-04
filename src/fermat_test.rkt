#lang racket

(require "base.rkt")
(require racket/block)

(define (fermat-test n)
  (define (try-it a)
    (congruent-modulo? n a))
  ;; since random doesnot accept random number greater than 4294967087
  ;; for numbers not too much larger than 4294967087, this test seems 
  ;; to be enough
  (try-it (+ 1 (random (min (- n 1) 4294967087)))))

(define (prime? n)
  (impprime? n 10))

(define (impprime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (impprime? n (- times 1)))
        (else false)))

(provide prime?)

(define (test)
    (display (prime? 109)))
(test)
