#lang racket

(require "base.rkt")
;(require "prime.rkt")
(require "fermat_test.rkt")
(require racket/block)

(define (search-for-primes lower-limit n)
  (if (even? lower-limit)
    (search-for-primes-dummy (+ 1 lower-limit) n)
    (search-for-primes-dummy lower-limit n)))

(define (search-for-primes-dummy lower-limit n)
  (cond ((= n 0) true)
        ((prime? lower-limit)
         (block
           (display lower-limit)
           (newline)
           (search-for-primes-dummy (+ 2 lower-limit) (- n 1))))
        (else (search-for-primes-dummy (+ 2 lower-limit) n))))


(define (test)
  (define (evaluates lst)
    (if (null? lst)
      (print "done!\n")
      (block
        (time (search-for-primes (car lst) 3))
        (evaluates (cdr lst)))))
  (evaluates '(100000000000 1000000000000 10000000000000 100000000000000)))
  ;(evaluates '(1000000000 10000000000 100000000000 1000000000000)))
    
(test)
;; the result shows that the algo has order of growth of O(√n)
