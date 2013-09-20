(import (rnrs)
        (stream)
        (utils))

(define primes (cons-stream 2
                            (stream-filter prime? (integers-start-from 3))))

(define (prime? n)
  (define (iter ps)
    (cond ((> (square (stream-car ps)) n) #t)
          ((divisible? n (stream-car ps)) #f)

(println (stream-ref primes 50))
