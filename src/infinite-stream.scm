(import (rnrs)
        (stream)
        (utils))

(define (fibgen a b)
  (cons-stream a (fibgen b (+ a b))))

;(define fibs (fibgen 0 1))

(define (seive s)
  (cons-stream
    (stream-car s)
    (seive (stream-filter
             (lambda (v)
               (not (divisible? v (stream-car s))))
             (stream-cdr s)))))

(define primes (seive (integers-start-from 2)))

(define ones
  (cons-stream 1 ones))

(define integers
  (cons-stream 1 (stream-add ones integers)))

; think about how to get the equation of geometric progression!!!
(define fibs (cons-stream 0
                          (cons-stream 1
                                       (stream-add (stream-cdr fibs)
                                                   fibs))))


(define double (cons-stream 1 (stream-scale double 2)))
