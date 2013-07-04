#lang racket

(define (make-rat n d)
  (cond ((or (and (< n 0) (< d 0)) (and (< n 0) (> d 0)))
            (make-rat (- n) (- d)))
        (else (let ((g (gcd n d)))
                (cons (/ n g) (/ d g))))))

(define numer car)
(define denom cdr)

(numer (make-rat 3 3))
(denom (make-rat 3 3))
(numer (make-rat -3 3))
(denom (make-rat -3 3))
(numer (make-rat 3 -3))
(denom (make-rat 3 -3))
(numer (make-rat -3 -3))
(numer (make-rat 3 -3))
