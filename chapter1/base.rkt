#lang racket

(define (square a)
  (* a a))

(define (cube a)
  (* a a a))

(define (even? n)
  (= (remainder n 2) 0))

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (divides? a b)
  (= (remainder b a) 0))

#|(define (fast-exp b n)|#
  ;(fast-exp-impl 1 b n))

;(define (fast-exp-impl product b n)
  ;(display (cons b n))
  ;(cond ((= n 0) 1)
        ;((even? n) (fast-exp-impl product (square b) (/ n 2)))
        #|(else (fast-exp-impl (* product b) b (- n 1)))))|#

(define (fast-exp b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-exp b (/ n 2))))
        (else (* b (fast-exp b (- n 1))))))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp) (remainder (square (expmod base (/ exp 2) m))
                                m))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))

;; check whether a^n % n = a(Ie, a^n and a are congruent modulo n, or a^n = a (mod n))
; here expmod is much more efficient than fast-exp
; since for expmod, it tries to reduce the base which
; greatly reduce the time complexity for large numbers.
(define (congruent-modulo? n a)
  ;(= (remainder (fast-exp a n) n) a))
  (= (expmod a n n) a))

(provide (all-defined-out))
