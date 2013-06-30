#lang racket

(define (square a)
  (* a a))

(define (cube a)
  (* a a a))

(define (even? n)
  (= (remainder n 2) 0))

(define (odd? n)
  (not (even? n)))

(define (double n)
  (+ n n))

(define (halve n)
  (/ n 2))

(define (divides? a b)
  (= (remainder b a) 0))

;; c1_16.rkt
#|(define (fast-exp b n)
  (fast-exp-iter 1 b n))

(define (fast-exp-iter product b n)
  (cond ((= n 0) product)
        ((even? n) (fast-exp-iter product
                                  (square b)
                                  (/ n 2)))
        (else (fast-exp-iter (* product b)
                             b
                             (- n 1)))))|#

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


(define (filter-accumulate combiner null-value filter term a next b)
  (cond ((> a b) null-value)
        ((filter a) (combiner (term a)
                              (filter-accumulate combiner
                                          null-value
                                          filter
                                          term
                                          (next a)
                                          next
                                          b)))
        (else (filter-accumulate combiner
                                 null-value
                                 filter
                                 term
                                 (next a)
                                 next
                                 b))))

(define (accumulate combiner null-value term a next b)
  (filter-accumulate combiner
                     null-value
                     (lambda (n) true)
                     term
                     a
                     next
                     b))

;; the iterative version of accumulate
(define (filter-accumulate-iter combiner null-value filter term a next b)
  (define (iter res a)
    (cond ((> a b) res)
          ((filter a) (iter (combiner res (term a)) (next a)))
          (else (iter res (next a)))))
  (iter null-value a))

(define (accumulate-iter combiner null-value term a next b)
  (filter-accumulate-iter combiner
                          null-value
                          (lambda (x) true)
                          term
                          a
                          next
                          b))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (filter-sum term a next filter b)
  (filter-accumulate + 0 filter term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (sum-iter term a next b)
  (accumulate-iter + 0 term a next b))

(define (product-iter term a next b)
  (accumulate-iter * 1 term a next b))

;; find the first number that pass filter and in range[a,b]
(define (next a b step-func filter)
  (cond ((> a b) a)
        ((filter a) a)
        (else (next (step-func a)
                    b
                    filter))))

(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

(provide (all-defined-out))
