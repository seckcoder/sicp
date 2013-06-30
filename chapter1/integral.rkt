#lang racket
(require "base.rkt")

(define (integral-1 f a b dx)
  (* (sum f
          (+ a (/ dx 2.0))
          (lambda (x) (+ x dx))
          b)
     dx))

;; using simpson's rule
(define (integral-2 f a b n)
  (define dx (/ (- b a) n))
  (define (factor k)
    (cond ((or (= k 0) (= k n)) 1)
          ((even? k) 2)
          (else 4)))

  (define (y k)
    (f (+ a (* k dx))))
  (define (term k)
    (* (factor k)
       (y k)))
   
  (if (odd? n)
    (error "n should be even number")
    (* (sum term
            0
            (lambda (k) (+ 1 k))
            n)
            (/ dx 3.0))))
 
(integral-1 cube 0 1 0.01)
(integral-2 cube 0 1 100)
(integral-2 cube 0 1 1000)
