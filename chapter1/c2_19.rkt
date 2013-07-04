#lang racket

(define (cc amount coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (null? coins)) 0)
        (else (+ (cc (- amount (car coins)) coins)
                 (cc amount (cdr coins))))))

(define us-coins '(50 25 10 5 1))
(define uk-coins '(100 50 20 10 5 2 1 0.5))
(cc 100 us-coins)
(cc 100 uk-coins)

;; the order of list coins doesnot affect the answer.
