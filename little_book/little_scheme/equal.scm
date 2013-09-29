#lang racket

; eq? : takes two non-numeric atom.
; = : for numbers
; eqan?: atom equal
; eqlist?: list equal
; equal? : s-expression equal

; for real-world scheme: eq? is for reference equal
; while equal? is for value equal.

(require "base.rkt")

(define (eqan? a1 a2)
  (cond ((and (number? a1) (number? a2))
         (= a1 a2))
        ; one is number while the other is not
        ((or (number? a1) (number? a2))
         #f)
        (else (eq? a1 a2))))


(define (eqlist? l1 l2)
  (cond ((and (null? l1) (null? l2)) #t)
        ((or (null? l1) (null? l2)) #f)
        ((and (atom? (car l1))
              (atom? (car l2)))
         (and (eqan? (car l1)
                     (car l2))
              (eqlist? (cdr l1)
                       (cdr l2))))
        ((or (atom? (car l1))
             (atom? (car l2))) #f)
        (else (and (eqlist? (car l1)
                            (car l2))
                   (eqlist? (cdr l1)
                            (cdr l2))))))

(define (equal? s1 s2)
  (cond ((and (atom? s1) (atom? s2))
         (eqan? s1 s2))
        ((or (atom? s1) (atom? s2)) #f)
        (else (eqlist? s1 s2))))
