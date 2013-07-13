#lang racket

; sets as ordered lists

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (car set)) #t)
        ((< x (car set)) #f)
        (else (element-of-set? x (cdr set)))))

(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((< (car set1) (car set2))
         (cons (car set1) (union-set (cdr set1)
                                     set2)))
        ((> (car set1) (car set2))
         (cons (car set2) (union-set set1
                                     (cdr set2))))
        (else (cons (car set1) (union-set (cdr set1)
                                     (cdr set2))))))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) null)
        ((< (car set1) (car set2))
         (intersection-set (cdr set1) set2))
        ((> (car set1) (car set2))
         (intersection-set set1 (cdr set2)))
        (else (cons (car set1)
                    (intersection-set (cdr set1)
                                      (cdr set2))))))

(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< x (car set)) (cons x set))
        ((= x (car set)) set)
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(provide (all-defined-out))
