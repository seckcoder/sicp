#lang racket


(define (select-lst b lst cmp)
  (filter (lambda (a)
            (cmp a b))
          lst))

(define (equal-v? v1 v2 cmp)
  (not (or (cmp v1 v2)
            (cmp v2 v1))))

(define (qsort lst cmp)
  (cond ((null? lst) null)
        ((null? (cdr lst)) lst)
        (else (let ((mark (car lst)))
                (append (qsort (select-lst mark
                                           lst
                                           cmp)
                               cmp)
                        (qsort (select-lst mark
                                           lst
                                           (lambda (v1 v2)
                                             (equal-v? v1 v2 cmp)))
                               cmp)
                        (qsort (select-lst mark
                                           lst
                                           (lambda (v1 v2)
                                             (cmp v2 v1))) cmp)
                        )))))

(provide (all-defined-out))
