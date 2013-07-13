#lang racket

(define (entry tree) (car tree))
(define (left-branch tree) (cadr tree))
(define (right-branch tree) (caddr tree))
(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) #f)
        ((= x (entry set)) #t)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        (else (element-of-set? x
                               (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x null null))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        (else
          (make-tree (entry set)
                     (left-branch set)
                     (adjoin-set x (right-branch set))))))

(define (adjoinlist-set lst set)
  (cond ((null? lst) set)
        (else (adjoinlist-set (cdr lst)
                              (adjoin-set (car lst)
                                          set)))))

(provide (all-defined-out))
