#lang racket

(require "set_tree.rkt")
(require "c2_63.rkt")
(require "c2_64.rkt")

(define (union-ordered-list-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((< (car set1) (car set2))
         (cons (car set1) (union-ordered-list-set (cdr set1)
                                     set2)))
        ((> (car set1) (car set2))
         (cons (car set2) (union-ordered-list-set set1
                                     (cdr set2))))
        (else (cons (car set1) (union-ordered-list-set (cdr set1)
                                     (cdr set2))))))
(define (intersection-ordered-list-set set1 set2)
  (cond ((or (null? set1) (null? set2)) null)
        ((< (car set1) (car set2))
         (intersection-ordered-list-set (cdr set1) set2))
        ((> (car set1) (car set2))
         (intersection-ordered-list-set set1 (cdr set2)))
        (else (cons (car set1)
                    (intersection-ordered-list-set (cdr set1)
                                      (cdr set2))))))
; set1/set2 is tree
(define (union-set set1 set2)
  (let ((list1 (tree->list-2 set1))
        (list2 (tree->list-2 set2)))
    (let ((unioned-list (union-ordered-list-set list1 list2)))
      (list->tree unioned-list))))


(define (intersection-set set1 set2)
  (let ((list1 (tree->list-2 set1))
        (list2 (tree->list-2 set2)))
    (let ((intersected-list (intersection-ordered-list-set list1 list2)))
      (list->tree intersected-list))))


(provide (all-defined-out))
