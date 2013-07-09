#lang racket

(require "base.rkt")
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))

(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cdr mobile))

(define (branch-length branch)
  (car branch))

(define (branch-structure branch)
  (cdr branch))

(define (leave-branch? branch)
  (atom? (cdr branch)))

(define (branch? branch)
  (atom? (car branch)))

(define (total-weight mobile)
  (display mobile)
  (newline)
  (if (leave-branch? mobile)
    (branch-structure mobile)
    (+ (total-weight (left-branch mobile))
       (total-weight (right-branch mobile)))))

(define (demo)
  (define br1 (make-branch 1 3))
  (define br2 (make-branch 2 4))
  (define br3 (make-branch 3
                           (make-mobile br1
                                        br2)))
  (define br4 (make-branch 4 5))
  (define mobile (make-mobile br4 br3))
  (display mobile))
  ;(total-weight mobile))
(demo)
