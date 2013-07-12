#lang racket
; the eight-queens puzzle

(require "base.rkt")
(require "functional.rkt")
(require "sort.rkt")

(define (point-x p)
  (car p))

(define (point-y p)
  (cadr p))

(define (make-point x y)
  (list x y))

(define (align? p1 p2)
  (or (= (point-x p1)
         (point-x p2))
      (= (point-y p1)
         (point-y p2))
      (= (- (point-x p1) (point-x p2))
         (- (point-y p1) (point-y p2)))))

(define (point-solution-conflict? p tmp-solution)
  (cond ((null? tmp-solution) #f)
        ((align? p (car tmp-solution)) #t)
        (else (point-solution-conflict? p (cdr tmp-solution)))))

(define (solution? solution)
  (if (null? solution)
    #t
    (let ((p1 (car solution))
          (last-solution (cdr solution)))
      (not (point-solution-conflict? p1 last-solution)))))

(define (queen)
  (define (queen-cls x)
    (cond ((<= x 0) (list '()))
          (else (flatmap (lambda (cols-solution)
                           (define (make-new-solutions)
                             (map (lambda (y)
                                    (cons (make-point x y) cols-solution))
                                  (enumerate-interval 1 8)))
                           (filter solution? (make-new-solutions)))
                         (queen-cls (sub1 x))))))
  (queen-cls 8))

(define (find-lst lst lst-of-lst)
  (cond ((null? lst-of-lst) #f)
        ((equal? lst (car lst-of-lst)) #t)
        (else (find-lst lst (cdr lst-of-lst)))))

;; find a right solution in queen solutions
(find-lst '((8 6) (7 4) (6 1) (5 5) (4 8) (3 2) (2 7) (1 3)) (queen))
