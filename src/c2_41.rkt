#lang racket

(require "functional.rkt")
(define (ordered-pairs n)
  (flatmap (lambda (i)
             (map (lambda (j)
                    (list i j))
                  (enumerate-interval 1 n)))
           (enumerate-interval 1 n)))

(define (ordered-triples n)
  (flatmap (lambda (pair)
             (map (lambda (j)
                    (cons j pair))
                  (enumerate-interval 1 n)))
           (ordered-pairs n)))

(define (sum triples)
  (foldl + 0 triples))

(define (sum-equal-triples n s)
  (filter (lambda (triple)
            (= (sum triple) s))
          (ordered-triples n)))

(sum-equal-triples 5 10)
