#lang racket

(define (subsets s)
  (if (null? s)
    (list '())
    (let ((r (subsets (cdr s))))
      (append r (map (lambda (x)
                          (cons (car s) x))
                        r)))))

(subsets '(1 (2 3)))
;(subsets '(1 2 3))
