#lang racket

(define (myreverse lst)
  (if (null? lst)
    null
    (append (myreverse (cdr lst))
            (list (car lst)))))

(myreverse '(1 2 3 4))
