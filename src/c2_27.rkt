#lang racket

(define (deep-reverse lst)
  (cond ((not (pair? lst)) lst)
        (else (append (deep-reverse (cdr lst))
                      (list (deep-reverse (car lst)))))))


(deep-reverse null)
(deep-reverse 3)
(deep-reverse '(1 2 3))
(deep-reverse '((1 2 3) 4 (5 6 (7 9))))
