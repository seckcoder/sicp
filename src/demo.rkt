#lang racket


(require "base.rkt")

(pair? '(1)) ; a not null list
(list? '(2)) ;; a null or non-null list
(atom? 3)
(atom? '()) ;; '() is null
(null? 2)

(append '(1 2 3) null)

