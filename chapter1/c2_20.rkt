#lang racket

(define (same-parity fv . restvs)
  (append (list fv)
          (if (even? fv)
            (filter even? restvs)
            (filter odd? restvs))))

(same-parity 1 2 3 4 5 6 7)
(same-parity 2 3 4 5 6 7)
