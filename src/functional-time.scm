(import (rnrs)
        (stream)
        (utils))

(define (make-simplified-withdraw balance)
  (lambda (amount)
    (set! balance (- balance amount))
    balance))

(define (stream-widthdraw balance amount-stream)
  (define balance-stream
    (cons-stream balance
                 (stream-minus balance-stream
                               amount-stream)))
  balance-stream)

(let ((bs (stream-widthdraw 1000 (list-stream 1 2 3 4))))
  (display (stream-ref bs 4)))
