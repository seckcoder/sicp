(import (rnrs)
        (stream)
        (utils))


(define fibs (cons-stream 0
                          (cons-stream 1
                                       (stream-add (stream-cdr fibs)
                                                   fibs))))

(stream-ref fibs 7)

; n additions are performed when optimization provided.
; Think about dynamic programming. (stream-ref fibs n) = (stream-ref fibs n-1) + (stream-ref fibs n-2)
; when no memoize enabled, for each (stream-ref fibs n-1), it will be recalculated.
