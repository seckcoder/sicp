; X0 = 1, Xn+1 = 2Xn

(import (rnrs)
        (stream)
        (utils))

(define s (cons-stream 1 (stream-add s s)))

(println (stream-ref s 10)) ; 1024
