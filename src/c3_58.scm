(import (rnrs)
        (stream)
        (utils))

(define (expand num den radix)
  (cons-stream
    (quotient (* num radix) den)
    (expand (remainder (* num radix) den) den radix)))

(stream-display-n (expand 1 7 10) 10)
(newline)
(stream-display-n (expand 3 8 10) 5)
