(import (rnrs)
        (stream)
        (utils))

(define factorials (cons-stream 1
                       (stream-mult (integers-start-from 2) s)))

(println (stream-ref factorials 3))
