(import (rnrs)
        (stream)
        (utils))

(define x (stream-map println (stream-enumerate-interval 0 10)))

(newline)
(stream-ref x 5)
(newline)
(stream-ref x 7)
