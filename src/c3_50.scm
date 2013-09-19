(import (rnrs)
        (stream)
        (utils))


(stream-display (stream-map square (list-stream 1 2 3)))

(stream-display (stream-map + (list-stream 1 2 3) (list-stream 2 3 4)))
