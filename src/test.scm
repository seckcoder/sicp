(import (rnrs)
        (stream)
        (utils))

(stream-display (stream-append (list-stream 1 2 3)
                   (list-stream 2 3 4)))
