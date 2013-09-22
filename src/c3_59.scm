(import (rnrs)
        (stream)
        (utils))

; (stream-display (integrate-series (list-stream 1 2 1)))

(stream-display-n sine-series 10)(newline)
(stream-display-n cosine-series 10)
