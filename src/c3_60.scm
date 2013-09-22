(import (rnrs)
        (stream)
        (utils)
        (series))

(stream-display-n (stream-add (mul-series sine-series sine-series)
                               (mul-series cosine-series cosine-series))
                  10)
; How did we get the result?
; see https://www.dropbox.com/s/qxnzmwe17kmd09z/2013-09-20%2023.45.44.jpg
