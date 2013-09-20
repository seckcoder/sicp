(import (rnrs)
        (stream)
        (utils))


(define (mul-series s1 s2)
  (cons-stream (* (stream-car s1) (stream-car s2))
               (add-streams (mul-series (stream-cdr s1) s2)
                            (scale-stream (stream-cdr s2) (stream-car s1)))))

; How did we get the result?
; see https://www.dropbox.com/s/qxnzmwe17kmd09z/2013-09-20%2023.45.44.jpg
