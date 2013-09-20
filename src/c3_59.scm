(import (rnrs)
        (stream)
        (utils))

(define (integrate-series s)
  (stream-div s
              (integers-start-from 1)))

; (stream-display (integrate-series (list-stream 1 2 1)))

(define exp-series
  (cons-stream 1 (integrate-series exp-series)))


(define sine-series
  (cons-stream 0 (integrate-series cosine-series)))
(define cosine-series
  (cons-stream 1 (integrate-series (negate sine-series))))

;(stream-display-n sine-series 10)(newline)
;(stream-display-n cosine-series 10)
