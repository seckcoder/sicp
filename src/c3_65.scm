(import (rnrs)
        (series)
        (stream)
        (utils))

(define (log2-summands n)
  (cons-stream (/ 1.0 n)
               (stream-negate (log2-summands (+ n 1)))))

(define log2-series (log2-summands 1))

(println (log 2))

(define log2 (partial-sum log2-series))
(stream-display-n log2 10)(newline)

(stream-display-n (euler-transform log2) 10)(newline)

(stream-display-n (accelerated-sequence euler-transform
                                        log2)
                  10)
