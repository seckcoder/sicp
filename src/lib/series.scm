(library
  (series)
  (export mul-series
          add-series
          integrate-series
          exp-series
          sine-series
          cosine-series
          invert-unit-series)

  (import (rnrs)
          (stream)
          (utils))

  (define (integrate-series s)
    (stream-div s
                (integers-start-from 1)))
  (define sine-series
    (cons-stream 0 (integrate-series cosine-series)))
  (define cosine-series
    (cons-stream 1 (integrate-series (stream-negate sine-series))))
  (define exp-series
    (cons-stream 1 (integrate-series exp-series)))
  (define (mul-series s1 s2)
    (cons-stream (* (stream-car s1) (stream-car s2))
                 (stream-add (mul-series (stream-cdr s1) s2)
                             (stream-scale (stream-cdr s2) (stream-car s1)))))
  (define add-series stream-add)
  (define minus-series stream-minus)
  (define (invert-unit-series s)
    (define inverted-series
      (cons-stream 1
                   (stream-negate
                     (mul-series (stream-cdr s)
                                 inverted-series))))
    inverted-series)
  )
