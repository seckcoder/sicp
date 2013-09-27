(library
  (series)
  (export mul-series
          add-series
          integrate-series
          exp-series
          sine-series
          cosine-series
          invert-unit-series
          div-series
          tan-series
          pi-series
          euler-transform
          accelerated-sequence)


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

  (define (div-series s1 s2)
    (let ((c2 (stream-car s2)))
      (if (= c2 0)
        (error div-series "series haszero constant term")
        (mul-series (stream-scale s1 (/ 1 c2))
                    (invert-unit-series (stream-scale s2 (/ 1 c2)))))))

  (define tan-series
    (div-series sine-series cosine-series))

  (define (euler-transform s)
    (let ((s0 (stream-ref s 0))
          (s1 (stream-ref s 1))
          (s2 (stream-ref s 2)))
      (cons-stream (- s2 (/ (square (- s2 s1))
                            (+ s0 (* -2 s1) s2)))
                   (euler-transform (stream-cdr s)))))

  (define (make-tableau transform s)
    (cons-stream s
                 (make-tableau transform
                               (transform s))))
  (define (accelerated-sequence transform s)
    (stream-map stream-car
                (make-tableau transform s)))

  (define (pi-summands n)
    (cons-stream (/ 1.0 n)
                 (stream-map - (pi-summands (+ n 2)))))
  (define pi-series (pi-summands 1))

  )
