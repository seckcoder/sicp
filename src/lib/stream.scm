(library
  (stream)
  (export cons-stream
          stream-car
          stream-cdr
          the-empty-stream
          stream-map
          stream-filter
          stream-enumerate-interval)

  (import (rnrs base))

  (define-syntax cons-stream
    (syntax-rules ()
                  ((cons-stream a b)
                   (cons a (lambda () b)))))

  (define (stream-car stream) (car stream))

  (define (stream-cdr stream) (force (cdr stream)))

  (define the-empty-stream '())

  (define (stream-null? s)
    (null? s))

  (define (stream-map proc s)
    (if (stream-null? s)
      the-empty-stream
      (cons-stream (proc (stream-car s))
                   (stream-map proc (stream-cdr s)))))

  (define (stream-filter proc s)
    (cond ((stream-null? s) the-empty-stream)
          ((proc (stream-car s))
           (cons-stream (stream-car s)
                        (stream-filter proc (stream-cdr s))))
          (else
            (stream-filter proc (stream-cdr s)))))

  (define (stream-enumerate-interval low high)
    (if (> low high)
      the-empty-stream
      (cons-stream
        low
        (stream-enumerate-interval (+ low 1) high))))

  (define (stream-for-each proc s)
    (if (stream-null? s)
      the-empty-stream
      (begin
        (proc (stream-car s))
        (stream-for-each proc (stream-cdr s)))))
  )
