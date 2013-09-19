(library
  (stream)
  (export cons-stream
          list-stream
          stream-car
          stream-cdr
          the-empty-stream
          stream-map
          stream-filter
          stream-enumerate-interval
          stream-display)

  (import (rnrs)
          (utils))

  (define (memo-proc proc)
    (let ((already-run #f) (result '()))
      (lambda ()
        (if (not already-run)
          (begin (set! result (proc))
                 (set! already-run #t)
                 result)
          result))))

  (define (force delayed-object)
    (delayed-object))

  (define-syntax cons-stream
    (syntax-rules ()
      ((cons-stream a b)
       (cons a (memo-proc (lambda () b))))))

  (define-syntax list-stream
    (syntax-rules ()
      [(_) the-empty-stream]
      [(_ a b ...)
       (cons-stream a
                    (list-stream b ...))]))

  (define (stream-car stream) (car stream))

  (define (stream-cdr stream) (force (cdr stream)))

  (define the-empty-stream '())

  (define (stream-null? s)
    (null? s))

  ;  (define (stream-map proc s)
  ;(if (stream-null? s)
  ;the-empty-stream
  ;(cons-stream (proc (stream-car s))
  ;(stream-map proc (stream-cdr s)))))


  ; (stream-map proc stream stream ...)
  (define (stream-map proc . stream-args)
    (if (stream-null? (car stream-args))
      the-empty-stream
      (cons-stream (apply proc (map stream-car stream-args))
                   (apply stream-map (cons proc (map stream-cdr stream-args))))))

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

  (define (stream-display s)
    (if (not (stream-null? s))
      (begin
        (println (stream-car s))
        (stream-display (stream-cdr s)))))
  )
