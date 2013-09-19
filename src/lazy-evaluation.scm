(import (rnrs base)
        (rnrs mutable-pairs)
        (prime)
        (functional))

(define-syntax cons-stream
  (syntax-rules ()
    ((cons-stream a b)
     (cons a (lambda () b)))))

(define (stream-car stream) (car stream))

(define (stream-cdr stream) (force (cdr stream)))

(define the-empty-stream '())

(define (stream-null? s)
  (eq? s '()))

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

;(car (cdr (filter prime?
                  ;(enumerate-interval 10000 10000000))))
                  
; slow
(display "run slow one")(newline)
(car (cdr (filter prime?
                  (enumerate-interval 10000 1000000))))

(display "lazy evaluation")(newline)
(stream-car 
  (stream-cdr (stream-filter prime?
                             (stream-enumerate-interval 10000 1000000))))
