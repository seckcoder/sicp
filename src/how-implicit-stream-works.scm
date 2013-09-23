; take partial-sum as example

(define (partial-sum s)
  (define sum (cons-stream (stream-car s)
                           (stream-add (stream-cdr s)
                                       sum)))
  sum)

(define ones (cons-stream 1 ones))

(partial-sum ones) ->

(partial-sum (cons-stream 1 ones)) ->

(cons-stream (stream-car (cons-stream 1 ones))
             (stream-add (stream-cdr (cons-stream 1 ones)
                         sum)) ->

; intepret first args of stream-add
(cons-stream 1
             (stream-add (cons-stream 1 ones)
                         sum)) ->

; interpret second args of stream-add
; use ... to replace the delayed part of sum
(cons-stream 1
             (stream-add (cons-stream 1 ones)
                         (cons-stream 1 ...))) ->

(cons-stream 1
             (cons-stream (+ (stream-car (cons-stream 1 ones))
                             (stream-car (cons-stream 1 ...)))
                          (stream-map +
                                      (stream-cdr (cons-stream 1 ones))
                                      (stream-cdr (cons-stream 1 ...))))) ->

(cons-stream 1
             (cons-stream 2
                          (stream-map +
                                      (cons-stream 1 ones)
                                      (cons-stream 2 ...))))  ->

(cons-stream 1
             (cons-stream 2
                          (cons-stream (+ (stream-car (cons-stream 1 ones))
                                          (stream-car (cons-stream 2 ...)))
                                       (stream-map +
                                                   (stream-cdr (cons-stream 1 ones))
                                                   (stream-cdr (cons-stream 2 ...)))))) ->

(cons-stream 1
             (cons-stream 2
                          (cons-stream 3
                                       (stream-map +
                                                   (cons-stream 1 ones)
                                                   (cons-stream 3 ...)))))

...
