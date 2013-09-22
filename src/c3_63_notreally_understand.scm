(import (rnrs)
        (stream)
        (utils))


;(stream-display-n (partial-sum1 integers) 3)
(println (stream-ref (partial-sum integers) 3))


(partial-sum integers) ->
(cons-stream (stream-car s)
             (cons-stream (+ (stream-car (stream-cdr s))
                             (stream-car sum))
                          (cons-stream (+ (stream-car (stream-cdr (stream-cdr s)))
                                          (stream-car (stream-cdr sum))))))
                          ...

(define (partial-sum s)
  (cons-stream (stream-car s)
               (stream-add (stream-cdr s)
                           (partial-sum s))))
(partial-sum integers) ->

(cons-stream (stream-car s)
             (stream-add (stream-cdr s)
                         (cons-stream (stream-car s)
                                      (stream-add (stream-cdr s)
                                                  (partial-sum s))))) ->
(cons-stream (stream-car s)
             (cons-stream (+ (stream-car (stream-cdr s))
                             (stream-car (cons-stream (stream-car s)
                                                      (stream-add (stream-cdr s)
                                                                  (partial-sum s)))))
                          (stream-map +
                                      (stream-cdr (stream-cdr s))
                                      (stream-cdr (cons-stream (stream-car s)
                                                               (stream-add (stream-cdr s)
                                                                           (partial-sum s)))))))
