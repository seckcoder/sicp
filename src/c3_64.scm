(import (rnrs)
        (stream)
        (utils))

(define (sqrt-improve guess x)
  (average guess (/ x guess)))

(define (sqrt-stream x)
  (define guesses
    (cons-stream 1.0
                 (stream-map (lambda (guess)
                               (sqrt-improve guess x))
                             guesses)))
  guesses)


(define (stream-limit s tolerance)
  (let ((v1 (stream-car s))
        (v2 (stream-car (stream-cdr s))))
    (if (< (abs (- v1 v2))
           tolerance)
      v1
      (stream-limit (stream-cdr s) tolerance))))

(define (mysqrt x tolerance)
  (stream-limit (sqrt-stream x) tolerance))

(println (mysqrt 4 0.001))
