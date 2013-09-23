(import (rnrs)
        (rnrs r5rs)
        (stream))

(define (solve f y0 dt)
  (define y (integral-delayed (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

; y = e^x
(display (stream-ref (solve (lambda (y) y) 1 0.001) 1000))(newline)


; 3.77
(define (integral-recur delayed-integrand initial-value dt)
  (cons-stream initial-value
               (let ((integrand (force delayed-integrand)))
                 (if (stream-null? integrand)
                   the-empty-stream
                   (integral-recur (delay (stream-cdr integrand))
                                   (+ (* dt (stream-car integrand))
                                      initial-value)
                                   dt)))))

(define (solve-use-recur f y0 dt)
  (define y(integral-recur (delay dy) y0 dt))
  (define dy (stream-map f y))
  y)

(stream-ref (solve-use-recur (lambda (y) y) 1 0.001) 1000)

; 3.78
(define (solve-2nd a b dt y0 dy0)
  (define y (integral-delayed (delay dy) y0 dt))
  (define dy (integral-delayed (delay ddy) dy0 dt))
  (define ddy (stream-add (stream-scale dy a)
                          (stream-scale y b)))
  y)

; y = e^x, solve value at x = 1000 * 0.001 = 1
(display (stream-ref (solve-2nd 2 -1 0.001 1 1) 1000))(newline)
; 3.79

(define (solve-2nd-general f y0 dy0 dt)
  (define y (integral-delayed (delay dy) y0 dt))
  (define dy (integral-delayed (delay ddy) dy0 dt))
  (define ddy (stream-map f dy y))
  y)

; y = e^x, f(dy,y) = 2*dy - y
(display (stream-ref (solve-2nd-general (lambda (dy y)
                                 (- (* 2 dy)
                                    y))
                               1
                               1
                               0.001)
            1000))(newline)

; 3.80 unfinished
