(import (rnrs)
        (stream)
        (utils)
        (prime))


(define (pairs s t)
  (let ((s0 (stream-car s))
        (t0 (stream-car t)))
    (cons-stream (list s0 t0)
                 (interleave (stream-map (lambda (ti)
                                           (list s0 ti))
                                         (stream-cdr t))
                             (pairs (stream-cdr s)
                                    (stream-cdr t))))))

(define int-pairs (pairs integers integers))

(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
                 (interleave s2
                             (stream-cdr s1)))))

(stream-display-n (stream-filter (lambda (pair)
                                   (prime? (+ (car pair)
                                              (cadr pair))))
                                 int-pairs)
                  10)
