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

(stream-ref (stream-filter (lambda (pair)
                             (prime? (+ (car pair)
                                        (cadr pair))))
                           int-pairs) 10)

; 3.67
(define (full-pairs s t)
  (let ((s0 (stream-car s))
        (t0 (stream-car t)))
    (cons-stream (list s0 t0)
                 (interleave (interleave (stream-map (lambda (ti)
                                                       (list s0 ti))
                                                     (stream-cdr t))
                                         (stream-map (lambda (si)
                                                       (list si t0))
                                                     (stream-cdr s)))
                             (full-pairs (stream-cdr s)
                                         (stream-cdr t))))))

(stream-ref (full-pairs integers integers) 20)

; 3.68
(define (louis-pairs s t)
  (interleave
    (stream-map (lambda (x) (list (stream-car s) x))
                t)
    (louis-pairs (stream-cdr s) (stream-cdr t))))

; (stream-display-n (louis-pairs integers integers) 10)
