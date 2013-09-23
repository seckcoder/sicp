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
                                    (stream-cdr t)
                                    )))))

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

; 3.69
(define (triples s t u)
  (cons-stream (list (stream-car s)
                     (stream-car t)
                     (stream-car u))
               (interleave (stream-map (lambda (pair)
                                         (cons (stream-car s) pair))
                                       (stream-cdr (pairs t u)))
                           (triples (stream-cdr s)
                                    (stream-cdr t)
                                    (stream-cdr u)))))

(define int-triples (triples integers integers integers))

(define pythagorean-triples (stream-filter (lambda (triple)
                                             (let ((i (car triple))
                                                   (j (cadr triple))
                                                   (k (caddr triple)))
                                               (pythagorean-triple? i j k)))
                                           int-triples))

(define (weighted-triples s t u weight)
  (cons-stream (list (stream-car s)
                     (stream-car t)
                     (stream-car u))
               (merge-weighted (stream-map (lambda (pair)
                                             (cons (stream-car s) pair))
                                           (stream-cdr (pairs t u)))
                               (triples (stream-cdr s)
                                        (stream-cdr t)
                                        (stream-cdr u)
                                        weight)
                               weight)))
; (stream-display-n pythagorean-triples 5)

(define (weighted-pairs s t weight)
  (let ((s0 (stream-car s))
        (t0 (stream-car t)))
    ; (s0 t0) must be the first, or there'll be problem for
    ; the func
    (cons-stream (list s0 t0)
                 (merge-weighted (stream-map (lambda (ti)
                                               (list s0 ti))
                                             (stream-cdr t))
                                 (weighted-pairs (stream-cdr s)
                                                 (stream-cdr t)
                                                 weight)
                                 weight))))

(define (merge-weighted s1 s2 weight)
  (cond ((stream-null? s1) s2)
        ((stream-null? s2) s1)
        (else
          (let ((s1car (stream-car s1))
                (s2car (stream-car s2)))
            ;(display s1car)(display " ")
            ;(display s2car)(newline)
            (cond ((weight s1car s2car)
                   (cons-stream s1car
                                (merge-weighted (stream-cdr s1)
                                                s2
                                                weight)))
                  ((weight s2car s1car)
                   (cons-stream s2car
                                (merge-weighted s1
                                                (stream-cdr s2)
                                                weight)))
                  (else
                    (cons-stream s1car
                                 (cons-stream s2car
                                              (merge-weighted (stream-cdr s1)
                                                              (stream-cdr s2)
                                                              weight)))))))))

; 3.70 a
(define weighted-pairs-by-a (weighted-pairs integers integers (lambda (p1 p2)
                                                                (< (+ (car p1)
                                                                      (cadr p1))
                                                                   (+ (car p2)
                                                                      (cadr p2))))))

(stream-ref weighted-pairs-by-a 10)

; 3.70 b
(define integer-not-divisible-235 (stream-filter (lambda (v)
                                                   (not (or (divisible? v 2)
                                                            (divisible? v 3)
                                                            (divisible? v 5))))
                                                 integers))

(define weighted-pairs-by-b (weighted-pairs integer-not-divisible-235
                                            integer-not-divisible-235
                                            (lambda (p1 p2)
                                              (define (sum-rule i j)
                                                (+ (* 2 i)
                                                   (* 3 j)
                                                   (* 5 i j)))
                                              (< (sum-rule (car p1)
                                                           (cadr p1))
                                                 (sum-rule (car p2)
                                                           (cadr p2))))))
; (stream-display-n weighted-pairs-by-b 10)

(define (ramanujan-sum p)
  (+ (cube (car p))
     (cube (cadr p))))
;3.71
(define ramanujan-weighted-pairs (weighted-pairs integers
                                                 integers
                                                 (lambda (p1 p2)
                                                   (< (ramanujan-sum p1)
                                                      (ramanujan-sum p2)))))


(define ramanujan-series (stream-filter (lambda (p1 p2)
                                          (= (ramanujan-sum p1)
                                             (ramanujan-sum p2)))
                                        ramanujan-weighted-pairs
                                        (stream-cdr ramanujan-weighted-pairs)))

(stream-ref (stream-map (lambda (p)
                          (ramanujan-sum (car p)))
                        ramanujan-series)
            5)
