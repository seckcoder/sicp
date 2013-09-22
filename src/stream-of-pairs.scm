(import (rnrs)
        (stream)
        (utils)
        (prime))


(define (pairs-base s t interleave)
  (let ((s0 (stream-car s))
        (t0 (stream-car t)))
    (cons-stream (list s0 t0)
                 (interleave (stream-map (lambda (ti)
                                           (list s0 ti))
                                         (stream-cdr t))
                             (pairs-base (stream-cdr s)
                                         (stream-cdr t)
                                         interleave)))))

(define (pairs s t)
  (pairs-base s t interleave))

(define int-pairs (pairs integers integers))

(define (interleave s1 s2)
  (if (stream-null? s1)
    s2
    (cons-stream (stream-car s1)
                 (interleave s2
                             (stream-cdr s1)))))

;(define (progressive-interleave s1 s2 n1 n2)
  ;(define (split-stream s n)
    ;(define (iter s-cur s-rest n)
      ;(cond ((or (= n 0)
                 ;(stream-null? s-rest))
             ;(list s-cur s-rest))
            ;(else
              ;(iter (stream-append s-cur
                                   ;(list (stream-car s-rest)))
                    ;(stream-cdr s-rest)
                    ;(- n 1)))))
    ;(iter the-empty-stream s n))

  ;(let* ((s1-pair (split-stream s1 n1))
         ;(s1-cur (car s1-pair))
         ;(s1-rest (cadr s1-pair)))
    ;(cond ((stream-null? s1-rest)
           ;(stream-append s1-cur s2))
          ;(else
            ;(stream-append s1-cur
                           ;(progressive-interleave s2 s1-rest n2 n1))))))

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

; 3.69 unfinished
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

; (stream-display-n pythagorean-triples 5)
